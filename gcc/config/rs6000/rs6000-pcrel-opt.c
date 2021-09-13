/* Subroutines used support the pc-relative linker optimization.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* This file implements a RTL pass that looks for pc-relative loads of the
   address of an external variable using the PCREL_GOT relocation and a single
   load that uses that external address.  If that is found we create the
   PCREL_OPT relocation to possibly convert:

	pld addr_reg,var@pcrel@got

	<possibly other insns that do not use 'addr_reg' or 'data_reg'>

	lwz data_reg,0(addr_reg)

   into:

	plwz data_reg,var@pcrel

	<possibly other insns that do not use 'addr_reg' or 'data_reg'>

	nop

   Of course it would be nice to be able to put the plwz in this example in
   place of the lwz but the linker cannot easily replace a 4-byte instruction
   with an 8-byte one.

   If the variable is not defined in the main program or the code using it is
   not in the main program, the linker puts the address in the .got section and
   generates:

		.section .got
	.Lvar_got:
		.dword var

   At the point where it is referenced, we have:

		.section .text
		pld addr_reg,.Lvar_got@pcrel

		<possibly other insns that do not use 'addr_reg' or 'data_reg'>

		lwz data_reg,0(addr_reg)

   We look for a single usage in the basic block where this external
   address is loaded, and convert it to a PCREL_OPT relocation so the
   linker can convert it to a single plwz in this case.  Multiple uses
   or references in another basic block will force us to not use the
   PCREL_OPT relocation.

   We also optimize stores to the address of an external variable using the
   PCREL_GOT relocation and a single store that uses that external address.  If
   that is found we create the PCREL_OPT relocation to possibly convert:

	pld addr_reg,var@pcrel@got

	<possibly other insns that do not use 'addr_reg' or 'data_reg'>

	stw data_reg,0(addr_reg)

   into:

	pstw data_reg,var@pcrel

	<possibly other insns that do not use 'addr_reg' or 'data_reg'>

	nop

   If the variable is not defined in the main program or the code using it is
   not in the main program, the linker puts the address in the .got section and
   generates:

		.section .got
	.Lvar_got:
		.dword var

   And at our point of reference we have:

		.section .text
		pld addr_reg,.Lvar_got@pcrel

		<possibly other insns that do not use 'addr_reg' or 'data_reg'>

		stw data_reg,0(addr_reg)

   We only look for a single usage in the basic block where the external
   address is loaded.  Multiple uses or references in another basic block will
   force us to not use the PCREL_OPT relocation.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "expmed.h"
#include "optabs.h"
#include "recog.h"
#include "df.h"
#include "tm_p.h"
#include "ira.h"
#include "print-tree.h"
#include "varasm.h"
#include "explow.h"
#include "expr.h"
#include "output.h"
#include "tree-pass.h"
#include "rtx-vector-builder.h"
#include "print-rtl.h"
#include "insn-attr.h"
#include "insn-codes.h"

/* Various counters.  */
static struct {
  unsigned long extern_addrs;
  unsigned long loads;
  unsigned long adjacent_loads;
  unsigned long failed_loads;
  unsigned long stores;
  unsigned long adjacent_stores;
  unsigned long failed_stores;
} counters;

/* Unique integer that is appended to .Lpcrel to make a pcrel_opt label. */
static unsigned int pcrel_opt_next_num;


/* Optimize a PC-relative load address to be used in a load. Before it calls
   this function, pcrel_opt_address () uses DF to make sure that it is safe
   to do the PCREL_OPT optimization on these insns.

   Convert insns of the form:

	(set (reg:DI addr)
	     (symbol_ref:DI "ext_symbol"))

	...

	(set (reg:<MODE> value)
	     (mem:<MODE> (reg:DI addr)))

   into:

	(parallel [(set (reg:DI addr)
			(unspec:<MODE> [(symbol_ref:DI "ext_symbol")
					(const_int label_num)]
				       UNSPEC_PCREL_OPT_LD_ADDR))
		   (set (reg:DI data)
			(unspec:DI [(const_int 0)]
				   UNSPEC_PCREL_OPT_LD_DATA))])

	...

	(parallel [(set (reg:<MODE>)
			(unspec:<MODE> [(mem:<MODE> (reg:DI addr))
					(reg:DI data)
					(const_int label_num)]
				       UNSPEC_PCREL_OPT_LD_RELOC))
		   (clobber (reg:DI addr))])

   Because PCREL_OPT will move the actual location of the load from the second
   insn to the first, we need to have the register for the load data be live
   starting at the first insn.

   If the destination register for the data being loaded is the same register
   used to hold the extern address, we generate this insn instead:

	(set (reg:DI data)
	     (unspec:DI [(symbol_ref:DI "ext_symbol")
			 (const_int label_num)]
			UNSPEC_PCREL_OPT_LD_SAME_REG))

   In the first insn, we set both the address of the external variable, and mark
   that the variable being loaded both are created in that insn, and are
   consumed in the second insn.  The mode used in the first insn for the data
   register that will be loaded in the second insn doesn't matter in the end so
   we use DImode.  We just need to mark that both registers may be set in the
   first insn, and will be used in the second insn.

   The UNSPEC_PCREL_OPT_LD_ADDR insn will generate the load address plus
   a definition of a label (.Lpcrel<n>), while the UNSPEC_PCREL_OPT_LD_RELOC
   insn will generate the .reloc to tell the linker to tie the load address and
   load using that address together.

	pld b,ext_symbol@got@pcrel
   .Lpcrel1:

	...

	.reloc .Lpcrel1-8,R_PPC64_PCREL_OPT,.-(.Lpcrel1-8)
	lwz r,0(b)

   If ext_symbol is defined in another object file in the main program and we
   are linking the main program, the linker will convert the above instructions
   to:

	plwz r,ext_symbol@got@pcrel

	...

	nop

   ADDR_INSN is the insn that is loading the address.
   LOAD_INSN is the insn that uses the address to load the actual data.  */

static void
pcrel_opt_load (rtx_insn *addr_insn, rtx_insn *load_insn)
{
  rtx addr_set = PATTERN (addr_insn);
  gcc_assert (GET_CODE (addr_set) == SET);

  rtx addr_reg = SET_DEST (addr_set);
  gcc_assert (base_reg_operand (addr_reg, Pmode));

  rtx addr_symbol = SET_SRC (addr_set);
  gcc_assert (pcrel_external_address (addr_symbol, Pmode));

  rtx load_set = PATTERN (load_insn);
  gcc_assert (GET_CODE (load_set) == SET);

  /* Make sure there are no references to the register being loaded
     between the two insns.  */
  rtx reg = SET_DEST (load_set);
  if (reg_used_between_p (reg, addr_insn, load_insn)
      || reg_set_between_p (reg, addr_insn, load_insn))
    return;

  rtx mem = SET_SRC (load_set);
  machine_mode reg_mode = GET_MODE (reg);
  machine_mode mem_mode = GET_MODE (mem);
  rtx mem_inner = mem;
  unsigned int reg_regno = reg_or_subregno (reg);

  /* Handle the fact that LWA is a DS format instruction, but LWZ is a D format
     instruction.  If the mem load is a signed SImode (i.e. LWA would be used)
     we set mem_mode to DImode so that pcrel_opt_valid_mem_p() will check that
     the address will work for a DS-form instruction. If it won't work, we skip
     the optimization.  The float loads are all indexed so there are no problems
     there.  */

  if (GET_CODE (mem) == SIGN_EXTEND && GET_MODE (XEXP (mem, 0)) == SImode)
    {
      if (!INT_REGNO_P (reg_regno))
	return;

      mem_inner = XEXP (mem, 0);
      mem_mode = DImode;
    }

  else if (GET_CODE (mem) == SIGN_EXTEND
	   || GET_CODE (mem) == ZERO_EXTEND
	   || GET_CODE (mem) == FLOAT_EXTEND)
    {
      mem_inner = XEXP (mem, 0);
      mem_mode = GET_MODE (mem_inner);
    }

  if (!MEM_P (mem_inner))
    return;

  /* Can we do PCREL_OPT for this reference?  */
  if (!pcrel_opt_valid_mem_p (reg, mem_mode, mem_inner))
    return;

  /* Allocate a new PC-relative label, and update the load external address
     insn.

     If the register being loaded is different from the address register, we
     need to indicate both registers are set at the load of the address.

	(parallel [(set (reg load)
			(unspec [(symbol_ref addr_symbol)
				 (const_int label_num)]
				UNSPEC_PCREL_OPT_LD_ADDR))
		   (set (reg addr)
			(unspec [(const_int 0)]
				UNSPEC_PCREL_OPT_LD_DATA))])

     If the register being loaded is the same as the address register, we use
     an alternate form:

	(set (reg load)
	     (unspec [(symbol_ref addr_symbol)
		      (const_int label_num)]
		     UNSPEC_PCREL_OPT_LD_SAME_REG))  */
  unsigned int addr_regno = reg_or_subregno (addr_reg);
  rtx label_num = GEN_INT (++pcrel_opt_next_num);
  rtx reg_di = gen_rtx_REG (DImode, reg_regno);
  rtx addr_pattern;

  /* Create the load address, either using the pattern with an explicit clobber
     if the address register is not the same as the register being loaded, or
     using the pattern that requires the address register to be the address
     loaded.  */
  if (addr_regno != reg_regno)
    addr_pattern = gen_pcrel_opt_ld_addr (addr_reg, addr_symbol, label_num,
					  reg_di);
  else
    addr_pattern = gen_pcrel_opt_ld_addr_same_reg (addr_reg, addr_symbol,
						   label_num);

  validate_change (addr_insn, &PATTERN (addr_insn), addr_pattern, false);

  /* Update the load insn.  If the mem had a sign/zero/float extend, add that
     also after doing the UNSPEC.  Add an explicit clobber of the external
     address register just to make it clear that the address register dies.

	(parallel [(set (reg:<MODE> data)
			(unspec:<MODE> [(mem (addr_reg)
					(reg:DI data)
					(const_int label_num)]
				       UNSPEC_PCREL_OPT_LD_RELOC))
		   (clobber (reg:DI addr_reg))])  */
  rtvec v_load = gen_rtvec (3, mem_inner, reg_di, label_num);
  rtx new_load = gen_rtx_UNSPEC (GET_MODE (mem_inner), v_load,
				 UNSPEC_PCREL_OPT_LD_RELOC);

  if (GET_CODE (mem) != GET_CODE (mem_inner))
    new_load = gen_rtx_fmt_e (GET_CODE (mem), reg_mode, new_load);

  rtx new_load_set = gen_rtx_SET (reg, new_load);
  rtx load_clobber = gen_rtx_CLOBBER (VOIDmode,
				      (addr_regno == reg_regno
				       ? gen_rtx_SCRATCH (Pmode)
				       : addr_reg));
  rtx new_load_pattern
    = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, new_load_set, load_clobber));

  validate_change (load_insn, &PATTERN (load_insn), new_load_pattern, false);

  /* Attempt to apply the changes:  */
  if (!apply_change_group ())
    {
      /* PCREL_OPT load optimization did not succeed.  */
      counters.failed_loads++;
      if (dump_file)
	fprintf (dump_file,
		 "PCREL_OPT load failed (addr insn = %d, use insn = %d).\n",
		 INSN_UID (addr_insn),
		 INSN_UID (load_insn));
      return;
    }

  /* PCREL_OPT load optimization succeeded.  */
  counters.loads++;
  if (next_nonnote_insn (addr_insn) == load_insn)
    counters.adjacent_loads++;

  if (dump_file)
    fprintf (dump_file,
	     "PCREL_OPT load (addr insn = %d, use insn = %d).\n",
	     INSN_UID (addr_insn),
	     INSN_UID (load_insn));

  /* Because we have set DF_DEFER_INSN_RESCAN, we have to explicitly do it
     after we have made changes to the insns.  */
  df_analyze ();

}

/* Optimize a PC-relative load address to be used in a store. Before calling
   this function, pcrel_opt_address () uses DF to make sure it is safe to do
   the PCREL_OPT optimization.

   Convert insns of the form:

	(set (reg:DI addr)
	     (symbol_ref:DI "ext_symbol"))

	...

	(set (mem:<MODE> (reg:DI addr))
	     (reg:<MODE> value))

   into:

	(parallel [(set (reg:DI addr)
			(unspec:DI [(symbol_ref:DI "ext_symbol")
				    (const_int label_num)]
				  UNSPEC_PCREL_OPT_ST_ADDR))
		  (use (reg:<MODE> value))])

	...

	(parallel [(set (mem:<MODE> (reg:DI addr))
			(unspec:<MODE> [(reg:<MODE>)
					(const_int label_num)]
				       UNSPEC_PCREL_OPT_ST_RELOC))
		   (clobber (reg:DI addr))])

   The UNSPEC_PCREL_OPT_ST_ADDR insn will generate the load address plus a
   definition of a label (.Lpcrel<n>), while the UNSPEC_PCREL_OPT_ST_RELOC insn
   will generate the .reloc to tell the linker to tie the load address and load
   using that address together.

	pld b,ext_symbol@got@pcrel
   .Lpcrel1:

	...

	.reloc .Lpcrel1-8,R_PPC64_PCREL_OPT,.-(.Lpcrel1-8)
	stw r,0(b)

   If ext_symbol is defined in another object file in the main program and we
   are linking the main program, the linker will convert the above instructions
   to:

	pstwz r,ext_symbol@got@pcrel

	...

	nop  */

static void
pcrel_opt_store (rtx_insn *addr_insn,		/* insn loading address.  */
		 rtx_insn *store_insn)		/* insn using address.  */
{
  rtx addr_old_set = PATTERN (addr_insn);
  gcc_assert (GET_CODE (addr_old_set) == SET);

  rtx addr_reg = SET_DEST (addr_old_set);
  gcc_assert (base_reg_operand (addr_reg, Pmode));

  rtx addr_symbol = SET_SRC (addr_old_set);
  gcc_assert (pcrel_external_address (addr_symbol, Pmode));

  rtx store_set = PATTERN (store_insn);
  gcc_assert (GET_CODE (store_set) == SET);

  rtx mem = SET_DEST (store_set);
  if (!MEM_P (mem))
    return;

  machine_mode mem_mode = GET_MODE (mem);
  rtx reg = SET_SRC (store_set);

  /* Don't allow storing the address of the external variable.  */
  if (reg_or_subregno (reg) == reg_or_subregno (addr_reg))
    return;

  /* Can we do PCREL_OPT for this reference?  */
  if (!pcrel_opt_valid_mem_p (reg, mem_mode, mem))
    return;

  /* Allocate a new PC-relative label, and update the load address insn.

	(parallel [(set (reg addr)
		       (unspec [(symbol_ref symbol)
				 (const_int label_num)]
				UNSPEC_PCREL_OPT_ST_ADDR))
		  (use (reg store))])
  */
  rtx label_num = GEN_INT (++pcrel_opt_next_num);
  rtvec v_addr = gen_rtvec (2, addr_symbol, label_num);
  rtx addr_unspec = gen_rtx_UNSPEC (Pmode, v_addr,
				   UNSPEC_PCREL_OPT_ST_ADDR);
  rtx addr_new_set = gen_rtx_SET (addr_reg, addr_unspec);
  rtx addr_use = gen_rtx_USE (VOIDmode, reg);
  rtx addr_new_pattern
    = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, addr_new_set, addr_use));

  validate_change (addr_insn, &PATTERN (addr_insn), addr_new_pattern, false);

  /* Update the store insn.  Add an explicit clobber of the external address
     register just to be sure there are no additional uses of the address
     register.

	(parallel [(set (mem (addr_reg)
			(unspec:<MODE> [(reg)
					(const_int label_num)]
				       UNSPEC_PCREL_OPT_ST_RELOC))
		  (clobber (reg:DI addr_reg))])  */
  rtvec v_store = gen_rtvec (2, reg, label_num);
  rtx new_store = gen_rtx_UNSPEC (mem_mode, v_store,
				  UNSPEC_PCREL_OPT_ST_RELOC);

  rtx new_store_set = gen_rtx_SET (mem, new_store);
  rtx store_clobber = gen_rtx_CLOBBER (VOIDmode, addr_reg);
  rtx new_store_pattern
    = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, new_store_set, store_clobber));

  validate_change (store_insn, &PATTERN (store_insn), new_store_pattern, false);

  /* Attempt to apply the changes:  */
  if (!apply_change_group ())
    {
      /* PCREL_OPT store failed.  */
      counters.failed_stores++;
      if (dump_file)
	fprintf (dump_file,
		 "PCREL_OPT store failed (addr insn = %d, use insn = %d).\n",
		 INSN_UID (addr_insn),
		 INSN_UID (store_insn));
      return;
    }

  /* PCREL_OPT store succeeded.  */
  counters.stores++;
  if (next_nonnote_insn (addr_insn) == store_insn)
    counters.adjacent_stores++;

  if (dump_file)
    fprintf (dump_file,
	     "PCREL_OPT store (addr insn = %d, use insn = %d).\n",
	     INSN_UID (addr_insn),
	     INSN_UID (store_insn));

  /* Because we have set DF_DEFER_INSN_RESCAN, we have to explicitly do it
     after we have made changes to the insns.  */
  df_analyze();

}

/* Return the register used as the base register of MEM, if the instruction has
   a pc-relative form.  We look for BSWAP to rule out LFIWAX/LFIWZX/STFIWX, and
   ROTATE/VEC_SELECT are RTX_EXTRA not RTX_UNARY which rules out lxvd2x. This
   excludes instructions that do not have a pc-relative form.  */

static rtx
get_mem_base_reg (rtx mem)
{
  const char * fmt;

  while (!MEM_P (mem))
    {
      if (GET_RTX_CLASS (GET_CODE (mem)) != RTX_UNARY
	  || GET_CODE (mem) == BSWAP)
	return NULL_RTX;
      fmt = GET_RTX_FORMAT (GET_CODE (mem));
      if (fmt[0] != 'e')
	return NULL_RTX;
      mem = XEXP (mem, 0);
      if (mem == NULL_RTX )
	return NULL_RTX;
    }

  if (!MEM_SIZE_KNOWN_P (mem))
    return NULL_RTX;

  rtx addr_rtx = (XEXP (mem, 0));
  if (GET_CODE (addr_rtx) == PRE_MODIFY)
    addr_rtx = XEXP (addr_rtx, 1);

  while (GET_CODE (addr_rtx) == PLUS
	 && CONST_INT_P (XEXP (addr_rtx, 1)))
    addr_rtx = XEXP (addr_rtx, 0);

  if (!REG_P (addr_rtx))
    return NULL_RTX;

  return addr_rtx;
}

/* Check whether INSN contains a reference to REGNO that will inhibit the
   PCREL_OPT optimization.  If TYPE is a load or store instruction, return true
   if there is a definition of REGNO.  If TYPE is a load instruction, then
   return true of there is a use of REGNO.  */

static bool
insn_references_regno_p (rtx_insn *insn, unsigned int regno,
		       enum attr_type type)
{
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
  df_ref ref;

  /* Return true if there is a definition of REGNO.  */
  for (ref = DF_INSN_INFO_DEFS (insn_info); ref; ref = DF_REF_NEXT_LOC (ref))
    if (DF_REF_REGNO (ref) == regno)
      return true;

  /* If type is a load, return true if there is a use of REGNO.  */
  if (type == TYPE_LOAD
      || type == TYPE_FPLOAD
      || type == TYPE_VECLOAD)
    for (ref = DF_INSN_INFO_USES (insn_info); ref; ref = DF_REF_NEXT_LOC (ref))
      if (DF_REF_REGNO (ref) == regno)
	return true;

  return false;
}

/* Given an insn that loads up a base register with the address of an
   external symbol, see if we can optimize it with the PCREL_OPT
   optimization.

   DF is used to make sure that there is exactly one definition and one
   non-debug use of the address register defined by the insn. The use insn must
   be a non-prefix insn, and must also be in the same basic block as the address
   insn.

   ADDR_INSN is the insn that loads the external symbol address.  */

static void
pcrel_opt_address (rtx_insn *addr_insn)
{
  counters.extern_addrs++;

  /* Do some basic validation.  */
  rtx addr_set = PATTERN (addr_insn);
  if (GET_CODE (addr_set) != SET)
    return;

  rtx addr_reg = SET_DEST (addr_set);
  rtx addr_symbol = SET_SRC (addr_set);

  if (!base_reg_operand (addr_reg, Pmode)
      || !pcrel_external_address (addr_symbol, Pmode))
    return;

  /* The address register must have exactly one definition.  */
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (addr_insn);
  if (!insn_info)
    return;

  df_ref def = df_single_def (insn_info);
  if (!def)
    return;

  /* Make sure there is at least one use.  */
  df_link *chain = DF_REF_CHAIN (def);
  if (!chain || !chain->ref)
    return;

  /* Get the insn of the possible load or store.  */
  rtx_insn *use_insn = DF_REF_INSN (chain->ref);

  /* Ensure there are no other uses.  */
  for (chain = chain->next; chain; chain = chain->next)
    if (chain->ref && DF_REF_INSN_INFO (chain->ref))
      {
	gcc_assert (DF_REF_INSN (chain->ref));
	if (NONDEBUG_INSN_P (DF_REF_INSN (chain->ref)))
	  return;
      }

  /* The use instruction must be a single non-prefixed instruction.  */
  if (get_attr_length (use_insn) != 4)
    return;

  /* The address and the memory operation must be in the same basic block.  */
  if (BLOCK_FOR_INSN (use_insn) != BLOCK_FOR_INSN (addr_insn))
    return;

  /* If this isn't a simple SET, skip doing the optimization.  */
  if (GET_CODE (PATTERN (use_insn)) != SET)
    return;

  enum attr_type use_insn_type = get_attr_type (use_insn);
  unsigned int use_regno;

  /* Make sure the use_insn is using addr_reg as its base register
     for the load or store, and determine the regno for the register
     used in the use_insn.  */
  rtx use_dest, use_src;
  switch (use_insn_type)
    {
    case TYPE_LOAD:
    case TYPE_FPLOAD:
    case TYPE_VECLOAD:
      /* Make sure our address register is the same register used in the
	 base address of the load.  */
      if (addr_reg != get_mem_base_reg (SET_SRC (PATTERN (use_insn))))
	return;
      /* Make sure we are setting a register before we look at REGNO.  */
      use_dest = SET_DEST (PATTERN (use_insn));
      if (!register_operand (use_dest, GET_MODE (use_dest)))
	return;
      use_regno = REGNO (use_dest);
      break;
    case TYPE_STORE:
    case TYPE_FPSTORE:
    case TYPE_VECSTORE:
      /* Make sure our address register is the same register used in the
	 base address of the store.  */
      if (addr_reg != get_mem_base_reg (SET_DEST (PATTERN (use_insn))))
	return;
      /* Make sure this is a register before we look at REGNO.  */
      use_src = SET_SRC (PATTERN (use_insn));
      if (!register_operand (use_src, GET_MODE (use_src)))
	return;
      use_regno = REGNO (use_src);
      break;
    default:
      /* We can only optimize loads and stores.  Ignore everything else.  */
      return;
    }

  rtx_insn *insn;
  for (insn = NEXT_INSN (addr_insn);
       insn != use_insn;
       insn = NEXT_INSN (insn))
    {
      /* If we see a call, do not do the PCREL_OPT optimization.  */
      if (CALL_P (insn))
	return;

      /* Skip debug insns.  */
      if (!NONDEBUG_INSN_P (insn))
	continue;

      /* See if it is a load or store.  */
      if (GET_CODE (PATTERN (insn)) != USE
	  && GET_CODE (PATTERN (insn)) != CLOBBER)
	{
	  switch (get_attr_type (insn))
	    {
	    case TYPE_LOAD:
	      /* While load of the external address is a 'load' for scheduling
		 purposes, it should be safe to allow loading other external
		 addresses between the load of the external address we are
		 currently looking at and the load or store using that
		 address.  */
	      if (get_attr_loads_external_address (insn)
		  == LOADS_EXTERNAL_ADDRESS_YES)
		break;
	      /* fall through */

	    case TYPE_FPLOAD:
	    case TYPE_VECLOAD:
	      /* Don't do the PCREL_OPT store optimization if there is a load
		 operation.  For example, the load might be trying to load the
		 value being stored in between getting the address and doing
		 the store.  */
	      if (use_insn_type == TYPE_STORE
		  || use_insn_type == TYPE_FPSTORE
		  || use_insn_type == TYPE_VECSTORE)
		return;
	      break;

	    case TYPE_STORE:
	    case TYPE_FPSTORE:
	    case TYPE_VECSTORE:
	      /* Don't do the PCREL_OPT load optimization if there is a store
		 operation.  Perhaps the store might be to the global variable
		 through a pointer.  */
	      return;

	    case TYPE_LOAD_L:
	    case TYPE_STORE_C:
	    case TYPE_HTM:
	    case TYPE_HTMSIMPLE:
	      /* Don't do the optimization through atomic operations.  */
	      return;

	    default:
	      break;
	    }
	}

      /* Check for invalid references of the non-address register that is
	 used in the load or store instruction.  */
      if (insn_references_regno_p (insn, use_regno, use_insn_type))
	return;
    }

  /* Is this a load or a store?  */
  switch (use_insn_type)
    {
    case TYPE_LOAD:
    case TYPE_FPLOAD:
    case TYPE_VECLOAD:
      pcrel_opt_load (addr_insn, use_insn);
      break;

    case TYPE_STORE:
    case TYPE_FPSTORE:
    case TYPE_VECSTORE:
      pcrel_opt_store (addr_insn, use_insn);
      break;

    default:
      gcc_unreachable ();
    }
}

/* Optimize pcrel external variable references.  */

static unsigned int
pcrel_opt_pass (function *fun)
{
  basic_block bb;
  rtx_insn *insn, *curr_insn = 0;

  memset (&counters, 0, sizeof (counters));

  /* Dataflow analysis for use-def chains.  However we have to specify both UD
   and DU as otherwise when we make changes to insns for the PCREL_OPT there
   will be dangling references.  */
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_DU_CHAIN + DF_UD_CHAIN);
  df_note_add_problem ();
  df_analyze ();

  /* Set the defer flag as our pattern of operation will be to modify two insns,
     then call df_analyze ().  */
  df_set_flags (DF_DEFER_INSN_RESCAN | DF_LR_RUN_DCE);

  if (dump_file)
    fprintf (dump_file, "\n");

  /* Look at each basic block to see if there is a load of an external
     variable's external address, and a single load/store using that external
     address.  */
  FOR_ALL_BB_FN (bb, fun)
    {
      FOR_BB_INSNS_SAFE (bb, insn, curr_insn)
	{
	  if (NONJUMP_INSN_P (insn)
	      && single_set (insn)
	      && get_attr_loads_external_address (insn)
	      == LOADS_EXTERNAL_ADDRESS_YES)
	    pcrel_opt_address (insn);
	}
    }

  if (dump_file)
    {
      fprintf (dump_file,
	       "\n# of loads of an address of an external symbol = %lu\n",
	       counters.extern_addrs);

      fprintf (dump_file, "# of PCREL_OPT loads = %lu (adjacent %lu)\n",
	       counters.loads, counters.adjacent_loads);

      if (counters.failed_loads)
	fprintf (dump_file, "# of failed PCREL_OPT loads = %lu\n",
		 counters.failed_loads);

      fprintf (dump_file, "# of PCREL_OPT stores = %lu (adjacent %lu)\n",
	       counters.stores, counters.adjacent_stores);

      if (counters.failed_stores)
	fprintf (dump_file, "# of failed PCREL_OPT stores = %lu\n",
		 counters.failed_stores);

      fprintf (dump_file, "\n");
    }

  df_remove_problem (df_chain);
  df_process_deferred_rescans ();
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS | DF_LR_RUN_DCE);
  df_analyze ();
  return 0;
}

/* Optimize pc-relative references for the new PCREL_OPT pass.  */
const pass_data pass_data_pcrel_opt =
{
  RTL_PASS,			/* type.  */
  "pcrel_opt",			/* name.  */
  OPTGROUP_NONE,		/* optinfo_flags.  */
  TV_NONE,			/* tv_id.  */
  0,				/* properties_required.  */
  0,				/* properties_provided.  */
  0,				/* properties_destroyed.  */
  0,				/* todo_flags_start.  */
  TODO_df_finish,		/* todo_flags_finish.  */
};

/* Pass data structures.  */
class pcrel_opt : public rtl_opt_pass
{
public:
  pcrel_opt (gcc::context *ctxt)
  : rtl_opt_pass (pass_data_pcrel_opt, ctxt)
  {}

  ~pcrel_opt (void)
  {}

  /* opt_pass methods:  */
  virtual bool gate (function *)
  {
    return (TARGET_PCREL && TARGET_PCREL_OPT && optimize);
  }

  virtual unsigned int execute (function *fun)
  {
    return pcrel_opt_pass (fun);
  }

  opt_pass *clone ()
  {
    return new pcrel_opt (m_ctxt);
  }
};

rtl_opt_pass *
make_pass_pcrel_opt (gcc::context *ctxt)
{
  return new pcrel_opt (ctxt);
}
