/* Exported functions from emit-rtl.c
   Copyright (C) 2004-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_EMIT_RTL_H
#define GCC_EMIT_RTL_H

/* Return whether two MEM_ATTRs are equal.  */
bool mem_attrs_eq_p (const struct mem_attrs *, const struct mem_attrs *);

/* Set the alias set of MEM to SET.  */
extern void set_mem_alias_set (rtx, alias_set_type);

/* Set the alignment of MEM to ALIGN bits.  */
extern void set_mem_align (rtx, unsigned int);

/* Set the address space of MEM to ADDRSPACE.  */
extern void set_mem_addr_space (rtx, addr_space_t);

/* Set the expr for MEM to EXPR.  */
extern void set_mem_expr (rtx, tree);

/* Set the offset for MEM to OFFSET.  */
extern void set_mem_offset (rtx, HOST_WIDE_INT);

/* Clear the offset recorded for MEM.  */
extern void clear_mem_offset (rtx);

/* Set the size for MEM to SIZE.  */
extern void set_mem_size (rtx, HOST_WIDE_INT);

/* Clear the size recorded for MEM.  */
extern void clear_mem_size (rtx);

/* Set the attributes for MEM appropriate for a spill slot.  */
extern void set_mem_attrs_for_spill (rtx);
extern tree get_spill_slot_decl (bool);

/* Return a memory reference like MEMREF, but with its address changed to
   ADDR.  The caller is asserting that the actual piece of memory pointed
   to is the same, just the form of the address is being changed, such as
   by putting something into a register.  */
extern rtx replace_equiv_address (rtx, rtx, bool = false);

/* Likewise, but the reference is not required to be valid.  */
extern rtx replace_equiv_address_nv (rtx, rtx, bool = false);

extern rtx gen_blockage (void);
extern rtvec gen_rtvec (int, ...);
extern rtx copy_insn_1 (rtx);
extern rtx copy_insn (rtx);
extern rtx_insn *copy_delay_slot_insn (rtx_insn *);
extern rtx gen_int_mode (HOST_WIDE_INT, machine_mode);
extern rtx_insn *emit_copy_of_insn_after (rtx_insn *, rtx_insn *);
extern void set_reg_attrs_from_value (rtx, rtx);
extern void set_reg_attrs_for_parm (rtx, rtx);
extern void set_reg_attrs_for_decl_rtl (tree t, rtx x);
extern void adjust_reg_mode (rtx, machine_mode);
extern int mem_expr_equal_p (const_tree, const_tree);

extern bool need_atomic_barrier_p (enum memmodel, bool);

/* Return the first insn of the current sequence or current function.  */

static inline rtx_insn *
get_insns (void)
{
  return crtl->emit.x_first_insn;
}

/* Specify a new insn as the first in the chain.  */

static inline void
set_first_insn (rtx_insn *insn)
{
  gcc_checking_assert (!insn || !PREV_INSN (insn));
  crtl->emit.x_first_insn = insn;
}

/* Return the last insn emitted in current sequence or current function.  */

static inline rtx_insn *
get_last_insn (void)
{
  return crtl->emit.x_last_insn;
}

/* Specify a new insn as the last in the chain.  */

static inline void
set_last_insn (rtx_insn *insn)
{
  gcc_checking_assert (!insn || !NEXT_INSN (insn));
  crtl->emit.x_last_insn = insn;
}

/* Return a number larger than any instruction's uid in this function.  */

static inline int
get_max_uid (void)
{
  return crtl->emit.x_cur_insn_uid;
}

extern void set_decl_incoming_rtl (tree, rtx, bool);

/* Return a memory reference like MEMREF, but with its mode changed
   to MODE and its address changed to ADDR.
   (VOIDmode means don't change the mode.
   NULL for ADDR means don't change the address.)  */
extern rtx change_address (rtx, machine_mode, rtx);

/* Return a memory reference like MEMREF, but with its mode changed
   to MODE and its address offset by OFFSET bytes.  */
#define adjust_address(MEMREF, MODE, OFFSET) \
  adjust_address_1 (MEMREF, MODE, OFFSET, 1, 1, 0, 0)

/* Likewise, but the reference is not required to be valid.  */
#define adjust_address_nv(MEMREF, MODE, OFFSET) \
  adjust_address_1 (MEMREF, MODE, OFFSET, 0, 1, 0, 0)

/* Return a memory reference like MEMREF, but with its mode changed
   to MODE and its address offset by OFFSET bytes.  Assume that it's
   for a bitfield and conservatively drop the underlying object if we
   cannot be sure to stay within its bounds.  */
#define adjust_bitfield_address(MEMREF, MODE, OFFSET) \
  adjust_address_1 (MEMREF, MODE, OFFSET, 1, 1, 1, 0)

/* As for adjust_bitfield_address, but specify that the width of
   BLKmode accesses is SIZE bytes.  */
#define adjust_bitfield_address_size(MEMREF, MODE, OFFSET, SIZE) \
  adjust_address_1 (MEMREF, MODE, OFFSET, 1, 1, 1, SIZE)

/* Likewise, but the reference is not required to be valid.  */
#define adjust_bitfield_address_nv(MEMREF, MODE, OFFSET) \
  adjust_address_1 (MEMREF, MODE, OFFSET, 0, 1, 1, 0)

/* Return a memory reference like MEMREF, but with its mode changed
   to MODE and its address changed to ADDR, which is assumed to be
   increased by OFFSET bytes from MEMREF.  */
#define adjust_automodify_address(MEMREF, MODE, ADDR, OFFSET) \
  adjust_automodify_address_1 (MEMREF, MODE, ADDR, OFFSET, 1)

/* Likewise, but the reference is not required to be valid.  */
#define adjust_automodify_address_nv(MEMREF, MODE, ADDR, OFFSET) \
  adjust_automodify_address_1 (MEMREF, MODE, ADDR, OFFSET, 0)

extern rtx adjust_address_1 (rtx, machine_mode, HOST_WIDE_INT, int, int,
			     int, HOST_WIDE_INT);
extern rtx adjust_automodify_address_1 (rtx, machine_mode, rtx,
					HOST_WIDE_INT, int);

/* Return a memory reference like MEMREF, but whose address is changed by
   adding OFFSET, an RTX, to it.  POW2 is the highest power of two factor
   known to be in OFFSET (possibly 1).  */
extern rtx offset_address (rtx, rtx, unsigned HOST_WIDE_INT);

/* Given REF, a MEM, and T, either the type of X or the expression
   corresponding to REF, set the memory attributes.  OBJECTP is nonzero
   if we are making a new object of this type.  */
extern void set_mem_attributes (rtx, tree, int);

/* Similar, except that BITPOS has not yet been applied to REF, so if
   we alter MEM_OFFSET according to T then we should subtract BITPOS
   expecting that it'll be added back in later.  */
extern void set_mem_attributes_minus_bitpos (rtx, tree, int, HOST_WIDE_INT);

/* Return OFFSET if XEXP (MEM, 0) - OFFSET is known to be ALIGN
   bits aligned for 0 <= OFFSET < ALIGN / BITS_PER_UNIT, or
   -1 if not known.  */
extern int get_mem_align_offset (rtx, unsigned int);

/* Return a memory reference like MEMREF, but with its mode widened to
   MODE and adjusted by OFFSET.  */
extern rtx widen_memory_access (rtx, machine_mode, HOST_WIDE_INT);

#endif /* GCC_EMIT_RTL_H */
