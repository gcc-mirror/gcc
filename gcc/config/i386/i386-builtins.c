/* Copyright (C) 1988-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "cfgbuild.h"
#include "alias.h"
#include "fold-const.h"
#include "attribs.h"
#include "calls.h"
#include "stor-layout.h"
#include "varasm.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "except.h"
#include "explow.h"
#include "expr.h"
#include "cfgrtl.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "reload.h"
#include "gimplify.h"
#include "dwarf2.h"
#include "tm-constrs.h"
#include "cselib.h"
#include "sched-int.h"
#include "opts.h"
#include "tree-pass.h"
#include "context.h"
#include "pass_manager.h"
#include "target-globals.h"
#include "gimple-iterator.h"
#include "tree-vectorizer.h"
#include "shrink-wrap.h"
#include "builtins.h"
#include "rtl-iter.h"
#include "tree-iterator.h"
#include "dbgcnt.h"
#include "case-cfn-macros.h"
#include "dojump.h"
#include "fold-const-call.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "selftest.h"
#include "selftest-rtl.h"
#include "print-rtl.h"
#include "intl.h"
#include "ifcvt.h"
#include "symbol-summary.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "wide-int-bitmask.h"
#include "tree-vector-builder.h"
#include "debug.h"
#include "dwarf2out.h"
#include "i386-builtins.h"
#include "common/config/i386/i386-isas.h"

#undef BDESC
#undef BDESC_FIRST
#undef BDESC_END

/* Macros for verification of enum ix86_builtins order.  */
#define BDESC_VERIFY(x, y, z) \
  gcc_checking_assert ((x) == (enum ix86_builtins) ((y) + (z)))
#define BDESC_VERIFYS(x, y, z) \
  STATIC_ASSERT ((x) == (enum ix86_builtins) ((y) + (z)))

BDESC_VERIFYS (IX86_BUILTIN__BDESC_PCMPESTR_FIRST,
	       IX86_BUILTIN__BDESC_COMI_LAST, 1);
BDESC_VERIFYS (IX86_BUILTIN__BDESC_PCMPISTR_FIRST,
	       IX86_BUILTIN__BDESC_PCMPESTR_LAST, 1);
BDESC_VERIFYS (IX86_BUILTIN__BDESC_SPECIAL_ARGS_FIRST,
	       IX86_BUILTIN__BDESC_PCMPISTR_LAST, 1);
BDESC_VERIFYS (IX86_BUILTIN__BDESC_ARGS_FIRST,
	       IX86_BUILTIN__BDESC_SPECIAL_ARGS_LAST, 1);
BDESC_VERIFYS (IX86_BUILTIN__BDESC_ROUND_ARGS_FIRST,
	       IX86_BUILTIN__BDESC_ARGS_LAST, 1);
BDESC_VERIFYS (IX86_BUILTIN__BDESC_MULTI_ARG_FIRST,
	       IX86_BUILTIN__BDESC_ROUND_ARGS_LAST, 1);
BDESC_VERIFYS (IX86_BUILTIN__BDESC_CET_FIRST,
	       IX86_BUILTIN__BDESC_MULTI_ARG_LAST, 1);
BDESC_VERIFYS (IX86_BUILTIN__BDESC_CET_NORMAL_FIRST,
	       IX86_BUILTIN__BDESC_CET_LAST, 1);
BDESC_VERIFYS (IX86_BUILTIN_MAX,
	       IX86_BUILTIN__BDESC_CET_NORMAL_LAST, 1);


/* Table for the ix86 builtin non-function types.  */
static GTY(()) tree ix86_builtin_type_tab[(int) IX86_BT_LAST_CPTR + 1];

/* Retrieve an element from the above table, building some of
   the types lazily.  */

static tree
ix86_get_builtin_type (enum ix86_builtin_type tcode)
{
  unsigned int index;
  tree type, itype;

  gcc_assert ((unsigned)tcode < ARRAY_SIZE(ix86_builtin_type_tab));

  type = ix86_builtin_type_tab[(int) tcode];
  if (type != NULL)
    return type;

  gcc_assert (tcode > IX86_BT_LAST_PRIM);
  if (tcode <= IX86_BT_LAST_VECT)
    {
      machine_mode mode;

      index = tcode - IX86_BT_LAST_PRIM - 1;
      itype = ix86_get_builtin_type (ix86_builtin_type_vect_base[index]);
      mode = ix86_builtin_type_vect_mode[index];

      type = build_vector_type_for_mode (itype, mode);
    }
  else
    {
      int quals;

      index = tcode - IX86_BT_LAST_VECT - 1;
      if (tcode <= IX86_BT_LAST_PTR)
	quals = TYPE_UNQUALIFIED;
      else
	quals = TYPE_QUAL_CONST;

      itype = ix86_get_builtin_type (ix86_builtin_type_ptr_base[index]);
      if (quals != TYPE_UNQUALIFIED)
	itype = build_qualified_type (itype, quals);

      type = build_pointer_type (itype);
    }

  ix86_builtin_type_tab[(int) tcode] = type;
  return type;
}

/* Table for the ix86 builtin function types.  */
static GTY(()) tree ix86_builtin_func_type_tab[(int) IX86_BT_LAST_ALIAS + 1];

/* Retrieve an element from the above table, building some of
   the types lazily.  */

static tree
ix86_get_builtin_func_type (enum ix86_builtin_func_type tcode)
{
  tree type;

  gcc_assert ((unsigned)tcode < ARRAY_SIZE (ix86_builtin_func_type_tab));

  type = ix86_builtin_func_type_tab[(int) tcode];
  if (type != NULL)
    return type;

  if (tcode <= IX86_BT_LAST_FUNC)
    {
      unsigned start = ix86_builtin_func_start[(int) tcode];
      unsigned after = ix86_builtin_func_start[(int) tcode + 1];
      tree rtype, atype, args = void_list_node;
      unsigned i;

      rtype = ix86_get_builtin_type (ix86_builtin_func_args[start]);
      for (i = after - 1; i > start; --i)
	{
	  atype = ix86_get_builtin_type (ix86_builtin_func_args[i]);
	  args = tree_cons (NULL, atype, args);
	}

      type = build_function_type (rtype, args);
    }
  else
    {
      unsigned index = tcode - IX86_BT_LAST_FUNC - 1;
      enum ix86_builtin_func_type icode;

      icode = ix86_builtin_func_alias_base[index];
      type = ix86_get_builtin_func_type (icode);
    }

  ix86_builtin_func_type_tab[(int) tcode] = type;
  return type;
}

/* Table for the ix86 builtin decls.  */
static GTY(()) tree ix86_builtins[(int) IX86_BUILTIN_MAX];

struct builtin_isa ix86_builtins_isa[(int) IX86_BUILTIN_MAX];

tree get_ix86_builtin (enum ix86_builtins c)
{
  return ix86_builtins[c];
}

/* Bits that can still enable any inclusion of a builtin.  */
HOST_WIDE_INT deferred_isa_values = 0;
HOST_WIDE_INT deferred_isa_values2 = 0;

/* Add an ix86 target builtin function with CODE, NAME and TYPE.  Save the
   MASK and MASK2 of which isa_flags and ix86_isa_flags2 to use in the
   ix86_builtins_isa array.  Stores the function decl in the ix86_builtins
   array.  Returns the function decl or NULL_TREE, if the builtin was not
   added.

   If the front end has a special hook for builtin functions, delay adding
   builtin functions that aren't in the current ISA until the ISA is changed
   with function specific optimization.  Doing so, can save about 300K for the
   default compiler.  When the builtin is expanded, check at that time whether
   it is valid.

   If the front end doesn't have a special hook, record all builtins, even if
   it isn't an instruction set in the current ISA in case the user uses
   function specific options for a different ISA, so that we don't get scope
   errors if a builtin is added in the middle of a function scope.  */

static inline tree
def_builtin (HOST_WIDE_INT mask, HOST_WIDE_INT mask2,
	     const char *name,
	     enum ix86_builtin_func_type tcode,
	     enum ix86_builtins code)
{
  tree decl = NULL_TREE;

  /* An instruction may be 64bit only regardless of ISAs.  */
  if (!(mask & OPTION_MASK_ISA_64BIT) || TARGET_64BIT)
    {
      ix86_builtins_isa[(int) code].isa = mask;
      ix86_builtins_isa[(int) code].isa2 = mask2;

      mask &= ~OPTION_MASK_ISA_64BIT;

      /* Filter out the masks most often ored together with others.  */
      if ((mask & ix86_isa_flags & OPTION_MASK_ISA_AVX512VL)
	  && mask != OPTION_MASK_ISA_AVX512VL)
	mask &= ~OPTION_MASK_ISA_AVX512VL;
      if ((mask & ix86_isa_flags & OPTION_MASK_ISA_AVX512BW)
	  && mask != OPTION_MASK_ISA_AVX512BW)
	mask &= ~OPTION_MASK_ISA_AVX512BW;

      if (((mask2 == 0 || (mask2 & ix86_isa_flags2) != 0)
	   && (mask == 0 || (mask & ix86_isa_flags) != 0))
	  || ((mask & OPTION_MASK_ISA_MMX) != 0 && TARGET_MMX_WITH_SSE)
	  || (lang_hooks.builtin_function
	      == lang_hooks.builtin_function_ext_scope))
	{
	  tree type = ix86_get_builtin_func_type (tcode);
	  decl = add_builtin_function (name, type, code, BUILT_IN_MD,
				       NULL, NULL_TREE);
	  ix86_builtins[(int) code] = decl;
	  ix86_builtins_isa[(int) code].set_and_not_built_p = false;
	}
      else
	{
	  /* Just MASK and MASK2 where set_and_not_built_p == true can potentially
	     include a builtin.  */
	  deferred_isa_values |= mask;
	  deferred_isa_values2 |= mask2;
	  ix86_builtins[(int) code] = NULL_TREE;
	  ix86_builtins_isa[(int) code].tcode = tcode;
	  ix86_builtins_isa[(int) code].name = name;
	  ix86_builtins_isa[(int) code].const_p = false;
	  ix86_builtins_isa[(int) code].pure_p = false;
	  ix86_builtins_isa[(int) code].set_and_not_built_p = true;
	}
    }

  return decl;
}

/* Like def_builtin, but also marks the function decl "const".  */

static inline tree
def_builtin_const (HOST_WIDE_INT mask, HOST_WIDE_INT mask2, const char *name,
		   enum ix86_builtin_func_type tcode, enum ix86_builtins code)
{
  tree decl = def_builtin (mask, mask2, name, tcode, code);
  if (decl)
    TREE_READONLY (decl) = 1;
  else
    ix86_builtins_isa[(int) code].const_p = true;

  return decl;
}

/* Like def_builtin, but also marks the function decl "pure".  */

static inline tree
def_builtin_pure (HOST_WIDE_INT mask, HOST_WIDE_INT mask2, const char *name,
		  enum ix86_builtin_func_type tcode, enum ix86_builtins code)
{
  tree decl = def_builtin (mask, mask2, name, tcode, code);
  if (decl)
    DECL_PURE_P (decl) = 1;
  else
    ix86_builtins_isa[(int) code].pure_p = true;

  return decl;
}

/* Add any new builtin functions for a given ISA that may not have been
   declared.  This saves a bit of space compared to adding all of the
   declarations to the tree, even if we didn't use them.  */

void
ix86_add_new_builtins (HOST_WIDE_INT isa, HOST_WIDE_INT isa2)
{
  isa &= ~OPTION_MASK_ISA_64BIT;

  if ((isa & deferred_isa_values) == 0
      && (isa2 & deferred_isa_values2) == 0
      && ((deferred_isa_values & OPTION_MASK_ISA_MMX) == 0
	  || !(TARGET_64BIT && (isa & OPTION_MASK_ISA_SSE2) != 0)))
    return;

  /* Bits in ISA value can be removed from potential isa values.  */
  deferred_isa_values &= ~isa;
  deferred_isa_values2 &= ~isa2;
  if (TARGET_64BIT && (isa & OPTION_MASK_ISA_SSE2) != 0)
    deferred_isa_values &= ~OPTION_MASK_ISA_MMX;

  int i;
  tree saved_current_target_pragma = current_target_pragma;
  current_target_pragma = NULL_TREE;

  for (i = 0; i < (int)IX86_BUILTIN_MAX; i++)
    {
      if (((ix86_builtins_isa[i].isa & isa) != 0
	   || (ix86_builtins_isa[i].isa2 & isa2) != 0
	   || ((ix86_builtins_isa[i].isa & OPTION_MASK_ISA_MMX) != 0
	       && TARGET_64BIT
	       && (isa & OPTION_MASK_ISA_SSE2) != 0))
	  && ix86_builtins_isa[i].set_and_not_built_p)
	{
	  tree decl, type;

	  /* Don't define the builtin again.  */
	  ix86_builtins_isa[i].set_and_not_built_p = false;

	  type = ix86_get_builtin_func_type (ix86_builtins_isa[i].tcode);
	  decl = add_builtin_function_ext_scope (ix86_builtins_isa[i].name,
						 type, i, BUILT_IN_MD, NULL,
						 NULL_TREE);

	  ix86_builtins[i] = decl;
	  if (ix86_builtins_isa[i].const_p)
	    TREE_READONLY (decl) = 1;
	}
    }

  current_target_pragma = saved_current_target_pragma;
}

/* TM vector builtins.  */

/* Reuse the existing x86-specific `struct builtin_description' cause
   we're lazy.  Add casts to make them fit.  */
static const struct builtin_description bdesc_tm[] =
{
  { OPTION_MASK_ISA_MMX, 0, CODE_FOR_nothing, "__builtin__ITM_WM64", (enum ix86_builtins) BUILT_IN_TM_STORE_M64, UNKNOWN, VOID_FTYPE_PV2SI_V2SI },
  { OPTION_MASK_ISA_MMX, 0, CODE_FOR_nothing, "__builtin__ITM_WaRM64", (enum ix86_builtins) BUILT_IN_TM_STORE_WAR_M64, UNKNOWN, VOID_FTYPE_PV2SI_V2SI },
  { OPTION_MASK_ISA_MMX, 0, CODE_FOR_nothing, "__builtin__ITM_WaWM64", (enum ix86_builtins) BUILT_IN_TM_STORE_WAW_M64, UNKNOWN, VOID_FTYPE_PV2SI_V2SI },
  { OPTION_MASK_ISA_MMX, 0, CODE_FOR_nothing, "__builtin__ITM_RM64", (enum ix86_builtins) BUILT_IN_TM_LOAD_M64, UNKNOWN, V2SI_FTYPE_PCV2SI },
  { OPTION_MASK_ISA_MMX, 0, CODE_FOR_nothing, "__builtin__ITM_RaRM64", (enum ix86_builtins) BUILT_IN_TM_LOAD_RAR_M64, UNKNOWN, V2SI_FTYPE_PCV2SI },
  { OPTION_MASK_ISA_MMX, 0, CODE_FOR_nothing, "__builtin__ITM_RaWM64", (enum ix86_builtins) BUILT_IN_TM_LOAD_RAW_M64, UNKNOWN, V2SI_FTYPE_PCV2SI },
  { OPTION_MASK_ISA_MMX, 0, CODE_FOR_nothing, "__builtin__ITM_RfWM64", (enum ix86_builtins) BUILT_IN_TM_LOAD_RFW_M64, UNKNOWN, V2SI_FTYPE_PCV2SI },

  { OPTION_MASK_ISA_SSE, 0, CODE_FOR_nothing, "__builtin__ITM_WM128", (enum ix86_builtins) BUILT_IN_TM_STORE_M128, UNKNOWN, VOID_FTYPE_PV4SF_V4SF },
  { OPTION_MASK_ISA_SSE, 0, CODE_FOR_nothing, "__builtin__ITM_WaRM128", (enum ix86_builtins) BUILT_IN_TM_STORE_WAR_M128, UNKNOWN, VOID_FTYPE_PV4SF_V4SF },
  { OPTION_MASK_ISA_SSE, 0, CODE_FOR_nothing, "__builtin__ITM_WaWM128", (enum ix86_builtins) BUILT_IN_TM_STORE_WAW_M128, UNKNOWN, VOID_FTYPE_PV4SF_V4SF },
  { OPTION_MASK_ISA_SSE, 0, CODE_FOR_nothing, "__builtin__ITM_RM128", (enum ix86_builtins) BUILT_IN_TM_LOAD_M128, UNKNOWN, V4SF_FTYPE_PCV4SF },
  { OPTION_MASK_ISA_SSE, 0, CODE_FOR_nothing, "__builtin__ITM_RaRM128", (enum ix86_builtins) BUILT_IN_TM_LOAD_RAR_M128, UNKNOWN, V4SF_FTYPE_PCV4SF },
  { OPTION_MASK_ISA_SSE, 0, CODE_FOR_nothing, "__builtin__ITM_RaWM128", (enum ix86_builtins) BUILT_IN_TM_LOAD_RAW_M128, UNKNOWN, V4SF_FTYPE_PCV4SF },
  { OPTION_MASK_ISA_SSE, 0, CODE_FOR_nothing, "__builtin__ITM_RfWM128", (enum ix86_builtins) BUILT_IN_TM_LOAD_RFW_M128, UNKNOWN, V4SF_FTYPE_PCV4SF },

  { OPTION_MASK_ISA_AVX, 0, CODE_FOR_nothing, "__builtin__ITM_WM256", (enum ix86_builtins) BUILT_IN_TM_STORE_M256, UNKNOWN, VOID_FTYPE_PV8SF_V8SF },
  { OPTION_MASK_ISA_AVX, 0, CODE_FOR_nothing, "__builtin__ITM_WaRM256", (enum ix86_builtins) BUILT_IN_TM_STORE_WAR_M256, UNKNOWN, VOID_FTYPE_PV8SF_V8SF },
  { OPTION_MASK_ISA_AVX, 0, CODE_FOR_nothing, "__builtin__ITM_WaWM256", (enum ix86_builtins) BUILT_IN_TM_STORE_WAW_M256, UNKNOWN, VOID_FTYPE_PV8SF_V8SF },
  { OPTION_MASK_ISA_AVX, 0, CODE_FOR_nothing, "__builtin__ITM_RM256", (enum ix86_builtins) BUILT_IN_TM_LOAD_M256, UNKNOWN, V8SF_FTYPE_PCV8SF },
  { OPTION_MASK_ISA_AVX, 0, CODE_FOR_nothing, "__builtin__ITM_RaRM256", (enum ix86_builtins) BUILT_IN_TM_LOAD_RAR_M256, UNKNOWN, V8SF_FTYPE_PCV8SF },
  { OPTION_MASK_ISA_AVX, 0, CODE_FOR_nothing, "__builtin__ITM_RaWM256", (enum ix86_builtins) BUILT_IN_TM_LOAD_RAW_M256, UNKNOWN, V8SF_FTYPE_PCV8SF },
  { OPTION_MASK_ISA_AVX, 0, CODE_FOR_nothing, "__builtin__ITM_RfWM256", (enum ix86_builtins) BUILT_IN_TM_LOAD_RFW_M256, UNKNOWN, V8SF_FTYPE_PCV8SF },

  { OPTION_MASK_ISA_MMX, 0, CODE_FOR_nothing, "__builtin__ITM_LM64", (enum ix86_builtins) BUILT_IN_TM_LOG_M64, UNKNOWN, VOID_FTYPE_PCVOID },
  { OPTION_MASK_ISA_SSE, 0, CODE_FOR_nothing, "__builtin__ITM_LM128", (enum ix86_builtins) BUILT_IN_TM_LOG_M128, UNKNOWN, VOID_FTYPE_PCVOID },
  { OPTION_MASK_ISA_AVX, 0, CODE_FOR_nothing, "__builtin__ITM_LM256", (enum ix86_builtins) BUILT_IN_TM_LOG_M256, UNKNOWN, VOID_FTYPE_PCVOID },
};

/* Initialize the transactional memory vector load/store builtins.  */

static void
ix86_init_tm_builtins (void)
{
  enum ix86_builtin_func_type ftype;
  const struct builtin_description *d;
  size_t i;
  tree decl;
  tree attrs_load, attrs_type_load, attrs_store, attrs_type_store;
  tree attrs_log, attrs_type_log;

  if (!flag_tm)
    return;

  /* If there are no builtins defined, we must be compiling in a
     language without trans-mem support.  */
  if (!builtin_decl_explicit_p (BUILT_IN_TM_LOAD_1))
    return;

  /* Use whatever attributes a normal TM load has.  */
  decl = builtin_decl_explicit (BUILT_IN_TM_LOAD_1);
  attrs_load = DECL_ATTRIBUTES (decl);
  attrs_type_load = TYPE_ATTRIBUTES (TREE_TYPE (decl));
  /* Use whatever attributes a normal TM store has.  */
  decl = builtin_decl_explicit (BUILT_IN_TM_STORE_1);
  attrs_store = DECL_ATTRIBUTES (decl);
  attrs_type_store = TYPE_ATTRIBUTES (TREE_TYPE (decl));
  /* Use whatever attributes a normal TM log has.  */
  decl = builtin_decl_explicit (BUILT_IN_TM_LOG);
  attrs_log = DECL_ATTRIBUTES (decl);
  attrs_type_log = TYPE_ATTRIBUTES (TREE_TYPE (decl));

  for (i = 0, d = bdesc_tm;
       i < ARRAY_SIZE (bdesc_tm);
       i++, d++)
    {
      if ((d->mask & ix86_isa_flags) != 0
	  || ((d->mask & OPTION_MASK_ISA_MMX) != 0 && TARGET_MMX_WITH_SSE)
	  || (lang_hooks.builtin_function
	      == lang_hooks.builtin_function_ext_scope))
	{
	  tree type, attrs, attrs_type;
	  enum built_in_function code = (enum built_in_function) d->code;

	  ftype = (enum ix86_builtin_func_type) d->flag;
	  type = ix86_get_builtin_func_type (ftype);

	  if (BUILTIN_TM_LOAD_P (code))
	    {
	      attrs = attrs_load;
	      attrs_type = attrs_type_load;
	    }
	  else if (BUILTIN_TM_STORE_P (code))
	    {
	      attrs = attrs_store;
	      attrs_type = attrs_type_store;
	    }
	  else
	    {
	      attrs = attrs_log;
	      attrs_type = attrs_type_log;
	    }
	  decl = add_builtin_function (d->name, type, code, BUILT_IN_NORMAL,
				       /* The builtin without the prefix for
					  calling it directly.  */
				       d->name + strlen ("__builtin_"),
				       attrs);
	  /* add_builtin_function() will set the DECL_ATTRIBUTES, now
	     set the TYPE_ATTRIBUTES.  */
	  decl_attributes (&TREE_TYPE (decl), attrs_type, ATTR_FLAG_BUILT_IN);

	  set_builtin_decl (code, decl, false);
	}
    }
}

/* Set up all the MMX/SSE builtins, even builtins for instructions that are not
   in the current target ISA to allow the user to compile particular modules
   with different target specific options that differ from the command line
   options.  */
static void
ix86_init_mmx_sse_builtins (void)
{
  const struct builtin_description * d;
  enum ix86_builtin_func_type ftype;
  size_t i;

  /* Add all special builtins with variable number of operands.  */
  for (i = 0, d = bdesc_special_args;
       i < ARRAY_SIZE (bdesc_special_args);
       i++, d++)
    {
      BDESC_VERIFY (d->code, IX86_BUILTIN__BDESC_SPECIAL_ARGS_FIRST, i);
      if (d->name == 0)
	continue;

      ftype = (enum ix86_builtin_func_type) d->flag;
      def_builtin (d->mask, d->mask2, d->name, ftype, d->code);
    }
  BDESC_VERIFYS (IX86_BUILTIN__BDESC_SPECIAL_ARGS_LAST,
		 IX86_BUILTIN__BDESC_SPECIAL_ARGS_FIRST,
		 ARRAY_SIZE (bdesc_special_args) - 1);

  /* Add all builtins with variable number of operands.  */
  for (i = 0, d = bdesc_args;
       i < ARRAY_SIZE (bdesc_args);
       i++, d++)
    {
      BDESC_VERIFY (d->code, IX86_BUILTIN__BDESC_ARGS_FIRST, i);
      if (d->name == 0)
	continue;

      ftype = (enum ix86_builtin_func_type) d->flag;
      def_builtin_const (d->mask, d->mask2, d->name, ftype, d->code);
    }
  BDESC_VERIFYS (IX86_BUILTIN__BDESC_ARGS_LAST,
		 IX86_BUILTIN__BDESC_ARGS_FIRST,
		 ARRAY_SIZE (bdesc_args) - 1);

  /* Add all builtins with rounding.  */
  for (i = 0, d = bdesc_round_args;
       i < ARRAY_SIZE (bdesc_round_args);
       i++, d++)
    {
      BDESC_VERIFY (d->code, IX86_BUILTIN__BDESC_ROUND_ARGS_FIRST, i);
      if (d->name == 0)
	continue;

      ftype = (enum ix86_builtin_func_type) d->flag;
      def_builtin_const (d->mask, d->mask2, d->name, ftype, d->code);
    }
  BDESC_VERIFYS (IX86_BUILTIN__BDESC_ROUND_ARGS_LAST,
		 IX86_BUILTIN__BDESC_ROUND_ARGS_FIRST,
		 ARRAY_SIZE (bdesc_round_args) - 1);

  /* pcmpestr[im] insns.  */
  for (i = 0, d = bdesc_pcmpestr;
       i < ARRAY_SIZE (bdesc_pcmpestr);
       i++, d++)
    {
      BDESC_VERIFY (d->code, IX86_BUILTIN__BDESC_PCMPESTR_FIRST, i);
      if (d->code == IX86_BUILTIN_PCMPESTRM128)
	ftype = V16QI_FTYPE_V16QI_INT_V16QI_INT_INT;
      else
	ftype = INT_FTYPE_V16QI_INT_V16QI_INT_INT;
      def_builtin_const (d->mask, d->mask2, d->name, ftype, d->code);
    }
  BDESC_VERIFYS (IX86_BUILTIN__BDESC_PCMPESTR_LAST,
		 IX86_BUILTIN__BDESC_PCMPESTR_FIRST,
		 ARRAY_SIZE (bdesc_pcmpestr) - 1);

  /* pcmpistr[im] insns.  */
  for (i = 0, d = bdesc_pcmpistr;
       i < ARRAY_SIZE (bdesc_pcmpistr);
       i++, d++)
    {
      BDESC_VERIFY (d->code, IX86_BUILTIN__BDESC_PCMPISTR_FIRST, i);
      if (d->code == IX86_BUILTIN_PCMPISTRM128)
	ftype = V16QI_FTYPE_V16QI_V16QI_INT;
      else
	ftype = INT_FTYPE_V16QI_V16QI_INT;
      def_builtin_const (d->mask, d->mask2, d->name, ftype, d->code);
    }
  BDESC_VERIFYS (IX86_BUILTIN__BDESC_PCMPISTR_LAST,
		 IX86_BUILTIN__BDESC_PCMPISTR_FIRST,
		 ARRAY_SIZE (bdesc_pcmpistr) - 1);

  /* comi/ucomi insns.  */
  for (i = 0, d = bdesc_comi; i < ARRAY_SIZE (bdesc_comi); i++, d++)
    {
      BDESC_VERIFY (d->code, IX86_BUILTIN__BDESC_COMI_FIRST, i);
      if (d->mask == OPTION_MASK_ISA_SSE2)
	ftype = INT_FTYPE_V2DF_V2DF;
      else
	ftype = INT_FTYPE_V4SF_V4SF;
      def_builtin_const (d->mask, d->mask2, d->name, ftype, d->code);
    }
  BDESC_VERIFYS (IX86_BUILTIN__BDESC_COMI_LAST,
		 IX86_BUILTIN__BDESC_COMI_FIRST,
		 ARRAY_SIZE (bdesc_comi) - 1);

  /* SSE */
  def_builtin (OPTION_MASK_ISA_SSE, 0,  "__builtin_ia32_ldmxcsr",
	       VOID_FTYPE_UNSIGNED, IX86_BUILTIN_LDMXCSR);
  def_builtin_pure (OPTION_MASK_ISA_SSE, 0, "__builtin_ia32_stmxcsr",
		    UNSIGNED_FTYPE_VOID, IX86_BUILTIN_STMXCSR);

  /* SSE or 3DNow!A */
  def_builtin (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A
	       /* As it uses V4HImode, we have to require -mmmx too.  */
	       | OPTION_MASK_ISA_MMX, 0,
	       "__builtin_ia32_maskmovq", VOID_FTYPE_V8QI_V8QI_PCHAR,
	       IX86_BUILTIN_MASKMOVQ);

  /* SSE2 */
  def_builtin (OPTION_MASK_ISA_SSE2, 0, "__builtin_ia32_maskmovdqu",
	       VOID_FTYPE_V16QI_V16QI_PCHAR, IX86_BUILTIN_MASKMOVDQU);

  def_builtin (OPTION_MASK_ISA_SSE2, 0, "__builtin_ia32_clflush",
	       VOID_FTYPE_PCVOID, IX86_BUILTIN_CLFLUSH);
  x86_mfence = def_builtin (OPTION_MASK_ISA_SSE2, 0, "__builtin_ia32_mfence",
			    VOID_FTYPE_VOID, IX86_BUILTIN_MFENCE);

  /* SSE3.  */
  def_builtin (OPTION_MASK_ISA_SSE3, 0, "__builtin_ia32_monitor",
	       VOID_FTYPE_PCVOID_UNSIGNED_UNSIGNED, IX86_BUILTIN_MONITOR);
  def_builtin (OPTION_MASK_ISA_SSE3, 0, "__builtin_ia32_mwait",
	       VOID_FTYPE_UNSIGNED_UNSIGNED, IX86_BUILTIN_MWAIT);

  /* AES */
  def_builtin_const (OPTION_MASK_ISA_AES | OPTION_MASK_ISA_SSE2, 0,
		     "__builtin_ia32_aesenc128",
		     V2DI_FTYPE_V2DI_V2DI, IX86_BUILTIN_AESENC128);
  def_builtin_const (OPTION_MASK_ISA_AES | OPTION_MASK_ISA_SSE2, 0,
		     "__builtin_ia32_aesenclast128",
		     V2DI_FTYPE_V2DI_V2DI, IX86_BUILTIN_AESENCLAST128);
  def_builtin_const (OPTION_MASK_ISA_AES | OPTION_MASK_ISA_SSE2, 0,
		     "__builtin_ia32_aesdec128",
		     V2DI_FTYPE_V2DI_V2DI, IX86_BUILTIN_AESDEC128);
  def_builtin_const (OPTION_MASK_ISA_AES | OPTION_MASK_ISA_SSE2, 0,
		     "__builtin_ia32_aesdeclast128",
		     V2DI_FTYPE_V2DI_V2DI, IX86_BUILTIN_AESDECLAST128);
  def_builtin_const (OPTION_MASK_ISA_AES | OPTION_MASK_ISA_SSE2, 0,
		     "__builtin_ia32_aesimc128",
		     V2DI_FTYPE_V2DI, IX86_BUILTIN_AESIMC128);
  def_builtin_const (OPTION_MASK_ISA_AES | OPTION_MASK_ISA_SSE2, 0,
		     "__builtin_ia32_aeskeygenassist128",
		     V2DI_FTYPE_V2DI_INT, IX86_BUILTIN_AESKEYGENASSIST128);

  /* PCLMUL */
  def_builtin_const (OPTION_MASK_ISA_PCLMUL | OPTION_MASK_ISA_SSE2, 0,
		     "__builtin_ia32_pclmulqdq128",
		     V2DI_FTYPE_V2DI_V2DI_INT, IX86_BUILTIN_PCLMULQDQ128);

  /* RDRND */
  def_builtin (OPTION_MASK_ISA_RDRND, 0, "__builtin_ia32_rdrand16_step",
	       INT_FTYPE_PUSHORT, IX86_BUILTIN_RDRAND16_STEP);
  def_builtin (OPTION_MASK_ISA_RDRND, 0,  "__builtin_ia32_rdrand32_step",
	       INT_FTYPE_PUNSIGNED, IX86_BUILTIN_RDRAND32_STEP);
  def_builtin (OPTION_MASK_ISA_RDRND | OPTION_MASK_ISA_64BIT, 0,
	       "__builtin_ia32_rdrand64_step", INT_FTYPE_PULONGLONG,
	       IX86_BUILTIN_RDRAND64_STEP);

  /* AVX2 */
  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gathersiv2df",
		    V2DF_FTYPE_V2DF_PCDOUBLE_V4SI_V2DF_INT,
		    IX86_BUILTIN_GATHERSIV2DF);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gathersiv4df",
		    V4DF_FTYPE_V4DF_PCDOUBLE_V4SI_V4DF_INT,
		    IX86_BUILTIN_GATHERSIV4DF);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gatherdiv2df",
		    V2DF_FTYPE_V2DF_PCDOUBLE_V2DI_V2DF_INT,
		    IX86_BUILTIN_GATHERDIV2DF);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gatherdiv4df",
		    V4DF_FTYPE_V4DF_PCDOUBLE_V4DI_V4DF_INT,
		    IX86_BUILTIN_GATHERDIV4DF);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gathersiv4sf",
		    V4SF_FTYPE_V4SF_PCFLOAT_V4SI_V4SF_INT,
		    IX86_BUILTIN_GATHERSIV4SF);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gathersiv8sf",
		    V8SF_FTYPE_V8SF_PCFLOAT_V8SI_V8SF_INT,
		    IX86_BUILTIN_GATHERSIV8SF);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gatherdiv4sf",
		    V4SF_FTYPE_V4SF_PCFLOAT_V2DI_V4SF_INT,
		    IX86_BUILTIN_GATHERDIV4SF);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gatherdiv4sf256",
		    V4SF_FTYPE_V4SF_PCFLOAT_V4DI_V4SF_INT,
		    IX86_BUILTIN_GATHERDIV8SF);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gathersiv2di",
		    V2DI_FTYPE_V2DI_PCINT64_V4SI_V2DI_INT,
		    IX86_BUILTIN_GATHERSIV2DI);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gathersiv4di",
		    V4DI_FTYPE_V4DI_PCINT64_V4SI_V4DI_INT,
		    IX86_BUILTIN_GATHERSIV4DI);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gatherdiv2di",
		    V2DI_FTYPE_V2DI_PCINT64_V2DI_V2DI_INT,
		    IX86_BUILTIN_GATHERDIV2DI);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gatherdiv4di",
		    V4DI_FTYPE_V4DI_PCINT64_V4DI_V4DI_INT,
		    IX86_BUILTIN_GATHERDIV4DI);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gathersiv4si",
		    V4SI_FTYPE_V4SI_PCINT_V4SI_V4SI_INT,
		    IX86_BUILTIN_GATHERSIV4SI);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gathersiv8si",
		    V8SI_FTYPE_V8SI_PCINT_V8SI_V8SI_INT,
		    IX86_BUILTIN_GATHERSIV8SI);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gatherdiv4si",
		    V4SI_FTYPE_V4SI_PCINT_V2DI_V4SI_INT,
		    IX86_BUILTIN_GATHERDIV4SI);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gatherdiv4si256",
		    V4SI_FTYPE_V4SI_PCINT_V4DI_V4SI_INT,
		    IX86_BUILTIN_GATHERDIV8SI);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gatheraltsiv4df ",
		    V4DF_FTYPE_V4DF_PCDOUBLE_V8SI_V4DF_INT,
		    IX86_BUILTIN_GATHERALTSIV4DF);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gatheraltdiv8sf ",
		    V8SF_FTYPE_V8SF_PCFLOAT_V4DI_V8SF_INT,
		    IX86_BUILTIN_GATHERALTDIV8SF);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gatheraltsiv4di ",
		    V4DI_FTYPE_V4DI_PCINT64_V8SI_V4DI_INT,
		    IX86_BUILTIN_GATHERALTSIV4DI);

  def_builtin_pure (OPTION_MASK_ISA_AVX2, 0, "__builtin_ia32_gatheraltdiv8si ",
		    V8SI_FTYPE_V8SI_PCINT_V4DI_V8SI_INT,
		    IX86_BUILTIN_GATHERALTDIV8SI);

  /* AVX512F */
  def_builtin_pure (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_gathersiv16sf",
		    V16SF_FTYPE_V16SF_PCVOID_V16SI_HI_INT,
		    IX86_BUILTIN_GATHER3SIV16SF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_gathersiv8df",
		    V8DF_FTYPE_V8DF_PCVOID_V8SI_QI_INT,
		    IX86_BUILTIN_GATHER3SIV8DF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_gatherdiv16sf",
		    V8SF_FTYPE_V8SF_PCVOID_V8DI_QI_INT,
		    IX86_BUILTIN_GATHER3DIV16SF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_gatherdiv8df",
		    V8DF_FTYPE_V8DF_PCVOID_V8DI_QI_INT,
		    IX86_BUILTIN_GATHER3DIV8DF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_gathersiv16si",
		    V16SI_FTYPE_V16SI_PCVOID_V16SI_HI_INT,
		    IX86_BUILTIN_GATHER3SIV16SI);

  def_builtin_pure (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_gathersiv8di",
		    V8DI_FTYPE_V8DI_PCVOID_V8SI_QI_INT,
		    IX86_BUILTIN_GATHER3SIV8DI);

  def_builtin_pure (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_gatherdiv16si",
		    V8SI_FTYPE_V8SI_PCVOID_V8DI_QI_INT,
		    IX86_BUILTIN_GATHER3DIV16SI);

  def_builtin_pure (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_gatherdiv8di",
		    V8DI_FTYPE_V8DI_PCVOID_V8DI_QI_INT,
		    IX86_BUILTIN_GATHER3DIV8DI);

  def_builtin_pure (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_gather3altsiv8df ",
		    V8DF_FTYPE_V8DF_PCDOUBLE_V16SI_QI_INT,
		    IX86_BUILTIN_GATHER3ALTSIV8DF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_gather3altdiv16sf ",
		    V16SF_FTYPE_V16SF_PCFLOAT_V8DI_HI_INT,
		    IX86_BUILTIN_GATHER3ALTDIV16SF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_gather3altsiv8di ",
		    V8DI_FTYPE_V8DI_PCINT64_V16SI_QI_INT,
		    IX86_BUILTIN_GATHER3ALTSIV8DI);

  def_builtin_pure (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_gather3altdiv16si ",
		    V16SI_FTYPE_V16SI_PCINT_V8DI_HI_INT,
		    IX86_BUILTIN_GATHER3ALTDIV16SI);

  def_builtin (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_scattersiv16sf",
	       VOID_FTYPE_PVOID_HI_V16SI_V16SF_INT,
	       IX86_BUILTIN_SCATTERSIV16SF);

  def_builtin (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_scattersiv8df",
	       VOID_FTYPE_PVOID_QI_V8SI_V8DF_INT,
	       IX86_BUILTIN_SCATTERSIV8DF);

  def_builtin (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_scatterdiv16sf",
	       VOID_FTYPE_PVOID_QI_V8DI_V8SF_INT,
	       IX86_BUILTIN_SCATTERDIV16SF);

  def_builtin (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_scatterdiv8df",
	       VOID_FTYPE_PVOID_QI_V8DI_V8DF_INT,
	       IX86_BUILTIN_SCATTERDIV8DF);

  def_builtin (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_scattersiv16si",
	       VOID_FTYPE_PVOID_HI_V16SI_V16SI_INT,
	       IX86_BUILTIN_SCATTERSIV16SI);

  def_builtin (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_scattersiv8di",
	       VOID_FTYPE_PVOID_QI_V8SI_V8DI_INT,
	       IX86_BUILTIN_SCATTERSIV8DI);

  def_builtin (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_scatterdiv16si",
	       VOID_FTYPE_PVOID_QI_V8DI_V8SI_INT,
	       IX86_BUILTIN_SCATTERDIV16SI);

  def_builtin (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_scatterdiv8di",
	       VOID_FTYPE_PVOID_QI_V8DI_V8DI_INT,
	       IX86_BUILTIN_SCATTERDIV8DI);

  /* AVX512VL */
  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3siv2df",
		    V2DF_FTYPE_V2DF_PCVOID_V4SI_QI_INT,
		    IX86_BUILTIN_GATHER3SIV2DF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3siv4df",
		    V4DF_FTYPE_V4DF_PCVOID_V4SI_QI_INT,
		    IX86_BUILTIN_GATHER3SIV4DF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3div2df",
		    V2DF_FTYPE_V2DF_PCVOID_V2DI_QI_INT,
		    IX86_BUILTIN_GATHER3DIV2DF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3div4df",
		    V4DF_FTYPE_V4DF_PCVOID_V4DI_QI_INT,
		    IX86_BUILTIN_GATHER3DIV4DF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3siv4sf",
		    V4SF_FTYPE_V4SF_PCVOID_V4SI_QI_INT,
		    IX86_BUILTIN_GATHER3SIV4SF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3siv8sf",
		    V8SF_FTYPE_V8SF_PCVOID_V8SI_QI_INT,
		    IX86_BUILTIN_GATHER3SIV8SF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3div4sf",
		    V4SF_FTYPE_V4SF_PCVOID_V2DI_QI_INT,
		    IX86_BUILTIN_GATHER3DIV4SF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3div8sf",
		    V4SF_FTYPE_V4SF_PCVOID_V4DI_QI_INT,
		    IX86_BUILTIN_GATHER3DIV8SF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3siv2di",
		    V2DI_FTYPE_V2DI_PCVOID_V4SI_QI_INT,
		    IX86_BUILTIN_GATHER3SIV2DI);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3siv4di",
		    V4DI_FTYPE_V4DI_PCVOID_V4SI_QI_INT,
		    IX86_BUILTIN_GATHER3SIV4DI);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3div2di",
		    V2DI_FTYPE_V2DI_PCVOID_V2DI_QI_INT,
		    IX86_BUILTIN_GATHER3DIV2DI);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3div4di",
		    V4DI_FTYPE_V4DI_PCVOID_V4DI_QI_INT,
		    IX86_BUILTIN_GATHER3DIV4DI);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3siv4si",
		    V4SI_FTYPE_V4SI_PCVOID_V4SI_QI_INT,
		    IX86_BUILTIN_GATHER3SIV4SI);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3siv8si",
		    V8SI_FTYPE_V8SI_PCVOID_V8SI_QI_INT,
		    IX86_BUILTIN_GATHER3SIV8SI);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3div4si",
		    V4SI_FTYPE_V4SI_PCVOID_V2DI_QI_INT,
		    IX86_BUILTIN_GATHER3DIV4SI);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3div8si",
		    V4SI_FTYPE_V4SI_PCVOID_V4DI_QI_INT,
		    IX86_BUILTIN_GATHER3DIV8SI);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3altsiv4df ",
		    V4DF_FTYPE_V4DF_PCDOUBLE_V8SI_QI_INT,
		    IX86_BUILTIN_GATHER3ALTSIV4DF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3altdiv8sf ",
		    V8SF_FTYPE_V8SF_PCFLOAT_V4DI_QI_INT,
		    IX86_BUILTIN_GATHER3ALTDIV8SF);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3altsiv4di ",
		    V4DI_FTYPE_V4DI_PCINT64_V8SI_QI_INT,
		    IX86_BUILTIN_GATHER3ALTSIV4DI);

  def_builtin_pure (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_gather3altdiv8si ",
		    V8SI_FTYPE_V8SI_PCINT_V4DI_QI_INT,
		    IX86_BUILTIN_GATHER3ALTDIV8SI);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scattersiv8sf",
	       VOID_FTYPE_PVOID_QI_V8SI_V8SF_INT,
	       IX86_BUILTIN_SCATTERSIV8SF);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scattersiv4sf",
	       VOID_FTYPE_PVOID_QI_V4SI_V4SF_INT,
	       IX86_BUILTIN_SCATTERSIV4SF);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scattersiv4df",
	       VOID_FTYPE_PVOID_QI_V4SI_V4DF_INT,
	       IX86_BUILTIN_SCATTERSIV4DF);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scattersiv2df",
	       VOID_FTYPE_PVOID_QI_V4SI_V2DF_INT,
	       IX86_BUILTIN_SCATTERSIV2DF);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatterdiv8sf",
	       VOID_FTYPE_PVOID_QI_V4DI_V4SF_INT,
	       IX86_BUILTIN_SCATTERDIV8SF);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatterdiv4sf",
	       VOID_FTYPE_PVOID_QI_V2DI_V4SF_INT,
	       IX86_BUILTIN_SCATTERDIV4SF);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatterdiv4df",
	       VOID_FTYPE_PVOID_QI_V4DI_V4DF_INT,
	       IX86_BUILTIN_SCATTERDIV4DF);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatterdiv2df",
	       VOID_FTYPE_PVOID_QI_V2DI_V2DF_INT,
	       IX86_BUILTIN_SCATTERDIV2DF);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scattersiv8si",
	       VOID_FTYPE_PVOID_QI_V8SI_V8SI_INT,
	       IX86_BUILTIN_SCATTERSIV8SI);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scattersiv4si",
	       VOID_FTYPE_PVOID_QI_V4SI_V4SI_INT,
	       IX86_BUILTIN_SCATTERSIV4SI);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scattersiv4di",
	       VOID_FTYPE_PVOID_QI_V4SI_V4DI_INT,
	       IX86_BUILTIN_SCATTERSIV4DI);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scattersiv2di",
	       VOID_FTYPE_PVOID_QI_V4SI_V2DI_INT,
	       IX86_BUILTIN_SCATTERSIV2DI);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatterdiv8si",
	       VOID_FTYPE_PVOID_QI_V4DI_V4SI_INT,
	       IX86_BUILTIN_SCATTERDIV8SI);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatterdiv4si",
	       VOID_FTYPE_PVOID_QI_V2DI_V4SI_INT,
	       IX86_BUILTIN_SCATTERDIV4SI);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatterdiv4di",
	       VOID_FTYPE_PVOID_QI_V4DI_V4DI_INT,
	       IX86_BUILTIN_SCATTERDIV4DI);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatterdiv2di",
	       VOID_FTYPE_PVOID_QI_V2DI_V2DI_INT,
	       IX86_BUILTIN_SCATTERDIV2DI);

  def_builtin (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_scatteraltsiv8df ",
	       VOID_FTYPE_PDOUBLE_QI_V16SI_V8DF_INT,
	       IX86_BUILTIN_SCATTERALTSIV8DF);

  def_builtin (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_scatteraltdiv16sf ",
	       VOID_FTYPE_PFLOAT_HI_V8DI_V16SF_INT,
	       IX86_BUILTIN_SCATTERALTDIV16SF);

  def_builtin (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_scatteraltsiv8di ",
	       VOID_FTYPE_PLONGLONG_QI_V16SI_V8DI_INT,
	       IX86_BUILTIN_SCATTERALTSIV8DI);

  def_builtin (OPTION_MASK_ISA_AVX512F, 0, "__builtin_ia32_scatteraltdiv16si ",
	       VOID_FTYPE_PINT_HI_V8DI_V16SI_INT,
	       IX86_BUILTIN_SCATTERALTDIV16SI);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatteraltsiv4df ",
	       VOID_FTYPE_PDOUBLE_QI_V8SI_V4DF_INT,
	       IX86_BUILTIN_SCATTERALTSIV4DF);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatteraltdiv8sf ",
	       VOID_FTYPE_PFLOAT_QI_V4DI_V8SF_INT,
	       IX86_BUILTIN_SCATTERALTDIV8SF);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatteraltsiv4di ",
	       VOID_FTYPE_PLONGLONG_QI_V8SI_V4DI_INT,
	       IX86_BUILTIN_SCATTERALTSIV4DI);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatteraltdiv8si ",
	       VOID_FTYPE_PINT_QI_V4DI_V8SI_INT,
	       IX86_BUILTIN_SCATTERALTDIV8SI);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatteraltsiv2df ",
	       VOID_FTYPE_PDOUBLE_QI_V4SI_V2DF_INT,
	       IX86_BUILTIN_SCATTERALTSIV2DF);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatteraltdiv4sf ",
	       VOID_FTYPE_PFLOAT_QI_V2DI_V4SF_INT,
	       IX86_BUILTIN_SCATTERALTDIV4SF);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatteraltsiv2di ",
	       VOID_FTYPE_PLONGLONG_QI_V4SI_V2DI_INT,
	       IX86_BUILTIN_SCATTERALTSIV2DI);

  def_builtin (OPTION_MASK_ISA_AVX512VL, 0, "__builtin_ia32_scatteraltdiv4si ",
	       VOID_FTYPE_PINT_QI_V2DI_V4SI_INT,
	       IX86_BUILTIN_SCATTERALTDIV4SI);

  /* AVX512PF */
  def_builtin (OPTION_MASK_ISA_AVX512PF, 0, "__builtin_ia32_gatherpfdpd",
	       VOID_FTYPE_QI_V8SI_PCVOID_INT_INT,
	       IX86_BUILTIN_GATHERPFDPD);
  def_builtin (OPTION_MASK_ISA_AVX512PF, 0, "__builtin_ia32_gatherpfdps",
	       VOID_FTYPE_HI_V16SI_PCVOID_INT_INT,
	       IX86_BUILTIN_GATHERPFDPS);
  def_builtin (OPTION_MASK_ISA_AVX512PF, 0, "__builtin_ia32_gatherpfqpd",
	       VOID_FTYPE_QI_V8DI_PCVOID_INT_INT,
	       IX86_BUILTIN_GATHERPFQPD);
  def_builtin (OPTION_MASK_ISA_AVX512PF, 0, "__builtin_ia32_gatherpfqps",
	       VOID_FTYPE_QI_V8DI_PCVOID_INT_INT,
	       IX86_BUILTIN_GATHERPFQPS);
  def_builtin (OPTION_MASK_ISA_AVX512PF, 0, "__builtin_ia32_scatterpfdpd",
	       VOID_FTYPE_QI_V8SI_PCVOID_INT_INT,
	       IX86_BUILTIN_SCATTERPFDPD);
  def_builtin (OPTION_MASK_ISA_AVX512PF, 0, "__builtin_ia32_scatterpfdps",
	       VOID_FTYPE_HI_V16SI_PCVOID_INT_INT,
	       IX86_BUILTIN_SCATTERPFDPS);
  def_builtin (OPTION_MASK_ISA_AVX512PF, 0, "__builtin_ia32_scatterpfqpd",
	       VOID_FTYPE_QI_V8DI_PCVOID_INT_INT,
	       IX86_BUILTIN_SCATTERPFQPD);
  def_builtin (OPTION_MASK_ISA_AVX512PF, 0, "__builtin_ia32_scatterpfqps",
	       VOID_FTYPE_QI_V8DI_PCVOID_INT_INT,
	       IX86_BUILTIN_SCATTERPFQPS);

  /* SHA */
  def_builtin_const (OPTION_MASK_ISA_SHA, 0, "__builtin_ia32_sha1msg1",
		     V4SI_FTYPE_V4SI_V4SI, IX86_BUILTIN_SHA1MSG1);
  def_builtin_const (OPTION_MASK_ISA_SHA, 0, "__builtin_ia32_sha1msg2",
		     V4SI_FTYPE_V4SI_V4SI, IX86_BUILTIN_SHA1MSG2);
  def_builtin_const (OPTION_MASK_ISA_SHA, 0, "__builtin_ia32_sha1nexte",
		     V4SI_FTYPE_V4SI_V4SI, IX86_BUILTIN_SHA1NEXTE);
  def_builtin_const (OPTION_MASK_ISA_SHA, 0, "__builtin_ia32_sha1rnds4",
		     V4SI_FTYPE_V4SI_V4SI_INT, IX86_BUILTIN_SHA1RNDS4);
  def_builtin_const (OPTION_MASK_ISA_SHA, 0, "__builtin_ia32_sha256msg1",
		     V4SI_FTYPE_V4SI_V4SI, IX86_BUILTIN_SHA256MSG1);
  def_builtin_const (OPTION_MASK_ISA_SHA, 0, "__builtin_ia32_sha256msg2",
		     V4SI_FTYPE_V4SI_V4SI, IX86_BUILTIN_SHA256MSG2);
  def_builtin_const (OPTION_MASK_ISA_SHA, 0, "__builtin_ia32_sha256rnds2",
		     V4SI_FTYPE_V4SI_V4SI_V4SI, IX86_BUILTIN_SHA256RNDS2);

  /* RTM.  */
  def_builtin (OPTION_MASK_ISA_RTM, 0, "__builtin_ia32_xabort",
	       VOID_FTYPE_UNSIGNED, IX86_BUILTIN_XABORT);

  /* MMX access to the vec_init patterns.  */
  def_builtin_const (OPTION_MASK_ISA_MMX, 0,
		     "__builtin_ia32_vec_init_v2si",
		     V2SI_FTYPE_INT_INT, IX86_BUILTIN_VEC_INIT_V2SI);

  def_builtin_const (OPTION_MASK_ISA_MMX, 0,
		     "__builtin_ia32_vec_init_v4hi",
		     V4HI_FTYPE_HI_HI_HI_HI,
		     IX86_BUILTIN_VEC_INIT_V4HI);

  def_builtin_const (OPTION_MASK_ISA_MMX, 0,
		     "__builtin_ia32_vec_init_v8qi",
		     V8QI_FTYPE_QI_QI_QI_QI_QI_QI_QI_QI,
		     IX86_BUILTIN_VEC_INIT_V8QI);

  /* Access to the vec_extract patterns.  */
  def_builtin_const (OPTION_MASK_ISA_SSE2, 0, "__builtin_ia32_vec_ext_v2df",
		     DOUBLE_FTYPE_V2DF_INT, IX86_BUILTIN_VEC_EXT_V2DF);
  def_builtin_const (OPTION_MASK_ISA_SSE2, 0, "__builtin_ia32_vec_ext_v2di",
		     DI_FTYPE_V2DI_INT, IX86_BUILTIN_VEC_EXT_V2DI);
  def_builtin_const (OPTION_MASK_ISA_SSE, 0, "__builtin_ia32_vec_ext_v4sf",
		     FLOAT_FTYPE_V4SF_INT, IX86_BUILTIN_VEC_EXT_V4SF);
  def_builtin_const (OPTION_MASK_ISA_SSE2, 0, "__builtin_ia32_vec_ext_v4si",
		     SI_FTYPE_V4SI_INT, IX86_BUILTIN_VEC_EXT_V4SI);
  def_builtin_const (OPTION_MASK_ISA_SSE2, 0, "__builtin_ia32_vec_ext_v8hi",
		     HI_FTYPE_V8HI_INT, IX86_BUILTIN_VEC_EXT_V8HI);

  def_builtin_const (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A
		     /* As it uses V4HImode, we have to require -mmmx too.  */
		     | OPTION_MASK_ISA_MMX, 0,
		     "__builtin_ia32_vec_ext_v4hi",
		     HI_FTYPE_V4HI_INT, IX86_BUILTIN_VEC_EXT_V4HI);

  def_builtin_const (OPTION_MASK_ISA_MMX, 0,
		     "__builtin_ia32_vec_ext_v2si",
		     SI_FTYPE_V2SI_INT, IX86_BUILTIN_VEC_EXT_V2SI);

  def_builtin_const (OPTION_MASK_ISA_SSE2, 0, "__builtin_ia32_vec_ext_v16qi",
		     QI_FTYPE_V16QI_INT, IX86_BUILTIN_VEC_EXT_V16QI);

  /* Access to the vec_set patterns.  */
  def_builtin_const (OPTION_MASK_ISA_SSE4_1 | OPTION_MASK_ISA_64BIT, 0,
		     "__builtin_ia32_vec_set_v2di",
		     V2DI_FTYPE_V2DI_DI_INT, IX86_BUILTIN_VEC_SET_V2DI);

  def_builtin_const (OPTION_MASK_ISA_SSE4_1, 0, "__builtin_ia32_vec_set_v4sf",
		     V4SF_FTYPE_V4SF_FLOAT_INT, IX86_BUILTIN_VEC_SET_V4SF);

  def_builtin_const (OPTION_MASK_ISA_SSE4_1, 0, "__builtin_ia32_vec_set_v4si",
		     V4SI_FTYPE_V4SI_SI_INT, IX86_BUILTIN_VEC_SET_V4SI);

  def_builtin_const (OPTION_MASK_ISA_SSE2, 0, "__builtin_ia32_vec_set_v8hi",
		     V8HI_FTYPE_V8HI_HI_INT, IX86_BUILTIN_VEC_SET_V8HI);

  def_builtin_const (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A
		     /* As it uses V4HImode, we have to require -mmmx too.  */
		     | OPTION_MASK_ISA_MMX, 0,
		     "__builtin_ia32_vec_set_v4hi",
		     V4HI_FTYPE_V4HI_HI_INT, IX86_BUILTIN_VEC_SET_V4HI);

  def_builtin_const (OPTION_MASK_ISA_SSE4_1, 0, "__builtin_ia32_vec_set_v16qi",
		     V16QI_FTYPE_V16QI_QI_INT, IX86_BUILTIN_VEC_SET_V16QI);

  /* RDSEED */
  def_builtin (OPTION_MASK_ISA_RDSEED, 0, "__builtin_ia32_rdseed_hi_step",
	       INT_FTYPE_PUSHORT, IX86_BUILTIN_RDSEED16_STEP);
  def_builtin (OPTION_MASK_ISA_RDSEED, 0, "__builtin_ia32_rdseed_si_step",
	       INT_FTYPE_PUNSIGNED, IX86_BUILTIN_RDSEED32_STEP);
  def_builtin (OPTION_MASK_ISA_RDSEED | OPTION_MASK_ISA_64BIT, 0,
	       "__builtin_ia32_rdseed_di_step",
	       INT_FTYPE_PULONGLONG, IX86_BUILTIN_RDSEED64_STEP);

  /* ADCX */
  def_builtin (0, 0, "__builtin_ia32_addcarryx_u32",
	       UCHAR_FTYPE_UCHAR_UINT_UINT_PUNSIGNED, IX86_BUILTIN_ADDCARRYX32);
  def_builtin (OPTION_MASK_ISA_64BIT, 0,
	       "__builtin_ia32_addcarryx_u64",
	       UCHAR_FTYPE_UCHAR_ULONGLONG_ULONGLONG_PULONGLONG,
	       IX86_BUILTIN_ADDCARRYX64);

  /* SBB */
  def_builtin (0, 0, "__builtin_ia32_sbb_u32",
	       UCHAR_FTYPE_UCHAR_UINT_UINT_PUNSIGNED, IX86_BUILTIN_SBB32);
  def_builtin (OPTION_MASK_ISA_64BIT, 0,
	       "__builtin_ia32_sbb_u64",
	       UCHAR_FTYPE_UCHAR_ULONGLONG_ULONGLONG_PULONGLONG,
	       IX86_BUILTIN_SBB64);

  /* Read/write FLAGS.  */
  if (TARGET_64BIT)
    {
      def_builtin (OPTION_MASK_ISA_64BIT, 0, "__builtin_ia32_readeflags_u64",
		   UINT64_FTYPE_VOID, IX86_BUILTIN_READ_FLAGS);
      def_builtin (OPTION_MASK_ISA_64BIT, 0, "__builtin_ia32_writeeflags_u64",
		   VOID_FTYPE_UINT64, IX86_BUILTIN_WRITE_FLAGS);
    }
  else
    {
      def_builtin (0, 0, "__builtin_ia32_readeflags_u32",
		   UNSIGNED_FTYPE_VOID, IX86_BUILTIN_READ_FLAGS);
      def_builtin (0, 0, "__builtin_ia32_writeeflags_u32",
		   VOID_FTYPE_UNSIGNED, IX86_BUILTIN_WRITE_FLAGS);
    }

  /* CLFLUSHOPT.  */
  def_builtin (OPTION_MASK_ISA_CLFLUSHOPT, 0, "__builtin_ia32_clflushopt",
	       VOID_FTYPE_PCVOID, IX86_BUILTIN_CLFLUSHOPT);

  /* CLWB.  */
  def_builtin (OPTION_MASK_ISA_CLWB, 0, "__builtin_ia32_clwb",
	       VOID_FTYPE_PCVOID, IX86_BUILTIN_CLWB);

  /* MONITORX and MWAITX.  */
  def_builtin (0, OPTION_MASK_ISA2_MWAITX, "__builtin_ia32_monitorx",
		VOID_FTYPE_PCVOID_UNSIGNED_UNSIGNED, IX86_BUILTIN_MONITORX);
  def_builtin (0, OPTION_MASK_ISA2_MWAITX, "__builtin_ia32_mwaitx",
		VOID_FTYPE_UNSIGNED_UNSIGNED_UNSIGNED, IX86_BUILTIN_MWAITX);

  /* CLZERO.  */
  def_builtin (0, OPTION_MASK_ISA2_CLZERO, "__builtin_ia32_clzero",
		VOID_FTYPE_PCVOID, IX86_BUILTIN_CLZERO);

  /* WAITPKG.  */
  def_builtin (0, OPTION_MASK_ISA2_WAITPKG, "__builtin_ia32_umonitor",
	       VOID_FTYPE_PVOID, IX86_BUILTIN_UMONITOR);
  def_builtin (0, OPTION_MASK_ISA2_WAITPKG, "__builtin_ia32_umwait",
	       UINT8_FTYPE_UNSIGNED_UINT64, IX86_BUILTIN_UMWAIT);
  def_builtin (0, OPTION_MASK_ISA2_WAITPKG, "__builtin_ia32_tpause",
	       UINT8_FTYPE_UNSIGNED_UINT64, IX86_BUILTIN_TPAUSE);

  /* CLDEMOTE.  */
  def_builtin (0, OPTION_MASK_ISA2_CLDEMOTE, "__builtin_ia32_cldemote",
	       VOID_FTYPE_PCVOID, IX86_BUILTIN_CLDEMOTE);

  /* Add FMA4 multi-arg argument instructions */
  for (i = 0, d = bdesc_multi_arg; i < ARRAY_SIZE (bdesc_multi_arg); i++, d++)
    {
      BDESC_VERIFY (d->code, IX86_BUILTIN__BDESC_MULTI_ARG_FIRST, i);
      if (d->name == 0)
	continue;

      ftype = (enum ix86_builtin_func_type) d->flag;
      def_builtin_const (d->mask, d->mask2, d->name, ftype, d->code);
    }
  BDESC_VERIFYS (IX86_BUILTIN__BDESC_MULTI_ARG_LAST,
		 IX86_BUILTIN__BDESC_MULTI_ARG_FIRST,
		 ARRAY_SIZE (bdesc_multi_arg) - 1);

  /* Add CET inrinsics.  */
  for (i = 0, d = bdesc_cet; i < ARRAY_SIZE (bdesc_cet); i++, d++)
    {
      BDESC_VERIFY (d->code, IX86_BUILTIN__BDESC_CET_FIRST, i);
      if (d->name == 0)
	continue;

      ftype = (enum ix86_builtin_func_type) d->flag;
      def_builtin (d->mask, d->mask2, d->name, ftype, d->code);
    }
  BDESC_VERIFYS (IX86_BUILTIN__BDESC_CET_LAST,
		 IX86_BUILTIN__BDESC_CET_FIRST,
		 ARRAY_SIZE (bdesc_cet) - 1);

  for (i = 0, d = bdesc_cet_rdssp;
       i < ARRAY_SIZE (bdesc_cet_rdssp);
       i++, d++)
    {
      BDESC_VERIFY (d->code, IX86_BUILTIN__BDESC_CET_NORMAL_FIRST, i);
      if (d->name == 0)
	continue;

      ftype = (enum ix86_builtin_func_type) d->flag;
      def_builtin (d->mask, d->mask2, d->name, ftype, d->code);
    }
  BDESC_VERIFYS (IX86_BUILTIN__BDESC_CET_NORMAL_LAST,
		 IX86_BUILTIN__BDESC_CET_NORMAL_FIRST,
		 ARRAY_SIZE (bdesc_cet_rdssp) - 1);
}

#undef BDESC_VERIFY
#undef BDESC_VERIFYS

/* Make builtins to detect cpu type and features supported.  NAME is
   the builtin name, CODE is the builtin code, and FTYPE is the function
   type of the builtin.  */

static void
make_cpu_type_builtin (const char* name, int code,
		       enum ix86_builtin_func_type ftype, bool is_const)
{
  tree decl;
  tree type;

  type = ix86_get_builtin_func_type (ftype);
  decl = add_builtin_function (name, type, code, BUILT_IN_MD,
			       NULL, NULL_TREE);
  gcc_assert (decl != NULL_TREE);
  ix86_builtins[(int) code] = decl;
  TREE_READONLY (decl) = is_const;
}

/* Make builtins to get CPU type and features supported.  The created
   builtins are :

   __builtin_cpu_init (), to detect cpu type and features,
   __builtin_cpu_is ("<CPUNAME>"), to check if cpu is of type <CPUNAME>,
   __builtin_cpu_supports ("<FEATURE>"), to check if cpu supports <FEATURE>
   */

static void
ix86_init_platform_type_builtins (void)
{
  make_cpu_type_builtin ("__builtin_cpu_init", IX86_BUILTIN_CPU_INIT,
			 INT_FTYPE_VOID, false);
  make_cpu_type_builtin ("__builtin_cpu_is", IX86_BUILTIN_CPU_IS,
			 INT_FTYPE_PCCHAR, true);
  make_cpu_type_builtin ("__builtin_cpu_supports", IX86_BUILTIN_CPU_SUPPORTS,
			 INT_FTYPE_PCCHAR, true);
}

/* Internal method for ix86_init_builtins.  */

static void
ix86_init_builtins_va_builtins_abi (void)
{
  tree ms_va_ref, sysv_va_ref;
  tree fnvoid_va_end_ms, fnvoid_va_end_sysv;
  tree fnvoid_va_start_ms, fnvoid_va_start_sysv;
  tree fnvoid_va_copy_ms, fnvoid_va_copy_sysv;
  tree fnattr_ms = NULL_TREE, fnattr_sysv = NULL_TREE;

  if (!TARGET_64BIT)
    return;
  fnattr_ms = build_tree_list (get_identifier ("ms_abi"), NULL_TREE);
  fnattr_sysv = build_tree_list (get_identifier ("sysv_abi"), NULL_TREE);
  ms_va_ref = build_reference_type (ms_va_list_type_node);
  sysv_va_ref = build_pointer_type (TREE_TYPE (sysv_va_list_type_node));

  fnvoid_va_end_ms = build_function_type_list (void_type_node, ms_va_ref,
					       NULL_TREE);
  fnvoid_va_start_ms
    = build_varargs_function_type_list (void_type_node, ms_va_ref, NULL_TREE);
  fnvoid_va_end_sysv
    = build_function_type_list (void_type_node, sysv_va_ref, NULL_TREE);
  fnvoid_va_start_sysv
    = build_varargs_function_type_list (void_type_node, sysv_va_ref,
					NULL_TREE);
  fnvoid_va_copy_ms
    = build_function_type_list (void_type_node, ms_va_ref,
				ms_va_list_type_node, NULL_TREE);
  fnvoid_va_copy_sysv
    = build_function_type_list (void_type_node, sysv_va_ref,
				sysv_va_ref, NULL_TREE);

  add_builtin_function ("__builtin_ms_va_start", fnvoid_va_start_ms,
  			BUILT_IN_VA_START, BUILT_IN_NORMAL, NULL, fnattr_ms);
  add_builtin_function ("__builtin_ms_va_end", fnvoid_va_end_ms,
  			BUILT_IN_VA_END, BUILT_IN_NORMAL, NULL, fnattr_ms);
  add_builtin_function ("__builtin_ms_va_copy", fnvoid_va_copy_ms,
			BUILT_IN_VA_COPY, BUILT_IN_NORMAL, NULL, fnattr_ms);
  add_builtin_function ("__builtin_sysv_va_start", fnvoid_va_start_sysv,
  			BUILT_IN_VA_START, BUILT_IN_NORMAL, NULL, fnattr_sysv);
  add_builtin_function ("__builtin_sysv_va_end", fnvoid_va_end_sysv,
  			BUILT_IN_VA_END, BUILT_IN_NORMAL, NULL, fnattr_sysv);
  add_builtin_function ("__builtin_sysv_va_copy", fnvoid_va_copy_sysv,
			BUILT_IN_VA_COPY, BUILT_IN_NORMAL, NULL, fnattr_sysv);
}

static void
ix86_init_builtin_types (void)
{
  tree float80_type_node, const_string_type_node;

  /* The __float80 type.  */
  float80_type_node = long_double_type_node;
  if (TYPE_MODE (float80_type_node) != XFmode)
    {
      if (float64x_type_node != NULL_TREE
	  && TYPE_MODE (float64x_type_node) == XFmode)
	float80_type_node = float64x_type_node;
      else
	{
	  /* The __float80 type.  */
	  float80_type_node = make_node (REAL_TYPE);

	  TYPE_PRECISION (float80_type_node) = 80;
	  layout_type (float80_type_node);
	}
    }
  lang_hooks.types.register_builtin_type (float80_type_node, "__float80");

  /* The __float128 type.  The node has already been created as
     _Float128, so we only need to register the __float128 name for
     it.  */
  lang_hooks.types.register_builtin_type (float128_type_node, "__float128");

  const_string_type_node
    = build_pointer_type (build_qualified_type
			  (char_type_node, TYPE_QUAL_CONST));

  /* This macro is built by i386-builtin-types.awk.  */
  DEFINE_BUILTIN_PRIMITIVE_TYPES;
}

void
ix86_init_builtins (void)
{
  tree ftype, decl;

  ix86_init_builtin_types ();

  /* Builtins to get CPU type and features. */
  ix86_init_platform_type_builtins ();

  /* TFmode support builtins.  */
  def_builtin_const (0, 0, "__builtin_infq",
		     FLOAT128_FTYPE_VOID, IX86_BUILTIN_INFQ);
  def_builtin_const (0, 0, "__builtin_huge_valq",
		     FLOAT128_FTYPE_VOID, IX86_BUILTIN_HUGE_VALQ);

  ftype = ix86_get_builtin_func_type (FLOAT128_FTYPE_CONST_STRING);
  decl = add_builtin_function ("__builtin_nanq", ftype, IX86_BUILTIN_NANQ,
			       BUILT_IN_MD, "nanq", NULL_TREE);
  TREE_READONLY (decl) = 1;
  ix86_builtins[(int) IX86_BUILTIN_NANQ] = decl;

  decl = add_builtin_function ("__builtin_nansq", ftype, IX86_BUILTIN_NANSQ,
			       BUILT_IN_MD, "nansq", NULL_TREE);
  TREE_READONLY (decl) = 1;
  ix86_builtins[(int) IX86_BUILTIN_NANSQ] = decl;

  /* We will expand them to normal call if SSE isn't available since
     they are used by libgcc. */
  ftype = ix86_get_builtin_func_type (FLOAT128_FTYPE_FLOAT128);
  decl = add_builtin_function ("__builtin_fabsq", ftype, IX86_BUILTIN_FABSQ,
			       BUILT_IN_MD, "__fabstf2", NULL_TREE);
  TREE_READONLY (decl) = 1;
  ix86_builtins[(int) IX86_BUILTIN_FABSQ] = decl;

  ftype = ix86_get_builtin_func_type (FLOAT128_FTYPE_FLOAT128_FLOAT128);
  decl = add_builtin_function ("__builtin_copysignq", ftype,
			       IX86_BUILTIN_COPYSIGNQ, BUILT_IN_MD,
			       "__copysigntf3", NULL_TREE);
  TREE_READONLY (decl) = 1;
  ix86_builtins[(int) IX86_BUILTIN_COPYSIGNQ] = decl;

  ix86_init_tm_builtins ();
  ix86_init_mmx_sse_builtins ();

  if (TARGET_LP64)
    ix86_init_builtins_va_builtins_abi ();

#ifdef SUBTARGET_INIT_BUILTINS
  SUBTARGET_INIT_BUILTINS;
#endif
}

/* Return the ix86 builtin for CODE.  */

tree
ix86_builtin_decl (unsigned code, bool)
{
  if (code >= IX86_BUILTIN_MAX)
    return error_mark_node;

  return ix86_builtins[code];
}

/* This returns the target-specific builtin with code CODE if
   current_function_decl has visibility on this builtin, which is checked
   using isa flags.  Returns NULL_TREE otherwise.  */

static tree ix86_get_builtin (enum ix86_builtins code)
{
  struct cl_target_option *opts;
  tree target_tree = NULL_TREE;

  /* Determine the isa flags of current_function_decl.  */

  if (current_function_decl)
    target_tree = DECL_FUNCTION_SPECIFIC_TARGET (current_function_decl);

  if (target_tree == NULL)
    target_tree = target_option_default_node;

  opts = TREE_TARGET_OPTION (target_tree);

  if ((ix86_builtins_isa[(int) code].isa & opts->x_ix86_isa_flags)
      || (ix86_builtins_isa[(int) code].isa2 & opts->x_ix86_isa_flags2))
    return ix86_builtin_decl (code, true);
  else
    return NULL_TREE;
}

/* Vectorization library interface and handlers.  */
tree (*ix86_veclib_handler) (combined_fn, tree, tree);

/* Returns a function decl for a vectorized version of the combined function
   with combined_fn code FN and the result vector type TYPE, or NULL_TREE
   if it is not available.  */

tree
ix86_builtin_vectorized_function (unsigned int fn, tree type_out,
				  tree type_in)
{
  machine_mode in_mode, out_mode;
  int in_n, out_n;

  if (TREE_CODE (type_out) != VECTOR_TYPE
      || TREE_CODE (type_in) != VECTOR_TYPE)
    return NULL_TREE;

  out_mode = TYPE_MODE (TREE_TYPE (type_out));
  out_n = TYPE_VECTOR_SUBPARTS (type_out);
  in_mode = TYPE_MODE (TREE_TYPE (type_in));
  in_n = TYPE_VECTOR_SUBPARTS (type_in);

  switch (fn)
    {
    CASE_CFN_EXP2:
      if (out_mode == SFmode && in_mode == SFmode)
	{
	  if (out_n == 16 && in_n == 16)
	    return ix86_get_builtin (IX86_BUILTIN_EXP2PS);
	}
      break;

    CASE_CFN_IFLOOR:
    CASE_CFN_LFLOOR:
    CASE_CFN_LLFLOOR:
      /* The round insn does not trap on denormals.  */
      if (flag_trapping_math || !TARGET_SSE4_1)
	break;

      if (out_mode == SImode && in_mode == DFmode)
	{
	  if (out_n == 4 && in_n == 2)
	    return ix86_get_builtin (IX86_BUILTIN_FLOORPD_VEC_PACK_SFIX);
	  else if (out_n == 8 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_FLOORPD_VEC_PACK_SFIX256);
	  else if (out_n == 16 && in_n == 8)
	    return ix86_get_builtin (IX86_BUILTIN_FLOORPD_VEC_PACK_SFIX512);
	}
      if (out_mode == SImode && in_mode == SFmode)
	{
	  if (out_n == 4 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_FLOORPS_SFIX);
	  else if (out_n == 8 && in_n == 8)
	    return ix86_get_builtin (IX86_BUILTIN_FLOORPS_SFIX256);
	  else if (out_n == 16 && in_n == 16)
	    return ix86_get_builtin (IX86_BUILTIN_FLOORPS_SFIX512);
	}
      break;

    CASE_CFN_ICEIL:
    CASE_CFN_LCEIL:
    CASE_CFN_LLCEIL:
      /* The round insn does not trap on denormals.  */
      if (flag_trapping_math || !TARGET_SSE4_1)
	break;

      if (out_mode == SImode && in_mode == DFmode)
	{
	  if (out_n == 4 && in_n == 2)
	    return ix86_get_builtin (IX86_BUILTIN_CEILPD_VEC_PACK_SFIX);
	  else if (out_n == 8 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_CEILPD_VEC_PACK_SFIX256);
	  else if (out_n == 16 && in_n == 8)
	    return ix86_get_builtin (IX86_BUILTIN_CEILPD_VEC_PACK_SFIX512);
	}
      if (out_mode == SImode && in_mode == SFmode)
	{
	  if (out_n == 4 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_CEILPS_SFIX);
	  else if (out_n == 8 && in_n == 8)
	    return ix86_get_builtin (IX86_BUILTIN_CEILPS_SFIX256);
	  else if (out_n == 16 && in_n == 16)
	    return ix86_get_builtin (IX86_BUILTIN_CEILPS_SFIX512);
	}
      break;

    CASE_CFN_IRINT:
    CASE_CFN_LRINT:
    CASE_CFN_LLRINT:
      if (out_mode == SImode && in_mode == DFmode)
	{
	  if (out_n == 4 && in_n == 2)
	    return ix86_get_builtin (IX86_BUILTIN_VEC_PACK_SFIX);
	  else if (out_n == 8 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_VEC_PACK_SFIX256);
	  else if (out_n == 16 && in_n == 8)
	    return ix86_get_builtin (IX86_BUILTIN_VEC_PACK_SFIX512);
	}
      if (out_mode == SImode && in_mode == SFmode)
	{
	  if (out_n == 4 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_CVTPS2DQ);
	  else if (out_n == 8 && in_n == 8)
	    return ix86_get_builtin (IX86_BUILTIN_CVTPS2DQ256);
	  else if (out_n == 16 && in_n == 16)
	    return ix86_get_builtin (IX86_BUILTIN_CVTPS2DQ512);
	}
      break;

    CASE_CFN_IROUND:
    CASE_CFN_LROUND:
    CASE_CFN_LLROUND:
      /* The round insn does not trap on denormals.  */
      if (flag_trapping_math || !TARGET_SSE4_1)
	break;

      if (out_mode == SImode && in_mode == DFmode)
	{
	  if (out_n == 4 && in_n == 2)
	    return ix86_get_builtin (IX86_BUILTIN_ROUNDPD_AZ_VEC_PACK_SFIX);
	  else if (out_n == 8 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_ROUNDPD_AZ_VEC_PACK_SFIX256);
	  else if (out_n == 16 && in_n == 8)
	    return ix86_get_builtin (IX86_BUILTIN_ROUNDPD_AZ_VEC_PACK_SFIX512);
	}
      if (out_mode == SImode && in_mode == SFmode)
	{
	  if (out_n == 4 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_ROUNDPS_AZ_SFIX);
	  else if (out_n == 8 && in_n == 8)
	    return ix86_get_builtin (IX86_BUILTIN_ROUNDPS_AZ_SFIX256);
	  else if (out_n == 16 && in_n == 16)
	    return ix86_get_builtin (IX86_BUILTIN_ROUNDPS_AZ_SFIX512);
	}
      break;

    CASE_CFN_FLOOR:
      /* The round insn does not trap on denormals.  */
      if (flag_trapping_math || !TARGET_SSE4_1)
	break;

      if (out_mode == DFmode && in_mode == DFmode)
	{
	  if (out_n == 2 && in_n == 2)
	    return ix86_get_builtin (IX86_BUILTIN_FLOORPD);
	  else if (out_n == 4 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_FLOORPD256);
	  else if (out_n == 8 && in_n == 8)
	    return ix86_get_builtin (IX86_BUILTIN_FLOORPD512);
	}
      if (out_mode == SFmode && in_mode == SFmode)
	{
	  if (out_n == 4 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_FLOORPS);
	  else if (out_n == 8 && in_n == 8)
	    return ix86_get_builtin (IX86_BUILTIN_FLOORPS256);
	  else if (out_n == 16 && in_n == 16)
	    return ix86_get_builtin (IX86_BUILTIN_FLOORPS512);
	}
      break;

    CASE_CFN_CEIL:
      /* The round insn does not trap on denormals.  */
      if (flag_trapping_math || !TARGET_SSE4_1)
	break;

      if (out_mode == DFmode && in_mode == DFmode)
	{
	  if (out_n == 2 && in_n == 2)
	    return ix86_get_builtin (IX86_BUILTIN_CEILPD);
	  else if (out_n == 4 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_CEILPD256);
	  else if (out_n == 8 && in_n == 8)
	    return ix86_get_builtin (IX86_BUILTIN_CEILPD512);
	}
      if (out_mode == SFmode && in_mode == SFmode)
	{
	  if (out_n == 4 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_CEILPS);
	  else if (out_n == 8 && in_n == 8)
	    return ix86_get_builtin (IX86_BUILTIN_CEILPS256);
	  else if (out_n == 16 && in_n == 16)
	    return ix86_get_builtin (IX86_BUILTIN_CEILPS512);
	}
      break;

    CASE_CFN_TRUNC:
      /* The round insn does not trap on denormals.  */
      if (flag_trapping_math || !TARGET_SSE4_1)
	break;

      if (out_mode == DFmode && in_mode == DFmode)
	{
	  if (out_n == 2 && in_n == 2)
	    return ix86_get_builtin (IX86_BUILTIN_TRUNCPD);
	  else if (out_n == 4 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_TRUNCPD256);
	  else if (out_n == 8 && in_n == 8)
	    return ix86_get_builtin (IX86_BUILTIN_TRUNCPD512);
	}
      if (out_mode == SFmode && in_mode == SFmode)
	{
	  if (out_n == 4 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_TRUNCPS);
	  else if (out_n == 8 && in_n == 8)
	    return ix86_get_builtin (IX86_BUILTIN_TRUNCPS256);
	  else if (out_n == 16 && in_n == 16)
	    return ix86_get_builtin (IX86_BUILTIN_TRUNCPS512);
	}
      break;

    CASE_CFN_FMA:
      if (out_mode == DFmode && in_mode == DFmode)
	{
	  if (out_n == 2 && in_n == 2)
	    return ix86_get_builtin (IX86_BUILTIN_VFMADDPD);
	  if (out_n == 4 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_VFMADDPD256);
	}
      if (out_mode == SFmode && in_mode == SFmode)
	{
	  if (out_n == 4 && in_n == 4)
	    return ix86_get_builtin (IX86_BUILTIN_VFMADDPS);
	  if (out_n == 8 && in_n == 8)
	    return ix86_get_builtin (IX86_BUILTIN_VFMADDPS256);
	}
      break;

    default:
      break;
    }

  /* Dispatch to a handler for a vectorization library.  */
  if (ix86_veclib_handler)
    return ix86_veclib_handler (combined_fn (fn), type_out, type_in);

  return NULL_TREE;
}

/* Returns a decl of a function that implements gather load with
   memory type MEM_VECTYPE and index type INDEX_VECTYPE and SCALE.
   Return NULL_TREE if it is not available.  */

tree
ix86_vectorize_builtin_gather (const_tree mem_vectype,
			       const_tree index_type, int scale)
{
  bool si;
  enum ix86_builtins code;

  if (! TARGET_AVX2 || !TARGET_USE_GATHER)
    return NULL_TREE;

  if ((TREE_CODE (index_type) != INTEGER_TYPE
       && !POINTER_TYPE_P (index_type))
      || (TYPE_MODE (index_type) != SImode
	  && TYPE_MODE (index_type) != DImode))
    return NULL_TREE;

  if (TYPE_PRECISION (index_type) > POINTER_SIZE)
    return NULL_TREE;

  /* v*gather* insn sign extends index to pointer mode.  */
  if (TYPE_PRECISION (index_type) < POINTER_SIZE
      && TYPE_UNSIGNED (index_type))
    return NULL_TREE;

  if (scale <= 0
      || scale > 8
      || (scale & (scale - 1)) != 0)
    return NULL_TREE;

  si = TYPE_MODE (index_type) == SImode;
  switch (TYPE_MODE (mem_vectype))
    {
    case E_V2DFmode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_GATHER3SIV2DF : IX86_BUILTIN_GATHER3DIV2DF;
      else
	code = si ? IX86_BUILTIN_GATHERSIV2DF : IX86_BUILTIN_GATHERDIV2DF;
      break;
    case E_V4DFmode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_GATHER3ALTSIV4DF : IX86_BUILTIN_GATHER3DIV4DF;
      else
	code = si ? IX86_BUILTIN_GATHERALTSIV4DF : IX86_BUILTIN_GATHERDIV4DF;
      break;
    case E_V2DImode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_GATHER3SIV2DI : IX86_BUILTIN_GATHER3DIV2DI;
      else
	code = si ? IX86_BUILTIN_GATHERSIV2DI : IX86_BUILTIN_GATHERDIV2DI;
      break;
    case E_V4DImode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_GATHER3ALTSIV4DI : IX86_BUILTIN_GATHER3DIV4DI;
      else
	code = si ? IX86_BUILTIN_GATHERALTSIV4DI : IX86_BUILTIN_GATHERDIV4DI;
      break;
    case E_V4SFmode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_GATHER3SIV4SF : IX86_BUILTIN_GATHER3DIV4SF;
      else
	code = si ? IX86_BUILTIN_GATHERSIV4SF : IX86_BUILTIN_GATHERDIV4SF;
      break;
    case E_V8SFmode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_GATHER3SIV8SF : IX86_BUILTIN_GATHER3ALTDIV8SF;
      else
	code = si ? IX86_BUILTIN_GATHERSIV8SF : IX86_BUILTIN_GATHERALTDIV8SF;
      break;
    case E_V4SImode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_GATHER3SIV4SI : IX86_BUILTIN_GATHER3DIV4SI;
      else
	code = si ? IX86_BUILTIN_GATHERSIV4SI : IX86_BUILTIN_GATHERDIV4SI;
      break;
    case E_V8SImode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_GATHER3SIV8SI : IX86_BUILTIN_GATHER3ALTDIV8SI;
      else
	code = si ? IX86_BUILTIN_GATHERSIV8SI : IX86_BUILTIN_GATHERALTDIV8SI;
      break;
    case E_V8DFmode:
      if (TARGET_AVX512F)
	code = si ? IX86_BUILTIN_GATHER3ALTSIV8DF : IX86_BUILTIN_GATHER3DIV8DF;
      else
	return NULL_TREE;
      break;
    case E_V8DImode:
      if (TARGET_AVX512F)
	code = si ? IX86_BUILTIN_GATHER3ALTSIV8DI : IX86_BUILTIN_GATHER3DIV8DI;
      else
	return NULL_TREE;
      break;
    case E_V16SFmode:
      if (TARGET_AVX512F)
	code = si ? IX86_BUILTIN_GATHER3SIV16SF : IX86_BUILTIN_GATHER3ALTDIV16SF;
      else
	return NULL_TREE;
      break;
    case E_V16SImode:
      if (TARGET_AVX512F)
	code = si ? IX86_BUILTIN_GATHER3SIV16SI : IX86_BUILTIN_GATHER3ALTDIV16SI;
      else
	return NULL_TREE;
      break;
    default:
      return NULL_TREE;
    }

  return ix86_get_builtin (code);
}

/* Returns a code for a target-specific builtin that implements
   reciprocal of the function, or NULL_TREE if not available.  */

tree
ix86_builtin_reciprocal (tree fndecl)
{
  enum ix86_builtins fn_code
    = (enum ix86_builtins) DECL_MD_FUNCTION_CODE (fndecl);
  switch (fn_code)
    {
      /* Vectorized version of sqrt to rsqrt conversion.  */
    case IX86_BUILTIN_SQRTPS_NR:
      return ix86_get_builtin (IX86_BUILTIN_RSQRTPS_NR);

    case IX86_BUILTIN_SQRTPS_NR256:
      return ix86_get_builtin (IX86_BUILTIN_RSQRTPS_NR256);

    default:
      return NULL_TREE;
    }
}

/* This parses the attribute arguments to target in DECL and determines
   the right builtin to use to match the platform specification.
   It returns the priority value for this version decl.  If PREDICATE_LIST
   is not NULL, it stores the list of cpu features that need to be checked
   before dispatching this function.  */

unsigned int
get_builtin_code_for_version (tree decl, tree *predicate_list)
{
  tree attrs;
  struct cl_target_option cur_target;
  tree target_node;
  struct cl_target_option *new_target;
  const char *arg_str = NULL;
  const char *attrs_str = NULL;
  char *tok_str = NULL;
  char *token;

  enum feature_priority priority = P_NONE;

  static unsigned int NUM_FEATURES
    = sizeof (isa_names_table) / sizeof (_isa_names_table);

  unsigned int i;

  tree predicate_chain = NULL_TREE;
  tree predicate_decl, predicate_arg;

  attrs = lookup_attribute ("target", DECL_ATTRIBUTES (decl));
  gcc_assert (attrs != NULL);

  attrs = TREE_VALUE (TREE_VALUE (attrs));

  gcc_assert (TREE_CODE (attrs) == STRING_CST);
  attrs_str = TREE_STRING_POINTER (attrs);

  /* Return priority zero for default function.  */
  if (strcmp (attrs_str, "default") == 0)
    return 0;

  /* Handle arch= if specified.  For priority, set it to be 1 more than
     the best instruction set the processor can handle.  For instance, if
     there is a version for atom and a version for ssse3 (the highest ISA
     priority for atom), the atom version must be checked for dispatch
     before the ssse3 version. */
  if (strstr (attrs_str, "arch=") != NULL)
    {
      cl_target_option_save (&cur_target, &global_options);
      target_node
	= ix86_valid_target_attribute_tree (decl, attrs, &global_options,
					    &global_options_set, 0);
    
      gcc_assert (target_node);
      if (target_node == error_mark_node)
	return 0;
      new_target = TREE_TARGET_OPTION (target_node);
      gcc_assert (new_target);
      
      if (new_target->arch_specified && new_target->arch > 0)
	for (i = 0; i < (unsigned int) pta_size; i++)
	  if (processor_alias_table[i].processor == new_target->arch)
	    {
	      const pta *arch_info = &processor_alias_table[i];
	      switch (arch_info->priority)
		{
		default:
		  arg_str = arch_info->name;
		  priority = arch_info->priority;
		  break;
		case P_PROC_DYNAMIC:
		  switch (new_target->arch)
		    {
		    case PROCESSOR_NEHALEM:
		      if (TARGET_PCLMUL_P (new_target->x_ix86_isa_flags))
			{
			  arg_str = "westmere";
			  priority = P_PCLMUL;
			}
		      else
			{
			  /* We translate "arch=corei7" and "arch=nehalem"
			     to "corei7" so that it will be mapped to
			     M_INTEL_COREI7 as cpu type to cover all
			     M_INTEL_COREI7_XXXs.  */
			  arg_str = "corei7";
			  priority = P_PROC_SSE4_2;
			}
		      break;
		    case PROCESSOR_SANDYBRIDGE:
		      if (TARGET_F16C_P (new_target->x_ix86_isa_flags))
			arg_str = "ivybridge";
		      else
			arg_str = "sandybridge";
		      priority = P_PROC_AVX;
		      break;
		    case PROCESSOR_HASWELL:
		      if (TARGET_ADX_P (new_target->x_ix86_isa_flags))
			arg_str = "broadwell";
		      else
			arg_str = "haswell";
		      priority = P_PROC_AVX2;
		      break;
		    case PROCESSOR_AMDFAM10:
		      arg_str = "amdfam10h";
		      priority = P_PROC_SSE4_A;
		      break;
		    default:
		      gcc_unreachable ();
		    }
		  break;
		case P_NONE:
		  break;
		}
	      break;
	    }

      cl_target_option_restore (&global_options, &cur_target);
	
      if (predicate_list && arg_str == NULL)
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "no dispatcher found for the versioning attributes");
	  return 0;
	}
    
      if (predicate_list)
	{
          predicate_decl = ix86_builtins [(int) IX86_BUILTIN_CPU_IS];
          /* For a C string literal the length includes the trailing NULL.  */
          predicate_arg = build_string_literal (strlen (arg_str) + 1, arg_str);
          predicate_chain = tree_cons (predicate_decl, predicate_arg,
				       predicate_chain);
	}
    }

  /* Process feature name.  */
  tok_str =  (char *) xmalloc (strlen (attrs_str) + 1);
  strcpy (tok_str, attrs_str);
  token = strtok (tok_str, ",");
  predicate_decl = ix86_builtins [(int) IX86_BUILTIN_CPU_SUPPORTS];

  while (token != NULL)
    {
      /* Do not process "arch="  */
      if (strncmp (token, "arch=", 5) == 0)
	{
	  token = strtok (NULL, ",");
	  continue;
	}
      for (i = 0; i < NUM_FEATURES; ++i)
	{
	  if (strcmp (token, isa_names_table[i].name) == 0)
	    {
	      if (predicate_list)
		{
		  predicate_arg = build_string_literal (
				  strlen (isa_names_table[i].name) + 1,
				  isa_names_table[i].name);
		  predicate_chain = tree_cons (predicate_decl, predicate_arg,
					       predicate_chain);
		}
	      /* Find the maximum priority feature.  */
	      if (isa_names_table[i].priority > priority)
		priority = isa_names_table[i].priority;

	      break;
	    }
	}
      if (predicate_list && priority == P_NONE)
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "ISA %qs is not supported in %<target%> attribute, "
		    "use %<arch=%> syntax", token);
	  return 0;
	}
      token = strtok (NULL, ",");
    }
  free (tok_str);

  if (predicate_list && predicate_chain == NULL_TREE)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
	        "no dispatcher found for the versioning attributes: %s",
	        attrs_str);
      return 0;
    }
  else if (predicate_list)
    {
      predicate_chain = nreverse (predicate_chain);
      *predicate_list = predicate_chain;
    }

  return priority; 
}

/* This builds the processor_model struct type defined in
   libgcc/config/i386/cpuinfo.c  */

static tree
build_processor_model_struct (void)
{
  const char *field_name[] = {"__cpu_vendor", "__cpu_type", "__cpu_subtype",
			      "__cpu_features"};
  tree field = NULL_TREE, field_chain = NULL_TREE;
  int i;
  tree type = make_node (RECORD_TYPE);

  /* The first 3 fields are unsigned int.  */
  for (i = 0; i < 3; ++i)
    {
      field = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			  get_identifier (field_name[i]), unsigned_type_node);
      if (field_chain != NULL_TREE)
	DECL_CHAIN (field) = field_chain;
      field_chain = field;
    }

  /* The last field is an array of unsigned integers of size one.  */
  field = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
		      get_identifier (field_name[3]),
		      build_array_type (unsigned_type_node,
					build_index_type (size_one_node)));
  if (field_chain != NULL_TREE)
    DECL_CHAIN (field) = field_chain;
  field_chain = field;

  finish_builtin_struct (type, "__processor_model", field_chain, NULL_TREE);
  return type;
}

/* Returns a extern, comdat VAR_DECL of type TYPE and name NAME. */

static tree
make_var_decl (tree type, const char *name)
{
  tree new_decl;

  new_decl = build_decl (UNKNOWN_LOCATION,
	                 VAR_DECL,
	  	         get_identifier(name),
		         type);

  DECL_EXTERNAL (new_decl) = 1;
  TREE_STATIC (new_decl) = 1;
  TREE_PUBLIC (new_decl) = 1;
  DECL_INITIAL (new_decl) = 0;
  DECL_ARTIFICIAL (new_decl) = 0;
  DECL_PRESERVE_P (new_decl) = 1;

  make_decl_one_only (new_decl, DECL_ASSEMBLER_NAME (new_decl));
  assemble_variable (new_decl, 0, 0, 0);

  return new_decl;
}

/* FNDECL is a __builtin_cpu_is or a __builtin_cpu_supports call that is folded
   into an integer defined in libgcc/config/i386/cpuinfo.c */

tree
fold_builtin_cpu (tree fndecl, tree *args)
{
  unsigned int i;
  enum ix86_builtins fn_code
    = (enum ix86_builtins) DECL_MD_FUNCTION_CODE (fndecl);
  tree param_string_cst = NULL;

  tree __processor_model_type = build_processor_model_struct ();
  tree __cpu_model_var = make_var_decl (__processor_model_type,
					"__cpu_model");


  varpool_node::add (__cpu_model_var);

  gcc_assert ((args != NULL) && (*args != NULL));

  param_string_cst = *args;
  while (param_string_cst
	 && TREE_CODE (param_string_cst) !=  STRING_CST)
    {
      /* *args must be a expr that can contain other EXPRS leading to a
	 STRING_CST.   */
      if (!EXPR_P (param_string_cst))
 	{
	  error ("parameter to builtin must be a string constant or literal");
	  return integer_zero_node;
	}
      param_string_cst = TREE_OPERAND (EXPR_CHECK (param_string_cst), 0);
    }

  gcc_assert (param_string_cst);

  if (fn_code == IX86_BUILTIN_CPU_IS)
    {
      tree ref;
      tree field;
      tree final;

      unsigned int field_val = 0;

      for (i = 0; i < num_arch_names; i++)
	if (processor_alias_table[i].model != 0
	    && strcmp (processor_alias_table[i].name,
		       TREE_STRING_POINTER (param_string_cst)) == 0)
	  break;

      if (i == num_arch_names)
	{
	  error ("parameter to builtin not valid: %s",
	         TREE_STRING_POINTER (param_string_cst));
	  return integer_zero_node;
	}

      field = TYPE_FIELDS (__processor_model_type);
      field_val = processor_alias_table[i].model;

      /* CPU types are stored in the next field.  */
      if (field_val > M_CPU_TYPE_START
	  && field_val < M_CPU_SUBTYPE_START)
	{
	  field = DECL_CHAIN (field);
	  field_val -= M_CPU_TYPE_START;
	}

      /* CPU subtypes are stored in the next field.  */
      if (field_val > M_CPU_SUBTYPE_START)
	{
	  field = DECL_CHAIN ( DECL_CHAIN (field));
	  field_val -= M_CPU_SUBTYPE_START;
	}

      /* Get the appropriate field in __cpu_model.  */
      ref = build3 (COMPONENT_REF, TREE_TYPE (field), __cpu_model_var,
		    field, NULL_TREE);

      /* Check the value.  */
      final = build2 (EQ_EXPR, unsigned_type_node, ref,
		      build_int_cstu (unsigned_type_node, field_val));
      return build1 (CONVERT_EXPR, integer_type_node, final);
    }
  else if (fn_code == IX86_BUILTIN_CPU_SUPPORTS)
    {
      tree ref;
      tree array_elt;
      tree field;
      tree final;

      unsigned int field_val = 0;
      unsigned int NUM_ISA_NAMES
	= sizeof (isa_names_table) / sizeof (struct _isa_names_table);

      for (i = 0; i < NUM_ISA_NAMES; i++)
	if (strcmp (isa_names_table[i].name,
	    TREE_STRING_POINTER (param_string_cst)) == 0)
	  break;

      if (i == NUM_ISA_NAMES)
	{
	  error ("parameter to builtin not valid: %s",
	       	 TREE_STRING_POINTER (param_string_cst));
	  return integer_zero_node;
	}

      if (isa_names_table[i].feature >= 32)
	{
	  tree index_type
	    = build_index_type (size_int (SIZE_OF_CPU_FEATURES));
	  tree type = build_array_type (unsigned_type_node, index_type);
	  tree __cpu_features2_var = make_var_decl (type,
						    "__cpu_features2");

	  varpool_node::add (__cpu_features2_var);
	  for (unsigned int j = 0; j < SIZE_OF_CPU_FEATURES; j++)
	    if (isa_names_table[i].feature < (32 + 32 + j * 32))
	      {
		field_val = (1U << (isa_names_table[i].feature
				    - (32 + j * 32)));
		tree index = size_int (j);
		array_elt = build4 (ARRAY_REF, unsigned_type_node,
				    __cpu_features2_var,
				    index, NULL_TREE, NULL_TREE);
		/* Return __cpu_features2[index] & field_val  */
		final = build2 (BIT_AND_EXPR, unsigned_type_node,
				array_elt,
				build_int_cstu (unsigned_type_node,
						field_val));
		return build1 (CONVERT_EXPR, integer_type_node, final);
	      }
	}

      field = TYPE_FIELDS (__processor_model_type);
      /* Get the last field, which is __cpu_features.  */
      while (DECL_CHAIN (field))
        field = DECL_CHAIN (field);

      /* Get the appropriate field: __cpu_model.__cpu_features  */
      ref = build3 (COMPONENT_REF, TREE_TYPE (field), __cpu_model_var,
		    field, NULL_TREE);

      /* Access the 0th element of __cpu_features array.  */
      array_elt = build4 (ARRAY_REF, unsigned_type_node, ref,
			  integer_zero_node, NULL_TREE, NULL_TREE);

      field_val = (1U << isa_names_table[i].feature);
      /* Return __cpu_model.__cpu_features[0] & field_val  */
      final = build2 (BIT_AND_EXPR, unsigned_type_node, array_elt,
		      build_int_cstu (unsigned_type_node, field_val));
      return build1 (CONVERT_EXPR, integer_type_node, final);
    }
  gcc_unreachable ();
}

#include "gt-i386-builtins.h"
