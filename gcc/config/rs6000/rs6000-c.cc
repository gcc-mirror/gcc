/* Subroutines for the C front end on the PowerPC architecture.
   Copyright (C) 2002-2025 Free Software Foundation, Inc.

   Contributed by Zack Weinberg <zack@codesourcery.com>
   and Paolo Bonzini <bonzini@gnu.org>

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "c-family/c-common.h"
#include "memmodel.h"
#include "tm_p.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "c-family/c-pragma.h"
#include "langhooks.h"
#include "c/c-tree.h"

#include "rs6000-internal.h"

/* Handle the machine specific pragma longcall.  Its syntax is

   # pragma longcall ( TOGGLE )

   where TOGGLE is either 0 or 1.

   rs6000_default_long_calls is set to the value of TOGGLE, changing
   whether or not new function declarations receive a longcall
   attribute by default.  */

void
rs6000_pragma_longcall (cpp_reader *pfile ATTRIBUTE_UNUSED)
{
#define SYNTAX_ERROR(gmsgid) do {					\
  warning (OPT_Wpragmas, gmsgid);					\
  warning (OPT_Wpragmas, "ignoring malformed %<#pragma longcall%>");	\
  return;								\
} while (0)



  tree x, n;

  /* If we get here, generic code has already scanned the directive
     leader and the word "longcall".  */

  if (pragma_lex (&x) != CPP_OPEN_PAREN)
    SYNTAX_ERROR ("missing open paren");
  if (pragma_lex (&n) != CPP_NUMBER)
    SYNTAX_ERROR ("missing number");
  if (pragma_lex (&x) != CPP_CLOSE_PAREN)
    SYNTAX_ERROR ("missing close paren");

  if (n != integer_zero_node && n != integer_one_node)
    SYNTAX_ERROR ("number must be 0 or 1");

  if (pragma_lex (&x) != CPP_EOF)
    warning (OPT_Wpragmas, "junk at end of %<#pragma longcall%>");

  rs6000_default_long_calls = (n == integer_one_node);
}

/* Handle defining many CPP flags based on TARGET_xxx.  As a general
   policy, rather than trying to guess what flags a user might want a
   #define for, it's better to define a flag for everything.  */

#define builtin_define(TXT) cpp_define (pfile, TXT)
#define builtin_assert(TXT) cpp_assert (pfile, TXT)

/* Keep the AltiVec keywords handy for fast comparisons.  */
static GTY(()) tree __vector_keyword;
static GTY(()) tree vector_keyword;
static GTY(()) tree __pixel_keyword;
static GTY(()) tree pixel_keyword;
static GTY(()) tree __bool_keyword;
static GTY(()) tree bool_keyword;
static GTY(()) tree _Bool_keyword;
static GTY(()) tree __int128_type;
static GTY(()) tree __uint128_type;

/* Preserved across calls.  */
static tree expand_bool_pixel;

static cpp_hashnode *
altivec_categorize_keyword (const cpp_token *tok)
{
  if (tok->type == CPP_NAME)
    {
      cpp_hashnode *ident = tok->val.node.node;

      if (ident == C_CPP_HASHNODE (vector_keyword))
	return C_CPP_HASHNODE (__vector_keyword);

      if (ident == C_CPP_HASHNODE (pixel_keyword))
	return C_CPP_HASHNODE (__pixel_keyword);

      if (ident == C_CPP_HASHNODE (bool_keyword))
	return C_CPP_HASHNODE (__bool_keyword);

      if (ident == C_CPP_HASHNODE (_Bool_keyword))
	return C_CPP_HASHNODE (__bool_keyword);

      return ident;
    }

  return 0;
}

static void
init_vector_keywords (void)
{
  /* Keywords without two leading underscores are context-sensitive, and hence
     implemented as conditional macros, controlled by the
     rs6000_macro_to_expand() function below.  If we have ISA 2.07 64-bit
     support, record the __int128_t and __uint128_t types.  */

  __vector_keyword = get_identifier ("__vector");
  C_CPP_HASHNODE (__vector_keyword)->flags |= NODE_CONDITIONAL;

  __pixel_keyword = get_identifier ("__pixel");
  C_CPP_HASHNODE (__pixel_keyword)->flags |= NODE_CONDITIONAL;

  __bool_keyword = get_identifier ("__bool");
  C_CPP_HASHNODE (__bool_keyword)->flags |= NODE_CONDITIONAL;

  vector_keyword = get_identifier ("vector");
  C_CPP_HASHNODE (vector_keyword)->flags |= NODE_CONDITIONAL;

  pixel_keyword = get_identifier ("pixel");
  C_CPP_HASHNODE (pixel_keyword)->flags |= NODE_CONDITIONAL;

  bool_keyword = get_identifier ("bool");
  C_CPP_HASHNODE (bool_keyword)->flags |= NODE_CONDITIONAL;

  _Bool_keyword = get_identifier ("_Bool");
  C_CPP_HASHNODE (_Bool_keyword)->flags |= NODE_CONDITIONAL;

  if (TARGET_VADDUQM)
    {
      __int128_type = get_identifier ("__int128_t");
      __uint128_type = get_identifier ("__uint128_t");
    }
}

/* Helper function to find out which RID_INT_N_* code is the one for
   __int128, if any.  Returns RID_MAX+1 if none apply, which is safe
   (for our purposes, since we always expect to have __int128) to
   compare against.  */
static int
rid_int128(void)
{
  int i;

  for (i = 0; i < NUM_INT_N_ENTS; i ++)
    if (int_n_enabled_p[i]
	&& int_n_data[i].bitsize == 128)
      return RID_INT_N_0 + i;

  return RID_MAX + 1;
}

/* Called to decide whether a conditional macro should be expanded
   by peeking two or more tokens(_bool/_pixel/int/long/double/...).  */

static cpp_hashnode *
rs6000_macro_to_expand (cpp_reader *pfile, const cpp_token *tok)
{
  cpp_hashnode *expand_this = tok->val.node.node;
  cpp_hashnode *ident;

  /* If the current machine does not have altivec, don't look for the
     keywords.  */
  if (!TARGET_ALTIVEC)
    return NULL;

  ident = altivec_categorize_keyword (tok);

  if (ident != expand_this)
    expand_this = NULL;

  if (ident == C_CPP_HASHNODE (__vector_keyword))
    {
      int idx = 0;
      do
	tok = cpp_peek_token (pfile, idx++);
      while (tok->type == CPP_PADDING);
      ident = altivec_categorize_keyword (tok);

      if (ident == C_CPP_HASHNODE (__pixel_keyword))
	{
	  expand_this = C_CPP_HASHNODE (__vector_keyword);
	  expand_bool_pixel = __pixel_keyword;
	}
      else if (ident == C_CPP_HASHNODE (__bool_keyword))
	{
	  expand_this = C_CPP_HASHNODE (__vector_keyword);
	  expand_bool_pixel = __bool_keyword;
	}
      /* The boost libraries have code with Iterator::vector vector in it.  If
	 we allow the normal handling, this module will be called recursively,
	 and the vector will be skipped.; */
      else if (ident && (ident != C_CPP_HASHNODE (__vector_keyword)))
	{
	  enum rid rid_code = (enum rid)(ident->rid_code);
	  bool is_macro = cpp_macro_p (ident);

	  /* If there is a function-like macro, check if it is going to be
	     invoked with or without arguments.  Without following ( treat
	     it like non-macro, otherwise the following cpp_get_token eats
	     what should be preserved.  */
	  if (is_macro && cpp_fun_like_macro_p (ident))
	    {
	      int idx2 = idx;
	      do
		tok = cpp_peek_token (pfile, idx2++);
	      while (tok->type == CPP_PADDING);
	      if (tok->type != CPP_OPEN_PAREN)
		is_macro = false;
	    }

	  if (is_macro)
	    {
	      do
		(void) cpp_get_token (pfile);
	      while (--idx > 0);
	      do
		tok = cpp_peek_token (pfile, idx++);
	      while (tok->type == CPP_PADDING);
	      ident = altivec_categorize_keyword (tok);
	      if (ident == C_CPP_HASHNODE (__pixel_keyword))
		{
		  expand_this = C_CPP_HASHNODE (__vector_keyword);
		  expand_bool_pixel = __pixel_keyword;
		  rid_code = RID_MAX;
		}
	      else if (ident == C_CPP_HASHNODE (__bool_keyword))
		{
		  expand_this = C_CPP_HASHNODE (__vector_keyword);
		  expand_bool_pixel = __bool_keyword;
		  rid_code = RID_MAX;
		}
	      else if (ident)
		rid_code = (enum rid)(ident->rid_code);
	    }

	  if (rid_code == RID_UNSIGNED || rid_code == RID_LONG
	      || rid_code == RID_SHORT || rid_code == RID_SIGNED
	      || rid_code == RID_INT || rid_code == RID_CHAR
	      || rid_code == RID_FLOAT
	      || (rid_code == RID_DOUBLE && TARGET_VSX)
	      || (rid_code == rid_int128 () && TARGET_VADDUQM))
	    {
	      expand_this = C_CPP_HASHNODE (__vector_keyword);
	      /* If the next keyword is bool or pixel, it
		 will need to be expanded as well.  */
	      do
		tok = cpp_peek_token (pfile, idx++);
	      while (tok->type == CPP_PADDING);
	      ident = altivec_categorize_keyword (tok);

	      if (ident == C_CPP_HASHNODE (__pixel_keyword))
		expand_bool_pixel = __pixel_keyword;
	      else if (ident == C_CPP_HASHNODE (__bool_keyword))
		expand_bool_pixel = __bool_keyword;

	      /* If there are more tokens to check.  */
	      else if (ident)
		{
		  /* Try two tokens down, too.  */
		  do
		    tok = cpp_peek_token (pfile, idx++);
		  while (tok->type == CPP_PADDING);
		  ident = altivec_categorize_keyword (tok);
		  if (ident == C_CPP_HASHNODE (__pixel_keyword))
		    expand_bool_pixel = __pixel_keyword;
		  else if (ident == C_CPP_HASHNODE (__bool_keyword))
		    expand_bool_pixel = __bool_keyword;
		}
	    }

	  /* Support vector __int128_t, but we don't need to worry about bool
	     or pixel on this type.  */
	  else if (TARGET_VADDUQM
		   && (ident == C_CPP_HASHNODE (__int128_type)
		       || ident == C_CPP_HASHNODE (__uint128_type)))
	    expand_this = C_CPP_HASHNODE (__vector_keyword);
	}
    }
  else if (expand_bool_pixel && ident == C_CPP_HASHNODE (__pixel_keyword))
    {
      expand_this = C_CPP_HASHNODE (__pixel_keyword);
      expand_bool_pixel = 0;
    }
  else if (expand_bool_pixel && ident == C_CPP_HASHNODE (__bool_keyword))
    {
      expand_this = C_CPP_HASHNODE (__bool_keyword);
      expand_bool_pixel = 0;
    }

  return expand_this;
}


/* Define or undefine a single macro.  */

static void
rs6000_define_or_undefine_macro (bool define_p, const char *name)
{
  if (TARGET_DEBUG_BUILTIN || TARGET_DEBUG_TARGET)
    fprintf (stderr, "#%s %s\n", (define_p) ? "define" : "undef", name);

  if (define_p)
    cpp_define (parse_in, name);
  else
    cpp_undef (parse_in, name);
}

/* Define or undefine macros based on the current target.  If the user does
   #pragma GCC target, we need to adjust the macros dynamically.  */

void
rs6000_target_modify_macros (bool define_p, HOST_WIDE_INT flags)
{
  if (TARGET_DEBUG_BUILTIN || TARGET_DEBUG_TARGET)
    fprintf (stderr,
	     "rs6000_target_modify_macros (%s, " HOST_WIDE_INT_PRINT_HEX ")\n",
	     (define_p) ? "define" : "undef",
	     flags);

  /* Each of the flags mentioned below controls whether certain
     preprocessor macros will be automatically defined when
     preprocessing source files for compilation by this compiler.
     While most of these flags can be enabled or disabled
     explicitly by specifying certain command-line options when
     invoking the compiler, there are also many ways in which these
     flags are enabled or disabled implicitly, based on compiler
     defaults, configuration choices, and on the presence of certain
     related command-line options.  Many, but not all, of these
     implicit behaviors can be found in file "rs6000.cc", the
     rs6000_option_override_internal() function.

     In general, each of the flags may be automatically enabled in
     any of the following conditions:

     1. If no -mcpu target is specified on the command line and no
	--with-cpu target is specified to the configure command line
	and the TARGET_DEFAULT macro for this default cpu host
	includes the flag, and the flag has not been explicitly disabled
	by command-line options.

     2. If the target specified with -mcpu=target on the command line, or
	in the absence of a -mcpu=target command-line option, if the
	target specified using --with-cpu=target on the configure
	command line, is disqualified because the associated binary
	tools (e.g. the assembler) lack support for the requested cpu,
	and the TARGET_DEFAULT macro for this default cpu host
	includes the flag, and the flag has not been explicitly disabled
	by command-line options.

     3. If either of the above two conditions apply except that the
	TARGET_DEFAULT macro is defined to equal zero, and
	TARGET_POWERPC64 and
	a) BYTES_BIG_ENDIAN and the flag to be enabled is either
	   OPTION_MASK_PPC_GFXOPT or MASK_POWERPC64 (flags for "powerpc64"
	   target), or
	b) !BYTES_BIG_ENDIAN and the flag to be enabled is either
	   MASK_POWERPC64 or it is one of the flags included in
	   ISA_2_7_MASKS_SERVER (flags for "powerpc64le" target).

     4. If a cpu has been requested with a -mcpu=target command-line option
	and this cpu has not been disqualified due to shortcomings of the
	binary tools, and the set of flags associated with the requested cpu
	include the flag to be enabled.  See rs6000-cpus.def for macro
	definitions that represent various ABI standards
	(e.g. ISA_2_1_MASKS, ISA_3_0_MASKS_SERVER) and for a list of
	the specific flags that are associated with each of the cpu
	choices that can be specified as the target of a -mcpu=target
	compile option, or as the target of a --with-cpu=target
	configure option.  Target flags that are specified in either
	of these two ways are considered "implicit" since the flags
	are not mentioned specifically by name.

	Additional documentation describing behavior specific to
	particular flags is provided below, immediately preceding the
	use of each relevant flag.

     5. If there is no -mcpu=target command-line option, and the cpu
	requested by a --with-cpu=target command-line option has not
	been disqualified due to shortcomings of the binary tools, and
	the set of flags associated with the specified target include
	the flag to be enabled.  See the notes immediately above for a
	summary of the flags associated with particular cpu
	definitions.  */

  /* rs6000_isa_flags based options.  */
  rs6000_define_or_undefine_macro (define_p, "_ARCH_PPC");
  if ((flags & OPTION_MASK_PPC_GPOPT) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PPCSQ");
  if ((flags & OPTION_MASK_PPC_GFXOPT) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PPCGR");
  if ((flags & OPTION_MASK_POWERPC64) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PPC64");
  if ((flags & OPTION_MASK_MFCRF) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PWR4");
  if ((flags & OPTION_MASK_POPCNTB) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PWR5");
  if ((flags & OPTION_MASK_FPRND) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PWR5X");
  if ((flags & OPTION_MASK_CMPB) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PWR6");
  if ((flags & OPTION_MASK_POPCNTD) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PWR7");
  if ((flags & OPTION_MASK_POWER8) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PWR8");
  if ((flags & OPTION_MASK_MODULO) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PWR9");
  if ((flags & OPTION_MASK_POWER10) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PWR10");
  if ((flags & OPTION_MASK_POWER11) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PWR11");
  if ((flags & OPTION_MASK_SOFT_FLOAT) != 0)
    rs6000_define_or_undefine_macro (define_p, "_SOFT_FLOAT");
  if ((flags & OPTION_MASK_RECIP_PRECISION) != 0)
    rs6000_define_or_undefine_macro (define_p, "__RECIP_PRECISION__");
  /* Note that the OPTION_MASK_ALTIVEC flag is automatically turned on
     in any of the following conditions:
     1. The operating system is Darwin and it is configured for 64
	bit.  (See darwin_rs6000_override_options.)
     2. The operating system is Darwin and the operating system
	version is 10.5 or higher and the user has not explicitly
	disabled ALTIVEC by specifying -mcpu=G3 or -mno-altivec and
	the compiler is not producing code for integration within the
	kernel.  (See darwin_rs6000_override_options.)
     Note that the OPTION_MASK_ALTIVEC flag is automatically turned
     off in any of the following conditions:
     1. The operating system does not support saving of AltiVec
	registers (OS_MISSING_ALTIVEC).
     2. If an inner context (as introduced by
	__attribute__((__target__())) or #pragma GCC target()
	requests a target that normally enables the
	OPTION_MASK_ALTIVEC flag but the outer-most "main target"
	does not support the rs6000_altivec_abi, this flag is
	turned off for the inner context unless OPTION_MASK_ALTIVEC
	was explicitly enabled for the inner context.  */
  if ((flags & OPTION_MASK_ALTIVEC) != 0)
    {
      const char *vec_str = (define_p) ? "__VEC__=10206" : "__VEC__";
      rs6000_define_or_undefine_macro (define_p, "__ALTIVEC__");
      rs6000_define_or_undefine_macro (define_p, vec_str);

	  /* Define this when supporting context-sensitive keywords.  */
      if (!flag_iso)
	rs6000_define_or_undefine_macro (define_p, "__APPLE_ALTIVEC__");
      if (rs6000_aix_extabi)
	rs6000_define_or_undefine_macro (define_p, "__EXTABI__");
    }
  /* Note that the OPTION_MASK_VSX flag is automatically turned on in
     the following conditions:
     1. TARGET_P8_VECTOR is explicitly turned on and the OPTION_MASK_VSX
        was not explicitly turned off.  Hereafter, the OPTION_MASK_VSX
        flag is considered to have been explicitly turned on.
     Note that the OPTION_MASK_VSX flag is automatically turned off in
     the following conditions:
     1. The operating system does not support saving of AltiVec
	registers (OS_MISSING_ALTIVEC).
     2. If the option TARGET_HARD_FLOAT is turned off.  Hereafter, the
	OPTION_MASK_VSX flag is considered to have been turned off
	explicitly.
     3. If TARGET_AVOID_XFORM is turned on explicitly at the outermost
	compilation context, or if it is turned on by any means in an
	inner compilation context.  Hereafter, the OPTION_MASK_VSX
	flag is considered to have been turned off explicitly.
     4. If TARGET_ALTIVEC was explicitly disabled.  Hereafter, the
	OPTION_MASK_VSX flag is considered to have been turned off
	explicitly.
     5. If an inner context (as introduced by
	__attribute__((__target__())) or #pragma GCC target()
	requests a target that normally enables the
	OPTION_MASK_VSX flag but the outer-most "main target"
	does not support the rs6000_altivec_abi, this flag is
	turned off for the inner context unless OPTION_MASK_VSX
	was explicitly enabled for the inner context.  */
  if ((flags & OPTION_MASK_VSX) != 0)
    rs6000_define_or_undefine_macro (define_p, "__VSX__");
  if ((flags & OPTION_MASK_HTM) != 0)
    {
      rs6000_define_or_undefine_macro (define_p, "__HTM__");
      /* Tell the user that our HTM insn patterns act as memory barriers.  */
      rs6000_define_or_undefine_macro (define_p, "__TM_FENCE__");
    }
  /* Note that the OPTION_MASK_P8_VECTOR flag is automatically turned
     on in the following conditions:
     1. TARGET_P9_VECTOR is explicitly turned on and
        OPTION_MASK_P8_VECTOR is not explicitly turned off.
        Hereafter, the OPTION_MASK_P8_VECTOR flag is considered to
        have been turned off explicitly.
     Note that the OPTION_MASK_P8_VECTOR flag is automatically turned
     off in the following conditions:
     1. If any of TARGET_HARD_FLOAT, TARGET_ALTIVEC, or TARGET_VSX
	were turned off explicitly and OPTION_MASK_P8_VECTOR flag was
	not turned on explicitly.
     2. If TARGET_ALTIVEC is turned off.  Hereafter, the
	OPTION_MASK_P8_VECTOR flag is considered to have been turned off
	explicitly.
     3. If TARGET_VSX is turned off and OPTION_MASK_P8_VECTOR was not
        explicitly enabled.  If TARGET_VSX is explicitly enabled, the
        OPTION_MASK_P8_VECTOR flag is hereafter also considered to
	have been turned off explicitly.  */
  if ((flags & OPTION_MASK_P8_VECTOR) != 0)
    rs6000_define_or_undefine_macro (define_p, "__POWER8_VECTOR__");
  /* Note that the OPTION_MASK_P9_VECTOR flag is automatically turned
     off in the following conditions:
     1. If TARGET_P8_VECTOR is turned off and OPTION_MASK_P9_VECTOR is
        not turned on explicitly. Hereafter, if OPTION_MASK_P8_VECTOR
        was turned on explicitly, the OPTION_MASK_P9_VECTOR flag is
        also considered to have been turned off explicitly.
     Note that the OPTION_MASK_P9_VECTOR is automatically turned on
     in the following conditions:
     1. If TARGET_P9_MINMAX was turned on explicitly.
        Hereafter, THE OPTION_MASK_P9_VECTOR flag is considered to
        have been turned on explicitly.  */
  if ((flags & OPTION_MASK_P9_VECTOR) != 0)
    rs6000_define_or_undefine_macro (define_p, "__POWER9_VECTOR__");
  /* Note that the OPTION_MASK_QUAD_MEMORY flag is automatically
     turned off in the following conditions:
     1. If TARGET_POWERPC64 is turned off.
     2. If WORDS_BIG_ENDIAN is false (non-atomic quad memory
	load/store are disabled on little endian).  */
  if ((flags & OPTION_MASK_QUAD_MEMORY) != 0)
    rs6000_define_or_undefine_macro (define_p, "__QUAD_MEMORY__");
  /* Note that the OPTION_MASK_QUAD_MEMORY_ATOMIC flag is automatically
     turned off in the following conditions:
     1. If TARGET_POWERPC64 is turned off.
     Note that the OPTION_MASK_QUAD_MEMORY_ATOMIC flag is
     automatically turned on in the following conditions:
     1. If TARGET_QUAD_MEMORY and this flag was not explicitly
	disabled.  */
  if ((flags & OPTION_MASK_QUAD_MEMORY_ATOMIC) != 0)
    rs6000_define_or_undefine_macro (define_p, "__QUAD_MEMORY_ATOMIC__");
  /* Note that the OPTION_MASK_CRYPTO flag is automatically turned off
     in the following conditions:
     1. If any of TARGET_HARD_FLOAT or TARGET_ALTIVEC or TARGET_VSX
	are turned off explicitly and OPTION_MASK_CRYPTO is not turned
	on explicitly.
     2. If TARGET_ALTIVEC is turned off.  */
  if ((flags & OPTION_MASK_CRYPTO) != 0)
    rs6000_define_or_undefine_macro (define_p, "__CRYPTO__");
  if ((flags & OPTION_MASK_FLOAT128_KEYWORD) != 0)
    {
      rs6000_define_or_undefine_macro (define_p, "__FLOAT128__");
      if (define_p)
	rs6000_define_or_undefine_macro (true, "__float128=__ieee128");
      else
	rs6000_define_or_undefine_macro (false, "__float128");
      if (ieee128_float_type_node && define_p)
	rs6000_define_or_undefine_macro (true, "__SIZEOF_FLOAT128__=16");
      else
	rs6000_define_or_undefine_macro (false, "__SIZEOF_FLOAT128__");
    }
  /* OPTION_MASK_FLOAT128_HARDWARE can be turned on if -mcpu=power9 is used or
     via the target attribute/pragma.  */
  if ((flags & OPTION_MASK_FLOAT128_HW) != 0)
    rs6000_define_or_undefine_macro (define_p, "__FLOAT128_HARDWARE__");

  /* Tell the user if we are targeting CELL.  */
  if (rs6000_cpu == PROCESSOR_CELL)
    rs6000_define_or_undefine_macro (define_p, "__PPU__");

  /* Tell the user if we support the MMA instructions.  */
  if ((flags & OPTION_MASK_MMA) != 0)
    rs6000_define_or_undefine_macro (define_p, "__MMA__");
  /* Whether pc-relative code is being generated.  */
  if ((flags & OPTION_MASK_PCREL) != 0)
    rs6000_define_or_undefine_macro (define_p, "__PCREL__");
  /* Tell the user -mrop-protect is in play.  */
  if (rs6000_rop_protect)
    rs6000_define_or_undefine_macro (define_p, "__ROP_PROTECT__");
  /* Tell the user __builtin_set_fpscr_rn now returns the FPSCR fields
     in a double.  Originally the built-in returned void.  */
  if ((flags & OPTION_MASK_SOFT_FLOAT) == 0)
    rs6000_define_or_undefine_macro (define_p,
				     "__SET_FPSCR_RN_RETURNS_FPSCR__");
}

void
rs6000_cpu_cpp_builtins (cpp_reader *pfile)
{
  /* Define all of the common macros.  */
  rs6000_target_modify_macros (true, rs6000_isa_flags);

  if (TARGET_FRE)
    builtin_define ("__RECIP__");
  if (TARGET_FRES)
    builtin_define ("__RECIPF__");
  if (TARGET_FRSQRTE)
    builtin_define ("__RSQRTE__");
  if (TARGET_FRSQRTES)
    builtin_define ("__RSQRTEF__");
  if (TARGET_FLOAT128_TYPE)
    builtin_define ("__FLOAT128_TYPE__");
  if (ibm128_float_type_node)
    builtin_define ("__SIZEOF_IBM128__=16");
  if (ieee128_float_type_node)
    builtin_define ("__SIZEOF_IEEE128__=16");
#ifdef TARGET_LIBC_PROVIDES_HWCAP_IN_TCB
  builtin_define ("__BUILTIN_CPU_SUPPORTS__");
#endif

  if (TARGET_EXTRA_BUILTINS && cpp_get_options (pfile)->lang != CLK_ASM)
    {
      /* Define the AltiVec syntactic elements.  */
      builtin_define ("__vector=__attribute__((altivec(vector__)))");
      builtin_define ("__pixel=__attribute__((altivec(pixel__))) unsigned short");
      builtin_define ("__bool=__attribute__((altivec(bool__))) unsigned");

      if (!flag_iso)
	{
	  builtin_define ("vector=vector");
	  builtin_define ("pixel=pixel");
	  builtin_define ("bool=bool");
	  builtin_define ("_Bool=_Bool");
	  init_vector_keywords ();

	  /* Enable context-sensitive macros.  */
	  cpp_get_callbacks (pfile)->macro_to_expand = rs6000_macro_to_expand;
	}
    }
  if (!TARGET_HARD_FLOAT)
    builtin_define ("_SOFT_DOUBLE");
  /* Used by lwarx/stwcx. errata work-around.  */
  if (rs6000_cpu == PROCESSOR_PPC405)
    builtin_define ("__PPC405__");
  /* Used by libstdc++.  */
  if (TARGET_NO_LWSYNC)
    builtin_define ("__NO_LWSYNC__");

  if (TARGET_EXTRA_BUILTINS)
    {
      /* For the VSX builtin functions identical to Altivec functions, just map
	 the altivec builtin into the vsx version (the altivec functions
	 generate VSX code if -mvsx).  */
      builtin_define ("__builtin_vsx_xxland=__builtin_vec_and");
      builtin_define ("__builtin_vsx_xxlandc=__builtin_vec_andc");
      builtin_define ("__builtin_vsx_xxlnor=__builtin_vec_nor");
      builtin_define ("__builtin_vsx_xxlor=__builtin_vec_or");
      builtin_define ("__builtin_vsx_xxlxor=__builtin_vec_xor");
      builtin_define ("__builtin_vsx_xxsel=__builtin_vec_sel");
      builtin_define ("__builtin_vsx_vperm=__builtin_vec_perm");

      /* Also map the a and m versions of the multiply/add instructions to the
	 builtin for people blindly going off the instruction manual.  */
      builtin_define ("__builtin_vsx_xvmaddadp=__builtin_vsx_xvmadddp");
      builtin_define ("__builtin_vsx_xvmaddmdp=__builtin_vsx_xvmadddp");
      builtin_define ("__builtin_vsx_xvmaddasp=__builtin_vsx_xvmaddsp");
      builtin_define ("__builtin_vsx_xvmaddmsp=__builtin_vsx_xvmaddsp");
      builtin_define ("__builtin_vsx_xvmsubadp=__builtin_vsx_xvmsubdp");
      builtin_define ("__builtin_vsx_xvmsubmdp=__builtin_vsx_xvmsubdp");
      builtin_define ("__builtin_vsx_xvmsubasp=__builtin_vsx_xvmsubsp");
      builtin_define ("__builtin_vsx_xvmsubmsp=__builtin_vsx_xvmsubsp");
      builtin_define ("__builtin_vsx_xvnmaddadp=__builtin_vsx_xvnmadddp");
      builtin_define ("__builtin_vsx_xvnmaddmdp=__builtin_vsx_xvnmadddp");
      builtin_define ("__builtin_vsx_xvnmaddasp=__builtin_vsx_xvnmaddsp");
      builtin_define ("__builtin_vsx_xvnmaddmsp=__builtin_vsx_xvnmaddsp");
      builtin_define ("__builtin_vsx_xvnmsubadp=__builtin_vsx_xvnmsubdp");
      builtin_define ("__builtin_vsx_xvnmsubmdp=__builtin_vsx_xvnmsubdp");
      builtin_define ("__builtin_vsx_xvnmsubasp=__builtin_vsx_xvnmsubsp");
      builtin_define ("__builtin_vsx_xvnmsubmsp=__builtin_vsx_xvnmsubsp");
    }

  /* Map the old _Float128 'q' builtins into the new 'f128' builtins.  */
  if (TARGET_FLOAT128_TYPE)
    {
      builtin_define ("__builtin_fabsq=__builtin_fabsf128");
      builtin_define ("__builtin_copysignq=__builtin_copysignf128");
      builtin_define ("__builtin_nanq=__builtin_nanf128");
      builtin_define ("__builtin_nansq=__builtin_nansf128");
      builtin_define ("__builtin_infq=__builtin_inff128");
      builtin_define ("__builtin_huge_valq=__builtin_huge_valf128");
    }

  /* Tell users they can use __builtin_bswap{16,64}.  */
  builtin_define ("__HAVE_BSWAP__");

  /* May be overridden by target configuration.  */
  RS6000_CPU_CPP_ENDIAN_BUILTINS();

  if (TARGET_LONG_DOUBLE_128)
    {
      builtin_define ("__LONG_DOUBLE_128__");
      builtin_define ("__LONGDOUBLE128");

      if (TARGET_IEEEQUAD)
	{
	  /* Older versions of GLIBC used __attribute__((__KC__)) to create the
	     IEEE 128-bit floating point complex type for C++ (which does not
	     support _Float128 _Complex).  If the default for long double is
	     IEEE 128-bit mode, the library would need to use
	     __attribute__((__TC__)) instead.  Defining __KF__ and __KC__
	     is a stop-gap to build with the older libraries, until we
	     get an updated library.  */
	  builtin_define ("__LONG_DOUBLE_IEEE128__");
	  builtin_define ("__KF__=__TF__");
	  builtin_define ("__KC__=__TC__");
	}
      else
	builtin_define ("__LONG_DOUBLE_IBM128__");
    }

  switch (TARGET_CMODEL)
    {
      /* Deliberately omit __CMODEL_SMALL__ since that was the default
	 before --mcmodel support was added.  */
    case CMODEL_MEDIUM:
      builtin_define ("__CMODEL_MEDIUM__");
      break;
    case CMODEL_LARGE:
      builtin_define ("__CMODEL_LARGE__");
      break;
    default:
      break;
    }

  switch (rs6000_current_abi)
    {
    case ABI_V4:
      builtin_define ("_CALL_SYSV");
      break;
    case ABI_AIX:
      builtin_define ("_CALL_AIXDESC");
      builtin_define ("_CALL_AIX");
      builtin_define ("_CALL_ELF=1");
      break;
    case ABI_ELFv2:
      builtin_define ("_CALL_ELF=2");
      break;
    case ABI_DARWIN:
      builtin_define ("_CALL_DARWIN");
      break;
    default:
      break;
    }

  /* Vector element order.  */
  if (BYTES_BIG_ENDIAN)
    builtin_define ("__VEC_ELEMENT_REG_ORDER__=__ORDER_BIG_ENDIAN__");
  else
    builtin_define ("__VEC_ELEMENT_REG_ORDER__=__ORDER_LITTLE_ENDIAN__");

  /* Let the compiled code know if 'f' class registers will not be available.  */
  if (TARGET_SOFT_FLOAT)
    builtin_define ("__NO_FPRS__");

  /* Whether aggregates passed by value are aligned to a 16 byte boundary
     if their alignment is 16 bytes or larger.  */
  if ((TARGET_MACHO && rs6000_darwin64_abi)
      || DEFAULT_ABI == ABI_ELFv2
      || (DEFAULT_ABI == ABI_AIX && !rs6000_compat_align_parm))
    builtin_define ("__STRUCT_PARM_ALIGN__=16");
}



/* Convert a type stored into a struct altivec_builtin_types as ID,
   into a tree.  The types are in rs6000_builtin_types: negative values
   create a pointer type for the type associated to ~ID.  Note it is
   a logical NOT, rather than a negation, otherwise you cannot represent
   a pointer type for ID 0.  */

static inline tree
rs6000_builtin_type (int id)
{
  tree t;
  t = rs6000_builtin_types[id < 0 ? ~id : id];
  return id < 0 ? build_pointer_type (t) : t;
}

/* Check whether the type of an argument, T, is compatible with a type ID
   stored into a struct altivec_builtin_types.  Integer types are considered
   compatible; otherwise, the language hook lang_hooks.types_compatible_p makes
   the decision.  Also allow long double and _Float128 to be compatible if
   -mabi=ieeelongdouble.  */

static inline bool
is_float128_p (tree t)
{
  return (t == float128_type_node
	  || (t && t == float128t_type_node)
	  || (TARGET_IEEEQUAD
	      && TARGET_LONG_DOUBLE_128
	      && t == long_double_type_node));
}


/* Return true iff ARGTYPE can be compatibly passed as PARMTYPE.  */
static bool
rs6000_builtin_type_compatible (tree parmtype, tree argtype)
{
  if (parmtype == error_mark_node)
    return false;

  if (INTEGRAL_TYPE_P (parmtype) && INTEGRAL_TYPE_P (argtype))
    return true;

  if (TARGET_IEEEQUAD && TARGET_LONG_DOUBLE_128
      && is_float128_p (parmtype) && is_float128_p (argtype))
    return true;

  if (POINTER_TYPE_P (parmtype) && POINTER_TYPE_P (argtype))
    {
      parmtype = TREE_TYPE (parmtype);
      argtype = TREE_TYPE (argtype);
      if (TYPE_READONLY (argtype))
	parmtype = build_qualified_type (parmtype, TYPE_QUAL_CONST);
    }

  return lang_hooks.types_compatible_p (parmtype, argtype);
}

/* In addition to calling fold_convert for EXPR of type TYPE, also
   call c_fully_fold to remove any C_MAYBE_CONST_EXPRs that could be
   hiding there (PR47197).  */

static tree
fully_fold_convert (tree type, tree expr)
{
  tree result = fold_convert (type, expr);
  bool maybe_const = true;

  if (!c_dialect_cxx ())
    result = c_fully_fold (result, false, &maybe_const);

  return result;
}

/* Build a tree for a function call to an Altivec non-overloaded builtin.
   The overloaded builtin that matched the types and args is described
   by DESC.  The N arguments are given in ARGS, respectively.

   Actually the only thing it does is calling fold_convert on ARGS, with
   a small exception for vec_{all,any}_{ge,le} predicates. */

static tree
altivec_build_resolved_builtin (tree *args, int n, tree fntype, tree ret_type,
				rs6000_gen_builtins bif_id,
				rs6000_gen_builtins ovld_id)
{
  tree argtypes = TYPE_ARG_TYPES (fntype);
  tree arg_type[MAX_OVLD_ARGS];
  tree fndecl = rs6000_builtin_decls[bif_id];

  for (int i = 0; i < n; i++)
    {
      arg_type[i] = TREE_VALUE (argtypes);
      argtypes = TREE_CHAIN (argtypes);
    }

  /* The AltiVec overloading implementation is overall gross, but this
     is particularly disgusting.  The vec_{all,any}_{ge,le} builtins
     are completely different for floating-point vs. integer vector
     types, because the former has vcmpgefp, but the latter should use
     vcmpgtXX.

     In practice, the second and third arguments are swapped, and the
     condition (LT vs. EQ, which is recognizable by bit 1 of the first
     argument) is reversed.  Patch the arguments here before building
     the resolved CALL_EXPR.  */
  if (n == 3
      && ovld_id == RS6000_OVLD_VEC_CMPGE_P
      && bif_id != RS6000_BIF_VCMPGEFP_P
      && bif_id != RS6000_BIF_XVCMPGEDP_P)
    {
      std::swap (args[1], args[2]);
      std::swap (arg_type[1], arg_type[2]);

      args[0] = fold_build2 (BIT_XOR_EXPR, TREE_TYPE (args[0]), args[0],
			     build_int_cst (NULL_TREE, 2));
    }

  for (int j = 0; j < n; j++)
    args[j] = fully_fold_convert (arg_type[j], args[j]);

  /* If the number of arguments to an overloaded function increases,
     we must expand this switch.  */
  gcc_assert (MAX_OVLD_ARGS <= 4);

  tree call;
  switch (n)
    {
    case 0:
      call = build_call_expr (fndecl, 0);
      break;
    case 1:
      call = build_call_expr (fndecl, 1, args[0]);
      break;
    case 2:
      call = build_call_expr (fndecl, 2, args[0], args[1]);
      break;
    case 3:
      call = build_call_expr (fndecl, 3, args[0], args[1], args[2]);
      break;
    case 4:
      call = build_call_expr (fndecl, 4, args[0], args[1], args[2], args[3]);
      break;
    default:
      gcc_unreachable ();
    }
  return fold_convert (ret_type, call);
}

/* Enumeration of possible results from attempted overload resolution.
   This is used by special-case helper functions to tell their caller
   whether they succeeded and what still needs to be done.

	unresolved = Still needs processing
	  resolved = Resolved (but may be an error_mark_node)
      resolved_bad = An error that needs handling by the caller.  */

enum resolution { unresolved, resolved, resolved_bad };

/* Resolve an overloaded vec_mul call and return a tree expression for the
   resolved call if successful.  ARGS contains the arguments to the call.
   TYPES contains their types.  RES must be set to indicate the status of
   the resolution attempt.  LOC contains statement location information.  */

static tree
resolve_vec_mul (resolution *res, tree *args, tree *types, location_t loc)
{
  /* vec_mul needs to be special cased because there are no instructions for it
     for the {un}signed char, {un}signed short, and {un}signed int types.  */

  /* Both arguments must be vectors and the types must be compatible.  */
  if (TREE_CODE (types[0]) != VECTOR_TYPE
      || !lang_hooks.types_compatible_p (types[0], types[1]))
    {
      *res = resolved_bad;
      return error_mark_node;
    }

  switch (TYPE_MODE (TREE_TYPE (types[0])))
    {
    case E_QImode:
    case E_HImode:
    case E_SImode:
    case E_DImode:
    case E_TImode:
      /* For scalar types just use a multiply expression.  */
      *res = resolved;
      return fold_build2_loc (loc, MULT_EXPR, types[0], args[0],
			      fold_convert (types[0], args[1]));
    case E_SFmode:
      {
	/* For floats use the xvmulsp instruction directly.  */
	*res = resolved;
	tree call = rs6000_builtin_decls[RS6000_BIF_XVMULSP];
	return build_call_expr (call, 2, args[0], args[1]);
      }
    case E_DFmode:
      {
	/* For doubles use the xvmuldp instruction directly.  */
	*res = resolved;
	tree call = rs6000_builtin_decls[RS6000_BIF_XVMULDP];
	return build_call_expr (call, 2, args[0], args[1]);
      }
    /* Other types are errors.  */
    default:
      *res = resolved_bad;
      return error_mark_node;
    }
}

/* Resolve an overloaded vec_cmpne call and return a tree expression for the
   resolved call if successful.  ARGS contains the arguments to the call.
   TYPES contains their types.  RES must be set to indicate the status of
   the resolution attempt.  LOC contains statement location information.  */

static tree
resolve_vec_cmpne (resolution *res, tree *args, tree *types, location_t loc)
{
  /* vec_cmpne needs to be special cased because there are no instructions
     for it (prior to power 9).  */

  /* Both arguments must be vectors and the types must be compatible.  */
  if (TREE_CODE (types[0]) != VECTOR_TYPE
      || !lang_hooks.types_compatible_p (types[0], types[1]))
    {
      *res = resolved_bad;
      return error_mark_node;
    }

  machine_mode arg0_elt_mode = TYPE_MODE (TREE_TYPE (types[0]));

  /* Power9 instructions provide the most efficient implementation of
     ALTIVEC_BUILTIN_VEC_CMPNE if the mode is not DImode or TImode
     or SFmode or DFmode.  */
  if (!TARGET_P9_VECTOR
      || arg0_elt_mode == DImode
      || arg0_elt_mode == TImode
      || arg0_elt_mode == SFmode
      || arg0_elt_mode == DFmode)
    {
      switch (arg0_elt_mode)
	{
	  /* vec_cmpneq (va, vb) == vec_nor (vec_cmpeq (va, vb),
					     vec_cmpeq (va, vb)).  */
	  /* Note:  vec_nand also works but opt changes vec_nand's
	     to vec_nor's anyway.  */
	case E_QImode:
	case E_HImode:
	case E_SImode:
	case E_DImode:
	case E_TImode:
	case E_SFmode:
	case E_DFmode:
	  {
	    /* call = vec_cmpeq (va, vb)
	       result = vec_nor (call, call).  */
	    vec<tree, va_gc> *params = make_tree_vector ();
	    vec_safe_push (params, args[0]);
	    vec_safe_push (params, args[1]);
	    tree decl = rs6000_builtin_decls[RS6000_OVLD_VEC_CMPEQ];
	    tree call = altivec_resolve_overloaded_builtin (loc, decl, params);
	    /* Use save_expr to ensure that operands used more than once
	       that may have side effects (like calls) are only evaluated
	       once.  */
	    call = save_expr (call);
	    params = make_tree_vector ();
	    vec_safe_push (params, call);
	    vec_safe_push (params, call);
	    decl = rs6000_builtin_decls[RS6000_OVLD_VEC_NOR];
	    *res = resolved;
	    return altivec_resolve_overloaded_builtin (loc, decl, params);
	  }
	  /* Other types are errors.  */
	default:
	  *res = resolved_bad;
	  return error_mark_node;
	}
    }

  /* Otherwise this call is unresolved, and altivec_resolve_overloaded_builtin
     will later process the Power9 alternative.  */
  *res = unresolved;
  return error_mark_node;
}

/* Resolve an overloaded vec_adde or vec_sube call and return a tree expression
   for the resolved call if successful.  ARGS contains the arguments to the
   call.  TYPES contains their arguments.  RES must be set to indicate the
   status of the resolution attempt.  LOC contains statement location
   information.  */

static tree
resolve_vec_adde_sube (resolution *res, rs6000_gen_builtins fcode,
		       tree *args, tree *types, location_t loc)
{
  /* vec_adde needs to be special cased because there is no instruction
     for the {un}signed int version.  */

  /* All 3 arguments must be vectors of (signed or unsigned) (int or
     __int128) and the types must be compatible.  */
  if (TREE_CODE (types[0]) != VECTOR_TYPE
      || !lang_hooks.types_compatible_p (types[0], types[1])
      || !lang_hooks.types_compatible_p (types[1], types[2]))
    {
      *res = resolved_bad;
      return error_mark_node;
    }

  switch (TYPE_MODE (TREE_TYPE (types[0])))
    {
      /* For {un}signed ints,
	 vec_adde (va, vb, carryv) == vec_add (vec_add (va, vb),
					       vec_and (carryv, 1)).
	 vec_sube (va, vb, carryv) == vec_sub (vec_sub (va, vb),
					       vec_and (carryv, 1)).  */
    case E_SImode:
      {
	vec<tree, va_gc> *params = make_tree_vector ();
	vec_safe_push (params, args[0]);
	vec_safe_push (params, args[1]);

	tree add_sub_builtin;
	if (fcode == RS6000_OVLD_VEC_ADDE)
	  add_sub_builtin = rs6000_builtin_decls[RS6000_OVLD_VEC_ADD];
	else
	  add_sub_builtin = rs6000_builtin_decls[RS6000_OVLD_VEC_SUB];

	tree call = altivec_resolve_overloaded_builtin (loc, add_sub_builtin,
							params);
	tree const1 = build_int_cstu (TREE_TYPE (types[0]), 1);
	tree ones_vector = build_vector_from_val (types[0], const1);
	tree and_expr = fold_build2_loc (loc, BIT_AND_EXPR, types[0],
					 args[2], ones_vector);
	params = make_tree_vector ();
	vec_safe_push (params, call);
	vec_safe_push (params, and_expr);
	*res = resolved;
	return altivec_resolve_overloaded_builtin (loc, add_sub_builtin,
						   params);
      }
      /* For {un}signed __int128s use the vaddeuqm/vsubeuqm instruction
	 directly using the standard machinery.  */
    case E_TImode:
      *res = unresolved;
      break;

      /* Types other than {un}signed int and {un}signed __int128
	 are errors.  */
    default:
      *res = resolved_bad;
    }

  return error_mark_node;
}

/* Resolve an overloaded vec_addec or vec_subec call and return a tree
   expression for the resolved call if successful.  ARGS contains the arguments
   to the call.  TYPES contains their types.  RES must be set to indicate the
   status of the resolution attempt.  LOC contains statement location
   information.  */

static tree
resolve_vec_addec_subec (resolution *res, rs6000_gen_builtins fcode,
			 tree *args, tree *types, location_t loc)
{
  /* vec_addec and vec_subec needs to be special cased because there is
     no instruction for the (un)signed int version.  */

  /* All 3 arguments must be vectors of (signed or unsigned) (int or
     __int128) and the types must be compatible.  */
  if (TREE_CODE (types[0]) != VECTOR_TYPE
      || !lang_hooks.types_compatible_p (types[0], types[1])
      || !lang_hooks.types_compatible_p (types[1], types[2]))
    {
      *res = resolved_bad;
      return error_mark_node;
    }

  switch (TYPE_MODE (TREE_TYPE (types[0])))
    {
      /* For {un}signed ints,
	   vec_addec (va, vb, carryv) ==
	     vec_or (vec_addc (va, vb),
		     vec_addc (vec_add (va, vb),
			       vec_and (carryv, 0x1))).  */
    case E_SImode:
      {
	/* Use save_expr to ensure that operands used more than once that may
	   have side effects (like calls) are only evaluated once.  */
	args[0] = save_expr (args[0]);
	args[1] = save_expr (args[1]);
	vec<tree, va_gc> *params = make_tree_vector ();
	vec_safe_push (params, args[0]);
	vec_safe_push (params, args[1]);

	tree as_c_builtin;
	if (fcode == RS6000_OVLD_VEC_ADDEC)
	  as_c_builtin = rs6000_builtin_decls[RS6000_OVLD_VEC_ADDC];
	else
	  as_c_builtin = rs6000_builtin_decls[RS6000_OVLD_VEC_SUBC];

	tree call1 = altivec_resolve_overloaded_builtin (loc, as_c_builtin,
							 params);
	params = make_tree_vector ();
	vec_safe_push (params, args[0]);
	vec_safe_push (params, args[1]);

	tree as_builtin;
	if (fcode == RS6000_OVLD_VEC_ADDEC)
	  as_builtin = rs6000_builtin_decls[RS6000_OVLD_VEC_ADD];
	else
	  as_builtin = rs6000_builtin_decls[RS6000_OVLD_VEC_SUB];

	tree call2 = altivec_resolve_overloaded_builtin (loc, as_builtin,
							 params);
	tree const1 = build_int_cstu (TREE_TYPE (types[0]), 1);
	tree ones_vector = build_vector_from_val (types[0], const1);
	tree and_expr = fold_build2_loc (loc, BIT_AND_EXPR, types[0],
					 args[2], ones_vector);
	params = make_tree_vector ();
	vec_safe_push (params, call2);
	vec_safe_push (params, and_expr);
	call2 = altivec_resolve_overloaded_builtin (loc, as_c_builtin, params);
	params = make_tree_vector ();
	vec_safe_push (params, call1);
	vec_safe_push (params, call2);
	tree or_builtin = rs6000_builtin_decls[RS6000_OVLD_VEC_OR];
	*res = resolved;
	return altivec_resolve_overloaded_builtin (loc, or_builtin, params);
      }
      /* For {un}signed __int128s use the vaddecuq/vsubbecuq
	 instructions.  This occurs through normal processing.  */
    case E_TImode:
      *res = unresolved;
      break;

      /* Types other than {un}signed int and {un}signed __int128
	 are errors.  */
    default:
      *res = resolved_bad;
    }

  return error_mark_node;
}

/* Resolve an overloaded vec_splats or vec_promote call and return a tree
   expression for the resolved call if successful.  NARGS is the number of
   arguments to the call.  ARGLIST contains the arguments.  RES must be set
   to indicate the status of the resolution attempt.  */

static tree
resolve_vec_splats (resolution *res, rs6000_gen_builtins fcode,
		    vec<tree, va_gc> *arglist, unsigned nargs)
{
  const char *name;
  name = fcode == RS6000_OVLD_VEC_SPLATS ? "vec_splats" : "vec_promote";

  if (fcode == RS6000_OVLD_VEC_SPLATS && nargs != 1)
    {
      error ("builtin %qs only accepts 1 argument", name);
      *res = resolved;
      return error_mark_node;
    }

  if (fcode == RS6000_OVLD_VEC_PROMOTE && nargs != 2)
    {
      error ("builtin %qs only accepts 2 arguments", name);
      *res = resolved;
      return error_mark_node;
    }

  /* Ignore promote's element argument.  */
  if (fcode == RS6000_OVLD_VEC_PROMOTE
      && !INTEGRAL_TYPE_P (TREE_TYPE ((*arglist)[1])))
    {
      *res = resolved_bad;
      return error_mark_node;
    }

  tree arg = (*arglist)[0];
  tree type = TREE_TYPE (arg);

  if (!SCALAR_FLOAT_TYPE_P (type) && !INTEGRAL_TYPE_P (type))
    {
      *res = resolved_bad;
      return error_mark_node;
    }

  bool unsigned_p = TYPE_UNSIGNED (type);
  int size;

  switch (TYPE_MODE (type))
    {
    case E_TImode:
      type = unsigned_p ? unsigned_V1TI_type_node : V1TI_type_node;
      size = 1;
      break;
    case E_DImode:
      type = unsigned_p ? unsigned_V2DI_type_node : V2DI_type_node;
      size = 2;
      break;
    case E_SImode:
      type = unsigned_p ? unsigned_V4SI_type_node : V4SI_type_node;
      size = 4;
      break;
    case E_HImode:
      type = unsigned_p ? unsigned_V8HI_type_node : V8HI_type_node;
      size = 8;
      break;
    case E_QImode:
      type = unsigned_p ? unsigned_V16QI_type_node : V16QI_type_node;
      size = 16;
      break;
    case E_SFmode:
      type = V4SF_type_node;
      size = 4;
      break;
    case E_DFmode:
      type = V2DF_type_node;
      size = 2;
      break;
    default:
      *res = resolved_bad;
      return error_mark_node;
    }

  arg = save_expr (fold_convert (TREE_TYPE (type), arg));
  vec<constructor_elt, va_gc> *vec;
  vec_alloc (vec, size);

  for (int i = 0; i < size; i++)
    {
      constructor_elt elt = {NULL_TREE, arg};
      vec->quick_push (elt);
    }

  *res = resolved;
  return build_constructor (type, vec);
}

/* Resolve an overloaded vec_extract call and return a tree expression for
   the resolved call if successful.  NARGS is the number of arguments to
   the call.  ARGLIST contains the arguments.  RES must be set to indicate
   the status of the resolution attempt.  LOC contains statement location
   information.  */

static tree
resolve_vec_extract (resolution *res, vec<tree, va_gc> *arglist,
		     unsigned nargs, location_t loc)
{
  if (nargs != 2)
    {
      error ("builtin %qs only accepts 2 arguments", "vec_extract");
      *res = resolved;
      return error_mark_node;
    }

  tree arg1 = (*arglist)[0];
  tree arg1_type = TREE_TYPE (arg1);
  tree arg2 = (*arglist)[1];

  if (TREE_CODE (arg1_type) != VECTOR_TYPE
      || !INTEGRAL_TYPE_P (TREE_TYPE (arg2)))
    {
      *res = resolved_bad;
      return error_mark_node;
    }

  /* See if we can optimize vec_extract with the current VSX instruction
     set.  */
  machine_mode mode = TYPE_MODE (arg1_type);
  tree arg1_inner_type;

  if (VECTOR_MEM_VSX_P (mode))
    {
      tree call = NULL_TREE;
      int nunits = GET_MODE_NUNITS (mode);
      arg2 = fold_for_warn (arg2);

      /* If the second argument is an integer constant, generate
	 the built-in code if we can.  We need 64-bit and direct
	 move to extract the small integer vectors.  */
      if (TREE_CODE (arg2) == INTEGER_CST)
	{
	  wide_int selector = wi::to_wide (arg2);
	  selector = wi::umod_trunc (selector, nunits);
	  arg2 = wide_int_to_tree (TREE_TYPE (arg2), selector);
	  switch (mode)
	    {
	    case E_V1TImode:
	      call = rs6000_builtin_decls[RS6000_BIF_VEC_EXT_V1TI];
	      break;

	    case E_V2DFmode:
	      call = rs6000_builtin_decls[RS6000_BIF_VEC_EXT_V2DF];
	      break;

	    case E_V2DImode:
	      call = rs6000_builtin_decls[RS6000_BIF_VEC_EXT_V2DI];
	      break;

	    case E_V4SFmode:
	      call = rs6000_builtin_decls[RS6000_BIF_VEC_EXT_V4SF];
	      break;

	    case E_V4SImode:
	      if (TARGET_DIRECT_MOVE_64BIT)
		call = rs6000_builtin_decls[RS6000_BIF_VEC_EXT_V4SI];
	      break;

	    case E_V8HImode:
	      if (TARGET_DIRECT_MOVE_64BIT)
		call = rs6000_builtin_decls[RS6000_BIF_VEC_EXT_V8HI];
	      break;

	    case E_V16QImode:
	      if (TARGET_DIRECT_MOVE_64BIT)
		call = rs6000_builtin_decls[RS6000_BIF_VEC_EXT_V16QI];
	      break;

	    default:
	      break;
	    }
	}

      /* If the second argument is variable, we can optimize it if we are
	 generating 64-bit code on a machine with direct move.  */
      else if (TREE_CODE (arg2) != INTEGER_CST && TARGET_DIRECT_MOVE_64BIT)
	{
	  switch (mode)
	    {
	    case E_V2DFmode:
	      call = rs6000_builtin_decls[RS6000_BIF_VEC_EXT_V2DF];
	      break;

	    case E_V2DImode:
	      call = rs6000_builtin_decls[RS6000_BIF_VEC_EXT_V2DI];
	      break;

	    case E_V4SFmode:
	      call = rs6000_builtin_decls[RS6000_BIF_VEC_EXT_V4SF];
	      break;

	    case E_V4SImode:
	      call = rs6000_builtin_decls[RS6000_BIF_VEC_EXT_V4SI];
	      break;

	    case E_V8HImode:
	      call = rs6000_builtin_decls[RS6000_BIF_VEC_EXT_V8HI];
	      break;

	    case E_V16QImode:
	      call = rs6000_builtin_decls[RS6000_BIF_VEC_EXT_V16QI];
	      break;

	    default:
	      break;
	    }
	}

      if (call)
	{
	  tree result = build_call_expr (call, 2, arg1, arg2);
	  /* Coerce the result to vector element type.  May be no-op.  */
	  arg1_inner_type = TREE_TYPE (arg1_type);
	  result = fold_convert (arg1_inner_type, result);
	  *res = resolved;
	  return result;
	}
    }

  /* Build *(((arg1_inner_type*) & (vector type){arg1}) + arg2). */
  arg1_inner_type = TREE_TYPE (arg1_type);
  tree subp = build_int_cst (TREE_TYPE (arg2),
			     TYPE_VECTOR_SUBPARTS (arg1_type) - 1);
  arg2 = build_binary_op (loc, BIT_AND_EXPR, arg2, subp, 0);

  tree decl = build_decl (loc, VAR_DECL, NULL_TREE, arg1_type);
  DECL_EXTERNAL (decl) = 0;
  TREE_PUBLIC (decl) = 0;
  DECL_CONTEXT (decl) = current_function_decl;
  TREE_USED (decl) = 1;
  TREE_TYPE (decl) = arg1_type;
  TREE_READONLY (decl) = TYPE_READONLY (arg1_type);

  tree stmt;
  if (c_dialect_cxx ())
    {
      stmt = build4 (TARGET_EXPR, arg1_type, decl, arg1, NULL_TREE, NULL_TREE);
      SET_EXPR_LOCATION (stmt, loc);
    }
  else
    {
      DECL_INITIAL (decl) = arg1;
      stmt = build1 (DECL_EXPR, arg1_type, decl);
      TREE_ADDRESSABLE (decl) = 1;
      SET_EXPR_LOCATION (stmt, loc);
      stmt = build1 (COMPOUND_LITERAL_EXPR, arg1_type, stmt);
    }

  tree innerptrtype = build_pointer_type (arg1_inner_type);
  stmt = build_unary_op (loc, ADDR_EXPR, stmt, 0);
  stmt = convert (innerptrtype, stmt);
  stmt = build_binary_op (loc, PLUS_EXPR, stmt, arg2, 1);
  stmt = build_indirect_ref (loc, stmt, RO_NULL);

  /* PR83660: We mark this as having side effects so that downstream in
     fold_build_cleanup_point_expr () it will get a CLEANUP_POINT_EXPR.  If it
     does not we can run into an ICE later in gimplify_cleanup_point_expr ().
     Potentially this causes missed optimization because there actually is no
     side effect.  */
  if (c_dialect_cxx ())
    TREE_SIDE_EFFECTS (stmt) = 1;

  *res = resolved;
  return stmt;
}

/* Resolve an overloaded vec_insert call and return a tree expression for
   the resolved call if successful.  NARGS is the number of arguments to
   the call.  ARGLIST contains the arguments.  RES must be set to indicate
   the status of the resolution attempt.  LOC contains statement location
   information.  */

static tree
resolve_vec_insert (resolution *res, vec<tree, va_gc> *arglist,
		    unsigned nargs, location_t loc)
{
  if (nargs != 3)
    {
      error ("builtin %qs only accepts 3 arguments", "vec_insert");
      *res = resolved;
      return error_mark_node;
    }

  tree arg0 = (*arglist)[0];
  tree arg1 = (*arglist)[1];
  tree arg1_type = TREE_TYPE (arg1);
  tree arg2 = fold_for_warn ((*arglist)[2]);

  if (TREE_CODE (arg1_type) != VECTOR_TYPE
      || !INTEGRAL_TYPE_P (TREE_TYPE (arg2)))
    {
      *res = resolved_bad;
      return error_mark_node;
    }

  /* Build *(((arg1_inner_type*) & (vector type){arg1}) + arg2) = arg0 with
     VIEW_CONVERT_EXPR.  i.e.:
       D.3192 = v1;
       _1 = n & 3;
       VIEW_CONVERT_EXPR<int[4]>(D.3192)[_1] = i;
       v1 = D.3192;
       D.3194 = v1;  */
  if (TYPE_VECTOR_SUBPARTS (arg1_type) == 1)
    arg2 = build_int_cst (TREE_TYPE (arg2), 0);
  else
    {
      tree c = build_int_cst (TREE_TYPE (arg2),
			      TYPE_VECTOR_SUBPARTS (arg1_type) - 1);
      arg2 = build_binary_op (loc, BIT_AND_EXPR, arg2, c, 0);
    }

  tree decl = build_decl (loc, VAR_DECL, NULL_TREE, arg1_type);
  DECL_EXTERNAL (decl) = 0;
  TREE_PUBLIC (decl) = 0;
  DECL_CONTEXT (decl) = current_function_decl;
  TREE_USED (decl) = 1;
  TREE_TYPE (decl) = arg1_type;
  TREE_READONLY (decl) = TYPE_READONLY (arg1_type);
  TREE_ADDRESSABLE (decl) = 1;

  tree stmt;
  if (c_dialect_cxx ())
    {
      stmt = build4 (TARGET_EXPR, arg1_type, decl, arg1, NULL_TREE, NULL_TREE);
      SET_EXPR_LOCATION (stmt, loc);
    }
  else
    {
      DECL_INITIAL (decl) = arg1;
      stmt = build1 (DECL_EXPR, arg1_type, decl);
      SET_EXPR_LOCATION (stmt, loc);
      stmt = build1 (COMPOUND_LITERAL_EXPR, arg1_type, stmt);
    }

  if (TARGET_VSX)
    {
      stmt = build_array_ref (loc, stmt, arg2);
      stmt = fold_build2 (MODIFY_EXPR, TREE_TYPE (arg0), stmt,
			  convert (TREE_TYPE (stmt), arg0));
      stmt = build2 (COMPOUND_EXPR, arg1_type, stmt, decl);
    }
  else
    {
      tree arg1_inner_type = TREE_TYPE (arg1_type);
      tree innerptrtype = build_pointer_type (arg1_inner_type);
      stmt = build_unary_op (loc, ADDR_EXPR, stmt, 0);
      stmt = convert (innerptrtype, stmt);
      stmt = build_binary_op (loc, PLUS_EXPR, stmt, arg2, 1);
      stmt = build_indirect_ref (loc, stmt, RO_NULL);
      stmt = build2 (MODIFY_EXPR, TREE_TYPE (stmt), stmt,
		     convert (TREE_TYPE (stmt), arg0));
      stmt = build2 (COMPOUND_EXPR, arg1_type, stmt, decl);
    }

  *res = resolved;
  return stmt;
}

/* Resolve an overloaded vec_step call and return a tree expression for
   the resolved call if successful.  NARGS is the number of arguments to
   the call.  ARGLIST contains the arguments.  RES must be set to indicate
   the status of the resolution attempt.  */

static tree
resolve_vec_step (resolution *res, vec<tree, va_gc> *arglist, unsigned nargs)
{
  if (nargs != 1)
    {
      error ("builtin %qs only accepts 1 argument", "vec_step");
      *res = resolved;
      return error_mark_node;
    }

  tree arg0 = (*arglist)[0];
  tree arg0_type = TREE_TYPE (arg0);

  if (TREE_CODE (arg0_type) != VECTOR_TYPE)
    {
      *res = resolved_bad;
      return error_mark_node;
    }

  *res = resolved;
  return build_int_cst (NULL_TREE, TYPE_VECTOR_SUBPARTS (arg0_type));
}

/* Look for a matching instance in a chain of instances.  INSTANCE points to
   the chain of instances; INSTANCE_CODE is the code identifying the specific
   built-in being searched for; FCODE is the overloaded function code; TYPES
   contains an array of NARGS types that must match the types of the
   instance's parameters; ARGS contains an array of NARGS arguments to be
   passed to the instance; and NARGS is the number of built-in arguments to
   check.  If found, resolve the built-in and return it, unless the built-in
   is not supported in context.  In that case, set UNSUPPORTED_BUILTIN to
   true.  If we don't match, return error_mark_node and leave
   UNSUPPORTED_BUILTIN alone.  */

static tree
find_instance (bool *unsupported_builtin, int *instance,
	       rs6000_gen_builtins instance_code,
	       rs6000_gen_builtins fcode,
	       tree *types, tree *args, int nargs)
{
  while (*instance != -1
	 && rs6000_instance_info[*instance].bifid != instance_code)
    *instance = rs6000_instance_info[*instance].next;

  int inst = *instance;
  gcc_assert (inst != -1);
  /* It is possible for an instance to require a data type that isn't
     defined on this target, in which case rs6000_instance_info_fntype[inst]
     will be NULL.  */
  if (!rs6000_instance_info_fntype[inst])
    return error_mark_node;
  rs6000_gen_builtins bifid = rs6000_instance_info[inst].bifid;
  tree fntype = rs6000_builtin_info_fntype[bifid];
  tree argtype = TYPE_ARG_TYPES (fntype);
  bool args_compatible = true;

  for (int i = 0; i < nargs; i++)
    {
      tree parmtype = TREE_VALUE (argtype);
      if (!rs6000_builtin_type_compatible (types[i], parmtype))
	{
	  args_compatible = false;
	  break;
	}
      argtype = TREE_CHAIN (argtype);
    }

  if (args_compatible)
    {
      if (rs6000_builtin_decl (bifid, false) != error_mark_node
	  && rs6000_builtin_is_supported (bifid))
	{
	  tree ret_type = TREE_TYPE (rs6000_instance_info_fntype[inst]);
	  return altivec_build_resolved_builtin (args, nargs, fntype, ret_type,
						 bifid, fcode);
	}
      else
	*unsupported_builtin = true;
    }

  return error_mark_node;
}

/* Implementation of the resolve_overloaded_builtin target hook, to
   support Altivec's overloaded builtins.  */

tree
altivec_resolve_overloaded_builtin (location_t loc, tree fndecl,
				    void *passed_arglist, bool)
{
  rs6000_gen_builtins fcode
    = (rs6000_gen_builtins) DECL_MD_FUNCTION_CODE (fndecl);

  /* Return immediately if this isn't an overload.  */
  if (fcode <= RS6000_OVLD_NONE)
    return NULL_TREE;

  if (TARGET_DEBUG_BUILTIN)
    fprintf (stderr, "altivec_resolve_overloaded_builtin, code = %4d, %s\n",
	     (int) fcode, IDENTIFIER_POINTER (DECL_NAME (fndecl)));

  /* vec_lvsl and vec_lvsr are deprecated for use with LE element order.  */
  if (fcode == RS6000_OVLD_VEC_LVSL && !BYTES_BIG_ENDIAN)
    warning (OPT_Wdeprecated,
	     "%<vec_lvsl%> is deprecated for little endian; use "
	     "assignment for unaligned loads and stores");
  else if (fcode == RS6000_OVLD_VEC_LVSR && !BYTES_BIG_ENDIAN)
    warning (OPT_Wdeprecated,
	     "%<vec_lvsr%> is deprecated for little endian; use "
	     "assignment for unaligned loads and stores");

  /* Gather the arguments and their types into arrays for easier handling.  */
  tree fnargs = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
  tree types[MAX_OVLD_ARGS];
  tree args[MAX_OVLD_ARGS];
  unsigned int n;

  /* Count the number of expected arguments.  */
  unsigned expected_args = 0;
  for (tree chain = fnargs;
       chain && !VOID_TYPE_P (TREE_VALUE (chain));
       chain = TREE_CHAIN (chain))
    expected_args++;

  vec<tree, va_gc> *arglist = static_cast<vec<tree, va_gc> *> (passed_arglist);
  unsigned int nargs = vec_safe_length (arglist);

  /* If the number of arguments did not match the prototype, return NULL
     and the generic code will issue the appropriate error message.  Skip
     this test for functions where we don't fully describe all the possible
     overload signatures in rs6000-overload.def (because they aren't relevant
     to the expansion here).  If we don't, we get confusing error messages.  */
  /* As an example, for vec_splats we have:

; There are no actual builtins for vec_splats.  There is special handling for
; this in altivec_resolve_overloaded_builtin in rs6000-c.cc, where the call
; is replaced by a constructor.  The single overload here causes
; __builtin_vec_splats to be registered with the front end so that can happen.
[VEC_SPLATS, vec_splats, __builtin_vec_splats]
  vsi __builtin_vec_splats (vsi);
    ABS_V4SI SPLATS_FAKERY

    So even though __builtin_vec_splats accepts all vector types, the
    infrastructure cheats and just records one prototype.  We end up getting
    an error message that refers to this specific prototype even when we
    are handling a different argument type.  That is completely confusing
    to the user, so it's best to let these cases be handled individually
    in the resolve_vec_splats, etc., helper functions.  */

  if (expected_args != nargs
      && !(fcode == RS6000_OVLD_VEC_PROMOTE
	   || fcode == RS6000_OVLD_VEC_SPLATS
	   || fcode == RS6000_OVLD_VEC_EXTRACT
	   || fcode == RS6000_OVLD_VEC_INSERT
	   || fcode == RS6000_OVLD_VEC_STEP))
    return NULL;

  for (n = 0;
       !VOID_TYPE_P (TREE_VALUE (fnargs)) && n < nargs;
       fnargs = TREE_CHAIN (fnargs), n++)
    {
      tree decl_type = TREE_VALUE (fnargs);
      tree arg = (*arglist)[n];

      if (arg == error_mark_node)
	return error_mark_node;

      if (n >= MAX_OVLD_ARGS)
	abort ();

      arg = default_conversion (arg);
      tree type = TREE_TYPE (arg);

      /* The C++ front-end converts float * to const void * using
	 NOP_EXPR<const void *> (NOP_EXPR<void *> (x)).  */
      if (POINTER_TYPE_P (type)
	  && TREE_CODE (arg) == NOP_EXPR
	  && lang_hooks.types_compatible_p (TREE_TYPE (arg),
					    const_ptr_type_node)
	  && lang_hooks.types_compatible_p (TREE_TYPE (TREE_OPERAND (arg, 0)),
					    ptr_type_node))
	{
	  arg = TREE_OPERAND (arg, 0);
	  type = TREE_TYPE (arg);
	}

      /* Remove the const from the pointers to simplify the overload
	 matching further down.  */
      if (POINTER_TYPE_P (decl_type)
	  && POINTER_TYPE_P (type)
	  && TYPE_QUALS (TREE_TYPE (type)) != 0)
	{
	  if (TYPE_READONLY (TREE_TYPE (type))
	      && !TYPE_READONLY (TREE_TYPE (decl_type)))
	    warning (0, "passing argument %d of %qE discards %qs "
		     "qualifier from pointer target type", n + 1, fndecl,
		     "const");
	  type = build_qualified_type (TREE_TYPE (type), 0);
	  type = build_pointer_type (type);
	  arg = fold_convert (type, arg);
	}

      /* For RS6000_OVLD_VEC_LXVL, convert any const * to its non constant
	 equivalent to simplify the overload matching below.  */
      if (fcode == RS6000_OVLD_VEC_LXVL
	  && POINTER_TYPE_P (type)
	  && TYPE_READONLY (TREE_TYPE (type)))
	{
	  type = build_qualified_type (TREE_TYPE (type), 0);
	  type = build_pointer_type (type);
	  arg = fold_convert (type, arg);
	}

      args[n] = arg;
      types[n] = type;
    }

  /* Some overloads require special handling.  */
  tree returned_expr = NULL;
  resolution res = unresolved;

  if (fcode == RS6000_OVLD_VEC_MUL)
    returned_expr = resolve_vec_mul (&res, args, types, loc);
  else if (fcode == RS6000_OVLD_VEC_CMPNE)
    returned_expr = resolve_vec_cmpne (&res, args, types, loc);
  else if (fcode == RS6000_OVLD_VEC_ADDE || fcode == RS6000_OVLD_VEC_SUBE)
    returned_expr = resolve_vec_adde_sube (&res, fcode, args, types, loc);
  else if (fcode == RS6000_OVLD_VEC_ADDEC || fcode == RS6000_OVLD_VEC_SUBEC)
    returned_expr = resolve_vec_addec_subec (&res, fcode, args, types, loc);
  else if (fcode == RS6000_OVLD_VEC_SPLATS || fcode == RS6000_OVLD_VEC_PROMOTE)
    returned_expr = resolve_vec_splats (&res, fcode, arglist, nargs);
  else if (fcode == RS6000_OVLD_VEC_EXTRACT)
    returned_expr = resolve_vec_extract (&res, arglist, nargs, loc);
  else if (fcode == RS6000_OVLD_VEC_INSERT)
    returned_expr = resolve_vec_insert (&res, arglist, nargs, loc);
  else if (fcode == RS6000_OVLD_VEC_STEP)
    returned_expr = resolve_vec_step (&res, arglist, nargs);

  if (res == resolved)
    return returned_expr;

  /* "Regular" built-in functions and overloaded functions share a namespace
     for some arrays, like rs6000_builtin_decls.  But rs6000_overload_info
     only has information for the overloaded functions, so we need an
     adjusted index for that.  */
  unsigned int adj_fcode = fcode - RS6000_OVLD_NONE;

  if (res == resolved_bad)
    {
      const char *name = rs6000_overload_info[adj_fcode].ovld_name;
      error ("invalid parameter combination for AltiVec intrinsic %qs", name);
      return error_mark_node;
    }

  bool unsupported_builtin = false;
  rs6000_gen_builtins instance_code;
  bool supported = false;
  int instance = rs6000_overload_info[adj_fcode].first_instance;
  gcc_assert (instance != -1);

  /* Functions with no arguments can have only one overloaded instance.  */
  gcc_assert (nargs > 0 || rs6000_instance_info[instance].next == -1);

  /* Standard overload processing involves determining whether an instance
     exists that is type-compatible with the overloaded function call.  In
     a couple of cases, we need to do some extra processing to disambiguate
     between multiple compatible instances.  */
  switch (fcode)
    {
      /* Need to special case __builtin_cmpb because the overloaded forms
	 of this function take (unsigned int, unsigned int) or (unsigned
	 long long int, unsigned long long int).  Since C conventions
	 allow the respective argument types to be implicitly coerced into
	 each other, the default handling does not provide adequate
	 discrimination between the desired forms of the function.  */
    case RS6000_OVLD_SCAL_CMPB:
      {
	machine_mode arg1_mode = TYPE_MODE (types[0]);
	machine_mode arg2_mode = TYPE_MODE (types[1]);

	/* If any supplied arguments are wider than 32 bits, resolve to
	   64-bit variant of built-in function.  */
	if (GET_MODE_PRECISION (arg1_mode) > 32
	    || GET_MODE_PRECISION (arg2_mode) > 32)
	  /* Assure all argument and result types are compatible with
	     the built-in function represented by RS6000_BIF_CMPB.  */
	  instance_code = RS6000_BIF_CMPB;
	else
	  /* Assure all argument and result types are compatible with
	     the built-in function represented by RS6000_BIF_CMPB_32.  */
	  instance_code = RS6000_BIF_CMPB_32;

	tree call = find_instance (&unsupported_builtin, &instance,
				   instance_code, fcode, types, args, nargs);
	if (call != error_mark_node)
	  return call;
	break;
      }
    case RS6000_OVLD_VEC_VSIE:
      {
	machine_mode arg1_mode = TYPE_MODE (types[0]);

	/* If supplied first argument is wider than 64 bits, resolve to
	   128-bit variant of built-in function.  */
	if (GET_MODE_PRECISION (arg1_mode) > 64)
	  {
	    /* If first argument is of float variety, choose the variant that
	       expects __ieee128 argument.  If the first argument is vector
	       int, choose the variant that expects vector unsigned
	       __int128 argument.  Otherwise, expect scalar __int128 argument.
	    */
	    if (GET_MODE_CLASS (arg1_mode) == MODE_FLOAT)
	      instance_code = RS6000_BIF_VSIEQPF;
	    else if (GET_MODE_CLASS (arg1_mode) == MODE_VECTOR_INT)
	      instance_code = RS6000_BIF_VSIEQPV;
	    else
	      instance_code = RS6000_BIF_VSIEQP;
	  }
	else
	  {
	    /* If first argument is of float variety, choose variant
	       that expects double argument.  Otherwise, expect
	       long long int argument.  */
	    if (GET_MODE_CLASS (arg1_mode) == MODE_FLOAT)
	      instance_code = RS6000_BIF_VSIEDPF;
	    else
	      instance_code = RS6000_BIF_VSIEDP;
	  }

	tree call = find_instance (&unsupported_builtin, &instance,
				   instance_code, fcode, types, args, nargs);
	if (call != error_mark_node)
	  return call;
	break;
      }
    case RS6000_OVLD_VEC_REPLACE_UN:
      {
	machine_mode arg2_mode = TYPE_MODE (types[1]);

	if (arg2_mode == SImode)
	  /* Signed and unsigned are handled the same.  */
	  instance_code = RS6000_BIF_VREPLACE_UN_USI;
	else if (arg2_mode == SFmode)
	  instance_code = RS6000_BIF_VREPLACE_UN_SF;
	else if (arg2_mode == DImode)
	  /* Signed and unsigned are handled the same.  */
	  instance_code = RS6000_BIF_VREPLACE_UN_UDI;
	else if (arg2_mode == DFmode)
	  instance_code = RS6000_BIF_VREPLACE_UN_DF;
	else
	  break;

	tree call = find_instance (&unsupported_builtin, &instance,
				   instance_code, fcode, types, args, nargs);
	if (call != error_mark_node)
	  return call;
	break;
      }
    default:
      /* Standard overload processing.  Look for an instance with compatible
	 parameter types.  If it is supported in the current context, resolve
	 the overloaded call to that instance.  */
      for (; instance != -1; instance = rs6000_instance_info[instance].next)
	{
	  tree fntype = rs6000_instance_info_fntype[instance];
	  rs6000_gen_builtins bifid = rs6000_instance_info[instance].bifid;
	  /* It is possible for an instance to require a data type that isn't
	     defined on this target, in which case fntype will be
	     NULL.  */
	  if (!fntype)
	    continue;

	  bool mismatch = false;
	  tree nextparm = TYPE_ARG_TYPES (fntype);

	  for (unsigned int arg_i = 0;
	       arg_i < nargs && nextparm != NULL;
	       arg_i++)
	    {
	      tree parmtype = TREE_VALUE (nextparm);
	      if (!rs6000_builtin_type_compatible (types[arg_i], parmtype))
		{
		  mismatch = true;
		  break;
		}
	      nextparm = TREE_CHAIN (nextparm);
	    }

	  if (mismatch)
	    continue;

	  supported = rs6000_builtin_is_supported (bifid);
	  if (rs6000_builtin_decl (bifid, false) != error_mark_node
	      && supported)
	    {
	      tree ret_type = TREE_TYPE (fntype);
	      fntype = rs6000_builtin_info_fntype[bifid];
	      return altivec_build_resolved_builtin (args, nargs, fntype,
						     ret_type, bifid, fcode);
	    }
	  else
	    {
	      unsupported_builtin = true;
	      break;
	    }
	}
    }

  if (unsupported_builtin)
    {
      const char *name = rs6000_overload_info[adj_fcode].ovld_name;
      if (!supported)
	{
	  /* Indicate that the instantiation of the overloaded builtin
	     name is not available with the target flags in effect.  */
	  rs6000_gen_builtins bifid = rs6000_instance_info[instance].bifid;
	  rs6000_gen_builtins fcode = (rs6000_gen_builtins) bifid;
	  rs6000_invalid_builtin (fcode);
	  /* Provide clarity of the relationship between the overload
	     and the instantiation.  */
	  const char *internal_name = rs6000_builtin_info[bifid].bifname;
	  rich_location richloc (line_table, input_location);
	  inform (&richloc,
		  "overloaded builtin %qs is implemented by builtin %qs",
		  name, internal_name);
	}
      else
	error ("%qs is not supported in this compiler configuration", name);

      return error_mark_node;
    }

  /* If we fall through to here, there were no compatible instances.  */
  const char *name = rs6000_overload_info[adj_fcode].ovld_name;
  error ("invalid parameter combination for AltiVec intrinsic %qs", name);
  return error_mark_node;
}
