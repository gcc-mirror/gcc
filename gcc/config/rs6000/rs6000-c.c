/* Subroutines for the C front end on the PowerPC architecture.
   Copyright (C) 2002-2020 Free Software Foundation, Inc.

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

/* Called to decide whether a conditional macro should be expanded.
   Since we have exactly one such macro (i.e, 'vector'), we do not
   need to examine the 'tok' parameter.  */

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
	      else
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
   #pragma GCC target, we need to adjust the macros dynamically.  Note, some of
   the options needed for builtins have been moved to separate variables, so
   have both the target flags and the builtin flags as arguments.  */

void
rs6000_target_modify_macros (bool define_p, HOST_WIDE_INT flags,
			     HOST_WIDE_INT bu_mask)
{
  if (TARGET_DEBUG_BUILTIN || TARGET_DEBUG_TARGET)
    fprintf (stderr,
	     "rs6000_target_modify_macros (%s, " HOST_WIDE_INT_PRINT_HEX
	     ", " HOST_WIDE_INT_PRINT_HEX ")\n",
	     (define_p) ? "define" : "undef",
	     flags, bu_mask);

  /* Each of the flags mentioned below controls whether certain
     preprocessor macros will be automatically defined when
     preprocessing source files for compilation by this compiler.
     While most of these flags can be enabled or disabled
     explicitly by specifying certain command-line options when
     invoking the compiler, there are also many ways in which these
     flags are enabled or disabled implicitly, based on compiler
     defaults, configuration choices, and on the presence of certain
     related command-line options.  Many, but not all, of these
     implicit behaviors can be found in file "rs6000.c", the
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
	   MASK_PPC_GFXOPT or MASK_POWERPC64 (flags for "powerpc64"
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
  /* Note that the OPTION_MASK_DIRECT_MOVE flag is automatically
     turned on in the following condition:
     1. TARGET_P8_VECTOR is enabled and OPTION_MASK_DIRECT_MOVE is not
        explicitly disabled.
        Hereafter, the OPTION_MASK_DIRECT_MOVE flag is considered to
        have been turned on explicitly.
     Note that the OPTION_MASK_DIRECT_MOVE flag is automatically
     turned off in any of the following conditions:
     1. TARGET_HARD_FLOAT, TARGET_ALTIVEC, or TARGET_VSX is explicitly
	disabled and OPTION_MASK_DIRECT_MOVE was not explicitly
	enabled.
     2. TARGET_VSX is off.  */
  if ((flags & OPTION_MASK_DIRECT_MOVE) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PWR8");
  if ((flags & OPTION_MASK_MODULO) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PWR9");
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
    }
  /* OPTION_MASK_FLOAT128_HARDWARE can be turned on if -mcpu=power9 is used or
     via the target attribute/pragma.  */
  if ((flags & OPTION_MASK_FLOAT128_HW) != 0)
    rs6000_define_or_undefine_macro (define_p, "__FLOAT128_HARDWARE__");

  /* options from the builtin masks.  */
  /* Note that RS6000_BTM_CELL is enabled only if (rs6000_cpu ==
     PROCESSOR_CELL) (e.g. -mcpu=cell).  */
  if ((bu_mask & RS6000_BTM_CELL) != 0)
    rs6000_define_or_undefine_macro (define_p, "__PPU__");
}

void
rs6000_cpu_cpp_builtins (cpp_reader *pfile)
{
  /* Define all of the common macros.  */
  rs6000_target_modify_macros (true, rs6000_isa_flags,
			       rs6000_builtin_mask_calculate ());

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
	  || (TARGET_IEEEQUAD
	      && TARGET_LONG_DOUBLE_128
	      && t == long_double_type_node));
}
  
static inline bool
rs6000_builtin_type_compatible (tree t, int id)
{
  tree builtin_type;
  builtin_type = rs6000_builtin_type (id);
  if (t == error_mark_node)
    return false;
  if (INTEGRAL_TYPE_P (t) && INTEGRAL_TYPE_P (builtin_type))
    return true;
  else if (TARGET_IEEEQUAD && TARGET_LONG_DOUBLE_128
	   && is_float128_p (t) && is_float128_p (builtin_type))
    return true;
  else
    return lang_hooks.types_compatible_p (t, builtin_type);
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
altivec_build_resolved_builtin (tree *args, int n,
				const struct altivec_builtin_types *desc)
{
  tree impl_fndecl = rs6000_builtin_decls[desc->overloaded_code];
  tree ret_type = rs6000_builtin_type (desc->ret_type);
  tree argtypes = TYPE_ARG_TYPES (TREE_TYPE (impl_fndecl));
  tree arg_type[3];
  tree call;

  int i;
  for (i = 0; i < n; i++)
    arg_type[i] = TREE_VALUE (argtypes), argtypes = TREE_CHAIN (argtypes);

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
      && desc->code == ALTIVEC_BUILTIN_VEC_VCMPGE_P
      && desc->overloaded_code != ALTIVEC_BUILTIN_VCMPGEFP_P
      && desc->overloaded_code != VSX_BUILTIN_XVCMPGEDP_P)
    {
      std::swap (args[1], args[2]);
      std::swap (arg_type[1], arg_type[2]);

      args[0] = fold_build2 (BIT_XOR_EXPR, TREE_TYPE (args[0]), args[0],
			     build_int_cst (NULL_TREE, 2));
    }

  switch (n)
    {
    case 0:
      call = build_call_expr (impl_fndecl, 0);
      break;
    case 1:
      call = build_call_expr (impl_fndecl, 1,
			      fully_fold_convert (arg_type[0], args[0]));
      break;
    case 2:
      call = build_call_expr (impl_fndecl, 2,
			      fully_fold_convert (arg_type[0], args[0]),
			      fully_fold_convert (arg_type[1], args[1]));
      break;
    case 3:
      call = build_call_expr (impl_fndecl, 3,
			      fully_fold_convert (arg_type[0], args[0]),
			      fully_fold_convert (arg_type[1], args[1]),
			      fully_fold_convert (arg_type[2], args[2]));
      break;
    default:
      gcc_unreachable ();
    }
  return fold_convert (ret_type, call);
}

/* Implementation of the resolve_overloaded_builtin target hook, to
   support Altivec's overloaded builtins.  */

tree
altivec_resolve_overloaded_builtin (location_t loc, tree fndecl,
				    void *passed_arglist)
{
  vec<tree, va_gc> *arglist = static_cast<vec<tree, va_gc> *> (passed_arglist);
  unsigned int nargs = vec_safe_length (arglist);
  enum rs6000_builtins fcode
    = (enum rs6000_builtins) DECL_MD_FUNCTION_CODE (fndecl);
  tree fnargs = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
  tree types[3], args[3];
  const struct altivec_builtin_types *desc;
  unsigned int n;

  if (!rs6000_overloaded_builtin_p (fcode))
    return NULL_TREE;

  if (TARGET_DEBUG_BUILTIN)
    fprintf (stderr, "altivec_resolve_overloaded_builtin, code = %4d, %s\n",
	     (int)fcode, IDENTIFIER_POINTER (DECL_NAME (fndecl)));
 
  /* vec_lvsl and vec_lvsr are deprecated for use with LE element order.  */
  if (fcode == ALTIVEC_BUILTIN_VEC_LVSL && !BYTES_BIG_ENDIAN)
    warning (OPT_Wdeprecated,
	     "%<vec_lvsl%> is deprecated for little endian; use "
	     "assignment for unaligned loads and stores");
  else if (fcode == ALTIVEC_BUILTIN_VEC_LVSR && !BYTES_BIG_ENDIAN)
    warning (OPT_Wdeprecated,
	     "%<vec_lvsr%> is deprecated for little endian; use "
	     "assignment for unaligned loads and stores");

  if (fcode == ALTIVEC_BUILTIN_VEC_MUL)
    {
      /* vec_mul needs to be special cased because there are no instructions
	 for it for the {un}signed char, {un}signed short, and {un}signed int
	 types.  */
      if (nargs != 2)
	{
	  error ("builtin %qs only accepts 2 arguments", "vec_mul");
	  return error_mark_node;
	}

      tree arg0 = (*arglist)[0];
      tree arg0_type = TREE_TYPE (arg0);
      tree arg1 = (*arglist)[1];
      tree arg1_type = TREE_TYPE (arg1);

      /* Both arguments must be vectors and the types must be compatible.  */
      if (TREE_CODE (arg0_type) != VECTOR_TYPE)
	goto bad;
      if (!lang_hooks.types_compatible_p (arg0_type, arg1_type))
	goto bad;

      switch (TYPE_MODE (TREE_TYPE (arg0_type)))
	{
	  case E_QImode:
	  case E_HImode:
	  case E_SImode:
	  case E_DImode:
	  case E_TImode:
	    {
	      /* For scalar types just use a multiply expression.  */
	      return fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (arg0), arg0,
				      fold_convert (TREE_TYPE (arg0), arg1));
	    }
	  case E_SFmode:
	    {
	      /* For floats use the xvmulsp instruction directly.  */
	      tree call = rs6000_builtin_decls[VSX_BUILTIN_XVMULSP];
	      return build_call_expr (call, 2, arg0, arg1);
	    }
	  case E_DFmode:
	    {
	      /* For doubles use the xvmuldp instruction directly.  */
	      tree call = rs6000_builtin_decls[VSX_BUILTIN_XVMULDP];
	      return build_call_expr (call, 2, arg0, arg1);
	    }
	  /* Other types are errors.  */
	  default:
	    goto bad;
	}
    }

  if (fcode == ALTIVEC_BUILTIN_VEC_CMPNE)
    {
      /* vec_cmpne needs to be special cased because there are no instructions
	 for it (prior to power 9).  */
      if (nargs != 2)
	{
	  error ("builtin %qs only accepts 2 arguments", "vec_cmpne");
	  return error_mark_node;
	}

      tree arg0 = (*arglist)[0];
      tree arg0_type = TREE_TYPE (arg0);
      tree arg1 = (*arglist)[1];
      tree arg1_type = TREE_TYPE (arg1);

      /* Both arguments must be vectors and the types must be compatible.  */
      if (TREE_CODE (arg0_type) != VECTOR_TYPE)
	goto bad;
      if (!lang_hooks.types_compatible_p (arg0_type, arg1_type))
	goto bad;

      /* Power9 instructions provide the most efficient implementation of
	 ALTIVEC_BUILTIN_VEC_CMPNE if the mode is not DImode or TImode
	 or SFmode or DFmode.  */
      if (!TARGET_P9_VECTOR
	  || (TYPE_MODE (TREE_TYPE (arg0_type)) == DImode)
	  || (TYPE_MODE (TREE_TYPE (arg0_type)) == TImode)
	  || (TYPE_MODE (TREE_TYPE (arg0_type)) == SFmode)
	  || (TYPE_MODE (TREE_TYPE (arg0_type)) == DFmode))
	{
	  switch (TYPE_MODE (TREE_TYPE (arg0_type)))
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
		vec_safe_push (params, arg0);
		vec_safe_push (params, arg1);
		tree call = altivec_resolve_overloaded_builtin
		  (loc, rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_CMPEQ],
		   params);
		/* Use save_expr to ensure that operands used more than once
		   that may have side effects (like calls) are only evaluated
		   once.  */
		call = save_expr (call);
		params = make_tree_vector ();
		vec_safe_push (params, call);
		vec_safe_push (params, call);
		return altivec_resolve_overloaded_builtin
		  (loc, rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_NOR], params);
	      }
	      /* Other types are errors.  */
	    default:
	      goto bad;
	    }
	}
      /* else, fall through and process the Power9 alternative below */
    }

  if (fcode == ALTIVEC_BUILTIN_VEC_ADDE
      || fcode == ALTIVEC_BUILTIN_VEC_SUBE)
    {
      /* vec_adde needs to be special cased because there is no instruction
	  for the {un}signed int version.  */
      if (nargs != 3)
	{
	  const char *name = fcode == ALTIVEC_BUILTIN_VEC_ADDE ?
	    "vec_adde": "vec_sube";
	  error ("builtin %qs only accepts 3 arguments", name);
	  return error_mark_node;
	}

      tree arg0 = (*arglist)[0];
      tree arg0_type = TREE_TYPE (arg0);
      tree arg1 = (*arglist)[1];
      tree arg1_type = TREE_TYPE (arg1);
      tree arg2 = (*arglist)[2];
      tree arg2_type = TREE_TYPE (arg2);

      /* All 3 arguments must be vectors of (signed or unsigned) (int or
	 __int128) and the types must be compatible.  */
      if (TREE_CODE (arg0_type) != VECTOR_TYPE)
	goto bad;
      if (!lang_hooks.types_compatible_p (arg0_type, arg1_type)
	  || !lang_hooks.types_compatible_p (arg1_type, arg2_type))
	goto bad;

      switch (TYPE_MODE (TREE_TYPE (arg0_type)))
	{
	  /* For {un}signed ints,
	     vec_adde (va, vb, carryv) == vec_add (vec_add (va, vb),
						   vec_and (carryv, 1)).
	     vec_sube (va, vb, carryv) == vec_sub (vec_sub (va, vb),
						   vec_and (carryv, 1)).  */
	  case E_SImode:
	    {
	      tree add_sub_builtin;

	      vec<tree, va_gc> *params = make_tree_vector ();
	      vec_safe_push (params, arg0);
	      vec_safe_push (params, arg1);

	      if (fcode == ALTIVEC_BUILTIN_VEC_ADDE)
		add_sub_builtin = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_ADD];
	      else
		add_sub_builtin = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_SUB];

	      tree call = altivec_resolve_overloaded_builtin (loc,
							      add_sub_builtin,
							      params);
	      tree const1 = build_int_cstu (TREE_TYPE (arg0_type), 1);
	      tree ones_vector = build_vector_from_val (arg0_type, const1);
	      tree and_expr = fold_build2_loc (loc, BIT_AND_EXPR, arg0_type,
					       arg2, ones_vector);
	      params = make_tree_vector ();
	      vec_safe_push (params, call);
	      vec_safe_push (params, and_expr);
	      return altivec_resolve_overloaded_builtin (loc, add_sub_builtin,
							 params);
	    }
	  /* For {un}signed __int128s use the vaddeuqm instruction
		directly.  */
	  case E_TImode:
	    {
	       tree bii;

	       if (fcode == ALTIVEC_BUILTIN_VEC_ADDE)
		 bii = rs6000_builtin_decls[P8V_BUILTIN_VEC_VADDEUQM];

	       else
		 bii = rs6000_builtin_decls[P8V_BUILTIN_VEC_VSUBEUQM];

	       return altivec_resolve_overloaded_builtin (loc, bii, arglist);
	    }

	  /* Types other than {un}signed int and {un}signed __int128
		are errors.  */
	  default:
	    goto bad;
	}
    }

  if (fcode == ALTIVEC_BUILTIN_VEC_ADDEC
      || fcode == ALTIVEC_BUILTIN_VEC_SUBEC)
    {
      /* vec_addec and vec_subec needs to be special cased because there is
	 no instruction for the {un}signed int version.  */
      if (nargs != 3)
	{
	  const char *name = fcode == ALTIVEC_BUILTIN_VEC_ADDEC ?
	    "vec_addec": "vec_subec";
	  error ("builtin %qs only accepts 3 arguments", name);
	  return error_mark_node;
	}

      tree arg0 = (*arglist)[0];
      tree arg0_type = TREE_TYPE (arg0);
      tree arg1 = (*arglist)[1];
      tree arg1_type = TREE_TYPE (arg1);
      tree arg2 = (*arglist)[2];
      tree arg2_type = TREE_TYPE (arg2);

      /* All 3 arguments must be vectors of (signed or unsigned) (int or
	 __int128) and the types must be compatible.  */
      if (TREE_CODE (arg0_type) != VECTOR_TYPE)
	goto bad;
      if (!lang_hooks.types_compatible_p (arg0_type, arg1_type)
	  || !lang_hooks.types_compatible_p (arg1_type, arg2_type))
	goto bad;

      switch (TYPE_MODE (TREE_TYPE (arg0_type)))
	{
	  /* For {un}signed ints,
	      vec_addec (va, vb, carryv) ==
				vec_or (vec_addc (va, vb),
					vec_addc (vec_add (va, vb),
						  vec_and (carryv, 0x1))).  */
	  case E_SImode:
	    {
	    /* Use save_expr to ensure that operands used more than once
		that may have side effects (like calls) are only evaluated
		once.  */
	    tree as_builtin;
	    tree as_c_builtin;

	    arg0 = save_expr (arg0);
	    arg1 = save_expr (arg1);
	    vec<tree, va_gc> *params = make_tree_vector ();
	    vec_safe_push (params, arg0);
	    vec_safe_push (params, arg1);

	    if (fcode == ALTIVEC_BUILTIN_VEC_ADDEC)
	      as_c_builtin = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_ADDC];
	    else
	      as_c_builtin = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_SUBC];

	    tree call1 = altivec_resolve_overloaded_builtin (loc, as_c_builtin,
							     params);
	    params = make_tree_vector ();
	    vec_safe_push (params, arg0);
	    vec_safe_push (params, arg1);


	    if (fcode == ALTIVEC_BUILTIN_VEC_ADDEC)
	      as_builtin = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_ADD];
	    else
	      as_builtin = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_SUB];

	    tree call2 = altivec_resolve_overloaded_builtin (loc, as_builtin,
							     params);
	    tree const1 = build_int_cstu (TREE_TYPE (arg0_type), 1);
	    tree ones_vector = build_vector_from_val (arg0_type, const1);
	    tree and_expr = fold_build2_loc (loc, BIT_AND_EXPR, arg0_type,
					     arg2, ones_vector);
	    params = make_tree_vector ();
	    vec_safe_push (params, call2);
	    vec_safe_push (params, and_expr);
	    call2 = altivec_resolve_overloaded_builtin (loc, as_c_builtin,
							params);
	    params = make_tree_vector ();
	    vec_safe_push (params, call1);
	    vec_safe_push (params, call2);
	    tree or_builtin = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_OR];
	    return altivec_resolve_overloaded_builtin (loc, or_builtin,
						       params);
	    }
	  /* For {un}signed __int128s use the vaddecuq/vsubbecuq
	     instructions.  */
	  case E_TImode:
	    {
	       tree bii;

	       if (fcode == ALTIVEC_BUILTIN_VEC_ADDEC)
		 bii = rs6000_builtin_decls[P8V_BUILTIN_VEC_VADDECUQ];

	       else
		 bii = rs6000_builtin_decls[P8V_BUILTIN_VEC_VSUBECUQ];

	       return altivec_resolve_overloaded_builtin (loc, bii, arglist);
	    }
	  /* Types other than {un}signed int and {un}signed __int128
		are errors.  */
	  default:
	    goto bad;
	}
    }

  /* For now treat vec_splats and vec_promote as the same.  */
  if (fcode == ALTIVEC_BUILTIN_VEC_SPLATS
      || fcode == ALTIVEC_BUILTIN_VEC_PROMOTE)
    {
      tree type, arg;
      int size;
      int i;
      bool unsigned_p;
      vec<constructor_elt, va_gc> *vec;
      const char *name = fcode == ALTIVEC_BUILTIN_VEC_SPLATS ? "vec_splats": "vec_promote";

      if (fcode == ALTIVEC_BUILTIN_VEC_SPLATS && nargs != 1)
	{
	  error ("builtin %qs only accepts 1 argument", name);
	  return error_mark_node;
	}
      if (fcode == ALTIVEC_BUILTIN_VEC_PROMOTE && nargs != 2)
	{
	  error ("builtin %qs only accepts 2 arguments", name);
	  return error_mark_node;
	}
      /* Ignore promote's element argument.  */
      if (fcode == ALTIVEC_BUILTIN_VEC_PROMOTE
	  && !INTEGRAL_TYPE_P (TREE_TYPE ((*arglist)[1])))
	goto bad;

      arg = (*arglist)[0];
      type = TREE_TYPE (arg);
      if (!SCALAR_FLOAT_TYPE_P (type)
	  && !INTEGRAL_TYPE_P (type))
	goto bad;
      unsigned_p = TYPE_UNSIGNED (type);
      switch (TYPE_MODE (type))
	{
	  case E_TImode:
	    type = (unsigned_p ? unsigned_V1TI_type_node : V1TI_type_node);
	    size = 1;
	    break;
	  case E_DImode:
	    type = (unsigned_p ? unsigned_V2DI_type_node : V2DI_type_node);
	    size = 2;
	    break;
	  case E_SImode:
	    type = (unsigned_p ? unsigned_V4SI_type_node : V4SI_type_node);
	    size = 4;
	    break;
	  case E_HImode:
	    type = (unsigned_p ? unsigned_V8HI_type_node : V8HI_type_node);
	    size = 8;
	    break;
	  case E_QImode:
	    type = (unsigned_p ? unsigned_V16QI_type_node : V16QI_type_node);
	    size = 16;
	    break;
	  case E_SFmode: type = V4SF_type_node; size = 4; break;
	  case E_DFmode: type = V2DF_type_node; size = 2; break;
	  default:
	    goto bad;
	}
      arg = save_expr (fold_convert (TREE_TYPE (type), arg));
      vec_alloc (vec, size);
      for(i = 0; i < size; i++)
	{
	  constructor_elt elt = {NULL_TREE, arg};
	  vec->quick_push (elt);
	}
	return build_constructor (type, vec);
    }

  /* For now use pointer tricks to do the extraction, unless we are on VSX
     extracting a double from a constant offset.  */
  if (fcode == ALTIVEC_BUILTIN_VEC_EXTRACT)
    {
      tree arg1;
      tree arg1_type;
      tree arg2;
      tree arg1_inner_type;
      tree decl, stmt;
      tree innerptrtype;
      machine_mode mode;

      /* No second argument. */
      if (nargs != 2)
	{
	  error ("builtin %qs only accepts 2 arguments", "vec_extract");
	  return error_mark_node;
	}

      arg2 = (*arglist)[1];
      arg1 = (*arglist)[0];
      arg1_type = TREE_TYPE (arg1);

      if (TREE_CODE (arg1_type) != VECTOR_TYPE)
	goto bad;
      if (!INTEGRAL_TYPE_P (TREE_TYPE (arg2)))
	goto bad;

      /* See if we can optimize vec_extracts with the current VSX instruction
	 set.  */
      mode = TYPE_MODE (arg1_type);
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
		default:
		  break;

		case E_V1TImode:
		  call = rs6000_builtin_decls[VSX_BUILTIN_VEC_EXT_V1TI];
		  break;

		case E_V2DFmode:
		  call = rs6000_builtin_decls[VSX_BUILTIN_VEC_EXT_V2DF];
		  break;

		case E_V2DImode:
		  call = rs6000_builtin_decls[VSX_BUILTIN_VEC_EXT_V2DI];
		  break;

		case E_V4SFmode:
		  call = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_EXT_V4SF];
		  break;

		case E_V4SImode:
		  if (TARGET_DIRECT_MOVE_64BIT)
		    call = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_EXT_V4SI];
		  break;

		case E_V8HImode:
		  if (TARGET_DIRECT_MOVE_64BIT)
		    call = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_EXT_V8HI];
		  break;

		case E_V16QImode:
		  if (TARGET_DIRECT_MOVE_64BIT)
		    call = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_EXT_V16QI];
		  break;
		}
	    }

	  /* If the second argument is variable, we can optimize it if we are
	     generating 64-bit code on a machine with direct move.  */
	  else if (TREE_CODE (arg2) != INTEGER_CST && TARGET_DIRECT_MOVE_64BIT)
	    {
	      switch (mode)
		{
		default:
		  break;

		case E_V2DFmode:
		  call = rs6000_builtin_decls[VSX_BUILTIN_VEC_EXT_V2DF];
		  break;

		case E_V2DImode:
		  call = rs6000_builtin_decls[VSX_BUILTIN_VEC_EXT_V2DI];
		  break;

		case E_V4SFmode:
		  call = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_EXT_V4SF];
		  break;

		case E_V4SImode:
		  call = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_EXT_V4SI];
		  break;

		case E_V8HImode:
		  call = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_EXT_V8HI];
		  break;

		case E_V16QImode:
		  call = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_EXT_V16QI];
		  break;
		}
	    }

	  if (call)
	    {
	      tree result = build_call_expr (call, 2, arg1, arg2);
	      /* Coerce the result to vector element type.  May be no-op.  */
	      arg1_inner_type = TREE_TYPE (arg1_type);
	      result = fold_convert (arg1_inner_type, result);
	      return result;
	    }
	}

      /* Build *(((arg1_inner_type*)&(vector type){arg1})+arg2). */
      arg1_inner_type = TREE_TYPE (arg1_type);
      arg2 = build_binary_op (loc, BIT_AND_EXPR, arg2,
			      build_int_cst (TREE_TYPE (arg2),
					     TYPE_VECTOR_SUBPARTS (arg1_type)
					     - 1), 0);
      decl = build_decl (loc, VAR_DECL, NULL_TREE, arg1_type);
      DECL_EXTERNAL (decl) = 0;
      TREE_PUBLIC (decl) = 0;
      DECL_CONTEXT (decl) = current_function_decl;
      TREE_USED (decl) = 1;
      TREE_TYPE (decl) = arg1_type;
      TREE_READONLY (decl) = TYPE_READONLY (arg1_type);
      if (c_dialect_cxx ())
	{
	  stmt = build4 (TARGET_EXPR, arg1_type, decl, arg1,
			 NULL_TREE, NULL_TREE);
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

      innerptrtype = build_pointer_type (arg1_inner_type);

      stmt = build_unary_op (loc, ADDR_EXPR, stmt, 0);
      stmt = convert (innerptrtype, stmt);
      stmt = build_binary_op (loc, PLUS_EXPR, stmt, arg2, 1);
      stmt = build_indirect_ref (loc, stmt, RO_NULL);

      /* PR83660: We mark this as having side effects so that
	 downstream in fold_build_cleanup_point_expr () it will get a
	 CLEANUP_POINT_EXPR.  If it does not we can run into an ICE
	 later in gimplify_cleanup_point_expr ().  Potentially this
	 causes missed optimization because the actually is no side
	 effect.  */
      if (c_dialect_cxx ())
	TREE_SIDE_EFFECTS (stmt) = 1;

      return stmt;
    }

  /* For now use pointer tricks to do the insertion, unless we are on VSX
     inserting a double to a constant offset..  */
  if (fcode == ALTIVEC_BUILTIN_VEC_INSERT)
    {
      tree arg0;
      tree arg1;
      tree arg2;
      tree arg1_type;
      tree arg1_inner_type;
      tree decl, stmt;
      tree innerptrtype;
      machine_mode mode;

      /* No second or third arguments. */
      if (nargs != 3)
	{
	  error ("builtin %qs only accepts 3 arguments", "vec_insert");
	  return error_mark_node;
	}

      arg0 = (*arglist)[0];
      arg1 = (*arglist)[1];
      arg1_type = TREE_TYPE (arg1);
      arg2 = fold_for_warn ((*arglist)[2]);

      if (TREE_CODE (arg1_type) != VECTOR_TYPE)
	goto bad;
      if (!INTEGRAL_TYPE_P (TREE_TYPE (arg2)))
	goto bad;

      /* If we can use the VSX xxpermdi instruction, use that for insert.  */
      mode = TYPE_MODE (arg1_type);
      if ((mode == V2DFmode || mode == V2DImode) && VECTOR_UNIT_VSX_P (mode)
	  && TREE_CODE (arg2) == INTEGER_CST)
	{
	  wide_int selector = wi::to_wide (arg2);
	  selector = wi::umod_trunc (selector, 2);
	  tree call = NULL_TREE;

	  arg2 = wide_int_to_tree (TREE_TYPE (arg2), selector);
	  if (mode == V2DFmode)
	    call = rs6000_builtin_decls[VSX_BUILTIN_VEC_SET_V2DF];
	  else if (mode == V2DImode)
	    call = rs6000_builtin_decls[VSX_BUILTIN_VEC_SET_V2DI];

	  /* Note, __builtin_vec_insert_<xxx> has vector and scalar types
	     reversed.  */
	  if (call)
	    return build_call_expr (call, 3, arg1, arg0, arg2);
	}
      else if (mode == V1TImode && VECTOR_UNIT_VSX_P (mode)
	       && TREE_CODE (arg2) == INTEGER_CST)
	{
	  tree call = rs6000_builtin_decls[VSX_BUILTIN_VEC_SET_V1TI];
	  wide_int selector = wi::zero(32);

	  arg2 = wide_int_to_tree (TREE_TYPE (arg2), selector);
	  /* Note, __builtin_vec_insert_<xxx> has vector and scalar types
	     reversed.  */
	  return build_call_expr (call, 3, arg1, arg0, arg2);
	}

      /* Build *(((arg1_inner_type*)&(vector type){arg1})+arg2) = arg0. */
      arg1_inner_type = TREE_TYPE (arg1_type);
      if (TYPE_VECTOR_SUBPARTS (arg1_type) == 1)
	arg2 = build_int_cst (TREE_TYPE (arg2), 0);
      else
	arg2 = build_binary_op (loc, BIT_AND_EXPR, arg2,
				build_int_cst (TREE_TYPE (arg2),
					       TYPE_VECTOR_SUBPARTS (arg1_type)
					       - 1), 0);
      decl = build_decl (loc, VAR_DECL, NULL_TREE, arg1_type);
      DECL_EXTERNAL (decl) = 0;
      TREE_PUBLIC (decl) = 0;
      DECL_CONTEXT (decl) = current_function_decl;
      TREE_USED (decl) = 1;
      TREE_TYPE (decl) = arg1_type;
      TREE_READONLY (decl) = TYPE_READONLY (arg1_type);
      if (c_dialect_cxx ())
	{
	  stmt = build4 (TARGET_EXPR, arg1_type, decl, arg1,
			 NULL_TREE, NULL_TREE);
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

      innerptrtype = build_pointer_type (arg1_inner_type);

      stmt = build_unary_op (loc, ADDR_EXPR, stmt, 0);
      stmt = convert (innerptrtype, stmt);
      stmt = build_binary_op (loc, PLUS_EXPR, stmt, arg2, 1);
      stmt = build_indirect_ref (loc, stmt, RO_NULL);
      stmt = build2 (MODIFY_EXPR, TREE_TYPE (stmt), stmt,
		     convert (TREE_TYPE (stmt), arg0));
      stmt = build2 (COMPOUND_EXPR, arg1_type, stmt, decl);
      return stmt;
    }

  for (n = 0;
       !VOID_TYPE_P (TREE_VALUE (fnargs)) && n < nargs;
       fnargs = TREE_CHAIN (fnargs), n++)
    {
      tree decl_type = TREE_VALUE (fnargs);
      tree arg = (*arglist)[n];
      tree type;

      if (arg == error_mark_node)
	return error_mark_node;

      if (n >= 3)
        abort ();

      arg = default_conversion (arg);

      /* The C++ front-end converts float * to const void * using
	 NOP_EXPR<const void *> (NOP_EXPR<void *> (x)).  */
      type = TREE_TYPE (arg);
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
	    warning (0, "passing argument %d of %qE discards qualifiers from "
		        "pointer target type", n + 1, fndecl);
	  type = build_pointer_type (build_qualified_type (TREE_TYPE (type),
							   0));
	  arg = fold_convert (type, arg);
	}

      /* For P9V_BUILTIN_VEC_LXVL, convert any const * to its non constant
	 equivalent to simplify the overload matching below.  */
      if (fcode == P9V_BUILTIN_VEC_LXVL)
	{
	  if (POINTER_TYPE_P (type)
	      && TYPE_READONLY (TREE_TYPE (type)))
	    {
	      type = build_pointer_type (build_qualified_type (
						TREE_TYPE (type),0));
	      arg = fold_convert (type, arg);
	    }
	}

      args[n] = arg;
      types[n] = type;
    }

  /* If the number of arguments did not match the prototype, return NULL
     and the generic code will issue the appropriate error message.  */
  if (!VOID_TYPE_P (TREE_VALUE (fnargs)) || n < nargs)
    return NULL;

  if (n == 0)
    abort ();

  if (fcode == ALTIVEC_BUILTIN_VEC_STEP)
    {
      if (TREE_CODE (types[0]) != VECTOR_TYPE)
	goto bad;

      return build_int_cst (NULL_TREE, TYPE_VECTOR_SUBPARTS (types[0]));
    }

  {
    bool unsupported_builtin = false;
    enum rs6000_builtins overloaded_code;
    tree result = NULL;
    for (desc = altivec_overloaded_builtins;
	 desc->code && desc->code != fcode; desc++)
      continue;

    /* Need to special case __builtin_cmp because the overloaded forms
       of this function take (unsigned int, unsigned int) or (unsigned
       long long int, unsigned long long int).  Since C conventions
       allow the respective argument types to be implicitly coerced into
       each other, the default handling does not provide adequate
       discrimination between the desired forms of the function.  */
    if (fcode == P6_OV_BUILTIN_CMPB)
      {
	machine_mode arg1_mode = TYPE_MODE (types[0]);
	machine_mode arg2_mode = TYPE_MODE (types[1]);

	if (nargs != 2)
	  {
	    error ("builtin %qs only accepts 2 arguments", "__builtin_cmpb");
	    return error_mark_node;
	  }

	/* If any supplied arguments are wider than 32 bits, resolve to
	   64-bit variant of built-in function.  */
	if ((GET_MODE_PRECISION (arg1_mode) > 32)
	    || (GET_MODE_PRECISION (arg2_mode) > 32))
	  {
	    /* Assure all argument and result types are compatible with
	       the built-in function represented by P6_BUILTIN_CMPB.  */
	    overloaded_code = P6_BUILTIN_CMPB;
	  }
	else
	  {
	    /* Assure all argument and result types are compatible with
	       the built-in function represented by P6_BUILTIN_CMPB_32.  */
	    overloaded_code = P6_BUILTIN_CMPB_32;
	  }

	while (desc->code && desc->code == fcode
	       && desc->overloaded_code != overloaded_code)
	  desc++;

	if (desc->code && (desc->code == fcode)
	    && rs6000_builtin_type_compatible (types[0], desc->op1)
	    && rs6000_builtin_type_compatible (types[1], desc->op2))
	  {
	    if (rs6000_builtin_decls[desc->overloaded_code] != NULL_TREE)
	      {
		result = altivec_build_resolved_builtin (args, n, desc);
		/* overloaded_code is set above */
		if (!rs6000_builtin_is_supported_p (overloaded_code))
		  unsupported_builtin = true;
		else
		  return result;
	      }
	    else
	      unsupported_builtin = true;
	  }
      }
    else if (fcode == P9V_BUILTIN_VEC_VSIEDP)
      {
	machine_mode arg1_mode = TYPE_MODE (types[0]);

	if (nargs != 2)
	  {
	    error ("builtin %qs only accepts 2 arguments",
		   "scalar_insert_exp");
	    return error_mark_node;
	  }

	/* If supplied first argument is wider than 64 bits, resolve to
	   128-bit variant of built-in function.  */
	if (GET_MODE_PRECISION (arg1_mode) > 64)
	  {
	    /* If first argument is of float variety, choose variant
	       that expects __ieee128 argument.  Otherwise, expect
	       __int128 argument.  */
	    if (GET_MODE_CLASS (arg1_mode) == MODE_FLOAT)
	      overloaded_code = P9V_BUILTIN_VSIEQPF;
	    else
	      overloaded_code = P9V_BUILTIN_VSIEQP;
	  }
	else
	  {
	    /* If first argument is of float variety, choose variant
	       that expects double argument.  Otherwise, expect
	       long long int argument.  */
	    if (GET_MODE_CLASS (arg1_mode) == MODE_FLOAT)
	      overloaded_code = P9V_BUILTIN_VSIEDPF;
	    else
	      overloaded_code = P9V_BUILTIN_VSIEDP;
	  }
	while (desc->code && desc->code == fcode
	       && desc->overloaded_code != overloaded_code)
	  desc++;

	if (desc->code && (desc->code == fcode)
	    && rs6000_builtin_type_compatible (types[0], desc->op1)
	    && rs6000_builtin_type_compatible (types[1], desc->op2))
	  {
	    if (rs6000_builtin_decls[desc->overloaded_code] != NULL_TREE)
	      {
		result = altivec_build_resolved_builtin (args, n, desc);
		/* overloaded_code is set above.  */
		if (!rs6000_builtin_is_supported_p (overloaded_code))
		  unsupported_builtin = true;
		else
		  return result;
	      }
	    else
	      unsupported_builtin = true;
	  }
      }
    else
      {
	/* For arguments after the last, we have RS6000_BTI_NOT_OPAQUE in
	   the opX fields.  */
	for (; desc->code == fcode; desc++)
	  {
	    if ((desc->op1 == RS6000_BTI_NOT_OPAQUE
		 || rs6000_builtin_type_compatible (types[0], desc->op1))
		&& (desc->op2 == RS6000_BTI_NOT_OPAQUE
		    || rs6000_builtin_type_compatible (types[1], desc->op2))
		&& (desc->op3 == RS6000_BTI_NOT_OPAQUE
		    || rs6000_builtin_type_compatible (types[2], desc->op3)))
	      {
		if (rs6000_builtin_decls[desc->overloaded_code] != NULL_TREE)
		  {
		    result = altivec_build_resolved_builtin (args, n, desc);
		    if (!rs6000_builtin_is_supported_p (desc->overloaded_code))
		      {
			/* Allow loop to continue in case a different
			   definition is supported.  */
			overloaded_code = desc->overloaded_code;
			unsupported_builtin = true;
		      }
		    else
		      return result;
		  }
		else
		  unsupported_builtin = true;
	      }
	  }
      }

    if (unsupported_builtin)
      {
	const char *name = rs6000_overloaded_builtin_name (fcode);
	if (result != NULL)
	  {
	    const char *internal_name
	      = rs6000_overloaded_builtin_name (overloaded_code);
	    /* An error message making reference to the name of the
	       non-overloaded function has already been issued.  Add
	       clarification of the previous message.  */
	    rich_location richloc (line_table, input_location);
	    inform (&richloc, "builtin %qs requires builtin %qs",
		    name, internal_name);
	  }
	else
	  error ("%qs is not supported in this compiler configuration", name);
	/* If an error-representing  result tree was returned from
	   altivec_build_resolved_builtin above, use it.  */
	return (result != NULL) ? result : error_mark_node;
      }
  }
 bad:
  {
    const char *name = rs6000_overloaded_builtin_name (fcode);
    error ("invalid parameter combination for AltiVec intrinsic %qs", name);
    return error_mark_node;
  }
}
