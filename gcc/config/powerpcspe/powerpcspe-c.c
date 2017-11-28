/* Subroutines for the C front end on the PowerPC architecture.
   Copyright (C) 2002-2017 Free Software Foundation, Inc.

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

#define SYNTAX_ERROR(gmsgid) do {					\
  warning (OPT_Wpragmas, gmsgid);					\
  warning (OPT_Wpragmas, "ignoring malformed #pragma longcall");	\
  return;								\
} while (0)

void
rs6000_pragma_longcall (cpp_reader *pfile ATTRIBUTE_UNUSED)
{
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
    warning (OPT_Wpragmas, "junk at end of #pragma longcall");

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
	  enum node_type itype = ident->type;
	  /* If there is a function-like macro, check if it is going to be
	     invoked with or without arguments.  Without following ( treat
	     it like non-macro, otherwise the following cpp_get_token eats
	     what should be preserved.  */
	  if (itype == NT_MACRO && cpp_fun_like_macro_p (ident))
	    {
	      int idx2 = idx;
	      do
		tok = cpp_peek_token (pfile, idx2++);
	      while (tok->type == CPP_PADDING);
	      if (tok->type != CPP_OPEN_PAREN)
		itype = NT_VOID;
	    }
	  if (itype == NT_MACRO)
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
	compile option, or as the the target of a --with-cpu=target
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
  if ((flags & OPTION_MASK_MFPGPR) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PWR6X");
  if ((flags & OPTION_MASK_POPCNTD) != 0)
    rs6000_define_or_undefine_macro (define_p, "_ARCH_PWR7");
  /* Note that the OPTION_MASK_DIRECT_MOVE flag is automatically
     turned on in the following condition:
     1. TARGET_P9_DFORM_SCALAR or TARGET_P9_DFORM_VECTOR are enabled
        and OPTION_MASK_DIRECT_MOVE is not explicitly disabled.
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
     1. The command line specifies either -maltivec=le or -maltivec=be.
     2. The operating system is Darwin and it is configured for 64
	bit.  (See darwin_rs6000_override_options.)
     3. The operating system is Darwin and the operating system
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
     2. If any of the options TARGET_HARD_FLOAT, TARGET_FPRS,
	TARGET_SINGLE_FLOAT, or TARGET_DOUBLE_FLOAT are turned off.
	Hereafter, the OPTION_MASK_VSX flag is considered to have been
	turned off explicitly.
     3. If TARGET_PAIRED_FLOAT was enabled.  Hereafter, the
	OPTION_MASK_VSX flag is considered to have been turned off
	explicitly.
     4. If TARGET_AVOID_XFORM is turned on explicitly at the outermost
	compilation context, or if it is turned on by any means in an
	inner compilation context.  Hereafter, the OPTION_MASK_VSX
	flag is considered to have been turned off explicitly.
     5. If TARGET_ALTIVEC was explicitly disabled.  Hereafter, the
	OPTION_MASK_VSX flag is considered to have been turned off
	explicitly.
     6. If an inner context (as introduced by
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
     1. If TARGET_P9_DFORM_SCALAR or TARGET_P9_DFORM_VECTOR and
        OPTION_MASK_P9_VECTOR was not turned off explicitly.
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
  /* Note that the OPTION_MASK_UPPER_REGS_DF flag is automatically
     turned on in the following conditions:
     1. If TARGET_UPPER_REGS is explicitly turned on and
	TARGET_VSX is turned on and OPTION_MASK_UPPER_REGS_DF is not
	explicitly turned off.  Hereafter, the
	OPTION_MASK_UPPER_REGS_DF flag is considered to have been
	explicitly set.
     Note that the OPTION_MASK_UPPER_REGS_DF flag is automatically
     turned off in the following conditions:
     1. If TARGET_UPPER_REGS is explicitly turned off and TARGET_VSX
	is turned on and OPTION_MASK_UPPER_REGS_DF is not explicitly
	turned on.  Hereafter, the OPTION_MASK_UPPER_REGS_DF flag is
	considered to have been explicitly cleared.
     2. If TARGET_UPPER_REGS_DF is turned on but TARGET_VSX is turned
	off.  */
  if ((flags & OPTION_MASK_UPPER_REGS_DF) != 0)
    rs6000_define_or_undefine_macro (define_p, "__UPPER_REGS_DF__");
  /* Note that the OPTION_MASK_UPPER_REGS_SF flag is automatically
     turned on in the following conditions:
     1. If TARGET_UPPER_REGS is explicitly turned on and
	TARGET_P8_VECTOR is on and OPTION_MASK_UPPER_REGS_SF is not
	turned off explicitly.  Hereafter, the
	OPTION_MASK_UPPER_REGS_SF flag is considered to have been
	explicitly set.
     Note that the OPTION_MASK_UPPER_REGS_SF flag is automatically
     turned off in the following conditions:
     1. If TARGET_UPPER_REGS is explicitly turned off and
	TARGET_P8_VECTOR is on and OPTION_MASK_UPPER_REGS_SF is not
	turned off explicitly.  Hereafter, the
	OPTION_MASK_UPPER_REGS_SF flag is considered to have been
	explicitly cleared.
     2. If TARGET_P8_VECTOR is off.  */
  if ((flags & OPTION_MASK_UPPER_REGS_SF) != 0)
    rs6000_define_or_undefine_macro (define_p, "__UPPER_REGS_SF__");

  /* options from the builtin masks.  */
  /* Note that RS6000_BTM_SPE is enabled only if TARGET_SPE
     (e.g. -mspe).  */
  if ((bu_mask & RS6000_BTM_SPE) != 0)
    rs6000_define_or_undefine_macro (define_p, "__SPE__");
  /* Note that RS6000_BTM_PAIRED is enabled only if
     TARGET_PAIRED_FLOAT is enabled (e.g. -mpaired).  */
  if ((bu_mask & RS6000_BTM_PAIRED) != 0)
    rs6000_define_or_undefine_macro (define_p, "__PAIRED__");
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
  if (TARGET_FLOAT128_KEYWORD)
    builtin_define ("__FLOAT128__");
  if (TARGET_FLOAT128_TYPE)
    builtin_define ("__FLOAT128_TYPE__");
  if (TARGET_FLOAT128_HW)
    builtin_define ("__FLOAT128_HARDWARE__");
  if (TARGET_LONG_DOUBLE_128 && FLOAT128_IBM_P (TFmode))
    builtin_define ("__ibm128=long double");

  /* We needed to create a keyword if -mfloat128-type was used but not -mfloat,
     so we used __ieee128.  If -mfloat128 was used, create a #define back to
     the real keyword in case somebody used it.  */
  if (TARGET_FLOAT128_KEYWORD)
    builtin_define ("__ieee128=__float128");

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
  if ((!(TARGET_HARD_FLOAT && (TARGET_FPRS || TARGET_E500_DOUBLE)))
      ||(TARGET_HARD_FLOAT && TARGET_FPRS && !TARGET_DOUBLE_FLOAT))
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

  /* Tell users they can use __builtin_bswap{16,64}.  */
  builtin_define ("__HAVE_BSWAP__");

  /* May be overridden by target configuration.  */
  RS6000_CPU_CPP_ENDIAN_BUILTINS();

  if (TARGET_LONG_DOUBLE_128)
    {
      builtin_define ("__LONG_DOUBLE_128__");
      builtin_define ("__LONGDOUBLE128");

      if (TARGET_IEEEQUAD)
	builtin_define ("__LONG_DOUBLE_IEEE128__");
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
  if (BYTES_BIG_ENDIAN || (rs6000_altivec_element_order == 2))
    builtin_define ("__VEC_ELEMENT_REG_ORDER__=__ORDER_BIG_ENDIAN__");
  else
    builtin_define ("__VEC_ELEMENT_REG_ORDER__=__ORDER_LITTLE_ENDIAN__");

  /* Let the compiled code know if 'f' class registers will not be available.  */
  if (TARGET_SOFT_FLOAT || !TARGET_FPRS)
    builtin_define ("__NO_FPRS__");

  /* Whether aggregates passed by value are aligned to a 16 byte boundary
     if their alignment is 16 bytes or larger.  */
  if ((TARGET_MACHO && rs6000_darwin64_abi)
      || DEFAULT_ABI == ABI_ELFv2
      || (DEFAULT_ABI == ABI_AIX && !rs6000_compat_align_parm))
    builtin_define ("__STRUCT_PARM_ALIGN__=16");

  /* Generate defines for Xilinx FPU. */
  if (rs6000_xilinx_fpu) 
    {
      builtin_define ("_XFPU");
      if (rs6000_single_float && ! rs6000_double_float)
	{
	  if (rs6000_simple_fpu) 
	    builtin_define ("_XFPU_SP_LITE"); 
	  else 
	    builtin_define ("_XFPU_SP_FULL");
	}
      if (rs6000_double_float)
	{
	  if (rs6000_simple_fpu) 
	    builtin_define ("_XFPU_DP_LITE");
	  else
	    builtin_define ("_XFPU_DP_FULL");
        }
    }
}


struct altivec_builtin_types
{
  enum rs6000_builtins code;
  enum rs6000_builtins overloaded_code;
  signed char ret_type;
  signed char op1;
  signed char op2;
  signed char op3;
};

const struct altivec_builtin_types altivec_overloaded_builtins[] = {
  /* Unary AltiVec/VSX builtins.  */
  { ALTIVEC_BUILTIN_VEC_ABS, ALTIVEC_BUILTIN_ABS_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_ABS, ALTIVEC_BUILTIN_ABS_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_ABS, ALTIVEC_BUILTIN_ABS_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_ABS, P8V_BUILTIN_ABS_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_ABS, ALTIVEC_BUILTIN_ABS_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_ABS, VSX_BUILTIN_XVABSDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_ABSS, ALTIVEC_BUILTIN_ABSS_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_ABSS, ALTIVEC_BUILTIN_ABSS_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_ABSS, ALTIVEC_BUILTIN_ABSS_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_CEIL, ALTIVEC_BUILTIN_VRFIP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_CEIL, VSX_BUILTIN_XVRDPIP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_EXPTE, ALTIVEC_BUILTIN_VEXPTEFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_FLOOR, VSX_BUILTIN_XVRDPIM,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_FLOOR, ALTIVEC_BUILTIN_VRFIM,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_LOGE, ALTIVEC_BUILTIN_VLOGEFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_MTVSCR, ALTIVEC_BUILTIN_MTVSCR,
    RS6000_BTI_void, RS6000_BTI_V4SI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_MTVSCR, ALTIVEC_BUILTIN_MTVSCR,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_MTVSCR, ALTIVEC_BUILTIN_MTVSCR,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_MTVSCR, ALTIVEC_BUILTIN_MTVSCR,
    RS6000_BTI_void, RS6000_BTI_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_MTVSCR, ALTIVEC_BUILTIN_MTVSCR,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_MTVSCR, ALTIVEC_BUILTIN_MTVSCR,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_MTVSCR, ALTIVEC_BUILTIN_MTVSCR,
    RS6000_BTI_void, RS6000_BTI_pixel_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_MTVSCR, ALTIVEC_BUILTIN_MTVSCR,
    RS6000_BTI_void, RS6000_BTI_V16QI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_MTVSCR, ALTIVEC_BUILTIN_MTVSCR,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_MTVSCR, ALTIVEC_BUILTIN_MTVSCR,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_RE, ALTIVEC_BUILTIN_VREFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_RE, VSX_BUILTIN_XVREDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_ROUND, ALTIVEC_BUILTIN_VRFIN,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_ROUND, VSX_BUILTIN_XVRDPI,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_RECIP, ALTIVEC_BUILTIN_VRECIPFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_RECIP, VSX_BUILTIN_RECIP_V2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_RSQRT, ALTIVEC_BUILTIN_VRSQRTFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_RSQRT, VSX_BUILTIN_RSQRT_2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_RSQRTE, ALTIVEC_BUILTIN_VRSQRTEFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_RSQRTE, VSX_BUILTIN_XVRSQRTEDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_TRUNC, ALTIVEC_BUILTIN_VRFIZ,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_TRUNC, VSX_BUILTIN_XVRDPIZ,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_UNPACKH, ALTIVEC_BUILTIN_VUPKHSB,
    RS6000_BTI_V8HI, RS6000_BTI_V16QI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_UNPACKH, ALTIVEC_BUILTIN_VUPKHSB,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V16QI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_UNPACKH, ALTIVEC_BUILTIN_VUPKHSH,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_UNPACKH, ALTIVEC_BUILTIN_VUPKHSH,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_UNPACKH, P8V_BUILTIN_VUPKHSW,
    RS6000_BTI_V2DI, RS6000_BTI_V4SI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_UNPACKH, P8V_BUILTIN_VUPKHSW,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V4SI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_UNPACKH, ALTIVEC_BUILTIN_VUPKHPX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_pixel_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKHSH, ALTIVEC_BUILTIN_VUPKHSH,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKHSH, ALTIVEC_BUILTIN_VUPKHSH,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKHSH, P8V_BUILTIN_VUPKHSW,
    RS6000_BTI_V2DI, RS6000_BTI_V4SI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKHSH, P8V_BUILTIN_VUPKHSW,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V4SI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKHPX, ALTIVEC_BUILTIN_VUPKHPX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKHPX, ALTIVEC_BUILTIN_VUPKHPX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_pixel_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKHSB, ALTIVEC_BUILTIN_VUPKHSB,
    RS6000_BTI_V8HI, RS6000_BTI_V16QI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKHSB, ALTIVEC_BUILTIN_VUPKHSB,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V16QI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_UNPACKL, ALTIVEC_BUILTIN_VUPKLSB,
    RS6000_BTI_V8HI, RS6000_BTI_V16QI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_UNPACKL, ALTIVEC_BUILTIN_VUPKLSB,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V16QI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_UNPACKL, ALTIVEC_BUILTIN_VUPKLPX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_pixel_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_UNPACKL, ALTIVEC_BUILTIN_VUPKLSH,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_UNPACKL, ALTIVEC_BUILTIN_VUPKLSH,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_UNPACKL, P8V_BUILTIN_VUPKLSW,
    RS6000_BTI_V2DI, RS6000_BTI_V4SI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_UNPACKL, P8V_BUILTIN_VUPKLSW,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V4SI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKLPX, ALTIVEC_BUILTIN_VUPKLPX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKLPX, ALTIVEC_BUILTIN_VUPKLPX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_pixel_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKLSH, ALTIVEC_BUILTIN_VUPKLSH,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKLSH, ALTIVEC_BUILTIN_VUPKLSH,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKLSB, ALTIVEC_BUILTIN_VUPKLSB,
    RS6000_BTI_V8HI, RS6000_BTI_V16QI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKLSB, ALTIVEC_BUILTIN_VUPKLSB,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V16QI, 0, 0 },

  /* Binary AltiVec/VSX builtins.  */
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUBM,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUBM,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUBM,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUHM,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUHM,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUHM,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUWM,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUWM,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUWM,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, P8V_BUILTIN_VADDUDM,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, P8V_BUILTIN_VADDUDM,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, P8V_BUILTIN_VADDUDM,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, P8V_BUILTIN_VADDUDM,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, P8V_BUILTIN_VADDUDM,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, P8V_BUILTIN_VADDUDM,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, VSX_BUILTIN_XVADDDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, P8V_BUILTIN_VADDUQM,
    RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, P8V_BUILTIN_VADDUQM,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI,
    RS6000_BTI_unsigned_V1TI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDFP, ALTIVEC_BUILTIN_VADDFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUWM, ALTIVEC_BUILTIN_VADDUWM,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUWM, ALTIVEC_BUILTIN_VADDUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUWM, ALTIVEC_BUILTIN_VADDUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUWM, ALTIVEC_BUILTIN_VADDUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUWM, ALTIVEC_BUILTIN_VADDUWM,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUWM, ALTIVEC_BUILTIN_VADDUWM,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUWM, ALTIVEC_BUILTIN_VADDUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUWM, ALTIVEC_BUILTIN_VADDUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUHM, ALTIVEC_BUILTIN_VADDUHM,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUHM, ALTIVEC_BUILTIN_VADDUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUHM, ALTIVEC_BUILTIN_VADDUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUHM, ALTIVEC_BUILTIN_VADDUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUHM, ALTIVEC_BUILTIN_VADDUHM,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUHM, ALTIVEC_BUILTIN_VADDUHM,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUHM, ALTIVEC_BUILTIN_VADDUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUHM, ALTIVEC_BUILTIN_VADDUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUBM, ALTIVEC_BUILTIN_VADDUBM,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUBM, ALTIVEC_BUILTIN_VADDUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUBM, ALTIVEC_BUILTIN_VADDUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUBM, ALTIVEC_BUILTIN_VADDUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUBM, ALTIVEC_BUILTIN_VADDUBM,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUBM, ALTIVEC_BUILTIN_VADDUBM,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUBM, ALTIVEC_BUILTIN_VADDUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUBM, ALTIVEC_BUILTIN_VADDUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDC, ALTIVEC_BUILTIN_VADDCUW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDC, ALTIVEC_BUILTIN_VADDCUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDC, P8V_BUILTIN_VADDCUQ,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI,
    RS6000_BTI_unsigned_V1TI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDC, P8V_BUILTIN_VADDCUQ,
    RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDEC, P8V_BUILTIN_VADDECUQ,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI },
  { ALTIVEC_BUILTIN_VEC_ADDEC, P8V_BUILTIN_VADDECUQ,
    RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDSBS,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDSBS,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDSBS,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDSHS,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDSHS,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDSHS,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDSWS,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDSWS,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ADDS, ALTIVEC_BUILTIN_VADDSWS,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDSWS, ALTIVEC_BUILTIN_VADDSWS,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDSWS, ALTIVEC_BUILTIN_VADDSWS,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDSWS, ALTIVEC_BUILTIN_VADDSWS,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUWS, ALTIVEC_BUILTIN_VADDUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUWS, ALTIVEC_BUILTIN_VADDUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUWS, ALTIVEC_BUILTIN_VADDUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUWS, ALTIVEC_BUILTIN_VADDUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUWS, ALTIVEC_BUILTIN_VADDUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDSHS, ALTIVEC_BUILTIN_VADDSHS,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDSHS, ALTIVEC_BUILTIN_VADDSHS,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDSHS, ALTIVEC_BUILTIN_VADDSHS,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUHS, ALTIVEC_BUILTIN_VADDUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUHS, ALTIVEC_BUILTIN_VADDUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUHS, ALTIVEC_BUILTIN_VADDUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUHS, ALTIVEC_BUILTIN_VADDUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUHS, ALTIVEC_BUILTIN_VADDUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDSBS, ALTIVEC_BUILTIN_VADDSBS,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDSBS, ALTIVEC_BUILTIN_VADDSBS,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDSBS, ALTIVEC_BUILTIN_VADDSBS,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUBS, ALTIVEC_BUILTIN_VADDUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUBS, ALTIVEC_BUILTIN_VADDUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUBS, ALTIVEC_BUILTIN_VADDUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUBS, ALTIVEC_BUILTIN_VADDUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VADDUBS, ALTIVEC_BUILTIN_VADDUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V4SF, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V2DF, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_AND, ALTIVEC_BUILTIN_VAND,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V4SF, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V2DF, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_ANDC, ALTIVEC_BUILTIN_VANDC,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_AVG, ALTIVEC_BUILTIN_VAVGUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_AVG, ALTIVEC_BUILTIN_VAVGSB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_AVG, ALTIVEC_BUILTIN_VAVGUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_AVG, ALTIVEC_BUILTIN_VAVGSH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_AVG, ALTIVEC_BUILTIN_VAVGUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_AVG, ALTIVEC_BUILTIN_VAVGSW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VAVGSW, ALTIVEC_BUILTIN_VAVGSW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VAVGUW, ALTIVEC_BUILTIN_VAVGUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VAVGSH, ALTIVEC_BUILTIN_VAVGSH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VAVGUH, ALTIVEC_BUILTIN_VAVGUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VAVGSB, ALTIVEC_BUILTIN_VAVGSB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VAVGUB, ALTIVEC_BUILTIN_VAVGUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPB, ALTIVEC_BUILTIN_VCMPBFP,
    RS6000_BTI_V4SI, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, ALTIVEC_BUILTIN_VCMPEQUB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, ALTIVEC_BUILTIN_VCMPEQUB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, ALTIVEC_BUILTIN_VCMPEQUB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, ALTIVEC_BUILTIN_VCMPEQUH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, ALTIVEC_BUILTIN_VCMPEQUH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, ALTIVEC_BUILTIN_VCMPEQUH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, ALTIVEC_BUILTIN_VCMPEQUW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, ALTIVEC_BUILTIN_VCMPEQUW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, ALTIVEC_BUILTIN_VCMPEQUW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, P8V_BUILTIN_VCMPEQUD,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, P8V_BUILTIN_VCMPEQUD,
    RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, P8V_BUILTIN_VCMPEQUD,
    RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, ALTIVEC_BUILTIN_VCMPEQFP,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, VSX_BUILTIN_XVCMPEQDP,
    RS6000_BTI_bool_V2DI, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPEQFP, ALTIVEC_BUILTIN_VCMPEQFP,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },

  { ALTIVEC_BUILTIN_VEC_VCMPEQUW, ALTIVEC_BUILTIN_VCMPEQUW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPEQUW, ALTIVEC_BUILTIN_VCMPEQUW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },

  { ALTIVEC_BUILTIN_VEC_VCMPEQUH, ALTIVEC_BUILTIN_VCMPEQUH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPEQUH, ALTIVEC_BUILTIN_VCMPEQUH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },

  { ALTIVEC_BUILTIN_VEC_VCMPEQUB, ALTIVEC_BUILTIN_VCMPEQUB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPEQUB, ALTIVEC_BUILTIN_VCMPEQUB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },

  { ALTIVEC_BUILTIN_VEC_CMPGE, ALTIVEC_BUILTIN_VCMPGEFP,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPGE, VSX_BUILTIN_XVCMPGEDP,
    RS6000_BTI_bool_V2DI, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPGE, VSX_BUILTIN_CMPGE_16QI,
    RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPGE, VSX_BUILTIN_CMPGE_U16QI,
    RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPGE, VSX_BUILTIN_CMPGE_8HI,
    RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPGE, VSX_BUILTIN_CMPGE_U8HI,
    RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPGE, VSX_BUILTIN_CMPGE_4SI,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPGE, VSX_BUILTIN_CMPGE_U4SI,
    RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPGE, VSX_BUILTIN_CMPGE_2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPGE, VSX_BUILTIN_CMPGE_U2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V2DI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPGT, ALTIVEC_BUILTIN_VCMPGTUB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPGT, ALTIVEC_BUILTIN_VCMPGTSB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPGT, ALTIVEC_BUILTIN_VCMPGTUH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPGT, ALTIVEC_BUILTIN_VCMPGTSH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPGT, ALTIVEC_BUILTIN_VCMPGTUW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPGT, ALTIVEC_BUILTIN_VCMPGTSW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPGT, P8V_BUILTIN_VCMPGTUD,
    RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPGT, P8V_BUILTIN_VCMPGTSD,
    RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPGT, ALTIVEC_BUILTIN_VCMPGTFP,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPGT, VSX_BUILTIN_XVCMPGTDP,
    RS6000_BTI_bool_V2DI, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPGTFP, ALTIVEC_BUILTIN_VCMPGTFP,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPGTSW, ALTIVEC_BUILTIN_VCMPGTSW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPGTSW, ALTIVEC_BUILTIN_VCMPGTSW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPGTUW, ALTIVEC_BUILTIN_VCMPGTUW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPGTUW, ALTIVEC_BUILTIN_VCMPGTUW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPGTSH, ALTIVEC_BUILTIN_VCMPGTSH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPGTSH, ALTIVEC_BUILTIN_VCMPGTSH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPGTUH, ALTIVEC_BUILTIN_VCMPGTUH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPGTUH, ALTIVEC_BUILTIN_VCMPGTUH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPGTSB, ALTIVEC_BUILTIN_VCMPGTSB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPGTSB, ALTIVEC_BUILTIN_VCMPGTSB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPGTUB, ALTIVEC_BUILTIN_VCMPGTUB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCMPGTUB, ALTIVEC_BUILTIN_VCMPGTUB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPLE, ALTIVEC_BUILTIN_VCMPGEFP,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPLE, VSX_BUILTIN_XVCMPGEDP,
    RS6000_BTI_bool_V2DI, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPLE, VSX_BUILTIN_CMPLE_16QI,
    RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPLE, VSX_BUILTIN_CMPLE_U16QI,
    RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPLE, VSX_BUILTIN_CMPLE_8HI,
    RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPLE, VSX_BUILTIN_CMPLE_U8HI,
    RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPLE, VSX_BUILTIN_CMPLE_4SI,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPLE, VSX_BUILTIN_CMPLE_U4SI,
    RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPLE, VSX_BUILTIN_CMPLE_2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPLE, VSX_BUILTIN_CMPLE_U2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V2DI, 0},
  { ALTIVEC_BUILTIN_VEC_CMPLT, ALTIVEC_BUILTIN_VCMPGTUB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPLT, ALTIVEC_BUILTIN_VCMPGTSB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPLT, ALTIVEC_BUILTIN_VCMPGTUH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPLT, ALTIVEC_BUILTIN_VCMPGTSH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPLT, ALTIVEC_BUILTIN_VCMPGTUW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPLT, ALTIVEC_BUILTIN_VCMPGTSW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPLT, P8V_BUILTIN_VCMPGTUD,
    RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPLT, P8V_BUILTIN_VCMPGTSD,
    RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPLT, ALTIVEC_BUILTIN_VCMPGTFP,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPLT, VSX_BUILTIN_XVCMPGTDP,
    RS6000_BTI_bool_V2DI, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_COPYSIGN, VSX_BUILTIN_CPSGNDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_COPYSIGN, ALTIVEC_BUILTIN_COPYSIGN_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_CTF, ALTIVEC_BUILTIN_VCFUX,
    RS6000_BTI_V4SF, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_CTF, ALTIVEC_BUILTIN_VCFSX,
    RS6000_BTI_V4SF, RS6000_BTI_V4SI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_CTF, VSX_BUILTIN_XVCVSXDDP_SCALE,
    RS6000_BTI_V2DF, RS6000_BTI_V2DI, RS6000_BTI_INTSI, 0},
  { ALTIVEC_BUILTIN_VEC_CTF, VSX_BUILTIN_XVCVUXDDP_SCALE,
    RS6000_BTI_V2DF, RS6000_BTI_unsigned_V2DI, RS6000_BTI_INTSI, 0},
  { ALTIVEC_BUILTIN_VEC_VCFSX, ALTIVEC_BUILTIN_VCFSX,
    RS6000_BTI_V4SF, RS6000_BTI_V4SI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCFUX, ALTIVEC_BUILTIN_VCFUX,
    RS6000_BTI_V4SF, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_CTS, ALTIVEC_BUILTIN_VCTSXS,
    RS6000_BTI_V4SI, RS6000_BTI_V4SF, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_CTS, VSX_BUILTIN_XVCVDPSXDS_SCALE,
    RS6000_BTI_V2DI, RS6000_BTI_V2DF, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_CTU, ALTIVEC_BUILTIN_VCTUXS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SF, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_CTU, VSX_BUILTIN_XVCVDPUXDS_SCALE,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_V2DF, RS6000_BTI_INTSI, 0 },
  { VSX_BUILTIN_VEC_DIV, VSX_BUILTIN_XVDIVSP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { VSX_BUILTIN_VEC_DIV, VSX_BUILTIN_XVDIVDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { VSX_BUILTIN_VEC_DIV, VSX_BUILTIN_DIV_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { VSX_BUILTIN_VEC_DIV, VSX_BUILTIN_UDIV_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { VSX_BUILTIN_VEC_DOUBLE, VSX_BUILTIN_XVCVSXDDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DI, 0, 0 },
  { VSX_BUILTIN_VEC_DOUBLE, VSX_BUILTIN_XVCVUXDDP,
    RS6000_BTI_V2DF, RS6000_BTI_unsigned_V2DI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V2DF,
    RS6000_BTI_V2DF, RS6000_BTI_INTSI, ~RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_INTSI, ~RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V4SI,
    RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V8HI,
    RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V8HI,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V16QI,
    RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDE, ALTIVEC_BUILTIN_LVEBX,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDE, ALTIVEC_BUILTIN_LVEBX,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDE, ALTIVEC_BUILTIN_LVEHX,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDE, ALTIVEC_BUILTIN_LVEHX,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDE, ALTIVEC_BUILTIN_LVEWX,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float, 0 },
  { ALTIVEC_BUILTIN_VEC_LDE, ALTIVEC_BUILTIN_LVEWX,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDE, ALTIVEC_BUILTIN_LVEWX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDE, ALTIVEC_BUILTIN_LVEWX,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LDE, ALTIVEC_BUILTIN_LVEWX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LVEWX, ALTIVEC_BUILTIN_LVEWX,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float, 0 },
  { ALTIVEC_BUILTIN_VEC_LVEWX, ALTIVEC_BUILTIN_LVEWX,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVEWX, ALTIVEC_BUILTIN_LVEWX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVEWX, ALTIVEC_BUILTIN_LVEWX,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LVEWX, ALTIVEC_BUILTIN_LVEWX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LVEHX, ALTIVEC_BUILTIN_LVEHX,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVEHX, ALTIVEC_BUILTIN_LVEHX,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVEBX, ALTIVEC_BUILTIN_LVEBX,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVEBX, ALTIVEC_BUILTIN_LVEBX,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V4SI,
    RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V8HI,
    RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V8HI,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V16QI,
    RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V2DF,
    RS6000_BTI_V2DF, RS6000_BTI_INTSI, ~RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_INTSI, ~RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL_V2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSL, ALTIVEC_BUILTIN_LVSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSL, ALTIVEC_BUILTIN_LVSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSL, ALTIVEC_BUILTIN_LVSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSL, ALTIVEC_BUILTIN_LVSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSL, ALTIVEC_BUILTIN_LVSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSL, ALTIVEC_BUILTIN_LVSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSL, ALTIVEC_BUILTIN_LVSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSL, ALTIVEC_BUILTIN_LVSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSL, ALTIVEC_BUILTIN_LVSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_float, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSL, ALTIVEC_BUILTIN_LVSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_double, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSL, ALTIVEC_BUILTIN_LVSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTDI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSL, ALTIVEC_BUILTIN_LVSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTDI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSL, ALTIVEC_BUILTIN_LVSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_long_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSL, ALTIVEC_BUILTIN_LVSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_long_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSR, ALTIVEC_BUILTIN_LVSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSR, ALTIVEC_BUILTIN_LVSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSR, ALTIVEC_BUILTIN_LVSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSR, ALTIVEC_BUILTIN_LVSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSR, ALTIVEC_BUILTIN_LVSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSR, ALTIVEC_BUILTIN_LVSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSR, ALTIVEC_BUILTIN_LVSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSR, ALTIVEC_BUILTIN_LVSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSR, ALTIVEC_BUILTIN_LVSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_float, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSR, ALTIVEC_BUILTIN_LVSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_double, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSR, ALTIVEC_BUILTIN_LVSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTDI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSR, ALTIVEC_BUILTIN_LVSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTDI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSR, ALTIVEC_BUILTIN_LVSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_long_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LVSR, ALTIVEC_BUILTIN_LVSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_long_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLX, ALTIVEC_BUILTIN_LVLX,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVLXL, ALTIVEC_BUILTIN_LVLXL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRX, ALTIVEC_BUILTIN_LVRX,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LVRXL, ALTIVEC_BUILTIN_LVRXL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXSB,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXSB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXSB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXSH,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXSH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXSH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXSW,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXSW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXSW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, P8V_BUILTIN_VMAXUD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, P8V_BUILTIN_VMAXUD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, P8V_BUILTIN_VMAXUD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, P8V_BUILTIN_VMAXSD,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, P8V_BUILTIN_VMAXSD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, P8V_BUILTIN_VMAXSD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, ALTIVEC_BUILTIN_VMAXFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_MAX, VSX_BUILTIN_XVMAXDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXFP, ALTIVEC_BUILTIN_VMAXFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXSW, ALTIVEC_BUILTIN_VMAXSW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXSW, ALTIVEC_BUILTIN_VMAXSW,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXSW, ALTIVEC_BUILTIN_VMAXSW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXUW, ALTIVEC_BUILTIN_VMAXUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXUW, ALTIVEC_BUILTIN_VMAXUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXUW, ALTIVEC_BUILTIN_VMAXUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXUW, ALTIVEC_BUILTIN_VMAXUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXUW, ALTIVEC_BUILTIN_VMAXUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXSH, ALTIVEC_BUILTIN_VMAXSH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXSH, ALTIVEC_BUILTIN_VMAXSH,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXSH, ALTIVEC_BUILTIN_VMAXSH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXUH, ALTIVEC_BUILTIN_VMAXUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXUH, ALTIVEC_BUILTIN_VMAXUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXUH, ALTIVEC_BUILTIN_VMAXUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXUH, ALTIVEC_BUILTIN_VMAXUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXUH, ALTIVEC_BUILTIN_VMAXUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXSB, ALTIVEC_BUILTIN_VMAXSB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXSB, ALTIVEC_BUILTIN_VMAXSB,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXSB, ALTIVEC_BUILTIN_VMAXSB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXUB, ALTIVEC_BUILTIN_VMAXUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXUB, ALTIVEC_BUILTIN_VMAXUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXUB, ALTIVEC_BUILTIN_VMAXUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXUB, ALTIVEC_BUILTIN_VMAXUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMAXUB, ALTIVEC_BUILTIN_VMAXUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, ALTIVEC_BUILTIN_VMRGHB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, ALTIVEC_BUILTIN_VMRGHB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, ALTIVEC_BUILTIN_VMRGHB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, ALTIVEC_BUILTIN_VMRGHH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, ALTIVEC_BUILTIN_VMRGHH,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, ALTIVEC_BUILTIN_VMRGHH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, ALTIVEC_BUILTIN_VMRGHH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, ALTIVEC_BUILTIN_VMRGHW,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, ALTIVEC_BUILTIN_VMRGHW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, ALTIVEC_BUILTIN_VMRGHW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, ALTIVEC_BUILTIN_VMRGHW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, VSX_BUILTIN_VEC_MERGEH_V2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, VSX_BUILTIN_VEC_MERGEH_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, VSX_BUILTIN_VEC_MERGEH_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, VSX_BUILTIN_VEC_MERGEH_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, VSX_BUILTIN_VEC_MERGEH_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, VSX_BUILTIN_VEC_MERGEH_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, VSX_BUILTIN_VEC_MERGEH_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEH, VSX_BUILTIN_VEC_MERGEH_V2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGHW, ALTIVEC_BUILTIN_VMRGHW,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGHW, ALTIVEC_BUILTIN_VMRGHW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGHW, ALTIVEC_BUILTIN_VMRGHW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGHW, ALTIVEC_BUILTIN_VMRGHW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGHH, ALTIVEC_BUILTIN_VMRGHH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGHH, ALTIVEC_BUILTIN_VMRGHH,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGHH, ALTIVEC_BUILTIN_VMRGHH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGHH, ALTIVEC_BUILTIN_VMRGHH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGHB, ALTIVEC_BUILTIN_VMRGHB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGHB, ALTIVEC_BUILTIN_VMRGHB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGHB, ALTIVEC_BUILTIN_VMRGHB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, ALTIVEC_BUILTIN_VMRGLB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, ALTIVEC_BUILTIN_VMRGLB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, ALTIVEC_BUILTIN_VMRGLB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, ALTIVEC_BUILTIN_VMRGLH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, ALTIVEC_BUILTIN_VMRGLH,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, ALTIVEC_BUILTIN_VMRGLH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, ALTIVEC_BUILTIN_VMRGLH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, ALTIVEC_BUILTIN_VMRGLW,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, ALTIVEC_BUILTIN_VMRGLW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, ALTIVEC_BUILTIN_VMRGLW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, ALTIVEC_BUILTIN_VMRGLW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, VSX_BUILTIN_VEC_MERGEL_V2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, VSX_BUILTIN_VEC_MERGEL_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, VSX_BUILTIN_VEC_MERGEL_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, VSX_BUILTIN_VEC_MERGEL_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, VSX_BUILTIN_VEC_MERGEL_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, VSX_BUILTIN_VEC_MERGEL_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, VSX_BUILTIN_VEC_MERGEL_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MERGEL, VSX_BUILTIN_VEC_MERGEL_V2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGLW, ALTIVEC_BUILTIN_VMRGLW,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGLW, ALTIVEC_BUILTIN_VMRGLW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGLW, ALTIVEC_BUILTIN_VMRGLW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGLW, ALTIVEC_BUILTIN_VMRGLW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGLH, ALTIVEC_BUILTIN_VMRGLH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGLH, ALTIVEC_BUILTIN_VMRGLH,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGLH, ALTIVEC_BUILTIN_VMRGLH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGLH, ALTIVEC_BUILTIN_VMRGLH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGLB, ALTIVEC_BUILTIN_VMRGLB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGLB, ALTIVEC_BUILTIN_VMRGLB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMRGLB, ALTIVEC_BUILTIN_VMRGLB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINSB,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINSB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINSB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINSH,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINSH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINSH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINSW,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINSW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINSW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, P8V_BUILTIN_VMINUD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, P8V_BUILTIN_VMINUD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, P8V_BUILTIN_VMINUD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, P8V_BUILTIN_VMINSD,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, P8V_BUILTIN_VMINSD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, P8V_BUILTIN_VMINSD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, ALTIVEC_BUILTIN_VMINFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_MIN, VSX_BUILTIN_XVMINDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINFP, ALTIVEC_BUILTIN_VMINFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINSW, ALTIVEC_BUILTIN_VMINSW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINSW, ALTIVEC_BUILTIN_VMINSW,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINSW, ALTIVEC_BUILTIN_VMINSW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINUW, ALTIVEC_BUILTIN_VMINUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINUW, ALTIVEC_BUILTIN_VMINUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINUW, ALTIVEC_BUILTIN_VMINUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINUW, ALTIVEC_BUILTIN_VMINUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINUW, ALTIVEC_BUILTIN_VMINUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINSH, ALTIVEC_BUILTIN_VMINSH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINSH, ALTIVEC_BUILTIN_VMINSH,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINSH, ALTIVEC_BUILTIN_VMINSH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINSB, ALTIVEC_BUILTIN_VMINSB,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINSB, ALTIVEC_BUILTIN_VMINSB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINSB, ALTIVEC_BUILTIN_VMINSB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINUH, ALTIVEC_BUILTIN_VMINUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINUH, ALTIVEC_BUILTIN_VMINUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINUH, ALTIVEC_BUILTIN_VMINUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINUH, ALTIVEC_BUILTIN_VMINUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINUH, ALTIVEC_BUILTIN_VMINUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINUB, ALTIVEC_BUILTIN_VMINUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINUB, ALTIVEC_BUILTIN_VMINUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINUB, ALTIVEC_BUILTIN_VMINUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINUB, ALTIVEC_BUILTIN_VMINUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMINUB, ALTIVEC_BUILTIN_VMINUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MULE, ALTIVEC_BUILTIN_VMULEUB,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MULE, ALTIVEC_BUILTIN_VMULESB,
    RS6000_BTI_V8HI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MULE, ALTIVEC_BUILTIN_VMULEUH,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MULE, ALTIVEC_BUILTIN_VMULESH,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MULE, ALTIVEC_BUILTIN_VMULESH,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MULE, ALTIVEC_BUILTIN_VMULESH,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMULEUB, ALTIVEC_BUILTIN_VMULEUB,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMULESB, ALTIVEC_BUILTIN_VMULESB,
    RS6000_BTI_V8HI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMULEUH, ALTIVEC_BUILTIN_VMULEUH,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMULESH, ALTIVEC_BUILTIN_VMULESH,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MULO, ALTIVEC_BUILTIN_VMULOUB,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MULO, ALTIVEC_BUILTIN_VMULOSB,
    RS6000_BTI_V8HI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MULO, ALTIVEC_BUILTIN_VMULOUH,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MULO, ALTIVEC_BUILTIN_VMULOSH,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MULO, ALTIVEC_BUILTIN_VMULOSH,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_MULO, ALTIVEC_BUILTIN_VMULOSH,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMULOSH, ALTIVEC_BUILTIN_VMULOSH,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMULOUH, ALTIVEC_BUILTIN_VMULOUH,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMULOSB, ALTIVEC_BUILTIN_VMULOSB,
    RS6000_BTI_V8HI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMULOUB, ALTIVEC_BUILTIN_VMULOUB,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_NABS, ALTIVEC_BUILTIN_NABS_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_NABS, ALTIVEC_BUILTIN_NABS_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_NABS, ALTIVEC_BUILTIN_NABS_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_NABS, ALTIVEC_BUILTIN_NABS_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_NABS, ALTIVEC_BUILTIN_NABS_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_NABS, VSX_BUILTIN_XVNABSDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_NEARBYINT, VSX_BUILTIN_XVRDPI,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_NEARBYINT, VSX_BUILTIN_XVRSPI,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },

  { ALTIVEC_BUILTIN_VEC_NEG, ALTIVEC_BUILTIN_NEG_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_NEG, ALTIVEC_BUILTIN_NEG_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_NEG, ALTIVEC_BUILTIN_NEG_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_NEG, ALTIVEC_BUILTIN_NEG_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_NEG, ALTIVEC_BUILTIN_NEG_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_NEG, ALTIVEC_BUILTIN_NEG_V2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0, 0 },

  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V4SF, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V2DF, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_OR, ALTIVEC_BUILTIN_VOR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACK, ALTIVEC_BUILTIN_VPKUHUM,
    RS6000_BTI_V16QI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACK, ALTIVEC_BUILTIN_VPKUHUM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACK, ALTIVEC_BUILTIN_VPKUHUM,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACK, ALTIVEC_BUILTIN_VPKUWUM,
    RS6000_BTI_V8HI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACK, ALTIVEC_BUILTIN_VPKUWUM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACK, ALTIVEC_BUILTIN_VPKUWUM,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACK, P8V_BUILTIN_VPKUDUM,
    RS6000_BTI_V4SI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACK, P8V_BUILTIN_VPKUDUM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACK, P8V_BUILTIN_VPKUDUM,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACK, P8V_BUILTIN_VPKUDUM,
    RS6000_BTI_V4SF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_VPKUWUM, ALTIVEC_BUILTIN_VPKUWUM,
    RS6000_BTI_V8HI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VPKUWUM, ALTIVEC_BUILTIN_VPKUWUM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VPKUWUM, ALTIVEC_BUILTIN_VPKUWUM,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VPKUHUM, ALTIVEC_BUILTIN_VPKUHUM,
    RS6000_BTI_V16QI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VPKUHUM, ALTIVEC_BUILTIN_VPKUHUM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VPKUHUM, ALTIVEC_BUILTIN_VPKUHUM,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACKPX, ALTIVEC_BUILTIN_VPKPX,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACKS, ALTIVEC_BUILTIN_VPKUHUS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACKS, ALTIVEC_BUILTIN_VPKSHSS,
    RS6000_BTI_V16QI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACKS, ALTIVEC_BUILTIN_VPKUWUS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACKS, ALTIVEC_BUILTIN_VPKSWSS,
    RS6000_BTI_V8HI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACKS, P8V_BUILTIN_VPKUDUS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACKS, P8V_BUILTIN_VPKSDSS,
    RS6000_BTI_V4SI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_VPKSWSS, ALTIVEC_BUILTIN_VPKSWSS,
    RS6000_BTI_V8HI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VPKUWUS, ALTIVEC_BUILTIN_VPKUWUS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VPKSHSS, ALTIVEC_BUILTIN_VPKSHSS,
    RS6000_BTI_V16QI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VPKUHUS, ALTIVEC_BUILTIN_VPKUHUS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACKSU, ALTIVEC_BUILTIN_VPKUHUS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACKSU, ALTIVEC_BUILTIN_VPKSHUS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACKSU, ALTIVEC_BUILTIN_VPKUWUS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACKSU, ALTIVEC_BUILTIN_VPKSWUS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACKSU, P8V_BUILTIN_VPKSDUS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_PACKSU, P8V_BUILTIN_VPKSDUS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_VPKSWUS, ALTIVEC_BUILTIN_VPKSWUS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VPKSHUS, ALTIVEC_BUILTIN_VPKSHUS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_RINT, VSX_BUILTIN_XVRDPIC,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_RINT, VSX_BUILTIN_XVRSPIC,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_RL, ALTIVEC_BUILTIN_VRLB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_RL, ALTIVEC_BUILTIN_VRLB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_RL, ALTIVEC_BUILTIN_VRLH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_RL, ALTIVEC_BUILTIN_VRLH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_RL, ALTIVEC_BUILTIN_VRLW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_RL, ALTIVEC_BUILTIN_VRLW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_RL, P8V_BUILTIN_VRLD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_RL, P8V_BUILTIN_VRLD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_VRLW, ALTIVEC_BUILTIN_VRLW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VRLW, ALTIVEC_BUILTIN_VRLW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VRLH, ALTIVEC_BUILTIN_VRLH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VRLH, ALTIVEC_BUILTIN_VRLH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VRLB, ALTIVEC_BUILTIN_VRLB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VRLB, ALTIVEC_BUILTIN_VRLB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { P9V_BUILTIN_VEC_RLMI, P9V_BUILTIN_VRLWMI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI },
  { P9V_BUILTIN_VEC_RLMI, P9V_BUILTIN_VRLDMI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI },
  { P9V_BUILTIN_VEC_RLNM, P9V_BUILTIN_VRLWNM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { P9V_BUILTIN_VEC_RLNM, P9V_BUILTIN_VRLDNM,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_SL, ALTIVEC_BUILTIN_VSLB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SL, ALTIVEC_BUILTIN_VSLB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SL, ALTIVEC_BUILTIN_VSLH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SL, ALTIVEC_BUILTIN_VSLH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SL, ALTIVEC_BUILTIN_VSLW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SL, ALTIVEC_BUILTIN_VSLW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SL, P8V_BUILTIN_VSLD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_SL, P8V_BUILTIN_VSLD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_SQRT, VSX_BUILTIN_XVSQRTDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_SQRT, VSX_BUILTIN_XVSQRTSP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VSLW, ALTIVEC_BUILTIN_VSLW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSLW, ALTIVEC_BUILTIN_VSLW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSLH, ALTIVEC_BUILTIN_VSLH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSLH, ALTIVEC_BUILTIN_VSLH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSLB, ALTIVEC_BUILTIN_VSLB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSLB, ALTIVEC_BUILTIN_VSLB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLL, ALTIVEC_BUILTIN_VSL,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SLO, ALTIVEC_BUILTIN_VSLO,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SPLAT, ALTIVEC_BUILTIN_VSPLTB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_SPLAT, ALTIVEC_BUILTIN_VSPLTB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_SPLAT, ALTIVEC_BUILTIN_VSPLTB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_SPLAT, ALTIVEC_BUILTIN_VSPLTH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_SPLAT, ALTIVEC_BUILTIN_VSPLTH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_SPLAT, ALTIVEC_BUILTIN_VSPLTH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_SPLAT, ALTIVEC_BUILTIN_VSPLTH,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_SPLAT, ALTIVEC_BUILTIN_VSPLTW,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_SPLAT, ALTIVEC_BUILTIN_VSPLTW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_SPLAT, ALTIVEC_BUILTIN_VSPLTW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_SPLAT, ALTIVEC_BUILTIN_VSPLTW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_SPLAT, VSX_BUILTIN_XXSPLTD_V2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_SPLAT, VSX_BUILTIN_XXSPLTD_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_SPLAT, VSX_BUILTIN_XXSPLTD_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_SPLAT, VSX_BUILTIN_XXSPLTD_V2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSPLTW, ALTIVEC_BUILTIN_VSPLTW,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSPLTW, ALTIVEC_BUILTIN_VSPLTW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSPLTW, ALTIVEC_BUILTIN_VSPLTW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSPLTW, ALTIVEC_BUILTIN_VSPLTW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSPLTH, ALTIVEC_BUILTIN_VSPLTH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSPLTH, ALTIVEC_BUILTIN_VSPLTH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSPLTH, ALTIVEC_BUILTIN_VSPLTH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSPLTH, ALTIVEC_BUILTIN_VSPLTH,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSPLTB, ALTIVEC_BUILTIN_VSPLTB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSPLTB, ALTIVEC_BUILTIN_VSPLTB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSPLTB, ALTIVEC_BUILTIN_VSPLTB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_SR, ALTIVEC_BUILTIN_VSRB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SR, ALTIVEC_BUILTIN_VSRB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SR, ALTIVEC_BUILTIN_VSRH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SR, ALTIVEC_BUILTIN_VSRH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SR, ALTIVEC_BUILTIN_VSRW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SR, ALTIVEC_BUILTIN_VSRW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SR, P8V_BUILTIN_VSRD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_SR, P8V_BUILTIN_VSRD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSRW, ALTIVEC_BUILTIN_VSRW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSRW, ALTIVEC_BUILTIN_VSRW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSRH, ALTIVEC_BUILTIN_VSRH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSRH, ALTIVEC_BUILTIN_VSRH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSRB, ALTIVEC_BUILTIN_VSRB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSRB, ALTIVEC_BUILTIN_VSRB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRA, ALTIVEC_BUILTIN_VSRAB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRA, ALTIVEC_BUILTIN_VSRAB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRA, ALTIVEC_BUILTIN_VSRAH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRA, ALTIVEC_BUILTIN_VSRAH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRA, ALTIVEC_BUILTIN_VSRAW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRA, ALTIVEC_BUILTIN_VSRAW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRA, P8V_BUILTIN_VSRAD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRA, P8V_BUILTIN_VSRAD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSRAW, ALTIVEC_BUILTIN_VSRAW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSRAW, ALTIVEC_BUILTIN_VSRAW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSRAH, ALTIVEC_BUILTIN_VSRAH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSRAH, ALTIVEC_BUILTIN_VSRAH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSRAB, ALTIVEC_BUILTIN_VSRAB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSRAB, ALTIVEC_BUILTIN_VSRAB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRL, ALTIVEC_BUILTIN_VSR,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SRO, ALTIVEC_BUILTIN_VSRO,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUBM,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUBM,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUBM,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUHM,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUHM,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUHM,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUWM,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUWM,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUWM,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, P8V_BUILTIN_VSUBUDM,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, P8V_BUILTIN_VSUBUDM,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, P8V_BUILTIN_VSUBUDM,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, P8V_BUILTIN_VSUBUDM,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, P8V_BUILTIN_VSUBUDM,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, P8V_BUILTIN_VSUBUDM,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, VSX_BUILTIN_XVSUBDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, P8V_BUILTIN_VSUBUQM,
    RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, P8V_BUILTIN_VSUBUQM,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI,
    RS6000_BTI_unsigned_V1TI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBFP, ALTIVEC_BUILTIN_VSUBFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUWM, ALTIVEC_BUILTIN_VSUBUWM,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUWM, ALTIVEC_BUILTIN_VSUBUWM,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUWM, ALTIVEC_BUILTIN_VSUBUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUWM, ALTIVEC_BUILTIN_VSUBUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUWM, ALTIVEC_BUILTIN_VSUBUWM,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUWM, ALTIVEC_BUILTIN_VSUBUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUWM, ALTIVEC_BUILTIN_VSUBUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUWM, ALTIVEC_BUILTIN_VSUBUWM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUHM, ALTIVEC_BUILTIN_VSUBUHM,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUHM, ALTIVEC_BUILTIN_VSUBUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUHM, ALTIVEC_BUILTIN_VSUBUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUHM, ALTIVEC_BUILTIN_VSUBUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUHM, ALTIVEC_BUILTIN_VSUBUHM,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUHM, ALTIVEC_BUILTIN_VSUBUHM,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUHM, ALTIVEC_BUILTIN_VSUBUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUHM, ALTIVEC_BUILTIN_VSUBUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUBM, ALTIVEC_BUILTIN_VSUBUBM,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUBM, ALTIVEC_BUILTIN_VSUBUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUBM, ALTIVEC_BUILTIN_VSUBUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUBM, ALTIVEC_BUILTIN_VSUBUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUBM, ALTIVEC_BUILTIN_VSUBUBM,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUBM, ALTIVEC_BUILTIN_VSUBUBM,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUBM, ALTIVEC_BUILTIN_VSUBUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUBM, ALTIVEC_BUILTIN_VSUBUBM,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBC, ALTIVEC_BUILTIN_VSUBCUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBSBS,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBSBS,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBSBS,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBSHS,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBSHS,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBSHS,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBSWS,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBSWS,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUBS, ALTIVEC_BUILTIN_VSUBSWS,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBSWS, ALTIVEC_BUILTIN_VSUBSWS,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBSWS, ALTIVEC_BUILTIN_VSUBSWS,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBSWS, ALTIVEC_BUILTIN_VSUBSWS,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUWS, ALTIVEC_BUILTIN_VSUBUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUWS, ALTIVEC_BUILTIN_VSUBUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUWS, ALTIVEC_BUILTIN_VSUBUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUWS, ALTIVEC_BUILTIN_VSUBUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUWS, ALTIVEC_BUILTIN_VSUBUWS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBSHS, ALTIVEC_BUILTIN_VSUBSHS,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBSHS, ALTIVEC_BUILTIN_VSUBSHS,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBSHS, ALTIVEC_BUILTIN_VSUBSHS,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUHS, ALTIVEC_BUILTIN_VSUBUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUHS, ALTIVEC_BUILTIN_VSUBUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUHS, ALTIVEC_BUILTIN_VSUBUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUHS, ALTIVEC_BUILTIN_VSUBUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUHS, ALTIVEC_BUILTIN_VSUBUHS,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBSBS, ALTIVEC_BUILTIN_VSUBSBS,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBSBS, ALTIVEC_BUILTIN_VSUBSBS,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBSBS, ALTIVEC_BUILTIN_VSUBSBS,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUBS, ALTIVEC_BUILTIN_VSUBUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUBS, ALTIVEC_BUILTIN_VSUBUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUBS, ALTIVEC_BUILTIN_VSUBUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUBS, ALTIVEC_BUILTIN_VSUBUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUBUBS, ALTIVEC_BUILTIN_VSUBUBS,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUM4S, ALTIVEC_BUILTIN_VSUM4UBS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUM4S, ALTIVEC_BUILTIN_VSUM4SBS,
    RS6000_BTI_V4SI, RS6000_BTI_V16QI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUM4S, ALTIVEC_BUILTIN_VSUM4SHS,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUM4SHS, ALTIVEC_BUILTIN_VSUM4SHS,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUM4SBS, ALTIVEC_BUILTIN_VSUM4SBS,
    RS6000_BTI_V4SI, RS6000_BTI_V16QI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_VSUM4UBS, ALTIVEC_BUILTIN_VSUM4UBS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUM2S, ALTIVEC_BUILTIN_VSUM2SWS,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_SUMS, ALTIVEC_BUILTIN_VSUMSWS,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V2DF,
    RS6000_BTI_V2DF, RS6000_BTI_INTSI, ~RS6000_BTI_V2DF, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V2DF,
    RS6000_BTI_V2DF, RS6000_BTI_INTSI, ~RS6000_BTI_double, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_INTSI, ~RS6000_BTI_V2DI, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_INTSI, ~RS6000_BTI_long_long, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V2DI, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_long_long, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V16QI, 0 },
  { VSX_BUILTIN_VEC_XL, VSX_BUILTIN_LD_ELEMREV_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V4SF, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V2DF, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_XOR, ALTIVEC_BUILTIN_VXOR,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },

  /* Ternary AltiVec/VSX builtins.  */
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_V16QI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_V4SI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_V4SF, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_UINTQI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_INTQI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_UINTHI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_INTHI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_UINTSI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_long, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_long, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DST, ALTIVEC_BUILTIN_DST,
    RS6000_BTI_void, ~RS6000_BTI_float, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_V16QI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_V4SI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_V4SF, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_UINTQI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_INTQI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_UINTHI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_INTHI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_UINTSI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_long, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_long, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTST, ALTIVEC_BUILTIN_DSTST,
    RS6000_BTI_void, ~RS6000_BTI_float, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_V16QI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_V4SI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_V4SF, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_UINTQI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_INTQI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_UINTHI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_INTHI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_UINTSI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_long, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_long, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTSTT, ALTIVEC_BUILTIN_DSTSTT,
    RS6000_BTI_void, ~RS6000_BTI_float, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_V16QI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_V4SI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_V4SF, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_UINTQI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_INTQI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_UINTHI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_INTHI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_UINTSI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_unsigned_long, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_long, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_DSTT, ALTIVEC_BUILTIN_DSTT,
    RS6000_BTI_void, ~RS6000_BTI_float, RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_MADD, ALTIVEC_BUILTIN_VMADDFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VEC_MADD, VSX_BUILTIN_XVMADDDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF },
  { ALTIVEC_BUILTIN_VEC_MADD, ALTIVEC_BUILTIN_VMLADDUHM,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_MADD, ALTIVEC_BUILTIN_VMLADDUHM,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_MADD, ALTIVEC_BUILTIN_VMLADDUHM,
    RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_MADD, ALTIVEC_BUILTIN_VMLADDUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_MADDS, ALTIVEC_BUILTIN_VMHADDSHS,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_MLADD, ALTIVEC_BUILTIN_VMLADDUHM,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_MLADD, ALTIVEC_BUILTIN_VMLADDUHM,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_MLADD, ALTIVEC_BUILTIN_VMLADDUHM,
    RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_MLADD, ALTIVEC_BUILTIN_VMLADDUHM,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_MRADDS, ALTIVEC_BUILTIN_VMHRADDSHS,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI },
  { VSX_BUILTIN_VEC_MSUB, VSX_BUILTIN_XVMSUBSP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF },
  { VSX_BUILTIN_VEC_MSUB, VSX_BUILTIN_XVMSUBDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF },
  { ALTIVEC_BUILTIN_VEC_MSUM, ALTIVEC_BUILTIN_VMSUMUBM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_MSUM, ALTIVEC_BUILTIN_VMSUMMBM,
    RS6000_BTI_V4SI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_MSUM, ALTIVEC_BUILTIN_VMSUMUHM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_MSUM, ALTIVEC_BUILTIN_VMSUMSHM,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_VMSUMSHM, ALTIVEC_BUILTIN_VMSUMSHM,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_VMSUMUHM, ALTIVEC_BUILTIN_VMSUMUHM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_VMSUMMBM, ALTIVEC_BUILTIN_VMSUMMBM,
    RS6000_BTI_V4SI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_VMSUMUBM, ALTIVEC_BUILTIN_VMSUMUBM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_MSUMS, ALTIVEC_BUILTIN_VMSUMUHS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_MSUMS, ALTIVEC_BUILTIN_VMSUMSHS,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_VMSUMSHS, ALTIVEC_BUILTIN_VMSUMSHS,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_VMSUMUHS, ALTIVEC_BUILTIN_VMSUMUHS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V4SI },
  { VSX_BUILTIN_VEC_NMADD, VSX_BUILTIN_XVNMADDSP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF },
  { VSX_BUILTIN_VEC_NMADD, VSX_BUILTIN_XVNMADDDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF },
  { ALTIVEC_BUILTIN_VEC_NMSUB, ALTIVEC_BUILTIN_VNMSUBFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VEC_NMSUB, VSX_BUILTIN_XVNMSUBDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI,
    RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_4SF,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_4SI,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_8HI,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_8HI,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_16QI,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_PERM, ALTIVEC_BUILTIN_VPERM_16QI,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_bool_V2DI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_unsigned_V2DI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_unsigned_V2DI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_V2DI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI,
    RS6000_BTI_bool_V2DI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI,
    RS6000_BTI_unsigned_V2DI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_4SF,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_4SF,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_4SI,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_4SI,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_4SI,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_4SI,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_8HI,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_8HI,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_16QI,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_SEL, ALTIVEC_BUILTIN_VSEL_16QI,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_SLD, ALTIVEC_BUILTIN_VSLDOI_4SF,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLD, ALTIVEC_BUILTIN_VSLDOI_4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLD, ALTIVEC_BUILTIN_VSLDOI_4SI,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLD, ALTIVEC_BUILTIN_VSLDOI_4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLD, ALTIVEC_BUILTIN_VSLDOI_8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLD, ALTIVEC_BUILTIN_VSLDOI_8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLD, ALTIVEC_BUILTIN_VSLDOI_8HI,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLD, ALTIVEC_BUILTIN_VSLDOI_8HI,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLD, ALTIVEC_BUILTIN_VSLDOI_16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLD, ALTIVEC_BUILTIN_VSLDOI_16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLD, ALTIVEC_BUILTIN_VSLDOI_16QI,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLD, ALTIVEC_BUILTIN_VSLDOI_2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLDW, VSX_BUILTIN_XXSLDWI_16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI,
    RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLDW, VSX_BUILTIN_XXSLDWI_16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLDW, VSX_BUILTIN_XXSLDWI_8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI,
    RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLDW, VSX_BUILTIN_XXSLDWI_8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLDW, VSX_BUILTIN_XXSLDWI_4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI,
    RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLDW, VSX_BUILTIN_XXSLDWI_4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLDW, VSX_BUILTIN_XXSLDWI_2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI,
    RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_SLDW, VSX_BUILTIN_XXSLDWI_2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_NOT_OPAQUE },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V2DF,
    RS6000_BTI_void, RS6000_BTI_V2DF, RS6000_BTI_INTSI, ~RS6000_BTI_V2DF },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V2DI,
    RS6000_BTI_void, RS6000_BTI_V2DI, RS6000_BTI_INTSI, ~RS6000_BTI_V2DI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V2DI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V2DI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V2DI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V2DI,
    RS6000_BTI_void, RS6000_BTI_bool_V2DI, RS6000_BTI_INTSI,
    ~RS6000_BTI_bool_V2DI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V4SF,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V4SF,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V4SI,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V4SI,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V4SI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V4SI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V4SI,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V4SI,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V4SI,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V8HI,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V8HI,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V8HI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V8HI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V8HI,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V8HI,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V8HI,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V16QI,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V16QI,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V16QI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V16QI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V16QI,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V16QI,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V16QI,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX_V8HI,
    RS6000_BTI_void, RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI },
  { ALTIVEC_BUILTIN_VEC_STE, ALTIVEC_BUILTIN_STVEBX,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_STE, ALTIVEC_BUILTIN_STVEBX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { ALTIVEC_BUILTIN_VEC_STE, ALTIVEC_BUILTIN_STVEBX,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_STE, ALTIVEC_BUILTIN_STVEBX,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { ALTIVEC_BUILTIN_VEC_STE, ALTIVEC_BUILTIN_STVEHX,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_STE, ALTIVEC_BUILTIN_STVEHX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_STE, ALTIVEC_BUILTIN_STVEHX,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_STE, ALTIVEC_BUILTIN_STVEHX,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_STE, ALTIVEC_BUILTIN_STVEHX,
    RS6000_BTI_void, RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_STE, ALTIVEC_BUILTIN_STVEHX,
    RS6000_BTI_void, RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_STE, ALTIVEC_BUILTIN_STVEWX,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float },
  { ALTIVEC_BUILTIN_VEC_STE, ALTIVEC_BUILTIN_STVEWX,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_STE, ALTIVEC_BUILTIN_STVEWX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_STE, ALTIVEC_BUILTIN_STVEWX,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_STE, ALTIVEC_BUILTIN_STVEWX,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_STVEWX, ALTIVEC_BUILTIN_STVEWX,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float },
  { ALTIVEC_BUILTIN_VEC_STVEWX, ALTIVEC_BUILTIN_STVEWX,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_STVEWX, ALTIVEC_BUILTIN_STVEWX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_STVEWX, ALTIVEC_BUILTIN_STVEWX,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_STVEWX, ALTIVEC_BUILTIN_STVEWX,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_STVEWX, ALTIVEC_BUILTIN_STVEWX,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_void },
  { ALTIVEC_BUILTIN_VEC_STVEWX, ALTIVEC_BUILTIN_STVEWX,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_void },
  { ALTIVEC_BUILTIN_VEC_STVEWX, ALTIVEC_BUILTIN_STVEWX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_void },
  { ALTIVEC_BUILTIN_VEC_STVEHX, ALTIVEC_BUILTIN_STVEHX,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_STVEHX, ALTIVEC_BUILTIN_STVEHX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_STVEHX, ALTIVEC_BUILTIN_STVEHX,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_STVEHX, ALTIVEC_BUILTIN_STVEHX,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_STVEHX, ALTIVEC_BUILTIN_STVEHX,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_void },
  { ALTIVEC_BUILTIN_VEC_STVEHX, ALTIVEC_BUILTIN_STVEHX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_void },
  { ALTIVEC_BUILTIN_VEC_STVEBX, ALTIVEC_BUILTIN_STVEBX,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_STVEBX, ALTIVEC_BUILTIN_STVEBX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { ALTIVEC_BUILTIN_VEC_STVEBX, ALTIVEC_BUILTIN_STVEBX,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_STVEBX, ALTIVEC_BUILTIN_STVEBX,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { ALTIVEC_BUILTIN_VEC_STVEBX, ALTIVEC_BUILTIN_STVEBX,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_void },
  { ALTIVEC_BUILTIN_VEC_STVEBX, ALTIVEC_BUILTIN_STVEBX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_void },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V4SF,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V4SF,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V4SI,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V4SI,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V4SI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V4SI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V4SI,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V4SI,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V4SI,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V8HI,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V8HI,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V8HI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V8HI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V8HI,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V8HI,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V8HI,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V16QI,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V16QI,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V16QI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V16QI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V16QI,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V16QI,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V16QI,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V8HI,
    RS6000_BTI_void, RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V2DF,
    RS6000_BTI_void, RS6000_BTI_V2DF, RS6000_BTI_INTSI, ~RS6000_BTI_V2DF },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V2DF,
    RS6000_BTI_void, RS6000_BTI_V2DF, RS6000_BTI_INTSI, ~RS6000_BTI_double },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V2DI,
    RS6000_BTI_void, RS6000_BTI_V2DI, RS6000_BTI_INTSI, ~RS6000_BTI_V2DI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V2DI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V2DI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V2DI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL_V2DI,
    RS6000_BTI_void, RS6000_BTI_bool_V2DI, RS6000_BTI_INTSI,
    ~RS6000_BTI_bool_V2DI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_STVLX, ALTIVEC_BUILTIN_STVLX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_STVLXL, ALTIVEC_BUILTIN_STVLXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_STVRX, ALTIVEC_BUILTIN_STVRX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_STVRXL, ALTIVEC_BUILTIN_STVRXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V2DF,
    RS6000_BTI_void, RS6000_BTI_V2DF, RS6000_BTI_INTSI, ~RS6000_BTI_V2DF },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V2DF,
    RS6000_BTI_void, RS6000_BTI_V2DF, RS6000_BTI_INTSI, ~RS6000_BTI_double },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V2DI,
    RS6000_BTI_void, RS6000_BTI_V2DI, RS6000_BTI_INTSI, ~RS6000_BTI_V2DI },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V2DI,
    RS6000_BTI_void, RS6000_BTI_V2DI, RS6000_BTI_INTSI,
    ~RS6000_BTI_long_long },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V2DI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V2DI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V2DI },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V2DI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V2DI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_long_long },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V4SF,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V4SF,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V4SI,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V4SI,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V4SI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V4SI },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V4SI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI,
    ~RS6000_BTI_UINTSI },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V8HI,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V8HI,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V8HI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V8HI },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V8HI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI,
    ~RS6000_BTI_UINTHI },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V16QI,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V16QI,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V16QI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V16QI },
  { VSX_BUILTIN_VEC_XST, VSX_BUILTIN_ST_ELEMREV_V16QI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI,
    ~RS6000_BTI_UINTQI },
  { VSX_BUILTIN_VEC_XXSLDWI, VSX_BUILTIN_XXSLDWI_16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXSLDWI, VSX_BUILTIN_XXSLDWI_16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXSLDWI, VSX_BUILTIN_XXSLDWI_8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXSLDWI, VSX_BUILTIN_XXSLDWI_8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXSLDWI, VSX_BUILTIN_XXSLDWI_4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXSLDWI, VSX_BUILTIN_XXSLDWI_4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXSLDWI, VSX_BUILTIN_XXSLDWI_2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXSLDWI, VSX_BUILTIN_XXSLDWI_2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXSLDWI, VSX_BUILTIN_XXSLDWI_4SF,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXSLDWI, VSX_BUILTIN_XXSLDWI_2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXPERMDI, VSX_BUILTIN_XXPERMDI_2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXPERMDI, VSX_BUILTIN_XXPERMDI_2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXPERMDI, VSX_BUILTIN_XXPERMDI_2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXPERMDI, VSX_BUILTIN_XXPERMDI_4SF,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXPERMDI, VSX_BUILTIN_XXPERMDI_4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXPERMDI, VSX_BUILTIN_XXPERMDI_4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXPERMDI, VSX_BUILTIN_XXPERMDI_8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXPERMDI, VSX_BUILTIN_XXPERMDI_8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXPERMDI, VSX_BUILTIN_XXPERMDI_16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_NOT_OPAQUE },
  { VSX_BUILTIN_VEC_XXPERMDI, VSX_BUILTIN_XXPERMDI_16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_NOT_OPAQUE },

  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVD2X_V2DF,
    RS6000_BTI_V2DF, RS6000_BTI_INTSI, ~RS6000_BTI_V2DF, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVD2X_V2DF,
    RS6000_BTI_V2DF, RS6000_BTI_INTSI, ~RS6000_BTI_double, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVD2X_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_INTSI, ~RS6000_BTI_V2DI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVD2X_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V2DI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVD2X_V2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V2DI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V4SI,
    RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_long, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V4SI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_long, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V8HI,
    RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V8HI,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V8HI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V16QI,
    RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V16QI, 0 },
  { VSX_BUILTIN_VEC_LD, VSX_BUILTIN_LXVW4X_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI, 0 },

  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVD2X_V2DF,
    RS6000_BTI_void, RS6000_BTI_V2DF, RS6000_BTI_INTSI, ~RS6000_BTI_V2DF },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVD2X_V2DF,
    RS6000_BTI_void, RS6000_BTI_V2DF, RS6000_BTI_INTSI, ~RS6000_BTI_double },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVD2X_V2DI,
    RS6000_BTI_void, RS6000_BTI_V2DI, RS6000_BTI_INTSI, ~RS6000_BTI_V2DI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVD2X_V2DI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V2DI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V2DI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVD2X_V2DI,
    RS6000_BTI_void, RS6000_BTI_bool_V2DI, RS6000_BTI_INTSI,
    ~RS6000_BTI_bool_V2DI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V4SF,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V4SF,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V4SI,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V4SI,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V4SI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V4SI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V4SI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI,
    ~RS6000_BTI_UINTSI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V4SI,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI,
    ~RS6000_BTI_bool_V4SI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V4SI,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI,
    ~RS6000_BTI_UINTSI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V4SI,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI,
    ~RS6000_BTI_INTSI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V8HI,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V8HI,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V8HI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V8HI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V8HI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI,
    ~RS6000_BTI_UINTHI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V8HI,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI,
    ~RS6000_BTI_bool_V8HI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V8HI,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI,
    ~RS6000_BTI_UINTHI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V8HI,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI,
    ~RS6000_BTI_INTHI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V16QI,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V16QI,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V16QI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI,
    ~RS6000_BTI_unsigned_V16QI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V16QI,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI,
    ~RS6000_BTI_UINTQI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V16QI,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI,
    ~RS6000_BTI_bool_V16QI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V16QI,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI,
    ~RS6000_BTI_UINTQI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V16QI,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI,
    ~RS6000_BTI_INTQI },
  { VSX_BUILTIN_VEC_ST, VSX_BUILTIN_STXVW4X_V16QI,
    RS6000_BTI_void, RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI,
    ~RS6000_BTI_pixel_V8HI },

  /* Predicates.  */
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V16QI, RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, P8V_BUILTIN_VCMPGTUD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, P8V_BUILTIN_VCMPGTUD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, P8V_BUILTIN_VCMPGTUD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, P8V_BUILTIN_VCMPGTSD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, P8V_BUILTIN_VCMPGTSD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, P8V_BUILTIN_VCMPGTSD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V2DI, RS6000_BTI_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTFP_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SF, RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VEC_VCMPGT_P, VSX_BUILTIN_XVCMPGTDP_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V2DF, RS6000_BTI_V2DF },


  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V16QI, RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, P8V_BUILTIN_VCMPEQUD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, P8V_BUILTIN_VCMPEQUD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, P8V_BUILTIN_VCMPEQUD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, P8V_BUILTIN_VCMPEQUD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, P8V_BUILTIN_VCMPEQUD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, P8V_BUILTIN_VCMPEQUD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V2DI, RS6000_BTI_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, P8V_BUILTIN_VCMPEQUD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQFP_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SF, RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VEC_VCMPEQ_P, VSX_BUILTIN_XVCMPEQDP_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V2DF, RS6000_BTI_V2DF },


  /* cmpge is the same as cmpgt for all cases except floating point.
     There is further code to deal with this special case in
     altivec_build_resolved_builtin.  */
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V16QI, RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, P8V_BUILTIN_VCMPGTUD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, P8V_BUILTIN_VCMPGTUD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, P8V_BUILTIN_VCMPGTUD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, P8V_BUILTIN_VCMPGTSD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, P8V_BUILTIN_VCMPGTSD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, P8V_BUILTIN_VCMPGTSD_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V2DI, RS6000_BTI_V2DI },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGEFP_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SF, RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VEC_VCMPGE_P, VSX_BUILTIN_XVCMPGEDP_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V2DF, RS6000_BTI_V2DF },

  /* Power8 vector overloaded functions.  */
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V16QI,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_bool_V16QI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V8HI,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_bool_V8HI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V4SI,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_bool_V4SI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI,
    RS6000_BTI_unsigned_V2DI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V2DI, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { P8V_BUILTIN_VEC_EQV, P8V_BUILTIN_EQV_V2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },

  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_bool_V16QI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V16QI,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_bool_V8HI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V8HI,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_bool_V4SI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V4SI,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI,
    RS6000_BTI_unsigned_V2DI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V2DI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { P8V_BUILTIN_VEC_NAND, P8V_BUILTIN_NAND_V2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },

  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_bool_V16QI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V16QI,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_bool_V8HI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V8HI,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_bool_V4SI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V4SI,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI,
    RS6000_BTI_unsigned_V2DI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V2DI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V2DI,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { P8V_BUILTIN_VEC_ORC, P8V_BUILTIN_ORC_V2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },

  { P8V_BUILTIN_VEC_VADDCUQ, P8V_BUILTIN_VADDCUQ,
    RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI, 0 },
  { P8V_BUILTIN_VEC_VADDCUQ, P8V_BUILTIN_VADDCUQ,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI,
    RS6000_BTI_unsigned_V1TI, 0 },

  { P8V_BUILTIN_VEC_VADDUDM, P8V_BUILTIN_VADDUDM,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { P8V_BUILTIN_VEC_VADDUDM, P8V_BUILTIN_VADDUDM,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_VADDUDM, P8V_BUILTIN_VADDUDM,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { P8V_BUILTIN_VEC_VADDUDM, P8V_BUILTIN_VADDUDM,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { P8V_BUILTIN_VEC_VADDUDM, P8V_BUILTIN_VADDUDM,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_VADDUDM, P8V_BUILTIN_VADDUDM,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },

  { P8V_BUILTIN_VEC_VADDUQM, P8V_BUILTIN_VADDUQM,
    RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI, 0 },
  { P8V_BUILTIN_VEC_VADDUQM, P8V_BUILTIN_VADDUQM,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI,
    RS6000_BTI_unsigned_V1TI, 0 },

  { P9V_BUILTIN_VEC_VBPERM, P9V_BUILTIN_VBPERMD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { P9V_BUILTIN_VEC_VBPERM, P8V_BUILTIN_VBPERMQ,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V1TI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { P9V_BUILTIN_VEC_VBPERM, P8V_BUILTIN_VBPERMQ2,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },

  { P8V_BUILTIN_VEC_VBPERMQ, P8V_BUILTIN_VBPERMQ,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { P8V_BUILTIN_VEC_VBPERMQ, P8V_BUILTIN_VBPERMQ,
    RS6000_BTI_V2DI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { P8V_BUILTIN_VEC_VBPERMQ, P8V_BUILTIN_VBPERMQ,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { P8V_BUILTIN_VEC_VBPERMQ, P8V_BUILTIN_VBPERMQ,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V1TI,
    RS6000_BTI_unsigned_V16QI, 0 },

  { P8V_BUILTIN_VEC_VCLZ, P8V_BUILTIN_VCLZB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0, 0 },
  { P8V_BUILTIN_VEC_VCLZ, P8V_BUILTIN_VCLZB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0, 0 },
  { P8V_BUILTIN_VEC_VCLZ, P8V_BUILTIN_VCLZH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0, 0 },
  { P8V_BUILTIN_VEC_VCLZ, P8V_BUILTIN_VCLZH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0, 0 },
  { P8V_BUILTIN_VEC_VCLZ, P8V_BUILTIN_VCLZW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0, 0 },
  { P8V_BUILTIN_VEC_VCLZ, P8V_BUILTIN_VCLZW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0, 0 },
  { P8V_BUILTIN_VEC_VCLZ, P8V_BUILTIN_VCLZD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0, 0 },
  { P8V_BUILTIN_VEC_VCLZ, P8V_BUILTIN_VCLZD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0, 0 },

  { P8V_BUILTIN_VEC_VCLZB, P8V_BUILTIN_VCLZB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0, 0 },
  { P8V_BUILTIN_VEC_VCLZB, P8V_BUILTIN_VCLZB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0, 0 },

  { P8V_BUILTIN_VEC_VCLZH, P8V_BUILTIN_VCLZH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0, 0 },
  { P8V_BUILTIN_VEC_VCLZH, P8V_BUILTIN_VCLZH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0, 0 },

  { P8V_BUILTIN_VEC_VCLZW, P8V_BUILTIN_VCLZW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0, 0 },
  { P8V_BUILTIN_VEC_VCLZW, P8V_BUILTIN_VCLZW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0, 0 },

  { P8V_BUILTIN_VEC_VCLZD, P8V_BUILTIN_VCLZD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0, 0 },
  { P8V_BUILTIN_VEC_VCLZD, P8V_BUILTIN_VCLZD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0, 0 },

  { P9_BUILTIN_DFP_TSTSFI_LT, MISC_BUILTIN_TSTSFI_LT_TD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat128, 0 },
  { P9_BUILTIN_DFP_TSTSFI_LT, MISC_BUILTIN_TSTSFI_LT_DD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat64, 0 },

  { P9_BUILTIN_DFP_TSTSFI_LT_TD, MISC_BUILTIN_TSTSFI_LT_TD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat128, 0 },
  { P9_BUILTIN_DFP_TSTSFI_LT_DD, MISC_BUILTIN_TSTSFI_LT_DD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat64, 0 },

  { P9_BUILTIN_DFP_TSTSFI_EQ, MISC_BUILTIN_TSTSFI_EQ_TD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat128, 0 },
  { P9_BUILTIN_DFP_TSTSFI_EQ, MISC_BUILTIN_TSTSFI_EQ_DD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat64, 0 },

  { P9_BUILTIN_DFP_TSTSFI_EQ_TD, MISC_BUILTIN_TSTSFI_EQ_TD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat128, 0 },
  { P9_BUILTIN_DFP_TSTSFI_EQ_DD, MISC_BUILTIN_TSTSFI_EQ_DD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat64, 0 },

  { P9_BUILTIN_DFP_TSTSFI_GT, MISC_BUILTIN_TSTSFI_GT_TD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat128, 0 },
  { P9_BUILTIN_DFP_TSTSFI_GT, MISC_BUILTIN_TSTSFI_GT_DD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat64, 0 },

  { P9_BUILTIN_DFP_TSTSFI_GT_TD, MISC_BUILTIN_TSTSFI_GT_TD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat128, 0 },
  { P9_BUILTIN_DFP_TSTSFI_GT_DD, MISC_BUILTIN_TSTSFI_GT_DD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat64, 0 },

  { P9_BUILTIN_DFP_TSTSFI_OV, MISC_BUILTIN_TSTSFI_OV_TD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat128, 0 },
  { P9_BUILTIN_DFP_TSTSFI_OV, MISC_BUILTIN_TSTSFI_OV_DD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat64, 0 },

  { P9_BUILTIN_DFP_TSTSFI_OV_TD, MISC_BUILTIN_TSTSFI_OV_TD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat128, 0 },
  { P9_BUILTIN_DFP_TSTSFI_OV_DD, MISC_BUILTIN_TSTSFI_OV_DD,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI, RS6000_BTI_dfloat64, 0 },

  { P9V_BUILTIN_VEC_VCTZ, P9V_BUILTIN_VCTZB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0, 0 },
  { P9V_BUILTIN_VEC_VCTZ, P9V_BUILTIN_VCTZB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0, 0 },
  { P9V_BUILTIN_VEC_VCTZ, P9V_BUILTIN_VCTZH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0, 0 },
  { P9V_BUILTIN_VEC_VCTZ, P9V_BUILTIN_VCTZH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0, 0 },
  { P9V_BUILTIN_VEC_VCTZ, P9V_BUILTIN_VCTZW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0, 0 },
  { P9V_BUILTIN_VEC_VCTZ, P9V_BUILTIN_VCTZW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0, 0 },
  { P9V_BUILTIN_VEC_VCTZ, P9V_BUILTIN_VCTZD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0, 0 },
  { P9V_BUILTIN_VEC_VCTZ, P9V_BUILTIN_VCTZD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0, 0 },

  { P9V_BUILTIN_VEC_VCTZB, P9V_BUILTIN_VCTZB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0, 0 },
  { P9V_BUILTIN_VEC_VCTZB, P9V_BUILTIN_VCTZB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0, 0 },

  { P9V_BUILTIN_VEC_VCTZH, P9V_BUILTIN_VCTZH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0, 0 },
  { P9V_BUILTIN_VEC_VCTZH, P9V_BUILTIN_VCTZH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0, 0 },

  { P9V_BUILTIN_VEC_VCTZW, P9V_BUILTIN_VCTZW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0, 0 },
  { P9V_BUILTIN_VEC_VCTZW, P9V_BUILTIN_VCTZW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0, 0 },

  { P9V_BUILTIN_VEC_VCTZD, P9V_BUILTIN_VCTZD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0, 0 },
  { P9V_BUILTIN_VEC_VCTZD, P9V_BUILTIN_VCTZD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0, 0 },

  { P9V_BUILTIN_VEC_VADU, P9V_BUILTIN_VADUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { P9V_BUILTIN_VEC_VADU, P9V_BUILTIN_VADUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },
  { P9V_BUILTIN_VEC_VADU, P9V_BUILTIN_VADUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },

  { P9V_BUILTIN_VEC_VADUB, P9V_BUILTIN_VADUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },

  { P9V_BUILTIN_VEC_VADUH, P9V_BUILTIN_VADUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },

  { P9V_BUILTIN_VEC_VADUW, P9V_BUILTIN_VADUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },

  { P9V_BUILTIN_VEC_VES, P9V_BUILTIN_VESSP,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SF, 0, 0 },
  { P9V_BUILTIN_VEC_VES, P9V_BUILTIN_VESDP,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_V2DF, 0, 0 },

  { P9V_BUILTIN_VEC_VESSP, P9V_BUILTIN_VESSP,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SF, 0, 0 },
  { P9V_BUILTIN_VEC_VESDP, P9V_BUILTIN_VESDP,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_V2DF, 0, 0 },

  { P9V_BUILTIN_VEC_VEE, P9V_BUILTIN_VEESP,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SF, 0, 0 },
  { P9V_BUILTIN_VEC_VEE, P9V_BUILTIN_VEEDP,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_V2DF, 0, 0 },

  { P9V_BUILTIN_VEC_VEESP, P9V_BUILTIN_VEESP,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SF, 0, 0 },
  { P9V_BUILTIN_VEC_VEEDP, P9V_BUILTIN_VEEDP,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_V2DF, 0, 0 },

  { P9V_BUILTIN_VEC_VTDC, P9V_BUILTIN_VTDCSP,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SF, RS6000_BTI_INTSI, 0 },
  { P9V_BUILTIN_VEC_VTDC, P9V_BUILTIN_VTDCDP,
    RS6000_BTI_bool_V2DI, RS6000_BTI_V2DF, RS6000_BTI_INTSI, 0 },

  { P9V_BUILTIN_VEC_VTDCSP, P9V_BUILTIN_VTDCSP,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SF, RS6000_BTI_INTSI, 0 },
  { P9V_BUILTIN_VEC_VTDCDP, P9V_BUILTIN_VTDCDP,
    RS6000_BTI_bool_V2DI, RS6000_BTI_V2DF, RS6000_BTI_INTSI, 0 },

  { P9V_BUILTIN_VEC_VIE, P9V_BUILTIN_VIESP,
    RS6000_BTI_V4SF, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { P9V_BUILTIN_VEC_VIE, P9V_BUILTIN_VIESP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_unsigned_V4SI, 0 },

  { P9V_BUILTIN_VEC_VIE, P9V_BUILTIN_VIEDP,
    RS6000_BTI_V2DF, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { P9V_BUILTIN_VEC_VIE, P9V_BUILTIN_VIEDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_unsigned_V2DI, 0 },

  { P9V_BUILTIN_VEC_VIESP, P9V_BUILTIN_VIESP,
    RS6000_BTI_V4SF, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
  { P9V_BUILTIN_VEC_VIESP, P9V_BUILTIN_VIESP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_unsigned_V4SI, 0 },

  { P9V_BUILTIN_VEC_VIEDP, P9V_BUILTIN_VIEDP,
    RS6000_BTI_V2DF, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { P9V_BUILTIN_VEC_VIEDP, P9V_BUILTIN_VIEDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_unsigned_V2DI, 0 },

  { P9V_BUILTIN_VEC_VSTDC, P9V_BUILTIN_VSTDCSP,
    RS6000_BTI_bool_int, RS6000_BTI_float, RS6000_BTI_INTSI, 0 },
  { P9V_BUILTIN_VEC_VSTDC, P9V_BUILTIN_VSTDCDP,
    RS6000_BTI_bool_int, RS6000_BTI_double, RS6000_BTI_INTSI, 0 },

  { P9V_BUILTIN_VEC_VSTDCSP, P9V_BUILTIN_VSTDCSP,
    RS6000_BTI_bool_int, RS6000_BTI_float, RS6000_BTI_INTSI, 0 },
  { P9V_BUILTIN_VEC_VSTDCDP, P9V_BUILTIN_VSTDCDP,
    RS6000_BTI_bool_int, RS6000_BTI_double, RS6000_BTI_INTSI, 0 },

  { P9V_BUILTIN_VEC_VSTDCN, P9V_BUILTIN_VSTDCNSP,
    RS6000_BTI_bool_int, RS6000_BTI_float, 0, 0 },
  { P9V_BUILTIN_VEC_VSTDCN, P9V_BUILTIN_VSTDCNDP,
    RS6000_BTI_bool_int, RS6000_BTI_double, 0, 0 },

  { P9V_BUILTIN_VEC_VSTDCNSP, P9V_BUILTIN_VSTDCNSP,
    RS6000_BTI_bool_int, RS6000_BTI_float, 0, 0 },
  { P9V_BUILTIN_VEC_VSTDCNDP, P9V_BUILTIN_VSTDCNDP,
    RS6000_BTI_bool_int, RS6000_BTI_double, 0, 0 },

  { P9V_BUILTIN_VEC_VSEEDP, P9V_BUILTIN_VSEEDP,
    RS6000_BTI_UINTSI, RS6000_BTI_double, 0, 0 },

  { P9V_BUILTIN_VEC_VSESDP, P9V_BUILTIN_VSESDP,
    RS6000_BTI_UINTDI, RS6000_BTI_double, 0, 0 },

  { P9V_BUILTIN_VEC_VSIEDP, P9V_BUILTIN_VSIEDP,
    RS6000_BTI_double, RS6000_BTI_UINTDI, RS6000_BTI_UINTDI, 0 },
  { P9V_BUILTIN_VEC_VSIEDP, P9V_BUILTIN_VSIEDPF,
    RS6000_BTI_double, RS6000_BTI_double, RS6000_BTI_UINTDI, 0 },

  { P9V_BUILTIN_VEC_VSCEDPGT, P9V_BUILTIN_VSCEDPGT,
    RS6000_BTI_INTSI, RS6000_BTI_double, RS6000_BTI_double, 0 },
  { P9V_BUILTIN_VEC_VSCEDPLT, P9V_BUILTIN_VSCEDPLT,
    RS6000_BTI_INTSI, RS6000_BTI_double, RS6000_BTI_double, 0 },
  { P9V_BUILTIN_VEC_VSCEDPEQ, P9V_BUILTIN_VSCEDPEQ,
    RS6000_BTI_INTSI, RS6000_BTI_double, RS6000_BTI_double, 0 },
  { P9V_BUILTIN_VEC_VSCEDPUO, P9V_BUILTIN_VSCEDPUO,
    RS6000_BTI_INTSI, RS6000_BTI_double, RS6000_BTI_double, 0 },

  { P9V_BUILTIN_VEC_LXVL, P9V_BUILTIN_LXVL,
    RS6000_BTI_V16QI, ~RS6000_BTI_INTQI,
    RS6000_BTI_unsigned_long_long, 0 },
  { P9V_BUILTIN_VEC_LXVL, P9V_BUILTIN_LXVL,
    RS6000_BTI_unsigned_V16QI, ~RS6000_BTI_UINTQI,
    RS6000_BTI_unsigned_long_long, 0 },

  { P9V_BUILTIN_VEC_LXVL, P9V_BUILTIN_LXVL,
    RS6000_BTI_V4SI, ~RS6000_BTI_INTSI,
    RS6000_BTI_unsigned_long_long, 0 },
  { P9V_BUILTIN_VEC_LXVL, P9V_BUILTIN_LXVL,
    RS6000_BTI_unsigned_V4SI, ~RS6000_BTI_UINTSI,
    RS6000_BTI_unsigned_long_long, 0 },

  { P9V_BUILTIN_VEC_LXVL, P9V_BUILTIN_LXVL,
    RS6000_BTI_V1TI, ~RS6000_BTI_INTTI,
    RS6000_BTI_unsigned_long_long, 0 },
  { P9V_BUILTIN_VEC_LXVL, P9V_BUILTIN_LXVL,
    RS6000_BTI_unsigned_V1TI, ~RS6000_BTI_UINTTI,
    RS6000_BTI_unsigned_long_long, 0 },

  { P9V_BUILTIN_VEC_LXVL, P9V_BUILTIN_LXVL,
    RS6000_BTI_V2DI, ~RS6000_BTI_long_long,
    RS6000_BTI_unsigned_long_long, 0 },
  { P9V_BUILTIN_VEC_LXVL, P9V_BUILTIN_LXVL,
    RS6000_BTI_unsigned_V2DI, ~RS6000_BTI_unsigned_long_long,
    RS6000_BTI_unsigned_long_long, 0 },

  { P9V_BUILTIN_VEC_LXVL, P9V_BUILTIN_LXVL,
    RS6000_BTI_V8HI, ~RS6000_BTI_INTHI,
    RS6000_BTI_unsigned_long_long, 0 },
  { P9V_BUILTIN_VEC_LXVL, P9V_BUILTIN_LXVL,
    RS6000_BTI_unsigned_V8HI, ~RS6000_BTI_UINTHI,
    RS6000_BTI_unsigned_long_long, 0 },

  { P9V_BUILTIN_VEC_LXVL, P9V_BUILTIN_LXVL,
    RS6000_BTI_V2DF, ~RS6000_BTI_double,
    RS6000_BTI_unsigned_long_long, 0 },
  { P9V_BUILTIN_VEC_LXVL, P9V_BUILTIN_LXVL,
    RS6000_BTI_V4SF, ~RS6000_BTI_float,
    RS6000_BTI_unsigned_long_long, 0 },
  /* At an appropriate future time, add support for the
     RS6000_BTI_Float16 (exact name to be determined) type here.  */

  { P9V_BUILTIN_VEC_STXVL, P9V_BUILTIN_STXVL,
    RS6000_BTI_void, RS6000_BTI_V16QI, ~RS6000_BTI_INTQI,
    RS6000_BTI_unsigned_long_long },
  { P9V_BUILTIN_VEC_STXVL, P9V_BUILTIN_STXVL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, ~RS6000_BTI_UINTQI,
    RS6000_BTI_unsigned_long_long },

  { P9V_BUILTIN_VEC_STXVL, P9V_BUILTIN_STXVL,
    RS6000_BTI_void, RS6000_BTI_V4SI, ~RS6000_BTI_INTSI,
    RS6000_BTI_unsigned_long_long },
  { P9V_BUILTIN_VEC_STXVL, P9V_BUILTIN_STXVL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, ~RS6000_BTI_UINTSI,
    RS6000_BTI_unsigned_long_long },

  { P9V_BUILTIN_VEC_STXVL, P9V_BUILTIN_STXVL,
    RS6000_BTI_void, RS6000_BTI_V1TI, ~RS6000_BTI_INTTI,
    RS6000_BTI_unsigned_long_long },
  { P9V_BUILTIN_VEC_STXVL, P9V_BUILTIN_STXVL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V1TI, ~RS6000_BTI_UINTTI,
    RS6000_BTI_unsigned_long_long },

  { P9V_BUILTIN_VEC_STXVL, P9V_BUILTIN_STXVL,
    RS6000_BTI_void, RS6000_BTI_V2DI, ~RS6000_BTI_long_long,
    RS6000_BTI_unsigned_long_long },
  { P9V_BUILTIN_VEC_STXVL, P9V_BUILTIN_STXVL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V2DI, ~RS6000_BTI_unsigned_long_long,
    RS6000_BTI_unsigned_long_long },

  { P9V_BUILTIN_VEC_STXVL, P9V_BUILTIN_STXVL,
    RS6000_BTI_void, RS6000_BTI_V8HI, ~RS6000_BTI_INTHI,
    RS6000_BTI_unsigned_long_long },
  { P9V_BUILTIN_VEC_STXVL, P9V_BUILTIN_STXVL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, ~RS6000_BTI_UINTHI,
    RS6000_BTI_unsigned_long_long },

  { P9V_BUILTIN_VEC_STXVL, P9V_BUILTIN_STXVL,
    RS6000_BTI_void, RS6000_BTI_V2DF, ~RS6000_BTI_double,
    RS6000_BTI_unsigned_long_long },
  { P9V_BUILTIN_VEC_STXVL, P9V_BUILTIN_STXVL,
    RS6000_BTI_void, RS6000_BTI_V4SF, ~RS6000_BTI_float,
    RS6000_BTI_unsigned_long_long },
  /* At an appropriate future time, add support for the
     RS6000_BTI_Float16 (exact name to be determined) type here.  */

  { ALTIVEC_BUILTIN_VEC_CMPNE, P9V_BUILTIN_CMPNEB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI,
    RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPNE, P9V_BUILTIN_CMPNEB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI,
    RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPNE, P9V_BUILTIN_CMPNEB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },

  { ALTIVEC_BUILTIN_VEC_CMPNE, P9V_BUILTIN_CMPNEH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI,
    RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPNE, P9V_BUILTIN_CMPNEH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI,
    RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPNE, P9V_BUILTIN_CMPNEH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },

  { ALTIVEC_BUILTIN_VEC_CMPNE, P9V_BUILTIN_CMPNEW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI,
    RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPNE, P9V_BUILTIN_CMPNEW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI,
    RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPNE, P9V_BUILTIN_CMPNEW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEB_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEB_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_bool_V16QI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEB_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEB_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI,
    RS6000_BTI_V16QI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEB_P,
    RS6000_BTI_INTSI, RS6000_BTI_V16QI,
    RS6000_BTI_bool_V16QI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEB_P,
    RS6000_BTI_INTSI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEB_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI,
    RS6000_BTI_bool_V16QI, 0 },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_bool_V8HI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI,
    RS6000_BTI_V8HI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_V8HI,
    RS6000_BTI_bool_V8HI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI,
    RS6000_BTI_bool_V8HI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_pixel_V8HI,
    RS6000_BTI_pixel_V8HI, 0 },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEW_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEW_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_bool_V4SI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEW_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEW_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI,
    RS6000_BTI_V4SI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEW_P,
    RS6000_BTI_INTSI, RS6000_BTI_V4SI,
    RS6000_BTI_bool_V4SI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEW_P,
    RS6000_BTI_INTSI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEW_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI,
    RS6000_BTI_bool_V4SI, 0 },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNED_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V2DI,
    RS6000_BTI_unsigned_V2DI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNED_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_bool_V2DI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNED_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V2DI, 0
  },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNED_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V2DI,
    RS6000_BTI_V2DI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNED_P,
    RS6000_BTI_INTSI, RS6000_BTI_V2DI,
    RS6000_BTI_bool_V2DI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNED_P,
    RS6000_BTI_INTSI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNED_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V2DI,
    RS6000_BTI_bool_V2DI, 0 },

  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEFP_P,
    RS6000_BTI_INTSI, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { P9V_BUILTIN_VEC_VCMPNE_P, P9V_BUILTIN_VCMPNEDP_P,
    RS6000_BTI_INTSI, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEB_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEB_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_bool_V16QI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEB_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEB_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI,
    RS6000_BTI_V16QI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEB_P,
    RS6000_BTI_INTSI, RS6000_BTI_V16QI,
    RS6000_BTI_bool_V16QI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEB_P,
    RS6000_BTI_INTSI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEB_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI,
    RS6000_BTI_bool_V16QI, 0 },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_bool_V8HI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI,
    RS6000_BTI_V8HI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_V8HI,
    RS6000_BTI_bool_V8HI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI,
    RS6000_BTI_bool_V8HI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEH_P,
    RS6000_BTI_INTSI, RS6000_BTI_pixel_V8HI,
    RS6000_BTI_pixel_V8HI, 0 },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEW_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEW_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_bool_V4SI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEW_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEW_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI,
    RS6000_BTI_V4SI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEW_P,
    RS6000_BTI_INTSI, RS6000_BTI_V4SI,
    RS6000_BTI_bool_V4SI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEW_P,
    RS6000_BTI_INTSI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEW_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI,
    RS6000_BTI_bool_V4SI, 0 },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAED_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V2DI,
    RS6000_BTI_unsigned_V2DI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAED_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_bool_V2DI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAED_P,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V2DI, 0
  },

  /* The following 2 entries have been deprecated.  */
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAED_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V2DI,
    RS6000_BTI_V2DI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAED_P,
    RS6000_BTI_INTSI, RS6000_BTI_V2DI,
    RS6000_BTI_bool_V2DI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAED_P,
    RS6000_BTI_INTSI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAED_P,
    RS6000_BTI_INTSI, RS6000_BTI_bool_V2DI,
    RS6000_BTI_bool_V2DI, 0 },

  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEFP_P,
    RS6000_BTI_INTSI, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { P9V_BUILTIN_VEC_VCMPAE_P, P9V_BUILTIN_VCMPAEDP_P,
    RS6000_BTI_INTSI, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },

  { P9V_BUILTIN_VEC_VCMPNEZ_P, P9V_BUILTIN_VCMPNEZB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI },
  { P9V_BUILTIN_VEC_VCMPNEZ_P, P9V_BUILTIN_VCMPNEZB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V16QI, RS6000_BTI_V16QI },

  { P9V_BUILTIN_VEC_VCMPNEZ_P, P9V_BUILTIN_VCMPNEZH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI },
  { P9V_BUILTIN_VEC_VCMPNEZ_P, P9V_BUILTIN_VCMPNEZH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V8HI, RS6000_BTI_V8HI },

  { P9V_BUILTIN_VEC_VCMPNEZ_P, P9V_BUILTIN_VCMPNEZW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI },
  { P9V_BUILTIN_VEC_VCMPNEZ_P, P9V_BUILTIN_VCMPNEZW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SI, RS6000_BTI_V4SI },

  { P9V_BUILTIN_VEC_CMPNEZ, P9V_BUILTIN_CMPNEZB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI,
    RS6000_BTI_V16QI, 0 },
  { P9V_BUILTIN_VEC_CMPNEZ, P9V_BUILTIN_CMPNEZB,
    RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },

  { P9V_BUILTIN_VEC_CMPNEZ, P9V_BUILTIN_CMPNEZH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI,
    RS6000_BTI_V8HI, 0 },
  { P9V_BUILTIN_VEC_CMPNEZ, P9V_BUILTIN_CMPNEZH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },

  { P9V_BUILTIN_VEC_CMPNEZ, P9V_BUILTIN_CMPNEZW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI,
    RS6000_BTI_V4SI, 0 },
  { P9V_BUILTIN_VEC_CMPNEZ, P9V_BUILTIN_CMPNEZW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },

  { P9V_BUILTIN_VEC_VCLZLSBB, P9V_BUILTIN_VCLZLSBB,
    RS6000_BTI_INTSI, RS6000_BTI_V16QI, 0, 0 },
  { P9V_BUILTIN_VEC_VCLZLSBB, P9V_BUILTIN_VCLZLSBB,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI, 0, 0 },

  { P9V_BUILTIN_VEC_VCTZLSBB, P9V_BUILTIN_VCTZLSBB,
    RS6000_BTI_INTSI, RS6000_BTI_V16QI, 0, 0 },
  { P9V_BUILTIN_VEC_VCTZLSBB, P9V_BUILTIN_VCTZLSBB,
    RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI, 0, 0 },

  { P9V_BUILTIN_VEC_VEXTRACT4B, P9V_BUILTIN_VEXTRACT4B,
    RS6000_BTI_INTDI, RS6000_BTI_V16QI, RS6000_BTI_UINTSI, 0 },
  { P9V_BUILTIN_VEC_VEXTRACT4B, P9V_BUILTIN_VEXTRACT4B,
    RS6000_BTI_INTDI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_UINTSI, 0 },

  { P9V_BUILTIN_VEC_VEXTULX, P9V_BUILTIN_VEXTUBLX,
    RS6000_BTI_INTQI, RS6000_BTI_UINTSI,
    RS6000_BTI_V16QI, 0 },
  { P9V_BUILTIN_VEC_VEXTULX, P9V_BUILTIN_VEXTUBLX,
    RS6000_BTI_UINTQI, RS6000_BTI_UINTSI,
    RS6000_BTI_unsigned_V16QI, 0 },

  { P9V_BUILTIN_VEC_VEXTULX, P9V_BUILTIN_VEXTUHLX,
    RS6000_BTI_INTHI, RS6000_BTI_UINTSI,
    RS6000_BTI_V8HI, 0 },
  { P9V_BUILTIN_VEC_VEXTULX, P9V_BUILTIN_VEXTUHLX,
    RS6000_BTI_UINTHI, RS6000_BTI_UINTSI,
    RS6000_BTI_unsigned_V8HI, 0 },

  { P9V_BUILTIN_VEC_VEXTULX, P9V_BUILTIN_VEXTUWLX,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI,
    RS6000_BTI_V4SI, 0 },
  { P9V_BUILTIN_VEC_VEXTULX, P9V_BUILTIN_VEXTUWLX,
    RS6000_BTI_UINTSI, RS6000_BTI_UINTSI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { P9V_BUILTIN_VEC_VEXTULX, P9V_BUILTIN_VEXTUWLX,
    RS6000_BTI_float, RS6000_BTI_UINTSI,
    RS6000_BTI_V4SF, 0 },

  { P9V_BUILTIN_VEC_VEXTURX, P9V_BUILTIN_VEXTUBRX,
    RS6000_BTI_INTQI, RS6000_BTI_UINTSI,
    RS6000_BTI_V16QI, 0 },
  { P9V_BUILTIN_VEC_VEXTURX, P9V_BUILTIN_VEXTUBRX,
    RS6000_BTI_UINTQI, RS6000_BTI_UINTSI,
    RS6000_BTI_unsigned_V16QI, 0 },

  { P9V_BUILTIN_VEC_VEXTURX, P9V_BUILTIN_VEXTUHRX,
    RS6000_BTI_INTHI, RS6000_BTI_UINTSI,
    RS6000_BTI_V8HI, 0 },
  { P9V_BUILTIN_VEC_VEXTURX, P9V_BUILTIN_VEXTUHRX,
    RS6000_BTI_UINTHI, RS6000_BTI_UINTSI,
    RS6000_BTI_unsigned_V8HI, 0 },

  { P9V_BUILTIN_VEC_VEXTURX, P9V_BUILTIN_VEXTUWRX,
    RS6000_BTI_INTSI, RS6000_BTI_UINTSI,
    RS6000_BTI_V4SI, 0 },
  { P9V_BUILTIN_VEC_VEXTURX, P9V_BUILTIN_VEXTUWRX,
    RS6000_BTI_UINTSI, RS6000_BTI_UINTSI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { P9V_BUILTIN_VEC_VEXTURX, P9V_BUILTIN_VEXTUWRX,
    RS6000_BTI_float, RS6000_BTI_UINTSI,
    RS6000_BTI_V4SF, 0 },

  { P8V_BUILTIN_VEC_VGBBD, P8V_BUILTIN_VGBBD,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0, 0 },
  { P8V_BUILTIN_VEC_VGBBD, P8V_BUILTIN_VGBBD,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0, 0 },

  { P9V_BUILTIN_VEC_VINSERT4B, P9V_BUILTIN_VINSERT4B,
    RS6000_BTI_V16QI, RS6000_BTI_V4SI,
    RS6000_BTI_V16QI, RS6000_BTI_UINTSI },
  { P9V_BUILTIN_VEC_VINSERT4B, P9V_BUILTIN_VINSERT4B,
    RS6000_BTI_V16QI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_V16QI, RS6000_BTI_UINTSI },
  { P9V_BUILTIN_VEC_VINSERT4B, P9V_BUILTIN_VINSERT4B,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_UINTSI },
  { P9V_BUILTIN_VEC_VINSERT4B, P9V_BUILTIN_VINSERT4B_DI,
    RS6000_BTI_V16QI, RS6000_BTI_INTDI,
    RS6000_BTI_V16QI, RS6000_BTI_UINTDI },
  { P9V_BUILTIN_VEC_VINSERT4B, P9V_BUILTIN_VINSERT4B_DI,
    RS6000_BTI_V16QI, RS6000_BTI_UINTDI,
    RS6000_BTI_V16QI, RS6000_BTI_UINTDI },
  { P9V_BUILTIN_VEC_VINSERT4B, P9V_BUILTIN_VINSERT4B_DI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTDI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_UINTDI },
  { P9V_BUILTIN_VEC_VINSERT4B, P9V_BUILTIN_VINSERT4B_DI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_UINTDI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_UINTDI },

  { P8V_BUILTIN_VEC_VADDECUQ, P8V_BUILTIN_VADDECUQ,
    RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI },
  { P8V_BUILTIN_VEC_VADDECUQ, P8V_BUILTIN_VADDECUQ,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI },

  { P8V_BUILTIN_VEC_VADDEUQM, P8V_BUILTIN_VADDEUQM,
    RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI },
  { P8V_BUILTIN_VEC_VADDEUQM, P8V_BUILTIN_VADDEUQM,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI },

  { P8V_BUILTIN_VEC_VSUBECUQ, P8V_BUILTIN_VSUBECUQ,
    RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI },
  { P8V_BUILTIN_VEC_VSUBECUQ, P8V_BUILTIN_VSUBECUQ,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI },

  { P8V_BUILTIN_VEC_VSUBEUQM, P8V_BUILTIN_VSUBEUQM,
    RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI },
  { P8V_BUILTIN_VEC_VSUBEUQM, P8V_BUILTIN_VSUBEUQM,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI },

  { P8V_BUILTIN_VEC_VMINSD, P8V_BUILTIN_VMINSD,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { P8V_BUILTIN_VEC_VMINSD, P8V_BUILTIN_VMINSD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_VMINSD, P8V_BUILTIN_VMINSD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },

  { P8V_BUILTIN_VEC_VMAXSD, P8V_BUILTIN_VMAXSD,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { P8V_BUILTIN_VEC_VMAXSD, P8V_BUILTIN_VMAXSD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_VMAXSD, P8V_BUILTIN_VMAXSD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },

  { P8V_BUILTIN_VEC_VMINUD, P8V_BUILTIN_VMINUD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI,
    RS6000_BTI_unsigned_V2DI, 0 },
  { P8V_BUILTIN_VEC_VMINUD, P8V_BUILTIN_VMINUD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_VMINUD, P8V_BUILTIN_VMINUD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V2DI, 0 },

  { P8V_BUILTIN_VEC_VMAXUD, P8V_BUILTIN_VMAXUD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI,
    RS6000_BTI_unsigned_V2DI, 0 },
  { P8V_BUILTIN_VEC_VMAXUD, P8V_BUILTIN_VMAXUD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_VMAXUD, P8V_BUILTIN_VMAXUD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V2DI, 0 },

  { P8V_BUILTIN_VEC_VMRGEW, P8V_BUILTIN_VMRGEW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { P8V_BUILTIN_VEC_VMRGEW, P8V_BUILTIN_VMRGEW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { P8V_BUILTIN_VEC_VMRGEW, P8V_BUILTIN_VMRGEW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },

  { P8V_BUILTIN_VEC_VMRGOW, P8V_BUILTIN_VMRGOW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { P8V_BUILTIN_VEC_VMRGOW, P8V_BUILTIN_VMRGOW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { P8V_BUILTIN_VEC_VMRGOW, P8V_BUILTIN_VMRGOW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI, 0 },

  { P8V_BUILTIN_VEC_VPMSUM, P8V_BUILTIN_VPMSUMB,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { P8V_BUILTIN_VEC_VPMSUM, P8V_BUILTIN_VPMSUMH,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },
  { P8V_BUILTIN_VEC_VPMSUM, P8V_BUILTIN_VPMSUMW,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { P8V_BUILTIN_VEC_VPMSUM, P8V_BUILTIN_VPMSUMD,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V2DI, 0 },

  { P8V_BUILTIN_VEC_VPOPCNT, P8V_BUILTIN_VPOPCNTB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0, 0 },
  { P8V_BUILTIN_VEC_VPOPCNT, P8V_BUILTIN_VPOPCNTB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0, 0 },
  { P8V_BUILTIN_VEC_VPOPCNT, P8V_BUILTIN_VPOPCNTH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0, 0 },
  { P8V_BUILTIN_VEC_VPOPCNT, P8V_BUILTIN_VPOPCNTH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0, 0 },
  { P8V_BUILTIN_VEC_VPOPCNT, P8V_BUILTIN_VPOPCNTW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0, 0 },
  { P8V_BUILTIN_VEC_VPOPCNT, P8V_BUILTIN_VPOPCNTW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0, 0 },
  { P8V_BUILTIN_VEC_VPOPCNT, P8V_BUILTIN_VPOPCNTD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0, 0 },
  { P8V_BUILTIN_VEC_VPOPCNT, P8V_BUILTIN_VPOPCNTD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0, 0 },

  { P8V_BUILTIN_VEC_VPOPCNTB, P8V_BUILTIN_VPOPCNTB,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0, 0 },
  { P8V_BUILTIN_VEC_VPOPCNTB, P8V_BUILTIN_VPOPCNTB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0, 0 },

  { P8V_BUILTIN_VEC_VPOPCNTU, P8V_BUILTIN_VPOPCNTUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_V16QI, 0, 0 },
  { P8V_BUILTIN_VEC_VPOPCNTU, P8V_BUILTIN_VPOPCNTUB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0, 0 },

  { P8V_BUILTIN_VEC_VPOPCNTU, P8V_BUILTIN_VPOPCNTUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_V8HI, 0, 0 },
  { P8V_BUILTIN_VEC_VPOPCNTU, P8V_BUILTIN_VPOPCNTUH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0, 0 },

  { P8V_BUILTIN_VEC_VPOPCNTU, P8V_BUILTIN_VPOPCNTUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SI, 0, 0 },
  { P8V_BUILTIN_VEC_VPOPCNTU, P8V_BUILTIN_VPOPCNTUW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0, 0 },

  { P8V_BUILTIN_VEC_VPOPCNTU, P8V_BUILTIN_VPOPCNTUD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_V2DI, 0, 0 },
  { P8V_BUILTIN_VEC_VPOPCNTU, P8V_BUILTIN_VPOPCNTUD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0, 0 },

  { P8V_BUILTIN_VEC_VPOPCNTH, P8V_BUILTIN_VPOPCNTH,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0, 0 },
  { P8V_BUILTIN_VEC_VPOPCNTH, P8V_BUILTIN_VPOPCNTH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0, 0 },

  { P8V_BUILTIN_VEC_VPOPCNTW, P8V_BUILTIN_VPOPCNTW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0, 0 },
  { P8V_BUILTIN_VEC_VPOPCNTW, P8V_BUILTIN_VPOPCNTW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0, 0 },

  { P8V_BUILTIN_VEC_VPOPCNTD, P8V_BUILTIN_VPOPCNTD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0, 0 },
  { P8V_BUILTIN_VEC_VPOPCNTD, P8V_BUILTIN_VPOPCNTD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0, 0 },

  { P9V_BUILTIN_VEC_VPRTYB, P9V_BUILTIN_VPRTYBW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0, 0 },
  { P9V_BUILTIN_VEC_VPRTYB, P9V_BUILTIN_VPRTYBW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0, 0 },
  { P9V_BUILTIN_VEC_VPRTYB, P9V_BUILTIN_VPRTYBD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0, 0 },
  { P9V_BUILTIN_VEC_VPRTYB, P9V_BUILTIN_VPRTYBD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0, 0 },
  { P9V_BUILTIN_VEC_VPRTYB, P9V_BUILTIN_VPRTYBQ,
    RS6000_BTI_V1TI, RS6000_BTI_V1TI, 0, 0 },
  { P9V_BUILTIN_VEC_VPRTYB, P9V_BUILTIN_VPRTYBQ,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI, 0, 0 },
  { P9V_BUILTIN_VEC_VPRTYB, P9V_BUILTIN_VPRTYBQ,
    RS6000_BTI_INTTI, RS6000_BTI_INTTI, 0, 0 },
  { P9V_BUILTIN_VEC_VPRTYB, P9V_BUILTIN_VPRTYBQ,
    RS6000_BTI_UINTTI, RS6000_BTI_UINTTI, 0, 0 },

  { P9V_BUILTIN_VEC_VPRTYBW, P9V_BUILTIN_VPRTYBW,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0, 0 },
  { P9V_BUILTIN_VEC_VPRTYBW, P9V_BUILTIN_VPRTYBW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0, 0 },

  { P9V_BUILTIN_VEC_VPRTYBD, P9V_BUILTIN_VPRTYBD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0, 0 },
  { P9V_BUILTIN_VEC_VPRTYBD, P9V_BUILTIN_VPRTYBD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0, 0 },

  { P9V_BUILTIN_VEC_VPRTYBQ, P9V_BUILTIN_VPRTYBQ,
    RS6000_BTI_V1TI, RS6000_BTI_V1TI, 0, 0 },
  { P9V_BUILTIN_VEC_VPRTYBQ, P9V_BUILTIN_VPRTYBQ,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI, 0, 0 },
  { P9V_BUILTIN_VEC_VPRTYBQ, P9V_BUILTIN_VPRTYBQ,
    RS6000_BTI_INTTI, RS6000_BTI_INTTI, 0, 0 },
  { P9V_BUILTIN_VEC_VPRTYBQ, P9V_BUILTIN_VPRTYBQ,
    RS6000_BTI_UINTTI, RS6000_BTI_UINTTI, 0, 0 },

  { P9_BUILTIN_CMPRB, P9_BUILTIN_SCALAR_CMPRB,
    RS6000_BTI_INTSI, RS6000_BTI_UINTQI, RS6000_BTI_UINTSI, 0 },
  { P9_BUILTIN_CMPRB2, P9_BUILTIN_SCALAR_CMPRB2,
    RS6000_BTI_INTSI, RS6000_BTI_UINTQI, RS6000_BTI_UINTSI, 0 },
  { P9_BUILTIN_CMPEQB, P9_BUILTIN_SCALAR_CMPEQB,
    RS6000_BTI_INTSI, RS6000_BTI_UINTQI, RS6000_BTI_UINTDI, 0 },

  { P8V_BUILTIN_VEC_VPKUDUM, P8V_BUILTIN_VPKUDUM,
    RS6000_BTI_V4SI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { P8V_BUILTIN_VEC_VPKUDUM, P8V_BUILTIN_VPKUDUM,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { P8V_BUILTIN_VEC_VPKUDUM, P8V_BUILTIN_VPKUDUM,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V2DI, 0 },

  { P8V_BUILTIN_VEC_VPKSDSS, P8V_BUILTIN_VPKSDSS,
    RS6000_BTI_V4SI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },

  { P8V_BUILTIN_VEC_VPKUDUS, P8V_BUILTIN_VPKUDUS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },

  { P8V_BUILTIN_VEC_VPKSDUS, P8V_BUILTIN_VPKSDUS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },

  { P8V_BUILTIN_VEC_VRLD, P8V_BUILTIN_VRLD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { P8V_BUILTIN_VEC_VRLD, P8V_BUILTIN_VRLD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },

  { P8V_BUILTIN_VEC_VSLD, P8V_BUILTIN_VSLD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { P8V_BUILTIN_VEC_VSLD, P8V_BUILTIN_VSLD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },

  { P8V_BUILTIN_VEC_VSRD, P8V_BUILTIN_VSRD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { P8V_BUILTIN_VEC_VSRD, P8V_BUILTIN_VSRD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },

  { P8V_BUILTIN_VEC_VSRAD, P8V_BUILTIN_VSRAD,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { P8V_BUILTIN_VEC_VSRAD, P8V_BUILTIN_VSRAD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },

  { P8V_BUILTIN_VEC_VSUBCUQ, P8V_BUILTIN_VSUBCUQ,
    RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI, 0 },
  { P8V_BUILTIN_VEC_VSUBCUQ, P8V_BUILTIN_VSUBCUQ,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI,
    RS6000_BTI_unsigned_V1TI, 0 },

  { P8V_BUILTIN_VEC_VSUBUDM, P8V_BUILTIN_VSUBUDM,
    RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_V2DI, 0 },
  { P8V_BUILTIN_VEC_VSUBUDM, P8V_BUILTIN_VSUBUDM,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_VSUBUDM, P8V_BUILTIN_VSUBUDM,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0 },
  { P8V_BUILTIN_VEC_VSUBUDM, P8V_BUILTIN_VSUBUDM,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, RS6000_BTI_unsigned_V2DI, 0 },
  { P8V_BUILTIN_VEC_VSUBUDM, P8V_BUILTIN_VSUBUDM,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_bool_V2DI, 0 },
  { P8V_BUILTIN_VEC_VSUBUDM, P8V_BUILTIN_VSUBUDM,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0 },

  { P8V_BUILTIN_VEC_VSUBUQM, P8V_BUILTIN_VSUBUQM,
    RS6000_BTI_V1TI, RS6000_BTI_V1TI, RS6000_BTI_V1TI, 0 },
  { P8V_BUILTIN_VEC_VSUBUQM, P8V_BUILTIN_VSUBUQM,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI,
    RS6000_BTI_unsigned_V1TI, 0 },

  { P6_OV_BUILTIN_CMPB, P6_BUILTIN_CMPB_32,
    RS6000_BTI_UINTSI, RS6000_BTI_UINTSI, RS6000_BTI_UINTSI, 0 },
  { P6_OV_BUILTIN_CMPB, P6_BUILTIN_CMPB,
    RS6000_BTI_UINTDI, RS6000_BTI_UINTDI, RS6000_BTI_UINTDI, 0 },

  { P8V_BUILTIN_VEC_VUPKHSW, P8V_BUILTIN_VUPKHSW,
    RS6000_BTI_V2DI, RS6000_BTI_V4SI, 0, 0 },
  { P8V_BUILTIN_VEC_VUPKHSW, P8V_BUILTIN_VUPKHSW,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V4SI, 0, 0 },

  { P8V_BUILTIN_VEC_VUPKLSW, P8V_BUILTIN_VUPKLSW,
    RS6000_BTI_V2DI, RS6000_BTI_V4SI, 0, 0 },
  { P8V_BUILTIN_VEC_VUPKLSW, P8V_BUILTIN_VUPKLSW,
    RS6000_BTI_bool_V2DI, RS6000_BTI_bool_V4SI, 0, 0 },

  { P9V_BUILTIN_VEC_VSLV, P9V_BUILTIN_VSLV,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { P9V_BUILTIN_VEC_VSRV, P9V_BUILTIN_VSRV,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },

  { P9V_BUILTIN_VEC_REVB, P9V_BUILTIN_XXBRQ_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0, 0 },
  { P9V_BUILTIN_VEC_REVB, P9V_BUILTIN_XXBRQ_V16QI,
    RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0, 0 },
  { P9V_BUILTIN_VEC_REVB, P9V_BUILTIN_XXBRQ_V1TI,
    RS6000_BTI_unsigned_V1TI, RS6000_BTI_unsigned_V1TI, 0, 0 },
  { P9V_BUILTIN_VEC_REVB, P9V_BUILTIN_XXBRQ_V1TI,
    RS6000_BTI_V1TI, RS6000_BTI_V1TI, 0, 0 },
  { P9V_BUILTIN_VEC_REVB, P9V_BUILTIN_XXBRD_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI, 0, 0 },
  { P9V_BUILTIN_VEC_REVB, P9V_BUILTIN_XXBRD_V2DI,
    RS6000_BTI_V2DI, RS6000_BTI_V2DI, 0, 0 },
  { P9V_BUILTIN_VEC_REVB, P9V_BUILTIN_XXBRD_V2DF,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0, 0 },
  { P9V_BUILTIN_VEC_REVB, P9V_BUILTIN_XXBRW_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0, 0 },
  { P9V_BUILTIN_VEC_REVB, P9V_BUILTIN_XXBRW_V4SI,
    RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0, 0 },
  { P9V_BUILTIN_VEC_REVB, P9V_BUILTIN_XXBRW_V4SF,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { P9V_BUILTIN_VEC_REVB, P9V_BUILTIN_XXBRH_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0, 0 },
  { P9V_BUILTIN_VEC_REVB, P9V_BUILTIN_XXBRH_V8HI,
    RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0, 0 },

  /* Crypto builtins.  */
  { CRYPTO_BUILTIN_VPERMXOR, CRYPTO_BUILTIN_VPERMXOR_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI },
  { CRYPTO_BUILTIN_VPERMXOR, CRYPTO_BUILTIN_VPERMXOR_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI },
  { CRYPTO_BUILTIN_VPERMXOR, CRYPTO_BUILTIN_VPERMXOR_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI },
  { CRYPTO_BUILTIN_VPERMXOR, CRYPTO_BUILTIN_VPERMXOR_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI },

  { CRYPTO_BUILTIN_VPMSUM, CRYPTO_BUILTIN_VPMSUMB,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI,
    RS6000_BTI_unsigned_V16QI, 0 },
  { CRYPTO_BUILTIN_VPMSUM, CRYPTO_BUILTIN_VPMSUMH,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI,
    RS6000_BTI_unsigned_V8HI, 0 },
  { CRYPTO_BUILTIN_VPMSUM, CRYPTO_BUILTIN_VPMSUMW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_unsigned_V4SI, 0 },
  { CRYPTO_BUILTIN_VPMSUM, CRYPTO_BUILTIN_VPMSUMD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_unsigned_V2DI, 0 },

  { CRYPTO_BUILTIN_VSHASIGMA, CRYPTO_BUILTIN_VSHASIGMAW,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI },
  { CRYPTO_BUILTIN_VSHASIGMA, CRYPTO_BUILTIN_VSHASIGMAD,
    RS6000_BTI_unsigned_V2DI, RS6000_BTI_unsigned_V2DI,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI },

  { (enum rs6000_builtins) 0, (enum rs6000_builtins) 0, 0, 0, 0, 0 }
};


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

/* Check whether the type of an argument, T, is compatible with a
   type ID stored into a struct altivec_builtin_types.  Integer
   types are considered compatible; otherwise, the language hook
   lang_hooks.types_compatible_p makes the decision.  */

static inline bool
rs6000_builtin_type_compatible (tree t, int id)
{
  tree builtin_type;
  builtin_type = rs6000_builtin_type (id);
  if (t == error_mark_node)
    return false;
  if (INTEGRAL_TYPE_P (t) && INTEGRAL_TYPE_P (builtin_type))
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
  if (desc->code == ALTIVEC_BUILTIN_VEC_VCMPGE_P
      && desc->overloaded_code != ALTIVEC_BUILTIN_VCMPGEFP_P
      && desc->overloaded_code != VSX_BUILTIN_XVCMPGEDP_P)
    {
      tree t;
      t = args[2], args[2] = args[1], args[1] = t;
      t = arg_type[2], arg_type[2] = arg_type[1], arg_type[1] = t;
      
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
    = (enum rs6000_builtins)DECL_FUNCTION_CODE (fndecl);
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
  if (fcode == ALTIVEC_BUILTIN_VEC_LVSL && !VECTOR_ELT_ORDER_BIG)
    warning (OPT_Wdeprecated,
	     "vec_lvsl is deprecated for little endian; use "
	     "assignment for unaligned loads and stores");
  else if (fcode == ALTIVEC_BUILTIN_VEC_LVSR && !VECTOR_ELT_ORDER_BIG)
    warning (OPT_Wdeprecated,
	     "vec_lvsr is deprecated for little endian; use "
	     "assignment for unaligned loads and stores");

  if (fcode == ALTIVEC_BUILTIN_VEC_MUL)
    {
      /* vec_mul needs to be special cased because there are no instructions
	 for it for the {un}signed char, {un}signed short, and {un}signed int
	 types.  */
      if (nargs != 2)
	{
	  error ("vec_mul only accepts 2 arguments");
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
	  error ("vec_cmpne only accepts 2 arguments");
	  return error_mark_node;
	}

      tree arg0 = (*arglist)[0];
      tree arg0_type = TREE_TYPE (arg0);
      tree arg1 = (*arglist)[1];
      tree arg1_type = TREE_TYPE (arg1);

      /* Power9 instructions provide the most efficient implementation of
	 ALTIVEC_BUILTIN_VEC_CMPNE if the mode is not DImode or TImode
	 or SFmode or DFmode.  */
      if (!TARGET_P9_VECTOR
	  || (TYPE_MODE (TREE_TYPE (arg0_type)) == DImode)
	  || (TYPE_MODE (TREE_TYPE (arg0_type)) == TImode)
	  || (TYPE_MODE (TREE_TYPE (arg0_type)) == SFmode)
	  || (TYPE_MODE (TREE_TYPE (arg0_type)) == DFmode))
	{
	  /* Both arguments must be vectors and the types must be compatible.  */
	  if (TREE_CODE (arg0_type) != VECTOR_TYPE)
	    goto bad;
	  if (!lang_hooks.types_compatible_p (arg0_type, arg1_type))
	    goto bad;

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

  if (fcode == ALTIVEC_BUILTIN_VEC_ADDE)
    {
      /* vec_adde needs to be special cased because there is no instruction
	  for the {un}signed int version.  */
      if (nargs != 3)
	{
	  error ("vec_adde only accepts 3 arguments");
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
      if (!lang_hooks.types_compatible_p (arg0_type, arg1_type) ||
	  !lang_hooks.types_compatible_p (arg1_type, arg2_type))
	goto bad;

      switch (TYPE_MODE (TREE_TYPE (arg0_type)))
	{
	  /* For {un}signed ints,
	     vec_adde (va, vb, carryv) == vec_add (vec_add (va, vb),
						   vec_and (carryv, 0x1)).  */
	  case E_SImode:
	    {
	      vec<tree, va_gc> *params = make_tree_vector ();
	      vec_safe_push (params, arg0);
	      vec_safe_push (params, arg1);
	      tree add_builtin = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_ADD];
	      tree call = altivec_resolve_overloaded_builtin (loc, add_builtin,
							      params);
	      tree const1 = build_int_cstu (TREE_TYPE (arg0_type), 1);
	      tree ones_vector = build_vector_from_val (arg0_type, const1);
	      tree and_expr = fold_build2_loc (loc, BIT_AND_EXPR, arg0_type,
					       arg2, ones_vector);
	      params = make_tree_vector ();
	      vec_safe_push (params, call);
	      vec_safe_push (params, and_expr);
	      return altivec_resolve_overloaded_builtin (loc, add_builtin,
							 params);
	    }
	  /* For {un}signed __int128s use the vaddeuqm instruction
		directly.  */
	  case E_TImode:
	    {
	      tree adde_bii = rs6000_builtin_decls[P8V_BUILTIN_VEC_VADDEUQM];
	      return altivec_resolve_overloaded_builtin (loc, adde_bii,
							 arglist);
	    }

	  /* Types other than {un}signed int and {un}signed __int128
		are errors.  */
	  default:
	    goto bad;
	}
    }

  if (fcode == ALTIVEC_BUILTIN_VEC_ADDEC)
    {
      /* vec_addec needs to be special cased because there is no instruction
	for the {un}signed int version.  */
      if (nargs != 3)
	{
	  error ("vec_addec only accepts 3 arguments");
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
      if (!lang_hooks.types_compatible_p (arg0_type, arg1_type) ||
	  !lang_hooks.types_compatible_p (arg1_type, arg2_type))
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
	    arg0 = save_expr (arg0);
	    arg1 = save_expr (arg1);
	    vec<tree, va_gc> *params = make_tree_vector ();
	    vec_safe_push (params, arg0);
	    vec_safe_push (params, arg1);
	    tree addc_builtin = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_ADDC];
	    tree call1 = altivec_resolve_overloaded_builtin (loc, addc_builtin,
							     params);
	    params = make_tree_vector ();
	    vec_safe_push (params, arg0);
	    vec_safe_push (params, arg1);
	    tree add_builtin = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_ADD];
	    tree call2 = altivec_resolve_overloaded_builtin (loc, add_builtin,
							     params);
	    tree const1 = build_int_cstu (TREE_TYPE (arg0_type), 1);
	    tree ones_vector = build_vector_from_val (arg0_type, const1);
	    tree and_expr = fold_build2_loc (loc, BIT_AND_EXPR, arg0_type,
					     arg2, ones_vector);
	    params = make_tree_vector ();
	    vec_safe_push (params, call2);
	    vec_safe_push (params, and_expr);
	    call2 = altivec_resolve_overloaded_builtin (loc, addc_builtin,
							params);
	    params = make_tree_vector ();
	    vec_safe_push (params, call1);
	    vec_safe_push (params, call2);
	    tree or_builtin = rs6000_builtin_decls[ALTIVEC_BUILTIN_VEC_OR];
	    return altivec_resolve_overloaded_builtin (loc, or_builtin,
						       params);
	    }
	  /* For {un}signed __int128s use the vaddecuq instruction.  */
	  case E_TImode:
	    {
	    tree VADDECUQ_bii = rs6000_builtin_decls[P8V_BUILTIN_VEC_VADDECUQ];
	    return altivec_resolve_overloaded_builtin (loc, VADDECUQ_bii,
						       arglist);
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

      if (nargs == 0)
	{
	  error ("%s only accepts %d arguments", name, (fcode == ALTIVEC_BUILTIN_VEC_PROMOTE)+1 );
	  return error_mark_node;
	}
      if (fcode == ALTIVEC_BUILTIN_VEC_SPLATS && nargs != 1)
	{
	  error ("%s only accepts 1 argument", name);
	  return error_mark_node;
	}
      if (fcode == ALTIVEC_BUILTIN_VEC_PROMOTE && nargs != 2)
	{
	  error ("%s only accepts 2 arguments", name);
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
	  error ("vec_extract only accepts 2 arguments");
	  return error_mark_node;
	}

      arg2 = (*arglist)[1];
      arg1 = (*arglist)[0];
      arg1_type = TREE_TYPE (arg1);

      if (TREE_CODE (arg1_type) != VECTOR_TYPE)
	goto bad;
      if (!INTEGRAL_TYPE_P (TREE_TYPE (arg2)))
	goto bad;

      /* If we are targeting little-endian, but -maltivec=be has been
	 specified to override the element order, adjust the element
	 number accordingly.  */
      if (!BYTES_BIG_ENDIAN && rs6000_altivec_element_order == 2)
	{
	  unsigned int last_elem = TYPE_VECTOR_SUBPARTS (arg1_type) - 1;
	  arg2 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (arg2),
				  build_int_cstu (TREE_TYPE (arg2), last_elem),
				  arg2);
	}

      /* See if we can optimize vec_extracts with the current VSX instruction
	 set.  */
      mode = TYPE_MODE (arg1_type);
      if (VECTOR_MEM_VSX_P (mode))

	{
	  tree call = NULL_TREE;
	  int nunits = GET_MODE_NUNITS (mode);

	  /* If the second argument is an integer constant, if the value is in
	     the expected range, generate the built-in code if we can.  We need
	     64-bit and direct move to extract the small integer vectors.  */
	  if (TREE_CODE (arg2) == INTEGER_CST
	      && wi::ltu_p (wi::to_wide (arg2), nunits))
	    {
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
	    return build_call_expr (call, 2, arg1, arg2);
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
	  error ("vec_insert only accepts 3 arguments");
	  return error_mark_node;
	}

      arg0 = (*arglist)[0];
      arg1 = (*arglist)[1];
      arg1_type = TREE_TYPE (arg1);
      arg2 = (*arglist)[2];

      if (TREE_CODE (arg1_type) != VECTOR_TYPE)
	goto bad;
      if (!INTEGRAL_TYPE_P (TREE_TYPE (arg2)))
	goto bad;

      /* If we are targeting little-endian, but -maltivec=be has been
	 specified to override the element order, adjust the element
	 number accordingly.  */
      if (!BYTES_BIG_ENDIAN && rs6000_altivec_element_order == 2)
	{
	  unsigned int last_elem = TYPE_VECTOR_SUBPARTS (arg1_type) - 1;
	  arg2 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (arg2),
				  build_int_cstu (TREE_TYPE (arg2), last_elem),
				  arg2);
	}

      /* If we can use the VSX xxpermdi instruction, use that for insert.  */
      mode = TYPE_MODE (arg1_type);
      if ((mode == V2DFmode || mode == V2DImode) && VECTOR_UNIT_VSX_P (mode)
	  && TREE_CODE (arg2) == INTEGER_CST
	  && wi::ltu_p (wi::to_wide (arg2), 2))
	{
	  tree call = NULL_TREE;

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
	       && TREE_CODE (arg2) == INTEGER_CST
	       && wi::eq_p (wi::to_wide (arg2), 0))
	{
	  tree call = rs6000_builtin_decls[VSX_BUILTIN_VEC_SET_V1TI];

	  /* Note, __builtin_vec_insert_<xxx> has vector and scalar types
	     reversed.  */
	  return build_call_expr (call, 3, arg1, arg0, arg2);
	}

      /* Build *(((arg1_inner_type*)&(vector type){arg1})+arg2) = arg0. */
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
      stmt = build2 (MODIFY_EXPR, TREE_TYPE (stmt), stmt,
		     convert (TREE_TYPE (stmt), arg0));
      stmt = build2 (COMPOUND_EXPR, arg1_type, stmt, decl);
      return stmt;
    }

  /* Expand vec_ld into an expression that masks the address and
     performs the load.  We need to expand this early to allow
     the best aliasing, as by the time we get into RTL we no longer
     are able to honor __restrict__, for example.  We may want to
     consider this for all memory access built-ins.

     When -maltivec=be is specified, or the wrong number of arguments
     is provided, simply punt to existing built-in processing.  */
  if (fcode == ALTIVEC_BUILTIN_VEC_LD
      && (BYTES_BIG_ENDIAN || !VECTOR_ELT_ORDER_BIG)
      && nargs == 2)
    {
      tree arg0 = (*arglist)[0];
      tree arg1 = (*arglist)[1];

      /* Strip qualifiers like "const" from the pointer arg.  */
      tree arg1_type = TREE_TYPE (arg1);
      tree inner_type = TREE_TYPE (arg1_type);
      if (TYPE_QUALS (TREE_TYPE (arg1_type)) != 0)
	{
	  arg1_type = build_pointer_type (build_qualified_type (inner_type,
								0));
	  arg1 = fold_convert (arg1_type, arg1);
	}

      /* Construct the masked address.  Let existing error handling take
	 over if we don't have a constant offset.  */
      arg0 = fold (arg0);

      if (TREE_CODE (arg0) == INTEGER_CST)
	{
	  if (!ptrofftype_p (TREE_TYPE (arg0)))
	    arg0 = build1 (NOP_EXPR, sizetype, arg0);

	  tree arg1_type = TREE_TYPE (arg1);
	  if (TREE_CODE (arg1_type) == ARRAY_TYPE)
	    {
	      arg1_type = TYPE_POINTER_TO (TREE_TYPE (arg1_type));
	      tree const0 = build_int_cstu (sizetype, 0);
	      tree arg1_elt0 = build_array_ref (loc, arg1, const0);
	      arg1 = build1 (ADDR_EXPR, arg1_type, arg1_elt0);
	    }

	  tree addr = fold_build2_loc (loc, POINTER_PLUS_EXPR, arg1_type,
				       arg1, arg0);
	  tree aligned = fold_build2_loc (loc, BIT_AND_EXPR, arg1_type, addr,
					  build_int_cst (arg1_type, -16));

	  /* Find the built-in to get the return type so we can convert
	     the result properly (or fall back to default handling if the
	     arguments aren't compatible).  */
	  for (desc = altivec_overloaded_builtins;
	       desc->code && desc->code != fcode; desc++)
	    continue;

	  for (; desc->code == fcode; desc++)
	    if (rs6000_builtin_type_compatible (TREE_TYPE (arg0), desc->op1)
		&& (rs6000_builtin_type_compatible (TREE_TYPE (arg1),
						    desc->op2)))
	      {
		tree ret_type = rs6000_builtin_type (desc->ret_type);
		if (TYPE_MODE (ret_type) == V2DImode)
		  /* Type-based aliasing analysis thinks vector long
		     and vector long long are different and will put them
		     in distinct alias classes.  Force our return type
		     to be a may-alias type to avoid this.  */
		  ret_type
		    = build_pointer_type_for_mode (ret_type, Pmode,
						   true/*can_alias_all*/);
		else
		  ret_type = build_pointer_type (ret_type);
		aligned = build1 (NOP_EXPR, ret_type, aligned);
		tree ret_val = build_indirect_ref (loc, aligned, RO_NULL);
		return ret_val;
	      }
	}
    }

  /* Similarly for stvx.  */
  if (fcode == ALTIVEC_BUILTIN_VEC_ST
      && (BYTES_BIG_ENDIAN || !VECTOR_ELT_ORDER_BIG)
      && nargs == 3)
    {
      tree arg0 = (*arglist)[0];
      tree arg1 = (*arglist)[1];
      tree arg2 = (*arglist)[2];

      /* Construct the masked address.  Let existing error handling take
	 over if we don't have a constant offset.  */
      arg1 = fold (arg1);

      if (TREE_CODE (arg1) == INTEGER_CST)
	{
	  if (!ptrofftype_p (TREE_TYPE (arg1)))
	    arg1 = build1 (NOP_EXPR, sizetype, arg1);

	  tree arg2_type = TREE_TYPE (arg2);
	  if (TREE_CODE (arg2_type) == ARRAY_TYPE)
	    {
	      arg2_type = TYPE_POINTER_TO (TREE_TYPE (arg2_type));
	      tree const0 = build_int_cstu (sizetype, 0);
	      tree arg2_elt0 = build_array_ref (loc, arg2, const0);
	      arg2 = build1 (ADDR_EXPR, arg2_type, arg2_elt0);
	    }

	  tree addr = fold_build2_loc (loc, POINTER_PLUS_EXPR, arg2_type,
				       arg2, arg1);
	  tree aligned = fold_build2_loc (loc, BIT_AND_EXPR, arg2_type, addr,
					  build_int_cst (arg2_type, -16));

	  /* Find the built-in to make sure a compatible one exists; if not
	     we fall back to default handling to get the error message.  */
	  for (desc = altivec_overloaded_builtins;
	       desc->code && desc->code != fcode; desc++)
	    continue;

	  for (; desc->code == fcode; desc++)
	    if (rs6000_builtin_type_compatible (TREE_TYPE (arg0), desc->op1)
		&& rs6000_builtin_type_compatible (TREE_TYPE (arg1), desc->op2)
		&& rs6000_builtin_type_compatible (TREE_TYPE (arg2),
						   desc->op3))
	      {
		tree arg0_type = TREE_TYPE (arg0);
		if (TYPE_MODE (arg0_type) == V2DImode)
		  /* Type-based aliasing analysis thinks vector long
		     and vector long long are different and will put them
		     in distinct alias classes.  Force our address type
		     to be a may-alias type to avoid this.  */
		  arg0_type
		    = build_pointer_type_for_mode (arg0_type, Pmode,
						   true/*can_alias_all*/);
		else
		  arg0_type = build_pointer_type (arg0_type);
		aligned = build1 (NOP_EXPR, arg0_type, aligned);
		tree stg = build_indirect_ref (loc, aligned, RO_NULL);
		tree retval = build2 (MODIFY_EXPR, TREE_TYPE (stg), stg,
				      convert (TREE_TYPE (stg), arg0));
		return retval;
	      }
	}
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
	    warning (0, "passing arg %d of %qE discards qualifiers from "
		        "pointer target type", n + 1, fndecl);
	  type = build_pointer_type (build_qualified_type (TREE_TYPE (type),
							   0));
	  arg = fold_convert (type, arg);
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
	int overloaded_code;
	machine_mode arg1_mode = TYPE_MODE (types[0]);
	machine_mode arg2_mode = TYPE_MODE (types[1]);

	if (nargs != 2)
	  {
	    error ("__builtin_cmpb only accepts 2 arguments");
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

	while (desc->code && desc->code == fcode &&
	       desc->overloaded_code != overloaded_code)
	  desc++;

	if (desc->code && (desc->code == fcode)
	    && rs6000_builtin_type_compatible (types[0], desc->op1)
	    && rs6000_builtin_type_compatible (types[1], desc->op2))
	  {
	    if (rs6000_builtin_decls[desc->overloaded_code] != NULL_TREE)
	      return altivec_build_resolved_builtin (args, n, desc);
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
		  return altivec_build_resolved_builtin (args, n, desc);
		else
		  unsupported_builtin = true;
	      }
	  }
      }

    if (unsupported_builtin)
      {
	const char *name = rs6000_overloaded_builtin_name (fcode);
	error ("Builtin function %s not supported in this compiler configuration",
	       name);
	return error_mark_node;
      }
  }
 bad:
    {
      const char *name = rs6000_overloaded_builtin_name (fcode);
      error ("invalid parameter combination for AltiVec intrinsic %s", name);
      return error_mark_node;
    }
}
