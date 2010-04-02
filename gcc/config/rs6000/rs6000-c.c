/* Subroutines for the C front end on the POWER and PowerPC architectures.
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

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
#include "tm.h"
#include "cpplib.h"
#include "tree.h"
#include "c-common.h"
#include "c-pragma.h"
#include "c-tree.h"
#include "toplev.h"
#include "tm_p.h"
#include "target.h"
#include "langhooks.h"



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
  /* Keywords without two leading underscores are context-sensitive,
     and hence implemented as conditional macros, controlled by the
     rs6000_macro_to_expand() function below.  */

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
}

/* Called to decide whether a conditional macro should be expanded.
   Since we have exactly one such macro (i.e, 'vector'), we do not
   need to examine the 'tok' parameter.  */

static cpp_hashnode *
rs6000_macro_to_expand (cpp_reader *pfile, const cpp_token *tok)
{
  cpp_hashnode *expand_this = tok->val.node.node;
  cpp_hashnode *ident;

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
      else if (ident)
	{
	  enum rid rid_code = (enum rid)(ident->rid_code);
	  if (ident->type == NT_MACRO)
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
	      || (rid_code == RID_DOUBLE && TARGET_VSX))
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

void
rs6000_cpu_cpp_builtins (cpp_reader *pfile)
{
  if (TARGET_POWER2)
    builtin_define ("_ARCH_PWR2");
  else if (TARGET_POWER)
    builtin_define ("_ARCH_PWR");
  if (TARGET_POWERPC)
    builtin_define ("_ARCH_PPC");
  if (TARGET_PPC_GPOPT)
    builtin_define ("_ARCH_PPCSQ");
  if (TARGET_PPC_GFXOPT)
    builtin_define ("_ARCH_PPCGR");
  if (TARGET_POWERPC64)
    builtin_define ("_ARCH_PPC64");
  if (TARGET_MFCRF)
    builtin_define ("_ARCH_PWR4");
  if (TARGET_POPCNTB)
    builtin_define ("_ARCH_PWR5");
  if (TARGET_FPRND)
    builtin_define ("_ARCH_PWR5X");
  if (TARGET_CMPB)
    builtin_define ("_ARCH_PWR6");
  if (TARGET_MFPGPR)
    builtin_define ("_ARCH_PWR6X");
  if (! TARGET_POWER && ! TARGET_POWER2 && ! TARGET_POWERPC)
    builtin_define ("_ARCH_COM");
  if (TARGET_POPCNTD)
    builtin_define ("_ARCH_PWR7");
  if (TARGET_ALTIVEC)
    {
      builtin_define ("__ALTIVEC__");
      builtin_define ("__VEC__=10206");

      /* Define the AltiVec syntactic elements.  */
      builtin_define ("__vector=__attribute__((altivec(vector__)))");
      builtin_define ("__pixel=__attribute__((altivec(pixel__))) unsigned short");
      builtin_define ("__bool=__attribute__((altivec(bool__))) unsigned");

      if (!flag_iso)
	{
	  /* Define this when supporting context-sensitive keywords.  */
	  builtin_define ("__APPLE_ALTIVEC__");
	  
	  builtin_define ("vector=vector");
	  builtin_define ("pixel=pixel");
	  builtin_define ("bool=bool");
	  builtin_define ("_Bool=_Bool");
	  init_vector_keywords ();

	  /* Enable context-sensitive macros.  */
	  cpp_get_callbacks (pfile)->macro_to_expand = rs6000_macro_to_expand;
	}
    }
  if (rs6000_cpu == PROCESSOR_CELL)
    builtin_define ("__PPU__");
  if (TARGET_SPE)
    builtin_define ("__SPE__");
  if (TARGET_PAIRED_FLOAT)
    builtin_define ("__PAIRED__");
  if (TARGET_SOFT_FLOAT)
    builtin_define ("_SOFT_FLOAT");
  if ((!(TARGET_HARD_FLOAT && (TARGET_FPRS || TARGET_E500_DOUBLE)))
      ||(TARGET_HARD_FLOAT && TARGET_FPRS && !TARGET_DOUBLE_FLOAT))
    builtin_define ("_SOFT_DOUBLE");
  /* Used by lwarx/stwcx. errata work-around.  */
  if (rs6000_cpu == PROCESSOR_PPC405)
    builtin_define ("__PPC405__");
  /* Used by libstdc++.  */
  if (TARGET_NO_LWSYNC)
    builtin_define ("__NO_LWSYNC__");
  if (TARGET_VSX)
    {
      builtin_define ("__VSX__");

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
    }

  switch (rs6000_current_abi)
    {
    case ABI_V4:
      builtin_define ("_CALL_SYSV");
      break;
    case ABI_AIX:
      builtin_define ("_CALL_AIXDESC");
      builtin_define ("_CALL_AIX");
      break;
    case ABI_DARWIN:
      builtin_define ("_CALL_DARWIN");
      break;
    default:
      break;
    }

  /* Let the compiled code know if 'f' class registers will not be available.  */
  if (TARGET_SOFT_FLOAT || !TARGET_FPRS)
    builtin_define ("__NO_FPRS__");

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
  { ALTIVEC_BUILTIN_VEC_ROUND, ALTIVEC_BUILTIN_VRFIN,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_RSQRTE, ALTIVEC_BUILTIN_VRSQRTEFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
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
  { ALTIVEC_BUILTIN_VEC_UNPACKH, ALTIVEC_BUILTIN_VUPKHPX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_pixel_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKHSH, ALTIVEC_BUILTIN_VUPKHSH,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_VUPKHSH, ALTIVEC_BUILTIN_VUPKHSH,
    RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V8HI, 0, 0 },
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
  { ALTIVEC_BUILTIN_VEC_ADD, ALTIVEC_BUILTIN_VADDFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_ADD, VSX_BUILTIN_XVADDDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
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
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
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
  { ALTIVEC_BUILTIN_VEC_CMPEQ, ALTIVEC_BUILTIN_VCMPEQUH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, ALTIVEC_BUILTIN_VCMPEQUH,
    RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, ALTIVEC_BUILTIN_VCMPEQUW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI, RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_CMPEQ, ALTIVEC_BUILTIN_VCMPEQUW,
    RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI, 0 },
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
  { ALTIVEC_BUILTIN_VEC_VCFSX, ALTIVEC_BUILTIN_VCFSX,
    RS6000_BTI_V4SF, RS6000_BTI_V4SI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_VCFUX, ALTIVEC_BUILTIN_VCFUX,
    RS6000_BTI_V4SF, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_CTS, ALTIVEC_BUILTIN_VCTSXS,
    RS6000_BTI_V4SI, RS6000_BTI_V4SF, RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_CTU, ALTIVEC_BUILTIN_VCTUXS,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_V4SF, RS6000_BTI_INTSI, 0 },
  { VSX_BUILTIN_VEC_DIV, VSX_BUILTIN_XVDIVSP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { VSX_BUILTIN_VEC_DIV, VSX_BUILTIN_XVDIVDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LD, ALTIVEC_BUILTIN_LVX,
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
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_long, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_LDL, ALTIVEC_BUILTIN_LVXL,
    RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI, 0 },
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
  { VSX_BUILTIN_VEC_MUL, VSX_BUILTIN_XVMULSP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { VSX_BUILTIN_VEC_MUL, VSX_BUILTIN_XVMULDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
  { ALTIVEC_BUILTIN_VEC_MULE, ALTIVEC_BUILTIN_VMULEUB,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MULE, ALTIVEC_BUILTIN_VMULESB,
    RS6000_BTI_V8HI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_MULE, ALTIVEC_BUILTIN_VMULEUH,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_MULE, ALTIVEC_BUILTIN_VMULESH,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
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
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMULOSH, ALTIVEC_BUILTIN_VMULOSH,
    RS6000_BTI_V4SI, RS6000_BTI_V8HI, RS6000_BTI_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMULOUH, ALTIVEC_BUILTIN_VMULOUH,
    RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMULOSB, ALTIVEC_BUILTIN_VMULOSB,
    RS6000_BTI_V8HI, RS6000_BTI_V16QI, RS6000_BTI_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_VMULOUB, ALTIVEC_BUILTIN_VMULOUB,
    RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI, 0 },
  { ALTIVEC_BUILTIN_VEC_NEARBYINT, VSX_BUILTIN_XVRDPI,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_NEARBYINT, VSX_BUILTIN_XVRSPI,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_NOR, ALTIVEC_BUILTIN_VNOR,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
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
  { ALTIVEC_BUILTIN_VEC_SUB, ALTIVEC_BUILTIN_VSUBFP,
    RS6000_BTI_V4SF, RS6000_BTI_V4SF, RS6000_BTI_V4SF, 0 },
  { ALTIVEC_BUILTIN_VEC_SUB, VSX_BUILTIN_XVSUBDP,
    RS6000_BTI_V2DF, RS6000_BTI_V2DF, RS6000_BTI_V2DF, 0 },
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
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_ST, ALTIVEC_BUILTIN_STVX,
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
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_V4SF, RS6000_BTI_INTSI, ~RS6000_BTI_float },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTSI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_bool_V4SI, RS6000_BTI_INTSI, ~RS6000_BTI_INTSI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTHI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_bool_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_INTHI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_unsigned_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_UINTQI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_bool_V16QI, RS6000_BTI_INTSI, ~RS6000_BTI_INTQI },
  { ALTIVEC_BUILTIN_VEC_STL, ALTIVEC_BUILTIN_STVXL,
    RS6000_BTI_void, RS6000_BTI_pixel_V8HI, RS6000_BTI_INTSI, ~RS6000_BTI_pixel_V8HI },
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

  /* Predicates.  */
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V16QI, RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTSW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VCMPGT_P, ALTIVEC_BUILTIN_VCMPGTFP_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SF, RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VCMPGT_P, VSX_BUILTIN_XVCMPGTDP_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V2DF, RS6000_BTI_V2DF },


  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V16QI, RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_pixel_V8HI, RS6000_BTI_pixel_V8HI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VCMPEQ_P, ALTIVEC_BUILTIN_VCMPEQFP_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SF, RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VCMPEQ_P, VSX_BUILTIN_XVCMPEQDP_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V2DF, RS6000_BTI_V2DF },


  /* cmpge is the same as cmpgt for all cases except floating point.
     There is further code to deal with this special case in
     altivec_build_resolved_builtin.  */
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V16QI, RS6000_BTI_unsigned_V16QI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V16QI, RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V16QI, RS6000_BTI_bool_V16QI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSB_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V16QI, RS6000_BTI_V16QI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V8HI, RS6000_BTI_unsigned_V8HI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V8HI, RS6000_BTI_V8HI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSH_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V8HI, RS6000_BTI_bool_V8HI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTUW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_unsigned_V4SI, RS6000_BTI_unsigned_V4SI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_bool_V4SI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SI, RS6000_BTI_bool_V4SI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGTSW_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SI, RS6000_BTI_V4SI },
  { ALTIVEC_BUILTIN_VCMPGE_P, ALTIVEC_BUILTIN_VCMPGEFP_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V4SF, RS6000_BTI_V4SF },
  { ALTIVEC_BUILTIN_VCMPGE_P, VSX_BUILTIN_XVCMPGEDP_P,
    RS6000_BTI_INTSI, RS6000_BTI_INTSI, RS6000_BTI_V2DF, RS6000_BTI_V2DF },

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
  if (desc->code == ALTIVEC_BUILTIN_VCMPGE_P
      && desc->overloaded_code != ALTIVEC_BUILTIN_VCMPGEFP_P)
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
			      fold_convert (arg_type[0], args[0]));
      break;
    case 2:
      call = build_call_expr (impl_fndecl, 2,
			      fold_convert (arg_type[0], args[0]),
			      fold_convert (arg_type[1], args[1]));
      break;
    case 3:
      call = build_call_expr (impl_fndecl, 3,
			      fold_convert (arg_type[0], args[0]),
			      fold_convert (arg_type[1], args[1]),
			      fold_convert (arg_type[2], args[2]));
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
  VEC(tree,gc) *arglist = (VEC(tree,gc) *) passed_arglist;
  unsigned int nargs = VEC_length (tree, arglist);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);
  tree fnargs = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
  tree types[3], args[3];
  const struct altivec_builtin_types *desc;
  unsigned int n;

  if ((fcode < ALTIVEC_BUILTIN_OVERLOADED_FIRST
       || fcode > ALTIVEC_BUILTIN_OVERLOADED_LAST)
      && (fcode < VSX_BUILTIN_OVERLOADED_FIRST
	  || fcode > VSX_BUILTIN_OVERLOADED_LAST))
    return NULL_TREE;

  /* For now treat vec_splats and vec_promote as the same.  */
  if (fcode == ALTIVEC_BUILTIN_VEC_SPLATS
      || fcode == ALTIVEC_BUILTIN_VEC_PROMOTE)
    {
      tree type, arg;
      int size;
      int i;
      bool unsigned_p;
      VEC(constructor_elt,gc) *vec;
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
	  && !INTEGRAL_TYPE_P (TREE_TYPE (VEC_index (tree, arglist, 1))))
	goto bad;

      arg = VEC_index (tree, arglist, 0);
      type = TREE_TYPE (arg);
      if (!SCALAR_FLOAT_TYPE_P (type)
	  && !INTEGRAL_TYPE_P (type))
	goto bad;
      unsigned_p = TYPE_UNSIGNED (type);
      switch (TYPE_MODE (type))
	{
	  case DImode:
	    type = (unsigned_p ? unsigned_V2DI_type_node : V2DI_type_node);
	    size = 2;
	    break;
	  case SImode:
	    type = (unsigned_p ? unsigned_V4SI_type_node : V4SI_type_node);
	    size = 4;
	    break;
	  case HImode:
	    type = (unsigned_p ? unsigned_V8HI_type_node : V8HI_type_node);
	    size = 8;
	    break;
	  case QImode:
	    type = (unsigned_p ? unsigned_V16QI_type_node : V16QI_type_node);
	    size = 16;
	    break;
	  case SFmode: type = V4SF_type_node; size = 4; break;
	  case DFmode: type = V2DF_type_node; size = 2; break;
	  default:
	    goto bad;
	}
      arg = save_expr (fold_convert (TREE_TYPE (type), arg));
      vec = VEC_alloc (constructor_elt, gc, size);
      for(i = 0; i < size; i++)
	{
	  constructor_elt *elt;

	  elt = VEC_quick_push (constructor_elt, vec, NULL);
	  elt->index = NULL_TREE;
	  elt->value = arg;
	}
	return build_constructor (type, vec);
    }

  /* For now use pointer tricks to do the extaction, unless we are on VSX
     extracting a double from a constant offset.  */
  if (fcode == ALTIVEC_BUILTIN_VEC_EXTRACT)
    {
      tree arg1;
      tree arg1_type;
      tree arg2;
      tree arg1_inner_type;
      tree decl, stmt;
      tree innerptrtype;
      enum machine_mode mode;

      /* No second argument. */
      if (nargs != 2)
	{
	  error ("vec_extract only accepts 2 arguments");
	  return error_mark_node;
	}

      arg2 = VEC_index (tree, arglist, 1);
      arg1 = VEC_index (tree, arglist, 0);
      arg1_type = TREE_TYPE (arg1);

      if (TREE_CODE (arg1_type) != VECTOR_TYPE)
	goto bad; 
      if (!INTEGRAL_TYPE_P (TREE_TYPE (arg2)))
	goto bad; 

      /* If we can use the VSX xxpermdi instruction, use that for extract.  */
      mode = TYPE_MODE (arg1_type);
      if ((mode == V2DFmode || mode == V2DImode) && VECTOR_MEM_VSX_P (mode)
	  && TREE_CODE (arg2) == INTEGER_CST
	  && TREE_INT_CST_HIGH (arg2) == 0
	  && (TREE_INT_CST_LOW (arg2) == 0 || TREE_INT_CST_LOW (arg2) == 1))
	{
	  tree call = NULL_TREE;

	  if (mode == V2DFmode)
	    call = rs6000_builtin_decls[VSX_BUILTIN_VEC_EXT_V2DF];
	  else if (mode == V2DImode)
	    call = rs6000_builtin_decls[VSX_BUILTIN_VEC_EXT_V2DI];

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
      DECL_INITIAL (decl) = arg1;
      stmt = build1 (DECL_EXPR, arg1_type, decl);
      TREE_ADDRESSABLE (decl) = 1;
      SET_EXPR_LOCATION (stmt, loc);
      stmt = build1 (COMPOUND_LITERAL_EXPR, arg1_type, stmt);

      innerptrtype = build_pointer_type (arg1_inner_type);

      stmt = build_unary_op (loc, ADDR_EXPR, stmt, 0);
      stmt = convert (innerptrtype, stmt);
      stmt = build_binary_op (loc, PLUS_EXPR, stmt, arg2, 1);
      stmt = build_indirect_ref (loc, stmt, RO_NULL);

      return stmt;
    }

  /* For now use pointer tricks to do the insertation, unless we are on VSX
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
      enum machine_mode mode;

      /* No second or third arguments. */
      if (nargs != 3)
	{
	  error ("vec_insert only accepts 3 arguments");
	  return error_mark_node;
	}

      arg0 = VEC_index (tree, arglist, 0);
      arg1 = VEC_index (tree, arglist, 1);
      arg1_type = TREE_TYPE (arg1);
      arg2 = VEC_index (tree, arglist, 2);

      if (TREE_CODE (arg1_type) != VECTOR_TYPE)
	goto bad; 
      if (!INTEGRAL_TYPE_P (TREE_TYPE (arg2)))
	goto bad; 

      /* If we can use the VSX xxpermdi instruction, use that for insert.  */
      mode = TYPE_MODE (arg1_type);
      if ((mode == V2DFmode || mode == V2DImode) && VECTOR_UNIT_VSX_P (mode)
	  && TREE_CODE (arg2) == INTEGER_CST
	  && TREE_INT_CST_HIGH (arg2) == 0
	  && (TREE_INT_CST_LOW (arg2) == 0 || TREE_INT_CST_LOW (arg2) == 1))
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
      DECL_INITIAL (decl) = arg1;
      stmt = build1 (DECL_EXPR, arg1_type, decl);
      TREE_ADDRESSABLE (decl) = 1;
      SET_EXPR_LOCATION (stmt, loc);
      stmt = build1 (COMPOUND_LITERAL_EXPR, arg1_type, stmt);

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
      tree arg = VEC_index (tree, arglist, n);
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
	    warning (0, "passing arg %d of %qE discards qualifiers from"
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

  for (desc = altivec_overloaded_builtins;
       desc->code && desc->code != fcode; desc++)
    continue;

  /* For arguments after the last, we have RS6000_BTI_NOT_OPAQUE in
     the opX fields.  */
  for (; desc->code == fcode; desc++)
    if ((desc->op1 == RS6000_BTI_NOT_OPAQUE
	 || rs6000_builtin_type_compatible (types[0], desc->op1))
	&& (desc->op2 == RS6000_BTI_NOT_OPAQUE
	    || rs6000_builtin_type_compatible (types[1], desc->op2))
	&& (desc->op3 == RS6000_BTI_NOT_OPAQUE
	    || rs6000_builtin_type_compatible (types[2], desc->op3)))
      return altivec_build_resolved_builtin (args, n, desc);

 bad:
  error ("invalid parameter combination for AltiVec intrinsic");
  return error_mark_node;
}
