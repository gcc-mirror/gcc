/* ACLE support for Arm MVE (function_base classes)
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_ARM_MVE_BUILTINS_FUNCTIONS_H
#define GCC_ARM_MVE_BUILTINS_FUNCTIONS_H

#include "arm-protos.h"

namespace arm_mve {

/* Wrap T, which is derived from function_base, and indicate that the
   function never has side effects.  It is only necessary to use this
   wrapper on functions that might have floating-point suffixes, since
   otherwise we assume by default that the function has no side effects.  */
template<typename T>
class quiet : public T
{
public:
  CONSTEXPR quiet () : T () {}

  unsigned int
  call_properties (const function_instance &) const override
  {
    return 0;
  }
};

/* An incomplete function_base for functions that have an associated
   rtx_code or an unspec for signed integers, unsigned integers and
   floating-point values for the non-predicated, non-suffixed
   intrinsics, and unspec codes, with separate codes for signed
   integers, unsigned integers and floating-point values for
   predicated and/or suffixed intrinsics.  The class simply records
   information about the mapping for derived classes to use and
   provides a generic expand_unspec () to avoid duplicating expansion
   code in derived classes.  */
class unspec_based_mve_function_base : public function_base
{
public:
  CONSTEXPR unspec_based_mve_function_base (rtx_code code_for_sint,
					    rtx_code code_for_uint,
					    rtx_code code_for_fp,
					    int unspec_for_sint,
					    int unspec_for_uint,
					    int unspec_for_fp,
					    int unspec_for_n_sint,
					    int unspec_for_n_uint,
					    int unspec_for_n_fp,
					    int unspec_for_m_sint,
					    int unspec_for_m_uint,
					    int unspec_for_m_fp,
					    int unspec_for_m_n_sint,
					    int unspec_for_m_n_uint,
					    int unspec_for_m_n_fp)
    : m_code_for_sint (code_for_sint),
      m_code_for_uint (code_for_uint),
      m_code_for_fp (code_for_fp),
      m_unspec_for_sint (unspec_for_sint),
      m_unspec_for_uint (unspec_for_uint),
      m_unspec_for_fp (unspec_for_fp),
      m_unspec_for_n_sint (unspec_for_n_sint),
      m_unspec_for_n_uint (unspec_for_n_uint),
      m_unspec_for_n_fp (unspec_for_n_fp),
      m_unspec_for_m_sint (unspec_for_m_sint),
      m_unspec_for_m_uint (unspec_for_m_uint),
      m_unspec_for_m_fp (unspec_for_m_fp),
      m_unspec_for_m_n_sint (unspec_for_m_n_sint),
      m_unspec_for_m_n_uint (unspec_for_m_n_uint),
      m_unspec_for_m_n_fp (unspec_for_m_n_fp)
  {}

  /* The rtx code to use for signed, unsigned integers and
     floating-point values respectively.  */
  rtx_code m_code_for_sint;
  rtx_code m_code_for_uint;
  rtx_code m_code_for_fp;

  /* The unspec code associated with signed-integer, unsigned-integer
     and floating-point operations respectively.  It covers the cases
     with the _n suffix, and/or the _m predicate.  */
  int m_unspec_for_sint;
  int m_unspec_for_uint;
  int m_unspec_for_fp;
  int m_unspec_for_n_sint;
  int m_unspec_for_n_uint;
  int m_unspec_for_n_fp;
  int m_unspec_for_m_sint;
  int m_unspec_for_m_uint;
  int m_unspec_for_m_fp;
  int m_unspec_for_m_n_sint;
  int m_unspec_for_m_n_uint;
  int m_unspec_for_m_n_fp;

  rtx expand_unspec (function_expander &e) const;
};

/* Expand the unspecs, which is common to all intrinsics using
   unspec_based_mve_function_base.  If some combinations are not
   supported for an intrinsics family, they should be handled by the
   caller (and not crash here).  */
rtx
unspec_based_mve_function_base::expand_unspec (function_expander &e) const
{
    machine_mode mode = e.vector_mode (0);
    insn_code code;

    switch (e.pred)
      {
      case PRED_none:
	switch (e.mode_suffix_id)
	  {
	  case MODE_none:
	    /* No predicate, no suffix.  */
	    if (e.type_suffix (0).integer_p)
	      {
		int unspec = (e.type_suffix (0).unsigned_p
			      ? m_unspec_for_uint
			      : m_unspec_for_sint);
		code = code_for_mve_q (unspec, unspec, mode);
	      }
	    else
	      code = code_for_mve_q_f (m_unspec_for_fp, mode);
	    break;

	  case MODE_n:
	    /* No predicate, _n suffix.  */
	    if (e.type_suffix (0).integer_p)
	      {
		int unspec = (e.type_suffix (0).unsigned_p
			      ? m_unspec_for_n_uint
			      : m_unspec_for_n_sint);
		code = code_for_mve_q_n (unspec, unspec, mode);
	      }
	    else
	      code = code_for_mve_q_n_f (m_unspec_for_n_fp, mode);
	    break;

	  default:
	    gcc_unreachable ();
	  }
	return e.use_exact_insn (code);

      case PRED_m:
      case PRED_x:
	switch (e.mode_suffix_id)
	  {
	  case MODE_none:
	    /* No suffix, "m" or "x" predicate.  */
	    if (e.type_suffix (0).integer_p)
	      {
		int unspec = (e.type_suffix (0).unsigned_p
			      ? m_unspec_for_m_uint
			      : m_unspec_for_m_sint);
		code = code_for_mve_q_m (unspec, unspec, mode);
	      }
	    else
	      code = code_for_mve_q_m_f (m_unspec_for_m_fp, mode);
	    break;

	  case MODE_n:
	    /* _n suffix, "m" or "x" predicate.  */
	    if (e.type_suffix (0).integer_p)
	      {
		int unspec = (e.type_suffix (0).unsigned_p
			      ? m_unspec_for_m_n_uint
			      : m_unspec_for_m_n_sint);
		code = code_for_mve_q_m_n (unspec, unspec, mode);
	      }
	    else
	      code = code_for_mve_q_m_n_f (m_unspec_for_m_n_fp, mode);
	    break;

	  default:
	    gcc_unreachable ();
	  }

	if (e.pred == PRED_m)
	  return e.use_cond_insn (code, 0);
	else
	  return e.use_pred_x_insn (code);
	break;

      default:
	gcc_unreachable ();
      }
}

/* Map the function directly to CODE (UNSPEC, M) where M is the vector
   mode associated with type suffix 0, except when there is no
   predicate and no _n suffix, in which case we use the appropriate
   rtx_code.  This is useful when the basic operation is mapped to a
   standard RTX code and all other versions use different unspecs.  */
class unspec_based_mve_function_exact_insn : public unspec_based_mve_function_base
{
public:
  CONSTEXPR unspec_based_mve_function_exact_insn (rtx_code code_for_sint,
						  rtx_code code_for_uint,
						  rtx_code code_for_fp,
						  int unspec_for_n_sint,
						  int unspec_for_n_uint,
						  int unspec_for_n_fp,
						  int unspec_for_m_sint,
						  int unspec_for_m_uint,
						  int unspec_for_m_fp,
						  int unspec_for_m_n_sint,
						  int unspec_for_m_n_uint,
						  int unspec_for_m_n_fp)
    : unspec_based_mve_function_base (code_for_sint,
				      code_for_uint,
				      code_for_fp,
				      -1,
				      -1,
				      -1,
				      unspec_for_n_sint,
				      unspec_for_n_uint,
				      unspec_for_n_fp,
				      unspec_for_m_sint,
				      unspec_for_m_uint,
				      unspec_for_m_fp,
				      unspec_for_m_n_sint,
				      unspec_for_m_n_uint,
				      unspec_for_m_n_fp)
  {}

  rtx
  expand (function_expander &e) const override
  {
    /* No suffix, no predicate, use the right RTX code.  */
    if ((e.mode_suffix_id != MODE_n)
	&& (e.pred == PRED_none))
      return e.map_to_rtx_codes (m_code_for_sint, m_code_for_uint,
				 m_code_for_fp);

    return expand_unspec (e);
  }
};

/* Map the function directly to CODE (UNSPEC, M) where M is the vector
   mode associated with type suffix 0.  */
class unspec_mve_function_exact_insn : public unspec_based_mve_function_base
{
public:
  CONSTEXPR unspec_mve_function_exact_insn (int unspec_for_sint,
					    int unspec_for_uint,
					    int unspec_for_fp,
					    int unspec_for_n_sint,
					    int unspec_for_n_uint,
					    int unspec_for_n_fp,
					    int unspec_for_m_sint,
					    int unspec_for_m_uint,
					    int unspec_for_m_fp,
					    int unspec_for_m_n_sint,
					    int unspec_for_m_n_uint,
					    int unspec_for_m_n_fp)
    : unspec_based_mve_function_base (UNKNOWN,
				      UNKNOWN,
				      UNKNOWN,
				      unspec_for_sint,
				      unspec_for_uint,
				      unspec_for_fp,
				      unspec_for_n_sint,
				      unspec_for_n_uint,
				      unspec_for_n_fp,
				      unspec_for_m_sint,
				      unspec_for_m_uint,
				      unspec_for_m_fp,
				      unspec_for_m_n_sint,
				      unspec_for_m_n_uint,
				      unspec_for_m_n_fp)
  {}

  rtx
  expand (function_expander &e) const override
  {
    return expand_unspec (e);
  }
};

/* Map the function directly to CODE (UNSPEC), when there is a
   non-predicated version and one with the "_p" predicate.  */
class unspec_mve_function_exact_insn_pred_p : public unspec_based_mve_function_base
{
public:
  CONSTEXPR unspec_mve_function_exact_insn_pred_p (int unspec_for_sint,
						   int unspec_for_uint,
						   int unspec_for_fp,
						   int unspec_for_p_sint,
						   int unspec_for_p_uint,
						   int unspec_for_p_fp)
    : unspec_based_mve_function_base (UNKNOWN, /* No RTX code.  */
				      UNKNOWN,
				      UNKNOWN,
				      unspec_for_sint,
				      unspec_for_uint,
				      unspec_for_fp,
				      -1, -1, -1, /* No _n intrinsics.  */
				      -1, -1, -1, /* No _m intrinsics.  */
				      -1, -1, -1), /* No _m_n intrinsics.  */
      m_unspec_for_p_sint (unspec_for_p_sint),
      m_unspec_for_p_uint (unspec_for_p_uint),
      m_unspec_for_p_fp (unspec_for_p_fp)
  {}

  /* The unspec code associated with signed-integer and
     unsigned-integer or floating-point operations with "_p"
     predicate.  */
  int m_unspec_for_p_sint;
  int m_unspec_for_p_uint;
  int m_unspec_for_p_fp;

  rtx
  expand (function_expander &e) const override
  {
    insn_code code;
    int unspec;

    if (m_unspec_for_sint == VADDLVQ_S
	|| m_unspec_for_sint == VADDLVAQ_S
	|| m_unspec_for_sint == VRMLALDAVHAQ_S
	|| m_unspec_for_sint == VRMLALDAVHAXQ_S
	|| m_unspec_for_sint == VRMLALDAVHQ_S
	|| m_unspec_for_sint == VRMLALDAVHXQ_S
	|| m_unspec_for_sint == VRMLSLDAVHAQ_S
	|| m_unspec_for_sint == VRMLSLDAVHAXQ_S
	|| m_unspec_for_sint == VRMLSLDAVHQ_S
	|| m_unspec_for_sint == VRMLSLDAVHXQ_S)
      {
	switch (e.pred)
	  {
	  case PRED_none:
	    unspec = (e.type_suffix (0).unsigned_p
		      ? m_unspec_for_uint
		      : m_unspec_for_sint);
	    code = code_for_mve_q_v4si (unspec, unspec);
	    return e.use_exact_insn (code);

	  case PRED_p:
	    unspec = (e.type_suffix (0).unsigned_p
		      ? m_unspec_for_p_uint
		      : m_unspec_for_p_sint);
	    code = code_for_mve_q_p_v4si (unspec, unspec);
	    return e.use_exact_insn (code);

	  default:
	    gcc_unreachable ();
	  }
      }

    if (e.pred == PRED_p)
      {
	machine_mode mode = e.vector_mode (0);

	if (e.type_suffix (0).integer_p)
	  {
	    unspec = (e.type_suffix (0).unsigned_p
		      ? m_unspec_for_p_uint
		      : m_unspec_for_p_sint);
	    code = code_for_mve_q_p (unspec, unspec, mode);
	  }
	else
	  code = code_for_mve_q_p_f (m_unspec_for_p_fp, mode);

	return e.use_exact_insn (code);
      }

    return expand_unspec (e);
  }
};

/* Map the function directly to CODE (UNSPEC, M) for vshl-like
   builtins. The difference with unspec_mve_function_exact_insn is
   that this function handles MODE_r and the related unspecs..  */
class unspec_mve_function_exact_insn_vshl : public unspec_based_mve_function_base
{
public:
  CONSTEXPR unspec_mve_function_exact_insn_vshl (int unspec_for_sint,
						 int unspec_for_uint,
						 int unspec_for_n_sint,
						 int unspec_for_n_uint,
						 int unspec_for_m_sint,
						 int unspec_for_m_uint,
						 int unspec_for_m_n_sint,
						 int unspec_for_m_n_uint,
						 int unspec_for_m_r_sint,
						 int unspec_for_m_r_uint,
						 int unspec_for_r_sint,
						 int unspec_for_r_uint)
    : unspec_based_mve_function_base (UNKNOWN,
				      UNKNOWN,
				      UNKNOWN,
				      unspec_for_sint,
				      unspec_for_uint,
				      -1,
				      unspec_for_n_sint,
				      unspec_for_n_uint,
				      -1,
				      unspec_for_m_sint,
				      unspec_for_m_uint,
				      -1,
				      unspec_for_m_n_sint,
				      unspec_for_m_n_uint,
				      -1),
      m_unspec_for_m_r_sint (unspec_for_m_r_sint),
      m_unspec_for_m_r_uint (unspec_for_m_r_uint),
      m_unspec_for_r_sint (unspec_for_r_sint),
      m_unspec_for_r_uint (unspec_for_r_uint)
  {}

  /* The unspec code associated with signed-integer and unsigned-integer
     operations with MODE_r with or without PRED_m.  */
  int m_unspec_for_m_r_sint;
  int m_unspec_for_m_r_uint;
  int m_unspec_for_r_sint;
  int m_unspec_for_r_uint;

  rtx
  expand (function_expander &e) const override
  {
    insn_code code;
    int unspec;

    if (e.mode_suffix_id == MODE_r)
      {
	machine_mode mode = e.vector_mode (0);
	switch (e.pred)
	  {
	  case PRED_none:
	    /* No predicate, _r suffix.  */
	    unspec = (e.type_suffix (0).unsigned_p
		      ? m_unspec_for_r_uint
		      : m_unspec_for_r_sint);
	    code = code_for_mve_q_r (unspec, unspec, mode);
	    return e.use_exact_insn (code);

	  case PRED_m:
	  case PRED_x:
	    /* _r suffix, "m" or "x" predicate.  */
	    unspec = (e.type_suffix (0).unsigned_p
		      ? m_unspec_for_m_r_uint
		      : m_unspec_for_m_r_sint);
	    code = code_for_mve_q_m_r (unspec, unspec, mode);

	    if (e.pred == PRED_m)
	      return e.use_cond_insn (code, 0);
	    else
	      return e.use_pred_x_insn (code);

	  default:
	    gcc_unreachable ();
	  }
      }

      return expand_unspec (e);
  }
};

/* Map the function directly to CODE (M) for vbic-like builtins. The difference
   with unspec_based_mve_function_exact_insn is that this function has vbic
   hardcoded for the PRED_none, MODE_none version, rather than using an
   RTX.  */
class unspec_based_mve_function_exact_insn_vbic : public unspec_based_mve_function_base
{
public:
  CONSTEXPR unspec_based_mve_function_exact_insn_vbic (int unspec_for_n_sint,
						       int unspec_for_n_uint,
						       int unspec_for_m_sint,
						       int unspec_for_m_uint,
						       int unspec_for_m_fp,
						       int unspec_for_m_n_sint,
						       int unspec_for_m_n_uint)
    : unspec_based_mve_function_base (UNKNOWN,
				      UNKNOWN,
				      UNKNOWN,
				      -1, -1, -1, /* No non-predicated, no mode intrinsics.  */
				      unspec_for_n_sint,
				      unspec_for_n_uint,
				      -1,
				      unspec_for_m_sint,
				      unspec_for_m_uint,
				      unspec_for_m_fp,
				      unspec_for_m_n_sint,
				      unspec_for_m_n_uint,
				      -1)
  {}

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    insn_code code;

    /* No suffix, no predicate, use the right RTX code.  */
    if (e.pred == PRED_none
	&& e.mode_suffix_id == MODE_none)
	  {
	    if (e.type_suffix (0).integer_p)
	      if (e.type_suffix (0).unsigned_p)
		code = code_for_mve_vbicq_u (mode);
	      else
		code = code_for_mve_vbicq_s (mode);
	    else
	      code = code_for_mve_vbicq_f (mode);

	    return e.use_exact_insn (code);
	  }

    return expand_unspec (e);
  }
};

/* Map the function directly to CODE (M) for vorn-like builtins. The difference
   with unspec_based_mve_function_exact_insn is that this function has vbic
   hardcoded for the PRED_none, MODE_none version, rather than using an
   RTX.  */
class unspec_based_mve_function_exact_insn_vorn : public unspec_based_mve_function_base
{
public:
  CONSTEXPR unspec_based_mve_function_exact_insn_vorn (int unspec_for_n_sint,
						       int unspec_for_n_uint,
						       int unspec_for_m_sint,
						       int unspec_for_m_uint,
						       int unspec_for_m_fp,
						       int unspec_for_m_n_sint,
						       int unspec_for_m_n_uint)
    : unspec_based_mve_function_base (UNKNOWN,
				      UNKNOWN,
				      UNKNOWN,
				      -1, -1, -1, /* No non-predicated, no mode unspec intrinsics.  */
				      unspec_for_n_sint,
				      unspec_for_n_uint,
				      -1,
				      unspec_for_m_sint,
				      unspec_for_m_uint,
				      unspec_for_m_fp,
				      unspec_for_m_n_sint,
				      unspec_for_m_n_uint,
				      -1)
  {}

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    insn_code code;

    /* No suffix, no predicate, use the right RTX code.  */
    if (e.pred == PRED_none
	&& e.mode_suffix_id == MODE_none)
      {
	if (e.type_suffix (0).integer_p)
	  if (e.type_suffix (0).unsigned_p)
	    code = code_for_mve_vornq_u (mode);
	  else
	    code = code_for_mve_vornq_s (mode);
	else
	  code = code_for_mve_vornq_f (mode);
	return e.use_exact_insn (code);
      }

    return expand_unspec (e);
  }
};

/* Map the comparison functions.  */
class unspec_based_mve_function_exact_insn_vcmp : public unspec_based_mve_function_base
{
public:
  CONSTEXPR unspec_based_mve_function_exact_insn_vcmp (rtx_code code_for_sint,
						       rtx_code code_for_uint,
						       rtx_code code_for_fp,
						       int unspec_for_m_sint,
						       int unspec_for_m_uint,
						       int unspec_for_m_fp,
						       int unspec_for_m_n_sint,
						       int unspec_for_m_n_uint,
						       int unspec_for_m_n_fp)
    : unspec_based_mve_function_base (code_for_sint,
				      code_for_uint,
				      code_for_fp,
				      -1, -1, -1, /* No non-predicated, no mode intrinsics.  */
				      -1, -1, -1, /* No _n intrinsics.  */
				      unspec_for_m_sint,
				      unspec_for_m_uint,
				      unspec_for_m_fp,
				      unspec_for_m_n_sint,
				      unspec_for_m_n_uint,
				      unspec_for_m_n_fp)
  {}

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    insn_code code;
    rtx target;

    /* No suffix, no predicate, use the right RTX code.  */
    if (e.pred == PRED_none)
      {
	rtx_code r_code;

	switch (e.mode_suffix_id)
	  {
	  case MODE_none:
	    if (e.type_suffix (0).integer_p)
	      {
		r_code = (e.type_suffix (0).unsigned_p
			  ? m_code_for_uint
			  : m_code_for_sint);
		code = code_for_mve_vcmpq (r_code, mode);
	      }
	    else
	      code = code_for_mve_vcmpq_f (m_code_for_fp, mode);
	    break;

	  case MODE_n:
	    if (e.type_suffix (0).integer_p)
	      {
		r_code = (e.type_suffix (0).unsigned_p
			  ? m_code_for_uint
			  : m_code_for_sint);
		code = code_for_mve_vcmpq_n (r_code, mode);
	      }
	    else
	      code = code_for_mve_vcmpq_n_f (m_code_for_fp, mode);
	    break;

	  default:
	    gcc_unreachable ();
	  }
	target = e.use_exact_insn (code);
      }
    else
      {
	int unspec;

	switch (e.pred)
	  {
	  case PRED_m:
	    switch (e.mode_suffix_id)
	      {
	      case MODE_none:
		/* No suffix, "m" predicate.  */
		if (e.type_suffix (0).integer_p)
		  {
		    unspec = (e.type_suffix (0).unsigned_p
			      ? m_unspec_for_m_uint
			      : m_unspec_for_m_sint);
		    code = code_for_mve_vcmpq_m (unspec, unspec, mode);
		  }
		else
		  code = code_for_mve_vcmpq_m_f (m_unspec_for_m_fp, mode);
		break;

	      case MODE_n:
		/* _n suffix, "m" predicate.  */
		if (e.type_suffix (0).integer_p)
		  {
		    unspec = (e.type_suffix (0).unsigned_p
			      ? m_unspec_for_m_n_uint
			      : m_unspec_for_m_n_sint);
		    code = code_for_mve_vcmpq_m_n (unspec, unspec, mode);
		  }
		else
		  code = code_for_mve_vcmpq_m_n_f (m_unspec_for_m_n_fp, mode);
		break;

	      default:
		gcc_unreachable ();
	      }
	    target = e.use_cond_insn (code, 0);
	    break;

	  default:
	    gcc_unreachable ();
	  }
      }

    rtx HItarget = gen_reg_rtx (HImode);
    emit_move_insn (HItarget, gen_lowpart (HImode, target));
    return HItarget;
  }
};

/* Map the function directly to CODE (UNSPEC, UNSPEC, UNSPEC, M) where
   M is the vector mode associated with type suffix 0.  USed for the
   operations where there is a "rot90" or "rot270" suffix, depending
   on the UNSPEC.  We cannot use
   unspec_based_mve_function_base::expand_unspec () because we call
   code_for_mve_q with one more parameter.  */
class unspec_mve_function_exact_insn_rot : public function_base
{
public:
  CONSTEXPR unspec_mve_function_exact_insn_rot (int unspec_for_sint,
						int unspec_for_uint,
						int unspec_for_fp,
						int unspec_for_m_sint,
						int unspec_for_m_uint,
						int unspec_for_m_fp)
    : m_unspec_for_sint (unspec_for_sint),
      m_unspec_for_uint (unspec_for_uint),
      m_unspec_for_fp (unspec_for_fp),
      m_unspec_for_m_sint (unspec_for_m_sint),
      m_unspec_for_m_uint (unspec_for_m_uint),
      m_unspec_for_m_fp (unspec_for_m_fp)
  {}

  /* The unspec code associated with signed-integer, unsigned-integer
     and floating-point operations respectively.  It covers the cases
     with and without the _m predicate.  */
  int m_unspec_for_sint;
  int m_unspec_for_uint;
  int m_unspec_for_fp;
  int m_unspec_for_m_sint;
  int m_unspec_for_m_uint;
  int m_unspec_for_m_fp;

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    insn_code code;
    int unspec;

    switch (e.pred)
      {
      case PRED_none:
	switch (e.mode_suffix_id)
	  {
	  case MODE_none:
	    /* No predicate, no suffix.  */
	    if (e.type_suffix (0).integer_p)
	      {
		unspec = (e.type_suffix (0).unsigned_p
			  ? m_unspec_for_uint
			  : m_unspec_for_sint);
		code = code_for_mve_q (unspec, unspec, unspec, mode);
	      }
	    else
	      code = code_for_mve_q_f (m_unspec_for_fp, m_unspec_for_fp, mode);
	    break;

	  default:
	    gcc_unreachable ();
	  }
	return e.use_exact_insn (code);

      case PRED_m:
      case PRED_x:
	switch (e.mode_suffix_id)
	  {
	  case MODE_none:
	    /* No suffix, "m" or "x" predicate.  */
	    if (e.type_suffix (0).integer_p)
	      {
		unspec = (e.type_suffix (0).unsigned_p
			  ? m_unspec_for_m_uint
			  : m_unspec_for_m_sint);
		code = code_for_mve_q_m (unspec, unspec, unspec, mode);
	      }
	    else
	      code = code_for_mve_q_m_f (m_unspec_for_m_fp, m_unspec_for_m_fp, mode);

	    if (e.pred == PRED_m)
	      return e.use_cond_insn (code, 0);
	    else
	      return e.use_pred_x_insn (code);
	    break;

	  default:
	    gcc_unreachable ();
	  }

      default:
	gcc_unreachable ();
      }

    gcc_unreachable ();
  }
};

/* Map the vmull-related function directly to CODE (UNSPEC, UNSPEC, M)
   where M is the vector mode associated with type suffix 0.  We need
   this special case because the builtins have _int in their
   names.  */
class unspec_mve_function_exact_insn_vmull : public function_base
{
public:
  CONSTEXPR unspec_mve_function_exact_insn_vmull (int unspec_for_sint,
						  int unspec_for_uint,
						  int unspec_for_m_sint,
						  int unspec_for_m_uint)
    : m_unspec_for_sint (unspec_for_sint),
      m_unspec_for_uint (unspec_for_uint),
      m_unspec_for_m_sint (unspec_for_m_sint),
      m_unspec_for_m_uint (unspec_for_m_uint)
  {}

  /* The unspec code associated with signed-integer and
     unsigned-integer operations respectively.  It covers the cases
     with and without the _m predicate.  */
  int m_unspec_for_sint;
  int m_unspec_for_uint;
  int m_unspec_for_m_sint;
  int m_unspec_for_m_uint;

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    insn_code code;
    int unspec;

    if (! e.type_suffix (0).integer_p)
      gcc_unreachable ();

    if (e.mode_suffix_id != MODE_none)
      gcc_unreachable ();

    switch (e.pred)
      {
      case PRED_none:
	/* No predicate, no suffix.  */
	unspec = (e.type_suffix (0).unsigned_p
		  ? m_unspec_for_uint
		  : m_unspec_for_sint);
	code = code_for_mve_q_int (unspec, unspec, mode);

	return e.use_exact_insn (code);

      case PRED_m:
      case PRED_x:
	/* No suffix, "m" or "x" predicate.  */
	unspec = (e.type_suffix (0).unsigned_p
		  ? m_unspec_for_m_uint
		  : m_unspec_for_m_sint);
	code = code_for_mve_q_int_m (unspec, unspec, mode);

	if (e.pred == PRED_m)
	  return e.use_cond_insn (code, 0);
	else
	  return e.use_pred_x_insn (code);

      default:
	gcc_unreachable ();
      }

    gcc_unreachable ();
  }
};

/* Map the vmull_poly-related function directly to CODE (UNSPEC,
   UNSPEC, M) where M is the vector mode associated with type suffix
   0.  We need this special case because the builtins have _poly in
   their names, and use the special poly type..  */
class unspec_mve_function_exact_insn_vmull_poly : public function_base
{
public:
  CONSTEXPR unspec_mve_function_exact_insn_vmull_poly (int unspec_for_poly,
						       int unspec_for_m_poly)
    : m_unspec_for_poly (unspec_for_poly),
      m_unspec_for_m_poly (unspec_for_m_poly)
  {}

  /* The unspec code associated with signed-integer, unsigned-integer
     and poly operations respectively.  It covers the cases with and
     without the _m predicate.  */
  int m_unspec_for_poly;
  int m_unspec_for_m_poly;

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    insn_code code;

    if (e.mode_suffix_id != MODE_none)
      gcc_unreachable ();

    if (! e.type_suffix (0).poly_p)
      gcc_unreachable ();

    switch (e.pred)
      {
      case PRED_none:
	/* No predicate, no suffix.  */
	code = code_for_mve_q_poly (m_unspec_for_poly, m_unspec_for_poly, mode);
	return e.use_exact_insn (code);

      case PRED_m:
      case PRED_x:
	/* No suffix, "m" or "x" predicate.  */
	code = code_for_mve_q_poly_m (m_unspec_for_m_poly, m_unspec_for_m_poly, mode);

	if (e.pred == PRED_m)
	  return e.use_cond_insn (code, 0);
	else
	  return e.use_pred_x_insn (code);

      default:
	gcc_unreachable ();
      }

    gcc_unreachable ();
  }
};

/* A function_base that sometimes or always operates on tuples of
   vectors.  */
class multi_vector_function : public function_base
{
public:
  CONSTEXPR multi_vector_function (unsigned int vectors_per_tuple)
    : m_vectors_per_tuple (vectors_per_tuple) {}

  unsigned int
  vectors_per_tuple () const override
  {
    return m_vectors_per_tuple;
  }

  /* The number of vectors in a tuple, or 1 if the function only operates
     on single vectors.  */
  unsigned int m_vectors_per_tuple;
};

/* A function_base that loads or stores contiguous memory elements
   without extending or truncating them.  */
class full_width_access : public multi_vector_function
{
public:
  CONSTEXPR full_width_access (unsigned int vectors_per_tuple = 1)
    : multi_vector_function (vectors_per_tuple) {}

  tree
  memory_scalar_type (const function_instance &fi) const override
  {
    return fi.scalar_type (0);
  }

  machine_mode
  memory_vector_mode (const function_instance &fi) const override
  {
    machine_mode mode = fi.vector_mode (0);

    if (m_vectors_per_tuple != 1)
      mode = targetm.array_mode (mode, m_vectors_per_tuple).require ();

    return mode;
  }
};

/* A function_base that loads elements from memory and extends them
   to a wider element.  The memory element type is a fixed part of
   the function base name.  */
class load_extending : public function_base
{
public:
  CONSTEXPR load_extending (type_suffix_index signed_memory_type,
			    type_suffix_index unsigned_memory_type,
			    type_suffix_index float_memory_type)
    : m_signed_memory_type (signed_memory_type),
      m_unsigned_memory_type (unsigned_memory_type),
      m_float_memory_type (float_memory_type)
  {}
  CONSTEXPR load_extending (type_suffix_index signed_memory_type,
			    type_suffix_index unsigned_memory_type)
    : m_signed_memory_type (signed_memory_type),
      m_unsigned_memory_type (unsigned_memory_type),
      m_float_memory_type (NUM_TYPE_SUFFIXES)
  {}

  unsigned int call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY;
  }

  tree memory_scalar_type (const function_instance &fi) const override
  {
    type_suffix_index memory_type_suffix
      = (fi.type_suffix (0).integer_p
	 ? (fi.type_suffix (0).unsigned_p
	    ? m_unsigned_memory_type
	    : m_signed_memory_type)
	 : m_float_memory_type);
    return scalar_types[type_suffixes[memory_type_suffix].vector_type];
  }

  machine_mode memory_vector_mode (const function_instance &fi) const override
  {
    type_suffix_index memory_type_suffix
      = (fi.type_suffix (0).integer_p
	 ? (fi.type_suffix (0).unsigned_p
	    ? m_unsigned_memory_type
	    : m_signed_memory_type)
	 : m_float_memory_type);
    machine_mode mem_mode = type_suffixes[memory_type_suffix].vector_mode;
    machine_mode reg_mode = fi.vector_mode (0);

    return arm_mve_data_mode (GET_MODE_INNER (mem_mode),
			      GET_MODE_NUNITS (reg_mode)).require ();
  }

  /* The type of the memory elements.  This is part of the function base
     name rather than a true type suffix.  */
  type_suffix_index m_signed_memory_type;
  type_suffix_index m_unsigned_memory_type;
  type_suffix_index m_float_memory_type;
};

/* A function_base that truncates vector elements and stores them to memory.
   The memory element width is a fixed part of the function base name.  */
class store_truncating : public function_base
{
public:
  CONSTEXPR store_truncating (scalar_mode to_int_mode,
			      opt_scalar_mode to_float_mode)
    : m_to_int_mode (to_int_mode), m_to_float_mode (to_float_mode)
  {}

  unsigned int call_properties (const function_instance &) const override
  {
    return CP_WRITE_MEMORY;
  }

  tree memory_scalar_type (const function_instance &fi) const override
  {
    /* In truncating stores, the signedness of the memory element is defined
       to be the same as the signedness of the vector element.  The signedness
       doesn't make any difference to the behavior of the function.  */
    type_class_index tclass = fi.type_suffix (0).tclass;
    unsigned int element_bits
      = GET_MODE_BITSIZE (fi.type_suffix (0).integer_p
			  ? m_to_int_mode
			  : m_to_float_mode.require ());
    type_suffix_index suffix = find_type_suffix (tclass, element_bits);
    return scalar_types[type_suffixes[suffix].vector_type];
  }

  machine_mode memory_vector_mode (const function_instance &fi) const override
  {
    poly_uint64 nunits = GET_MODE_NUNITS (fi.vector_mode (0));
    scalar_mode mode = (fi.type_suffix (0).integer_p
			? m_to_int_mode
			: m_to_float_mode.require ());
    return arm_mve_data_mode (mode, nunits).require ();
  }

  /* The mode of a single memory element.  */
  scalar_mode m_to_int_mode;
  opt_scalar_mode m_to_float_mode;
};

} /* end namespace arm_mve */

/* Declare the global function base NAME, creating it from an instance
   of class CLASS with constructor arguments ARGS.  */
#define FUNCTION(NAME, CLASS, ARGS) \
  namespace { static CONSTEXPR const CLASS NAME##_obj ARGS; } \
  namespace functions { const function_base *const NAME = &NAME##_obj; }

#endif
