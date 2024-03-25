/* ACLE support for AArch64 SVE (function shapes)
   Copyright (C) 2018-2024 Free Software Foundation, Inc.

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

#ifndef GCC_AARCH64_SVE_BUILTINS_SHAPES_H
#define GCC_AARCH64_SVE_BUILTINS_SHAPES_H

namespace aarch64_sve
{
  /* The naming convention is:

     - to use the name of the function if the rules are very specific to
       a particular function (e.g. svext, for which the range of the
       final immediate value is in no way generic).

     - to use names like "unary" etc. if the rules are somewhat generic,
       especially if there are no ranges involved.

     When using generic names, the handling of the final vector argument
     can be modified as follows:

     - an "_n" suffix changes the argument from a vector to a scalar.

     - an "_opt_n" suffix says that there are two forms of each function:
       one in which the argument is the usual vector, and one in which it
       is replaced by a scalar.

     - an "_opt_single" suffix similarly says that the function can take
       a vector or tuple argument, with the former having a "_single" suffix.

     - "_int" and "_uint" replace the argument's element type with a
       signed or unsigned integer of the same width.  The suffixes above
       then indicate whether this final argument is or might be a scalar.

     - "_int64" and "_uint64" similarly replace the argument's element type
       with int64_t or uint64_t.

     - "_wide" replaces the argument's element type with a 64-bit integer
       of the same signedness.  This only makes sense for integer elements.

     - "_lane" indicates that the argument is indexed by a constant lane
       number, provided as an immediately-following argument of type uint64_t.

     Also:

     - "inherent" means that the function takes no arguments.

     - "_rotate" means that the final argument is a rotation amount
       (0, 90, 180 or 270).

     - "_scalar" indicates that all data arguments are scalars rather
       than vectors.

     - in gather/scatter addresses, "sv" stands for "scalar base,
       vector displacement" while "vs" stands for "vector base,
       scalar displacement".

     - "_pred" indicates that the function takes an svbool_t argument
       that does not act as a governing predicate..  */
  namespace shapes
  {
    extern const function_shape *const adr_index;
    extern const function_shape *const adr_offset;
    extern const function_shape *const binary;
    extern const function_shape *const binary_int_opt_n;
    extern const function_shape *const binary_int_opt_single_n;
    extern const function_shape *const binary_lane;
    extern const function_shape *const binary_long_lane;
    extern const function_shape *const binary_long_opt_n;
    extern const function_shape *const binary_n;
    extern const function_shape *const binary_narrowb_opt_n;
    extern const function_shape *const binary_narrowt_opt_n;
    extern const function_shape *const binary_opt_n;
    extern const function_shape *const binary_opt_single_n;
    extern const function_shape *const binary_pred;
    extern const function_shape *const binary_rotate;
    extern const function_shape *const binary_scalar;
    extern const function_shape *const binary_single;
    extern const function_shape *const binary_to_uint;
    extern const function_shape *const binary_uint;
    extern const function_shape *const binary_uint_n;
    extern const function_shape *const binary_uint_opt_n;
    extern const function_shape *const binary_uint64_n;
    extern const function_shape *const binary_uint64_opt_n;
    extern const function_shape *const binary_wide;
    extern const function_shape *const binary_wide_opt_n;
    extern const function_shape *const binary_za_int_m;
    extern const function_shape *const binary_za_m;
    extern const function_shape *const binary_za_slice_lane;
    extern const function_shape *const binary_za_slice_int_opt_single;
    extern const function_shape *const binary_za_slice_opt_single;
    extern const function_shape *const binary_za_slice_uint_opt_single;
    extern const function_shape *const binary_za_uint_m;
    extern const function_shape *const binaryxn;
    extern const function_shape *const bool_inherent;
    extern const function_shape *const clamp;
    extern const function_shape *const clast;
    extern const function_shape *const compare;
    extern const function_shape *const compare_opt_n;
    extern const function_shape *const compare_ptr;
    extern const function_shape *const compare_scalar;
    extern const function_shape *const compare_scalar_count;
    extern const function_shape *const compare_wide_opt_n;
    extern const function_shape *const count_inherent;
    extern const function_shape *const count_pat;
    extern const function_shape *const count_pred;
    extern const function_shape *const count_pred_c;
    extern const function_shape *const count_vector;
    extern const function_shape *const create;
    extern const function_shape *const dot_za_slice_int_lane;
    extern const function_shape *const dot_za_slice_lane;
    extern const function_shape *const dot_za_slice_uint_lane;
    extern const function_shape *const dupq;
    extern const function_shape *const dup_neonq;
    extern const function_shape *const ext;
    extern const function_shape *const extract_pred;
    extern const function_shape *const fold_left;
    extern const function_shape *const get;
    extern const function_shape *const get_neonq;
    extern const function_shape *const inc_dec;
    extern const function_shape *const inc_dec_pat;
    extern const function_shape *const inc_dec_pred;
    extern const function_shape *const inc_dec_pred_scalar;
    extern const function_shape *const inherent;
    extern const function_shape *const inherent_b;
    extern const function_shape *const inherent_za;
    extern const function_shape *const inherent_zt;
    extern const function_shape *const inherent_mask_za;
    extern const function_shape *const ldr_zt;
    extern const function_shape *const ldr_za;
    extern const function_shape *const load;
    extern const function_shape *const load_ext;
    extern const function_shape *const load_ext_gather_index;
    extern const function_shape *const load_ext_gather_index_restricted;
    extern const function_shape *const load_ext_gather_offset;
    extern const function_shape *const load_ext_gather_offset_restricted;
    extern const function_shape *const load_gather_sv;
    extern const function_shape *const load_gather_sv_restricted;
    extern const function_shape *const load_gather_vs;
    extern const function_shape *const load_replicate;
    extern const function_shape *const load_za;
    extern const function_shape *const luti2_lane_zt;
    extern const function_shape *const luti4_lane_zt;
    extern const function_shape *const mmla;
    extern const function_shape *const pattern_pred;
    extern const function_shape *const prefetch;
    extern const function_shape *const prefetch_gather_index;
    extern const function_shape *const prefetch_gather_offset;
    extern const function_shape *const ptest;
    extern const function_shape *const rdffr;
    extern const function_shape *const read_za;
    extern const function_shape *const read_za_m;
    extern const function_shape *const read_za_slice;
    extern const function_shape *const reduction;
    extern const function_shape *const reduction_wide;
    extern const function_shape *const reinterpret;
    extern const function_shape *const select_pred;
    extern const function_shape *const set;
    extern const function_shape *const setffr;
    extern const function_shape *const set_neonq;
    extern const function_shape *const shift_left_imm_long;
    extern const function_shape *const shift_left_imm_to_uint;
    extern const function_shape *const shift_right_imm;
    extern const function_shape *const shift_right_imm_narrowb;
    extern const function_shape *const shift_right_imm_narrowt;
    extern const function_shape *const shift_right_imm_narrowxn;
    extern const function_shape *const shift_right_imm_narrowb_to_uint;
    extern const function_shape *const shift_right_imm_narrowt_to_uint;
    extern const function_shape *const store;
    extern const function_shape *const store_scatter_index;
    extern const function_shape *const store_scatter_index_restricted;
    extern const function_shape *const store_scatter_offset;
    extern const function_shape *const store_scatter_offset_restricted;
    extern const function_shape *const store_za;
    extern const function_shape *const storexn;
    extern const function_shape *const str_za;
    extern const function_shape *const str_zt;
    extern const function_shape *const tbl_tuple;
    extern const function_shape *const ternary_bfloat;
    extern const function_shape *const ternary_bfloat_lane;
    extern const function_shape *const ternary_bfloat_lanex2;
    extern const function_shape *const ternary_bfloat_opt_n;
    extern const function_shape *const ternary_intq_uintq_lane;
    extern const function_shape *const ternary_intq_uintq_opt_n;
    extern const function_shape *const ternary_lane;
    extern const function_shape *const ternary_lane_rotate;
    extern const function_shape *const ternary_long_lane;
    extern const function_shape *const ternary_long_opt_n;
    extern const function_shape *const ternary_opt_n;
    extern const function_shape *const ternary_qq_or_011_lane;
    extern const function_shape *const ternary_qq_lane_rotate;
    extern const function_shape *const ternary_qq_opt_n_or_011;
    extern const function_shape *const ternary_qq_rotate;
    extern const function_shape *const ternary_rotate;
    extern const function_shape *const ternary_shift_left_imm;
    extern const function_shape *const ternary_shift_right_imm;
    extern const function_shape *const ternary_uint;
    extern const function_shape *const ternary_uintq_intq;
    extern const function_shape *const ternary_uintq_intq_lane;
    extern const function_shape *const ternary_uintq_intq_opt_n;
    extern const function_shape *const tmad;
    extern const function_shape *const unary;
    extern const function_shape *const unary_convert;
    extern const function_shape *const unary_convert_narrowt;
    extern const function_shape *const unary_convertxn;
    extern const function_shape *const unary_long;
    extern const function_shape *const unary_n;
    extern const function_shape *const unary_narrowb;
    extern const function_shape *const unary_narrowt;
    extern const function_shape *const unary_narrowb_to_uint;
    extern const function_shape *const unary_narrowt_to_uint;
    extern const function_shape *const unary_pred;
    extern const function_shape *const unary_to_int;
    extern const function_shape *const unary_to_uint;
    extern const function_shape *const unary_uint;
    extern const function_shape *const unary_widen;
    extern const function_shape *const unary_za_m;
    extern const function_shape *const unary_za_slice;
    extern const function_shape *const unaryxn;
    extern const function_shape *const write_za;
    extern const function_shape *const write_za_m;
    extern const function_shape *const write_za_slice;
  }
}

#endif
