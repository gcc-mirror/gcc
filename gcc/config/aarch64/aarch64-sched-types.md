;; Instruction Classification for AArch64.
;;
;; Copyright (C) 2026-2026 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Attribute that classifies SVE instructions by their functional type.
;; This attribute is currently only used for dispatch scheduling.
(define_attr "sve_type"
  "none,\
   sve_pred_logical,sve_pred_cnt_ctrl,sve_pred_cnt_scalar,sve_pred_misc,\
   sve_pred_vec,\
   sve_fp_arith,sve_fp_mul,sve_fp_assoc_add,sve_fp_cmp,sve_fp_cvt,sve_fp_log,\
   sve_fp_exp,sve_fp_div,sve_fp_sqrt,sve_fp_reduc,sve_fp_misc,\
   sve_int_general,sve_int_shift,sve_int_mul,sve_int_dot,sve_int_cmp_set,\
   sve_int_extend,sve_int_extract,sve_int_cvt,sve_int_div,sve_int_index,\
   sve_int_match,sve_int_recip_est,sve_int_reduc,sve_int_accum,\
   sve_int_pmul,sve_int_bit_perm,\
   sve_load_pred,sve_load_1reg,sve_load_2reg,sve_load_3reg,sve_load_4reg,\
   sve_gatherload_32,sve_gatherload_64,\
   sve_store_pred,sve_store_1reg,sve_store_2reg,sve_store_3reg,sve_store_4reg,\
   sve_scatterstore_32,sve_scatterstore_64,\
   sve_ffr,\
   sve_crypto_sha3"
  (const_string "none"))
