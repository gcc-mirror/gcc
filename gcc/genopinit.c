/* Generate code to initialize optabs from machine description.
   Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
   2001, 2002, 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

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


#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "errors.h"
#include "gensupport.h"


/* Many parts of GCC use arrays that are indexed by machine mode and
   contain the insn codes for pattern in the MD file that perform a given
   operation on operands of that mode.

   These patterns are present in the MD file with names that contain
   the mode(s) used and the name of the operation.  This program
   writes a function `init_all_optabs' that initializes the optabs with
   all the insn codes of the relevant patterns present in the MD file.

   This array contains a list of optabs that need to be initialized.  Within
   each string, the name of the pattern to be matched against is delimited
   with $( and $).  In the string, $a and $b are used to match a short mode
   name (the part of the mode name not including `mode' and converted to
   lower-case).  When writing out the initializer, the entire string is
   used.  $A and $B are replaced with the full name of the mode; $a and $b
   are replaced with the short form of the name, as above.

   If $N is present in the pattern, it means the two modes must be consecutive
   widths in the same mode class (e.g, QImode and HImode).  $I means that
   only full integer modes should be considered for the next mode, and $F
   means that only float modes should be considered.
   $P means that both full and partial integer modes should be considered.
   $Q means that only fixed-point modes should be considered.

   $V means to emit 'v' if the first mode is a MODE_FLOAT mode.

   For some optabs, we store the operation by RTL codes.  These are only
   used for comparisons.  In that case, $c and $C are the lower-case and
   upper-case forms of the comparison, respectively.  */

static const char * const optabs[] =
{ "convert_optab_handler (sext_optab, $B, $A)->insn_code = CODE_FOR_$(extend$a$b2$)",
  "convert_optab_handler (zext_optab, $B, $A)->insn_code = CODE_FOR_$(zero_extend$a$b2$)",
  "convert_optab_handler (sfix_optab, $B, $A)->insn_code = CODE_FOR_$(fix$F$a$I$b2$)",
  "convert_optab_handler (ufix_optab, $B, $A)->insn_code = CODE_FOR_$(fixuns$F$a$b2$)",
  "convert_optab_handler (sfixtrunc_optab, $B, $A)->insn_code = CODE_FOR_$(fix_trunc$F$a$I$b2$)",
  "convert_optab_handler (ufixtrunc_optab, $B, $A)->insn_code = CODE_FOR_$(fixuns_trunc$F$a$I$b2$)",
  "convert_optab_handler (sfloat_optab, $B, $A)->insn_code = CODE_FOR_$(float$I$a$F$b2$)",
  "convert_optab_handler (ufloat_optab, $B, $A)->insn_code = CODE_FOR_$(floatuns$I$a$F$b2$)",
  "convert_optab_handler (trunc_optab, $B, $A)->insn_code = CODE_FOR_$(trunc$a$b2$)",
  "convert_optab_handler (fract_optab, $B, $A)->insn_code = CODE_FOR_$(fract$a$b2$)",
  "convert_optab_handler (fractuns_optab, $B, $A)->insn_code = CODE_FOR_$(fractuns$I$a$Q$b2$)",
  "convert_optab_handler (fractuns_optab, $B, $A)->insn_code = CODE_FOR_$(fractuns$Q$a$I$b2$)",
  "convert_optab_handler (satfract_optab, $B, $A)->insn_code = CODE_FOR_$(satfract$a$Q$b2$)",
  "convert_optab_handler (satfractuns_optab, $B, $A)->insn_code = CODE_FOR_$(satfractuns$I$a$Q$b2$)",
  "optab_handler (add_optab, $A)->insn_code = CODE_FOR_$(add$P$a3$)",
  "optab_handler (addv_optab, $A)->insn_code =\n\
    optab_handler (add_optab, $A)->insn_code = CODE_FOR_$(add$F$a3$)",
  "optab_handler (addv_optab, $A)->insn_code = CODE_FOR_$(addv$I$a3$)",
  "optab_handler (add_optab, $A)->insn_code = CODE_FOR_$(add$Q$a3$)",
  "optab_handler (ssadd_optab, $A)->insn_code = CODE_FOR_$(ssadd$Q$a3$)",
  "optab_handler (usadd_optab, $A)->insn_code = CODE_FOR_$(usadd$Q$a3$)",
  "optab_handler (sub_optab, $A)->insn_code = CODE_FOR_$(sub$P$a3$)",
  "optab_handler (subv_optab, $A)->insn_code =\n\
    optab_handler (sub_optab, $A)->insn_code = CODE_FOR_$(sub$F$a3$)",
  "optab_handler (subv_optab, $A)->insn_code = CODE_FOR_$(subv$I$a3$)",
  "optab_handler (sub_optab, $A)->insn_code = CODE_FOR_$(sub$Q$a3$)",
  "optab_handler (sssub_optab, $A)->insn_code = CODE_FOR_$(sssub$Q$a3$)",
  "optab_handler (ussub_optab, $A)->insn_code = CODE_FOR_$(ussub$Q$a3$)",
  "optab_handler (smul_optab, $A)->insn_code = CODE_FOR_$(mul$Q$a3$)",
  "optab_handler (ssmul_optab, $A)->insn_code = CODE_FOR_$(ssmul$Q$a3$)",
  "optab_handler (usmul_optab, $A)->insn_code = CODE_FOR_$(usmul$Q$a3$)",
  "optab_handler (smul_optab, $A)->insn_code = CODE_FOR_$(mul$P$a3$)",
  "optab_handler (smulv_optab, $A)->insn_code =\n\
    optab_handler (smul_optab, $A)->insn_code = CODE_FOR_$(mul$F$a3$)",
  "optab_handler (smulv_optab, $A)->insn_code = CODE_FOR_$(mulv$I$a3$)",
  "optab_handler (umul_highpart_optab, $A)->insn_code = CODE_FOR_$(umul$a3_highpart$)",
  "optab_handler (smul_highpart_optab, $A)->insn_code = CODE_FOR_$(smul$a3_highpart$)",
  "optab_handler (smul_widen_optab, $B)->insn_code = CODE_FOR_$(mul$a$b3$)$N",
  "optab_handler (umul_widen_optab, $B)->insn_code = CODE_FOR_$(umul$a$b3$)$N",
  "optab_handler (usmul_widen_optab, $B)->insn_code = CODE_FOR_$(usmul$a$b3$)$N",
  "optab_handler (smadd_widen_optab, $B)->insn_code = CODE_FOR_$(madd$a$b4$)$N",
  "optab_handler (umadd_widen_optab, $B)->insn_code = CODE_FOR_$(umadd$a$b4$)$N",
  "optab_handler (ssmadd_widen_optab, $B)->insn_code = CODE_FOR_$(ssmadd$a$b4$)$N",
  "optab_handler (usmadd_widen_optab, $B)->insn_code = CODE_FOR_$(usmadd$a$b4$)$N",
  "optab_handler (smsub_widen_optab, $B)->insn_code = CODE_FOR_$(msub$a$b4$)$N",
  "optab_handler (umsub_widen_optab, $B)->insn_code = CODE_FOR_$(umsub$a$b4$)$N",
  "optab_handler (ssmsub_widen_optab, $B)->insn_code = CODE_FOR_$(ssmsub$a$b4$)$N",
  "optab_handler (usmsub_widen_optab, $B)->insn_code = CODE_FOR_$(usmsub$a$b4$)$N",
  "optab_handler (sdiv_optab, $A)->insn_code = CODE_FOR_$(div$a3$)",
  "optab_handler (ssdiv_optab, $A)->insn_code = CODE_FOR_$(ssdiv$Q$a3$)",
  "optab_handler (sdivv_optab, $A)->insn_code = CODE_FOR_$(div$V$I$a3$)",
  "optab_handler (udiv_optab, $A)->insn_code = CODE_FOR_$(udiv$I$a3$)",
  "optab_handler (udiv_optab, $A)->insn_code = CODE_FOR_$(udiv$Q$a3$)",
  "optab_handler (usdiv_optab, $A)->insn_code = CODE_FOR_$(usdiv$Q$a3$)",
  "optab_handler (sdivmod_optab, $A)->insn_code = CODE_FOR_$(divmod$a4$)",
  "optab_handler (udivmod_optab, $A)->insn_code = CODE_FOR_$(udivmod$a4$)",
  "optab_handler (smod_optab, $A)->insn_code = CODE_FOR_$(mod$a3$)",
  "optab_handler (umod_optab, $A)->insn_code = CODE_FOR_$(umod$a3$)",
  "optab_handler (fmod_optab, $A)->insn_code = CODE_FOR_$(fmod$a3$)",
  "optab_handler (remainder_optab, $A)->insn_code = CODE_FOR_$(remainder$a3$)",
  "optab_handler (ftrunc_optab, $A)->insn_code = CODE_FOR_$(ftrunc$F$a2$)",
  "optab_handler (and_optab, $A)->insn_code = CODE_FOR_$(and$a3$)",
  "optab_handler (ior_optab, $A)->insn_code = CODE_FOR_$(ior$a3$)",
  "optab_handler (xor_optab, $A)->insn_code = CODE_FOR_$(xor$a3$)",
  "optab_handler (ashl_optab, $A)->insn_code = CODE_FOR_$(ashl$a3$)",
  "optab_handler (ssashl_optab, $A)->insn_code = CODE_FOR_$(ssashl$Q$a3$)",
  "optab_handler (usashl_optab, $A)->insn_code = CODE_FOR_$(usashl$Q$a3$)",
  "optab_handler (ashr_optab, $A)->insn_code = CODE_FOR_$(ashr$a3$)",
  "optab_handler (lshr_optab, $A)->insn_code = CODE_FOR_$(lshr$a3$)",
  "optab_handler (rotl_optab, $A)->insn_code = CODE_FOR_$(rotl$a3$)",
  "optab_handler (rotr_optab, $A)->insn_code = CODE_FOR_$(rotr$a3$)",
  "optab_handler (vashr_optab, $A)->insn_code = CODE_FOR_$(vashr$a3$)",
  "optab_handler (vlshr_optab, $A)->insn_code = CODE_FOR_$(vlshr$a3$)",
  "optab_handler (vashl_optab, $A)->insn_code = CODE_FOR_$(vashl$a3$)",
  "optab_handler (vrotl_optab, $A)->insn_code = CODE_FOR_$(vrotl$a3$)",
  "optab_handler (vrotr_optab, $A)->insn_code = CODE_FOR_$(vrotr$a3$)",
  "optab_handler (smin_optab, $A)->insn_code = CODE_FOR_$(smin$a3$)",
  "optab_handler (smax_optab, $A)->insn_code = CODE_FOR_$(smax$a3$)",
  "optab_handler (umin_optab, $A)->insn_code = CODE_FOR_$(umin$I$a3$)",
  "optab_handler (umax_optab, $A)->insn_code = CODE_FOR_$(umax$I$a3$)",
  "optab_handler (pow_optab, $A)->insn_code = CODE_FOR_$(pow$a3$)",
  "optab_handler (atan2_optab, $A)->insn_code = CODE_FOR_$(atan2$a3$)",
  "optab_handler (neg_optab, $A)->insn_code = CODE_FOR_$(neg$P$a2$)",
  "optab_handler (negv_optab, $A)->insn_code =\n\
    optab_handler (neg_optab, $A)->insn_code = CODE_FOR_$(neg$F$a2$)",
  "optab_handler (negv_optab, $A)->insn_code = CODE_FOR_$(negv$I$a2$)",
  "optab_handler (neg_optab, $A)->insn_code = CODE_FOR_$(neg$Q$a2$)",
  "optab_handler (ssneg_optab, $A)->insn_code = CODE_FOR_$(ssneg$Q$a2$)",
  "optab_handler (usneg_optab, $A)->insn_code = CODE_FOR_$(usneg$Q$a2$)",
  "optab_handler (abs_optab, $A)->insn_code = CODE_FOR_$(abs$P$a2$)",
  "optab_handler (absv_optab, $A)->insn_code =\n\
    optab_handler (abs_optab, $A)->insn_code = CODE_FOR_$(abs$F$a2$)",
  "optab_handler (absv_optab, $A)->insn_code = CODE_FOR_$(absv$I$a2$)",
  "optab_handler (copysign_optab, $A)->insn_code = CODE_FOR_$(copysign$F$a3$)",
  "optab_handler (signbit_optab, $A)->insn_code = CODE_FOR_$(signbit$F$a2$)",
  "optab_handler (isinf_optab, $A)->insn_code = CODE_FOR_$(isinf$a2$)",
  "optab_handler (sqrt_optab, $A)->insn_code = CODE_FOR_$(sqrt$a2$)",
  "optab_handler (floor_optab, $A)->insn_code = CODE_FOR_$(floor$a2$)",
  "convert_optab_handler (lfloor_optab, $B, $A)->insn_code = CODE_FOR_$(lfloor$F$a$I$b2$)",
  "optab_handler (ceil_optab, $A)->insn_code = CODE_FOR_$(ceil$a2$)",
  "convert_optab_handler (lceil_optab, $B, $A)->insn_code = CODE_FOR_$(lceil$F$a$I$b2$)",
  "optab_handler (round_optab, $A)->insn_code = CODE_FOR_$(round$a2$)",
  "optab_handler (btrunc_optab, $A)->insn_code = CODE_FOR_$(btrunc$a2$)",
  "optab_handler (nearbyint_optab, $A)->insn_code = CODE_FOR_$(nearbyint$a2$)",
  "optab_handler (rint_optab, $A)->insn_code = CODE_FOR_$(rint$a2$)",
  "convert_optab_handler (lrint_optab, $B, $A)->insn_code = CODE_FOR_$(lrint$F$a$I$b2$)",
  "convert_optab_handler (lround_optab, $B, $A)->insn_code = CODE_FOR_$(lround$F$a$I$b2$)",
  "optab_handler (sincos_optab, $A)->insn_code = CODE_FOR_$(sincos$a3$)",
  "optab_handler (sin_optab, $A)->insn_code = CODE_FOR_$(sin$a2$)",
  "optab_handler (asin_optab, $A)->insn_code = CODE_FOR_$(asin$a2$)",
  "optab_handler (cos_optab, $A)->insn_code = CODE_FOR_$(cos$a2$)",
  "optab_handler (acos_optab, $A)->insn_code = CODE_FOR_$(acos$a2$)",
  "optab_handler (exp_optab, $A)->insn_code = CODE_FOR_$(exp$a2$)",
  "optab_handler (exp10_optab, $A)->insn_code = CODE_FOR_$(exp10$a2$)",
  "optab_handler (exp2_optab, $A)->insn_code = CODE_FOR_$(exp2$a2$)",
  "optab_handler (expm1_optab, $A)->insn_code = CODE_FOR_$(expm1$a2$)",
  "optab_handler (ldexp_optab, $A)->insn_code = CODE_FOR_$(ldexp$a3$)",
  "optab_handler (scalb_optab, $A)->insn_code = CODE_FOR_$(scalb$a3$)",
  "optab_handler (logb_optab, $A)->insn_code = CODE_FOR_$(logb$a2$)",
  "optab_handler (ilogb_optab, $A)->insn_code = CODE_FOR_$(ilogb$a2$)",
  "optab_handler (log_optab, $A)->insn_code = CODE_FOR_$(log$a2$)",
  "optab_handler (log10_optab, $A)->insn_code = CODE_FOR_$(log10$a2$)",  
  "optab_handler (log2_optab, $A)->insn_code = CODE_FOR_$(log2$a2$)",  
  "optab_handler (log1p_optab, $A)->insn_code = CODE_FOR_$(log1p$a2$)",  
  "optab_handler (tan_optab, $A)->insn_code = CODE_FOR_$(tan$a2$)",
  "optab_handler (atan_optab, $A)->insn_code = CODE_FOR_$(atan$a2$)",
  "optab_handler (strlen_optab, $A)->insn_code = CODE_FOR_$(strlen$a$)",
  "optab_handler (one_cmpl_optab, $A)->insn_code = CODE_FOR_$(one_cmpl$a2$)",
  "optab_handler (bswap_optab, $A)->insn_code = CODE_FOR_$(bswap$a2$)",
  "optab_handler (ffs_optab, $A)->insn_code = CODE_FOR_$(ffs$a2$)",
  "optab_handler (clz_optab, $A)->insn_code = CODE_FOR_$(clz$a2$)",
  "optab_handler (ctz_optab, $A)->insn_code = CODE_FOR_$(ctz$a2$)",
  "optab_handler (popcount_optab, $A)->insn_code = CODE_FOR_$(popcount$a2$)",
  "optab_handler (parity_optab, $A)->insn_code = CODE_FOR_$(parity$a2$)",
  "optab_handler (mov_optab, $A)->insn_code = CODE_FOR_$(mov$a$)",
  "optab_handler (movstrict_optab, $A)->insn_code = CODE_FOR_$(movstrict$a$)",
  "optab_handler (movmisalign_optab, $A)->insn_code = CODE_FOR_$(movmisalign$a$)",
  "optab_handler (storent_optab, $A)->insn_code = CODE_FOR_$(storent$a$)",
  "optab_handler (cmp_optab, $A)->insn_code = CODE_FOR_$(cmp$a$)",
  "optab_handler (tst_optab, $A)->insn_code = CODE_FOR_$(tst$a$)",
  "optab_handler (addcc_optab, $A)->insn_code = CODE_FOR_$(add$acc$)",
  "bcc_gen_fctn[$C] = gen_$(b$c$)",
  "setcc_gen_code[$C] = CODE_FOR_$(s$c$)",
  "movcc_gen_code[$A] = CODE_FOR_$(mov$acc$)",
  "optab_handler (cbranch_optab, $A)->insn_code = CODE_FOR_$(cbranch$a4$)",
  "optab_handler (cmov_optab, $A)->insn_code = CODE_FOR_$(cmov$a6$)",
  "optab_handler (cstore_optab, $A)->insn_code = CODE_FOR_$(cstore$a4$)",
  "optab_handler (push_optab, $A)->insn_code = CODE_FOR_$(push$a1$)",
  "reload_in_optab[$A] = CODE_FOR_$(reload_in$a$)",
  "reload_out_optab[$A] = CODE_FOR_$(reload_out$a$)",
  "movmem_optab[$A] = CODE_FOR_$(movmem$a$)",
  "cmpstr_optab[$A] = CODE_FOR_$(cmpstr$a$)",
  "cmpstrn_optab[$A] = CODE_FOR_$(cmpstrn$a$)",
  "cmpmem_optab[$A] = CODE_FOR_$(cmpmem$a$)",
  "setmem_optab[$A] = CODE_FOR_$(setmem$a$)",
  "sync_add_optab[$A] = CODE_FOR_$(sync_add$I$a$)",
  "sync_sub_optab[$A] = CODE_FOR_$(sync_sub$I$a$)",
  "sync_ior_optab[$A] = CODE_FOR_$(sync_ior$I$a$)",
  "sync_and_optab[$A] = CODE_FOR_$(sync_and$I$a$)",
  "sync_xor_optab[$A] = CODE_FOR_$(sync_xor$I$a$)",
  "sync_nand_optab[$A] = CODE_FOR_$(sync_nand$I$a$)",
  "sync_old_add_optab[$A] = CODE_FOR_$(sync_old_add$I$a$)",
  "sync_old_sub_optab[$A] = CODE_FOR_$(sync_old_sub$I$a$)",
  "sync_old_ior_optab[$A] = CODE_FOR_$(sync_old_ior$I$a$)",
  "sync_old_and_optab[$A] = CODE_FOR_$(sync_old_and$I$a$)",
  "sync_old_xor_optab[$A] = CODE_FOR_$(sync_old_xor$I$a$)",
  "sync_old_nand_optab[$A] = CODE_FOR_$(sync_old_nand$I$a$)",
  "sync_new_add_optab[$A] = CODE_FOR_$(sync_new_add$I$a$)",
  "sync_new_sub_optab[$A] = CODE_FOR_$(sync_new_sub$I$a$)",
  "sync_new_ior_optab[$A] = CODE_FOR_$(sync_new_ior$I$a$)",
  "sync_new_and_optab[$A] = CODE_FOR_$(sync_new_and$I$a$)",
  "sync_new_xor_optab[$A] = CODE_FOR_$(sync_new_xor$I$a$)",
  "sync_new_nand_optab[$A] = CODE_FOR_$(sync_new_nand$I$a$)",
  "sync_compare_and_swap[$A] = CODE_FOR_$(sync_compare_and_swap$I$a$)",
  "sync_compare_and_swap_cc[$A] = CODE_FOR_$(sync_compare_and_swap_cc$I$a$)",
  "sync_lock_test_and_set[$A] = CODE_FOR_$(sync_lock_test_and_set$I$a$)",
  "sync_lock_release[$A] = CODE_FOR_$(sync_lock_release$I$a$)",
  "optab_handler (vec_set_optab, $A)->insn_code = CODE_FOR_$(vec_set$a$)",
  "optab_handler (vec_extract_optab, $A)->insn_code = CODE_FOR_$(vec_extract$a$)",
  "optab_handler (vec_extract_even_optab, $A)->insn_code = CODE_FOR_$(vec_extract_even$a$)",
  "optab_handler (vec_extract_odd_optab, $A)->insn_code = CODE_FOR_$(vec_extract_odd$a$)",
  "optab_handler (vec_interleave_high_optab, $A)->insn_code = CODE_FOR_$(vec_interleave_high$a$)",
  "optab_handler (vec_interleave_low_optab, $A)->insn_code = CODE_FOR_$(vec_interleave_low$a$)",
  "optab_handler (vec_init_optab, $A)->insn_code = CODE_FOR_$(vec_init$a$)",
  "optab_handler (vec_shl_optab, $A)->insn_code = CODE_FOR_$(vec_shl_$a$)",
  "optab_handler (vec_shr_optab, $A)->insn_code = CODE_FOR_$(vec_shr_$a$)",
  "optab_handler (vec_realign_load_optab, $A)->insn_code = CODE_FOR_$(vec_realign_load_$a$)",
  "vcond_gen_code[$A] = CODE_FOR_$(vcond$a$)",
  "vcondu_gen_code[$A] = CODE_FOR_$(vcondu$a$)",
  "optab_handler (ssum_widen_optab, $A)->insn_code = CODE_FOR_$(widen_ssum$I$a3$)",
  "optab_handler (usum_widen_optab, $A)->insn_code = CODE_FOR_$(widen_usum$I$a3$)",
  "optab_handler (udot_prod_optab, $A)->insn_code = CODE_FOR_$(udot_prod$I$a$)",
  "optab_handler (sdot_prod_optab, $A)->insn_code = CODE_FOR_$(sdot_prod$I$a$)",
  "optab_handler (reduc_smax_optab, $A)->insn_code = CODE_FOR_$(reduc_smax_$a$)",
  "optab_handler (reduc_umax_optab, $A)->insn_code = CODE_FOR_$(reduc_umax_$a$)",
  "optab_handler (reduc_smin_optab, $A)->insn_code = CODE_FOR_$(reduc_smin_$a$)",
  "optab_handler (reduc_umin_optab, $A)->insn_code = CODE_FOR_$(reduc_umin_$a$)",
  "optab_handler (reduc_splus_optab, $A)->insn_code = CODE_FOR_$(reduc_splus_$a$)" ,
  "optab_handler (reduc_uplus_optab, $A)->insn_code = CODE_FOR_$(reduc_uplus_$a$)",
  "optab_handler (vec_widen_umult_hi_optab, $A)->insn_code = CODE_FOR_$(vec_widen_umult_hi_$a$)",
  "optab_handler (vec_widen_umult_lo_optab, $A)->insn_code = CODE_FOR_$(vec_widen_umult_lo_$a$)",
  "optab_handler (vec_widen_smult_hi_optab, $A)->insn_code = CODE_FOR_$(vec_widen_smult_hi_$a$)",
  "optab_handler (vec_widen_smult_lo_optab, $A)->insn_code = CODE_FOR_$(vec_widen_smult_lo_$a$)",
  "optab_handler (vec_unpacks_hi_optab, $A)->insn_code = CODE_FOR_$(vec_unpacks_hi_$a$)",
  "optab_handler (vec_unpacks_lo_optab, $A)->insn_code = CODE_FOR_$(vec_unpacks_lo_$a$)",
  "optab_handler (vec_unpacku_hi_optab, $A)->insn_code = CODE_FOR_$(vec_unpacku_hi_$a$)",
  "optab_handler (vec_unpacku_lo_optab, $A)->insn_code = CODE_FOR_$(vec_unpacku_lo_$a$)",
  "optab_handler (vec_unpacks_float_hi_optab, $A)->insn_code = CODE_FOR_$(vec_unpacks_float_hi_$a$)",
  "optab_handler (vec_unpacks_float_lo_optab, $A)->insn_code = CODE_FOR_$(vec_unpacks_float_lo_$a$)",
  "optab_handler (vec_unpacku_float_hi_optab, $A)->insn_code = CODE_FOR_$(vec_unpacku_float_hi_$a$)",
  "optab_handler (vec_unpacku_float_lo_optab, $A)->insn_code = CODE_FOR_$(vec_unpacku_float_lo_$a$)",
  "optab_handler (vec_pack_trunc_optab, $A)->insn_code = CODE_FOR_$(vec_pack_trunc_$a$)",
  "optab_handler (vec_pack_ssat_optab, $A)->insn_code = CODE_FOR_$(vec_pack_ssat_$a$)",
  "optab_handler (vec_pack_usat_optab, $A)->insn_code = CODE_FOR_$(vec_pack_usat_$a$)",
  "optab_handler (vec_pack_sfix_trunc_optab, $A)->insn_code = CODE_FOR_$(vec_pack_sfix_trunc_$a$)",
  "optab_handler (vec_pack_ufix_trunc_optab, $A)->insn_code = CODE_FOR_$(vec_pack_ufix_trunc_$a$)"
};

static void gen_insn (rtx);

static void
gen_insn (rtx insn)
{
  const char *name = XSTR (insn, 0);
  int m1 = 0, m2 = 0, op = 0;
  size_t pindex;
  int i;
  const char *np, *pp, *p, *q;

  /* Don't mention instructions whose names are the null string.
     They are in the machine description just to be recognized.  */
  if (*name == 0)
    return;

  /* See if NAME matches one of the patterns we have for the optabs we know
     about.  */

  for (pindex = 0; pindex < ARRAY_SIZE (optabs); pindex++)
    {
      int force_float = 0, force_int = 0, force_partial_int = 0;
      int force_fixed = 0;
      int force_consec = 0;
      int matches = 1;

      for (pp = optabs[pindex]; pp[0] != '$' || pp[1] != '('; pp++)
	;

      for (pp += 2, np = name; matches && ! (pp[0] == '$' && pp[1] == ')');
	   pp++)
	{
	  if (*pp != '$')
	    {
	      if (*pp != *np++)
		break;
	    }
	  else
	    switch (*++pp)
	      {
	      case 'N':
		force_consec = 1;
		break;
	      case 'I':
		force_int = 1;
		break;
	      case 'P':
                force_partial_int = 1;
                break;
	      case 'F':
		force_float = 1;
		break;
	      case 'Q':
		force_fixed = 1;
		break;
	      case 'V':
                break;
	      case 'c':
		for (op = 0; op < NUM_RTX_CODE; op++)
		  {
		    for (p = GET_RTX_NAME(op), q = np; *p; p++, q++)
		      if (*p != *q)
			break;

		    /* We have to be concerned about matching "gt" and
		       missing "gtu", e.g., so verify we have reached the
		       end of thing we are to match.  */
		    if (*p == 0 && *q == 0
			&& (GET_RTX_CLASS (op) == RTX_COMPARE
			    || GET_RTX_CLASS (op) == RTX_COMM_COMPARE))
		      break;
		  }

		if (op == NUM_RTX_CODE)
		  matches = 0;
		else
		  np += strlen (GET_RTX_NAME(op));
		break;
	      case 'a':
	      case 'b':
		/* This loop will stop at the first prefix match, so
                   look through the modes in reverse order, in case
                   there are extra CC modes and CC is a prefix of the
                   CC modes (as it should be).  */
		for (i = (MAX_MACHINE_MODE) - 1; i >= 0; i--)
		  {
		    for (p = GET_MODE_NAME(i), q = np; *p; p++, q++)
		      if (TOLOWER (*p) != *q)
			break;

		    if (*p == 0
			&& (! force_int || mode_class[i] == MODE_INT 
			    || mode_class[i] == MODE_VECTOR_INT)
		        && (! force_partial_int
                            || mode_class[i] == MODE_INT
                            || mode_class[i] == MODE_PARTIAL_INT
			    || mode_class[i] == MODE_VECTOR_INT)
			&& (! force_float
			    || mode_class[i] == MODE_FLOAT 
			    || mode_class[i] == MODE_DECIMAL_FLOAT
			    || mode_class[i] == MODE_COMPLEX_FLOAT
			    || mode_class[i] == MODE_VECTOR_FLOAT)
			&& (! force_fixed
			    || mode_class[i] == MODE_FRACT
			    || mode_class[i] == MODE_UFRACT
			    || mode_class[i] == MODE_ACCUM
			    || mode_class[i] == MODE_UACCUM
			    || mode_class[i] == MODE_VECTOR_FRACT
			    || mode_class[i] == MODE_VECTOR_UFRACT
			    || mode_class[i] == MODE_VECTOR_ACCUM
			    || mode_class[i] == MODE_VECTOR_UACCUM))
		      break;
		  }

		if (i < 0)
		  matches = 0;
		else if (*pp == 'a')
		  m1 = i, np += strlen (GET_MODE_NAME(i));
		else
		  m2 = i, np += strlen (GET_MODE_NAME(i));

		force_int = force_partial_int = force_float = force_fixed = 0;
		break;

	      default:
		gcc_unreachable ();
	      }
	}

      if (matches && pp[0] == '$' && pp[1] == ')'
	  && *np == 0
	  && (! force_consec || (int) GET_MODE_WIDER_MODE(m1) == m2))
	break;
    }

  if (pindex == ARRAY_SIZE (optabs))
    return;

  /* We found a match.  If this pattern is only conditionally present,
     write out the "if" and two extra blanks.  */

  if (*XSTR (insn, 2) != 0)
    printf ("  if (HAVE_%s)\n  ", name);

  printf ("  ");

  /* Now write out the initialization, making all required substitutions.  */
  for (pp = optabs[pindex]; *pp; pp++)
    {
      if (*pp != '$')
	putchar (*pp);
      else
	switch (*++pp)
	  {
	  case '(':  case ')':
	  case 'I':  case 'F':  case 'N':
	    break;
	  case 'V':
	    if (SCALAR_FLOAT_MODE_P (m1))
              printf ("v");
            break;
	  case 'a':
	    for (np = GET_MODE_NAME(m1); *np; np++)
	      putchar (TOLOWER (*np));
	    break;
	  case 'b':
	    for (np = GET_MODE_NAME(m2); *np; np++)
	      putchar (TOLOWER (*np));
	    break;
	  case 'A':
	    printf ("%smode", GET_MODE_NAME(m1));
	    break;
	  case 'B':
	    printf ("%smode", GET_MODE_NAME(m2));
	    break;
	  case 'c':
	    printf ("%s", GET_RTX_NAME(op));
	    break;
	  case 'C':
	    for (np = GET_RTX_NAME(op); *np; np++)
	      putchar (TOUPPER (*np));
	    break;
	  }
    }

  printf (";\n");
}

extern int main (int, char **);

int
main (int argc, char **argv)
{
  rtx desc;

  progname = "genopinit";

  if (init_md_reader_args (argc, argv) != SUCCESS_EXIT_CODE)
    return (FATAL_EXIT_CODE);

  printf ("/* Generated automatically by the program `genopinit'\n\
from the machine description file `md'.  */\n\n");

  printf ("#include \"config.h\"\n");
  printf ("#include \"system.h\"\n");
  printf ("#include \"coretypes.h\"\n");
  printf ("#include \"tm.h\"\n");
  printf ("#include \"rtl.h\"\n");
  printf ("#include \"tm_p.h\"\n");
  printf ("#include \"flags.h\"\n");
  printf ("#include \"insn-config.h\"\n");
  printf ("#include \"recog.h\"\n");
  printf ("#include \"expr.h\"\n");
  printf ("#include \"optabs.h\"\n");
  printf ("#include \"reload.h\"\n\n");

  printf ("void\ninit_all_optabs (void)\n{\n");

  puts ("\
#ifdef FIXUNS_TRUNC_LIKE_FIX_TRUNC\n\
  int i, j;\n\
#endif\n");

  /* Read the machine description.  */

  while (1)
    {
      int line_no, insn_code_number = 0;

      desc = read_md_rtx (&line_no, &insn_code_number);
      if (desc == NULL)
	break;

      if (GET_CODE (desc) == DEFINE_INSN || GET_CODE (desc) == DEFINE_EXPAND)
	gen_insn (desc);
    }

  puts ("\
\n\
#ifdef FIXUNS_TRUNC_LIKE_FIX_TRUNC\n\
  /* This flag says the same insns that convert to a signed fixnum\n\
     also convert validly to an unsigned one.  */\n\
  for (i = 0; i < NUM_MACHINE_MODES; i++)\n\
    for (j = 0; j < NUM_MACHINE_MODES; j++)\n\
      convert_optab_handler (ufixtrunc_optab, i, j)->insn_code\n\
      = convert_optab_handler (sfixtrunc_optab, i, j)->insn_code;\n\
#endif\n\
}");

  fflush (stdout);
  return (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}
