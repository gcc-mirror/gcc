/* GNU compiler vector extension intrinsics
   Copyright (C) 2015-2016 Free Software Foundation, Inc.
   Contributed by Andreas Krebbel (Andreas.Krebbel@de.ibm.com)

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

#ifndef _VECINTRIN_H
#define _VECINTRIN_H

#define __VFTCI_ZERO           1<<11
#define __VFTCI_ZERO_N         1<<10
#define __VFTCI_NORMAL          1<<9
#define __VFTCI_NORMAL_N        1<<8
#define __VFTCI_SUBNORMAL       1<<7
#define __VFTCI_SUBNORMAL_N     1<<6
#define __VFTCI_INF             1<<5
#define __VFTCI_INF_N           1<<4
#define __VFTCI_QNAN            1<<3
#define __VFTCI_QNAN_N          1<<2
#define __VFTCI_SNAN            1<<1
#define __VFTCI_SNAN_N          1<<0

/* This also accepts a type for its parameter, so it is not enough
   to #define vec_step to __builtin_vec_step.  */
#define vec_step(x) __builtin_vec_step (* (__typeof__ (x) *) 0)

static inline int
__lcbb(const void *ptr, int bndry)
{
  int code;
  switch (bndry)
    {
    case 64: code = 0; break;
    case 128: code = 1; break;
    case 256: code = 2; break;
    case 512: code = 3; break;
    case 1024: code = 4; break;
    case 2048: code = 5; break;
    case 4096: code = 6; break;
    default: return 0;
    }
  return __builtin_s390_lcbb (ptr, code);
}

#define vec_all_nle(X, Y) vec_all_nge ((Y), (X))
#define vec_all_nlt(X, Y) vec_all_ngt ((Y), (X))
#define vec_any_nle(X, Y) vec_any_nge ((Y), (X))
#define vec_any_nlt(X, Y) vec_any_ngt ((Y), (X))
#define vec_genmask __builtin_s390_vgbm
#define vec_genmasks_8 __builtin_s390_vgmb
#define vec_genmasks_16 __builtin_s390_vgmh
#define vec_genmasks_32 __builtin_s390_vgmf
#define vec_genmasks_64 __builtin_s390_vgmg
#define vec_splat_u8 __builtin_s390_vec_splat_u8
#define vec_splat_s8 __builtin_s390_vec_splat_s8
#define vec_splat_u16 __builtin_s390_vec_splat_u16
#define vec_splat_s16 __builtin_s390_vec_splat_s16
#define vec_splat_u32 __builtin_s390_vec_splat_u32
#define vec_splat_s32 __builtin_s390_vec_splat_s32
#define vec_splat_u64 __builtin_s390_vec_splat_u64
#define vec_splat_s64 __builtin_s390_vec_splat_s64
#define vec_checksum __builtin_s390_vcksm
#define vec_gfmsum_128 __builtin_s390_vgfmg
#define vec_gfmsum_accum_128 __builtin_s390_vgfmag
#define vec_ceil(X) __builtin_s390_vfidb((X), 4, 6)
#define vec_roundp(X) __builtin_s390_vfidb((X), 4, 6)
#define vec_floor(X) __builtin_s390_vfidb((X), 4, 7)
#define vec_roundm(X) __builtin_s390_vfidb((X), 4, 7)
#define vec_trunc(X) __builtin_s390_vfidb((X), 4, 5)
#define vec_roundz(X) __builtin_s390_vfidb((X), 4, 5)
#define vec_roundc(X) __builtin_s390_vfidb((X), 4, 0)
#define vec_round(X) __builtin_s390_vfidb((X), 4, 4)
#define vec_madd __builtin_s390_vfmadb
#define vec_msub __builtin_s390_vfmsdb

#define vec_all_nan(a)						\
  __extension__ ({						\
      int __cc;							\
      __builtin_s390_vftcidb (a,				\
			      __VFTCI_QNAN			\
			      | __VFTCI_QNAN_N			\
			      | __VFTCI_SNAN			\
			      | __VFTCI_SNAN_N, &__cc);		\
      __cc == 0 ? 1 : 0;					\
    })

#define vec_all_numeric(a)					\
  __extension__ ({						\
      int __cc;							\
      __builtin_s390_vftcidb (a,				\
			      __VFTCI_NORMAL			\
			      | __VFTCI_NORMAL_N		\
			      | __VFTCI_SUBNORMAL		\
			      | __VFTCI_SUBNORMAL_N, &__cc);	\
      __cc == 0 ? 1 : 0;					\
    })

#define vec_any_nan(a)						\
  __extension__ ({						\
      int __cc;							\
      __builtin_s390_vftcidb (a,				\
			      __VFTCI_QNAN			\
			      | __VFTCI_QNAN_N			\
			      | __VFTCI_SNAN			\
			      | __VFTCI_SNAN_N, &cc);		\
      cc != 3 ? 1 : 0;						\
    })

#define vec_any_numeric(a)					\
  __extension__ ({						\
      int __cc;							\
      __builtin_s390_vftcidb (a,				\
			      __VFTCI_NORMAL			\
			      | __VFTCI_NORMAL_N		\
			      | __VFTCI_SUBNORMAL		\
			      | __VFTCI_SUBNORMAL_N, &cc);	\
      cc != 3 ? 1 : 0;						\
    })

#define vec_gather_element __builtin_s390_vec_gather_element
#define vec_xld2 __builtin_s390_vec_xld2
#define vec_xlw4 __builtin_s390_vec_xlw4
#define vec_splats __builtin_s390_vec_splats
#define vec_insert __builtin_s390_vec_insert
#define vec_promote __builtin_s390_vec_promote
#define vec_extract __builtin_s390_vec_extract
#define vec_insert_and_zero __builtin_s390_vec_insert_and_zero
#define vec_load_bndry __builtin_s390_vec_load_bndry
#define vec_load_pair __builtin_s390_vec_load_pair
#define vec_load_len __builtin_s390_vec_load_len
#define vec_mergeh __builtin_s390_vec_mergeh
#define vec_mergel __builtin_s390_vec_mergel
#define vec_pack __builtin_s390_vec_pack
#define vec_packs __builtin_s390_vec_packs
#define vec_packs_cc __builtin_s390_vec_packs_cc
#define vec_packsu __builtin_s390_vec_packsu
#define vec_packsu_cc __builtin_s390_vec_packsu_cc
#define vec_perm __builtin_s390_vec_perm
#define vec_permi __builtin_s390_vec_permi
#define vec_splat __builtin_s390_vec_splat
#define vec_scatter_element __builtin_s390_vec_scatter_element
#define vec_sel __builtin_s390_vec_sel
#define vec_extend_s64 __builtin_s390_vec_extend_s64
#define vec_xstd2 __builtin_s390_vec_xstd2
#define vec_xstw4 __builtin_s390_vec_xstw4
#define vec_store_len __builtin_s390_vec_store_len
#define vec_unpackh __builtin_s390_vec_unpackh
#define vec_unpackl __builtin_s390_vec_unpackl
#define vec_addc __builtin_s390_vec_addc
#define vec_add_u128 __builtin_s390_vec_add_u128
#define vec_addc_u128 __builtin_s390_vec_addc_u128
#define vec_adde_u128 __builtin_s390_vec_adde_u128
#define vec_addec_u128 __builtin_s390_vec_addec_u128
#define vec_and __builtin_s390_vec_and
#define vec_andc __builtin_s390_vec_andc
#define vec_avg __builtin_s390_vec_avg
#define vec_all_eq __builtin_s390_vec_all_eq
#define vec_all_ne __builtin_s390_vec_all_ne
#define vec_all_ge __builtin_s390_vec_all_ge
#define vec_all_gt __builtin_s390_vec_all_gt
#define vec_all_le __builtin_s390_vec_all_le
#define vec_all_lt __builtin_s390_vec_all_lt
#define vec_any_eq __builtin_s390_vec_any_eq
#define vec_any_ne __builtin_s390_vec_any_ne
#define vec_any_ge __builtin_s390_vec_any_ge
#define vec_any_gt __builtin_s390_vec_any_gt
#define vec_any_le __builtin_s390_vec_any_le
#define vec_any_lt __builtin_s390_vec_any_lt
#define vec_cmpeq __builtin_s390_vec_cmpeq
#define vec_cmpge __builtin_s390_vec_cmpge
#define vec_cmpgt __builtin_s390_vec_cmpgt
#define vec_cmple __builtin_s390_vec_cmple
#define vec_cmplt __builtin_s390_vec_cmplt
#define vec_cntlz __builtin_s390_vec_cntlz
#define vec_cnttz __builtin_s390_vec_cnttz
#define vec_xor __builtin_s390_vec_xor
#define vec_gfmsum __builtin_s390_vec_gfmsum
#define vec_gfmsum_accum __builtin_s390_vec_gfmsum_accum
#define vec_abs __builtin_s390_vec_abs
#define vec_max __builtin_s390_vec_max
#define vec_min __builtin_s390_vec_min
#define vec_mladd __builtin_s390_vec_mladd
#define vec_mhadd __builtin_s390_vec_mhadd
#define vec_meadd __builtin_s390_vec_meadd
#define vec_moadd __builtin_s390_vec_moadd
#define vec_mulh __builtin_s390_vec_mulh
#define vec_mule __builtin_s390_vec_mule
#define vec_mulo __builtin_s390_vec_mulo
#define vec_nor __builtin_s390_vec_nor
#define vec_or __builtin_s390_vec_or
#define vec_popcnt __builtin_s390_vec_popcnt
#define vec_rl __builtin_s390_vec_rl
#define vec_rli __builtin_s390_vec_rli
#define vec_rl_mask __builtin_s390_vec_rl_mask
#define vec_sll __builtin_s390_vec_sll
#define vec_slb __builtin_s390_vec_slb
#define vec_sld __builtin_s390_vec_sld
#define vec_sldw __builtin_s390_vec_sldw
#define vec_sral __builtin_s390_vec_sral
#define vec_srab __builtin_s390_vec_srab
#define vec_srl __builtin_s390_vec_srl
#define vec_srb __builtin_s390_vec_srb
#define vec_subc __builtin_s390_vec_subc
#define vec_sub_u128 __builtin_s390_vec_sub_u128
#define vec_subc_u128 __builtin_s390_vec_subc_u128
#define vec_sube_u128 __builtin_s390_vec_sube_u128
#define vec_subec_u128 __builtin_s390_vec_subec_u128
#define vec_sum2 __builtin_s390_vec_sum2
#define vec_sum_u128 __builtin_s390_vec_sum_u128
#define vec_sum4 __builtin_s390_vec_sum4
#define vec_test_mask __builtin_s390_vec_test_mask
#define vec_find_any_eq_idx __builtin_s390_vec_find_any_eq_idx
#define vec_find_any_ne_idx __builtin_s390_vec_find_any_ne_idx
#define vec_find_any_eq_or_0_idx __builtin_s390_vec_find_any_eq_or_0_idx
#define vec_find_any_ne_or_0_idx __builtin_s390_vec_find_any_ne_or_0_idx
#define vec_find_any_eq __builtin_s390_vec_find_any_eq
#define vec_find_any_ne __builtin_s390_vec_find_any_ne
#define vec_find_any_eq_idx_cc __builtin_s390_vec_find_any_eq_idx_cc
#define vec_find_any_ne_idx_cc __builtin_s390_vec_find_any_ne_idx_cc
#define vec_find_any_eq_or_0_idx_cc __builtin_s390_vec_find_any_eq_or_0_idx_cc
#define vec_find_any_ne_or_0_idx_cc __builtin_s390_vec_find_any_ne_or_0_idx_cc
#define vec_find_any_eq_cc __builtin_s390_vec_find_any_eq_cc
#define vec_find_any_ne_cc __builtin_s390_vec_find_any_ne_cc
#define vec_cmpeq_idx __builtin_s390_vec_cmpeq_idx
#define vec_cmpeq_or_0_idx __builtin_s390_vec_cmpeq_or_0_idx
#define vec_cmpeq_idx_cc __builtin_s390_vec_cmpeq_idx_cc
#define vec_cmpeq_or_0_idx_cc __builtin_s390_vec_cmpeq_or_0_idx_cc
#define vec_cmpne_idx __builtin_s390_vec_cmpne_idx
#define vec_cmpne_or_0_idx __builtin_s390_vec_cmpne_or_0_idx
#define vec_cmpne_idx_cc __builtin_s390_vec_cmpne_idx_cc
#define vec_cmpne_or_0_idx_cc __builtin_s390_vec_cmpne_or_0_idx_cc
#define vec_cp_until_zero __builtin_s390_vec_cp_until_zero
#define vec_cp_until_zero_cc __builtin_s390_vec_cp_until_zero_cc
#define vec_cmprg_idx __builtin_s390_vec_cmprg_idx
#define vec_cmpnrg_idx __builtin_s390_vec_cmpnrg_idx
#define vec_cmprg_or_0_idx __builtin_s390_vec_cmprg_or_0_idx
#define vec_cmpnrg_or_0_idx __builtin_s390_vec_cmpnrg_or_0_idx
#define vec_cmprg __builtin_s390_vec_cmprg
#define vec_cmpnrg __builtin_s390_vec_cmpnrg
#define vec_cmprg_idx_cc __builtin_s390_vec_cmprg_idx_cc
#define vec_cmpnrg_idx_cc __builtin_s390_vec_cmpnrg_idx_cc
#define vec_cmprg_or_0_idx_cc __builtin_s390_vec_cmprg_or_0_idx_cc
#define vec_cmpnrg_or_0_idx_cc __builtin_s390_vec_cmpnrg_or_0_idx_cc
#define vec_cmprg_cc __builtin_s390_vec_cmprg_cc
#define vec_cmpnrg_cc __builtin_s390_vec_cmpnrg_cc
#define vec_all_nge __builtin_s390_vec_all_nge
#define vec_all_ngt __builtin_s390_vec_all_ngt
#define vec_any_nge __builtin_s390_vec_any_nge
#define vec_any_ngt __builtin_s390_vec_any_ngt
#define vec_ctd __builtin_s390_vec_ctd
#define vec_ctd_s64 __builtin_s390_vec_ctd_s64
#define vec_ctd_u64 __builtin_s390_vec_ctd_u64
#define vec_ctsl __builtin_s390_vec_ctsl
#define vec_ctul __builtin_s390_vec_ctul
#define vec_ld2f __builtin_s390_vec_ld2f
#define vec_st2f __builtin_s390_vec_st2f
#endif /* _VECINTRIN_H */
