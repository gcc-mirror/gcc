
;; Iterators for RISC-V 'V' Extension for GNU compiler.
;; Copyright (C) 2022-2025 Free Software Foundation, Inc.
;; Contributed by Juzhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_c_enum "unspec" [
  UNSPEC_VSETVL
  UNSPEC_VUNDEF
  UNSPEC_VPREDICATE
  UNSPEC_VLMAX
  UNSPEC_UNIT_STRIDED
  UNSPEC_STRIDED

  ;; It's used to specify ordered/unordered operation.
  UNSPEC_ORDERED
  UNSPEC_UNORDERED

  ;; vmulh/vmulhu/vmulhsu
  UNSPEC_VMULHS
  UNSPEC_VMULHU
  UNSPEC_VMULHSU

  UNSPEC_VADC
  UNSPEC_VSBC
  UNSPEC_VMADC
  UNSPEC_VMSBC
  UNSPEC_OVERFLOW

  UNSPEC_VNCLIP
  UNSPEC_VNCLIPU
  UNSPEC_VSSRL
  UNSPEC_VSSRA
  UNSPEC_VAADDU
  UNSPEC_VAADD
  UNSPEC_VASUBU
  UNSPEC_VASUB
  UNSPEC_VSMUL

  UNSPEC_VMSBF
  UNSPEC_VMSIF
  UNSPEC_VMSOF
  UNSPEC_VIOTA

  UNSPEC_VFRSQRT7
  UNSPEC_VFREC7
  UNSPEC_VFCLASS

  UNSPEC_VCOPYSIGN
  UNSPEC_VXORSIGN

  UNSPEC_VFCVT
  UNSPEC_UNSIGNED_VFCVT
  UNSPEC_ROD

  UNSPEC_VSLIDEUP
  UNSPEC_VSLIDEDOWN
  UNSPEC_VSLIDE1UP
  UNSPEC_VSLIDE1DOWN
  UNSPEC_VFSLIDE1UP
  UNSPEC_VFSLIDE1DOWN
  UNSPEC_VRGATHER
  UNSPEC_VRGATHEREI16
  UNSPEC_VCOMPRESS
  UNSPEC_VLEFF
  UNSPEC_MODIFY_VL

  UNSPEC_VFFMA

  UNSPEC_VFMAX
  UNSPEC_VFMIN

  ;; Integer and Float Reduction
  UNSPEC_REDUC
  UNSPEC_REDUC_SUM
  UNSPEC_REDUC_SUM_VL0_SAFE
  UNSPEC_REDUC_SUM_ORDERED
  UNSPEC_REDUC_SUM_UNORDERED
  UNSPEC_REDUC_SUM_ORDERED_VL0_SAFE
  UNSPEC_REDUC_SUM_UNORDERED_VL0_SAFE
  UNSPEC_REDUC_MAXU
  UNSPEC_REDUC_MAX
  UNSPEC_REDUC_MINU
  UNSPEC_REDUC_MIN
  UNSPEC_REDUC_AND
  UNSPEC_REDUC_OR
  UNSPEC_REDUC_XOR
  UNSPEC_REDUC_MAXU_VL0_SAFE
  UNSPEC_REDUC_MAX_VL0_SAFE
  UNSPEC_REDUC_MINU_VL0_SAFE
  UNSPEC_REDUC_MIN_VL0_SAFE
  UNSPEC_REDUC_AND_VL0_SAFE
  UNSPEC_REDUC_OR_VL0_SAFE
  UNSPEC_REDUC_XOR_VL0_SAFE

  UNSPEC_WREDUC_SUM
  UNSPEC_WREDUC_SUMU
  UNSPEC_WREDUC_SUM_VL0_SAFE
  UNSPEC_WREDUC_SUMU_VL0_SAFE
  UNSPEC_WREDUC_SUM_ORDERED
  UNSPEC_WREDUC_SUM_UNORDERED
  UNSPEC_WREDUC_SUM_ORDERED_VL0_SAFE
  UNSPEC_WREDUC_SUM_UNORDERED_VL0_SAFE
  UNSPEC_SELECT_MASK

  UNSPEC_SF_VFNRCLIP
  UNSPEC_SF_VFNRCLIPU
])

(define_c_enum "unspecv" [
  UNSPECV_FRM_RESTORE_EXIT
])

;; Subset of VI with fractional LMUL types
(define_mode_iterator VI_FRAC [
  RVVMF2QI RVVMF4QI (RVVMF8QI "TARGET_VECTOR_ELEN_64")
  RVVMF2HI (RVVMF4HI "TARGET_VECTOR_ELEN_64")
  (RVVMF2SI "TARGET_VECTOR_ELEN_64")
])

;; Subset of VI with non-fractional LMUL types
(define_mode_iterator VI_NOFRAC [
  RVVM8QI RVVM4QI RVVM2QI RVVM1QI
  RVVM8HI RVVM4HI RVVM2HI RVVM1HI
  RVVM8SI RVVM4SI RVVM2SI RVVM1SI
  (RVVM8DI "TARGET_VECTOR_ELEN_64") (RVVM4DI "TARGET_VECTOR_ELEN_64")
  (RVVM2DI "TARGET_VECTOR_ELEN_64") (RVVM1DI "TARGET_VECTOR_ELEN_64")
])

(define_mode_iterator VI [ VI_NOFRAC (VI_FRAC "!TARGET_XTHEADVECTOR") ])

;; This iterator is the same as above but with TARGET_VECTOR_ELEN_FP_16
;; changed to TARGET_ZVFH.  TARGET_VECTOR_ELEN_FP_16 is also true for
;; TARGET_ZVFHMIN while we actually want to disable all instructions apart
;; from load, store and convert for it.
;; It is not enough to set the "enabled" attribute to false
;; since this will only disable insn alternatives in reload but still
;; allow the instruction and mode to be matched during combine et al.
(define_mode_iterator VF [
  (RVVM8HF "TARGET_ZVFH") (RVVM4HF "TARGET_ZVFH") (RVVM2HF "TARGET_ZVFH")
  (RVVM1HF "TARGET_ZVFH") (RVVMF2HF "TARGET_ZVFH")
  (RVVMF4HF "TARGET_ZVFH && TARGET_VECTOR_ELEN_64")

  (RVVM8SF "TARGET_VECTOR_ELEN_FP_32") (RVVM4SF "TARGET_VECTOR_ELEN_FP_32") (RVVM2SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM1SF "TARGET_VECTOR_ELEN_FP_32") (RVVMF2SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_VECTOR_ELEN_64")

  (RVVM8DF "TARGET_VECTOR_ELEN_FP_64") (RVVM4DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM2DF "TARGET_VECTOR_ELEN_FP_64") (RVVM1DF "TARGET_VECTOR_ELEN_FP_64")
])

(define_mode_iterator VF_ZVFBF16 [
  (RVVM8BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM4BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM1BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF4BF "TARGET_VECTOR_ELEN_BF_16 && TARGET_VECTOR_ELEN_64")
])

(define_mode_iterator VF_ZVFHMIN [
  (RVVM8HF "TARGET_VECTOR_ELEN_FP_16") (RVVM4HF "TARGET_VECTOR_ELEN_FP_16") (RVVM2HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM1HF "TARGET_VECTOR_ELEN_FP_16") (RVVMF2HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVMF4HF "TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_64")

  (RVVM8SF "TARGET_VECTOR_ELEN_FP_32") (RVVM4SF "TARGET_VECTOR_ELEN_FP_32") (RVVM2SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM1SF "TARGET_VECTOR_ELEN_FP_32") (RVVMF2SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_VECTOR_ELEN_64")

  (RVVM8DF "TARGET_VECTOR_ELEN_FP_64") (RVVM4DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM2DF "TARGET_VECTOR_ELEN_FP_64") (RVVM1DF "TARGET_VECTOR_ELEN_FP_64")
])

(define_mode_iterator VLSI [
  (V1QI "riscv_vector::vls_mode_valid_p (V1QImode)")
  (V2QI "riscv_vector::vls_mode_valid_p (V2QImode)")
  (V4QI "riscv_vector::vls_mode_valid_p (V4QImode)")
  (V8QI "riscv_vector::vls_mode_valid_p (V8QImode)")
  (V16QI "riscv_vector::vls_mode_valid_p (V16QImode)")
  (V32QI "riscv_vector::vls_mode_valid_p (V32QImode)")
  (V64QI "riscv_vector::vls_mode_valid_p (V64QImode) && TARGET_MIN_VLEN >= 64")
  (V128QI "riscv_vector::vls_mode_valid_p (V128QImode) && TARGET_MIN_VLEN >= 128")
  (V256QI "riscv_vector::vls_mode_valid_p (V256QImode) && TARGET_MIN_VLEN >= 256")
  (V512QI "riscv_vector::vls_mode_valid_p (V512QImode) && TARGET_MIN_VLEN >= 512")
  (V1024QI "riscv_vector::vls_mode_valid_p (V1024QImode) && TARGET_MIN_VLEN >= 1024")
  (V2048QI "riscv_vector::vls_mode_valid_p (V2048QImode) && TARGET_MIN_VLEN >= 2048")
  (V4096QI "riscv_vector::vls_mode_valid_p (V4096QImode) && TARGET_MIN_VLEN >= 4096")
  (V1HI "riscv_vector::vls_mode_valid_p (V1HImode)")
  (V2HI "riscv_vector::vls_mode_valid_p (V2HImode)")
  (V4HI "riscv_vector::vls_mode_valid_p (V4HImode)")
  (V8HI "riscv_vector::vls_mode_valid_p (V8HImode)")
  (V16HI "riscv_vector::vls_mode_valid_p (V16HImode)")
  (V32HI "riscv_vector::vls_mode_valid_p (V32HImode) && TARGET_MIN_VLEN >= 64")
  (V64HI "riscv_vector::vls_mode_valid_p (V64HImode) && TARGET_MIN_VLEN >= 128")
  (V128HI "riscv_vector::vls_mode_valid_p (V128HImode) && TARGET_MIN_VLEN >= 256")
  (V256HI "riscv_vector::vls_mode_valid_p (V256HImode) && TARGET_MIN_VLEN >= 512")
  (V512HI "riscv_vector::vls_mode_valid_p (V512HImode) && TARGET_MIN_VLEN >= 1024")
  (V1024HI "riscv_vector::vls_mode_valid_p (V1024HImode) && TARGET_MIN_VLEN >= 2048")
  (V2048HI "riscv_vector::vls_mode_valid_p (V2048HImode) && TARGET_MIN_VLEN >= 4096")
  (V1SI "riscv_vector::vls_mode_valid_p (V1SImode)")
  (V2SI "riscv_vector::vls_mode_valid_p (V2SImode)")
  (V4SI "riscv_vector::vls_mode_valid_p (V4SImode)")
  (V8SI "riscv_vector::vls_mode_valid_p (V8SImode)")
  (V16SI "riscv_vector::vls_mode_valid_p (V16SImode) && TARGET_MIN_VLEN >= 64")
  (V32SI "riscv_vector::vls_mode_valid_p (V32SImode) && TARGET_MIN_VLEN >= 128")
  (V64SI "riscv_vector::vls_mode_valid_p (V64SImode) && TARGET_MIN_VLEN >= 256")
  (V128SI "riscv_vector::vls_mode_valid_p (V128SImode) && TARGET_MIN_VLEN >= 512")
  (V256SI "riscv_vector::vls_mode_valid_p (V256SImode) && TARGET_MIN_VLEN >= 1024")
  (V512SI "riscv_vector::vls_mode_valid_p (V512SImode) && TARGET_MIN_VLEN >= 2048")
  (V1024SI "riscv_vector::vls_mode_valid_p (V1024SImode) && TARGET_MIN_VLEN >= 4096")
  (V1DI "riscv_vector::vls_mode_valid_p (V1DImode) && TARGET_VECTOR_ELEN_64")
  (V2DI "riscv_vector::vls_mode_valid_p (V2DImode) && TARGET_VECTOR_ELEN_64")
  (V4DI "riscv_vector::vls_mode_valid_p (V4DImode) && TARGET_VECTOR_ELEN_64")
  (V8DI "riscv_vector::vls_mode_valid_p (V8DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 64")
  (V16DI "riscv_vector::vls_mode_valid_p (V16DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 128")
  (V32DI "riscv_vector::vls_mode_valid_p (V32DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 256")
  (V64DI "riscv_vector::vls_mode_valid_p (V64DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 512")
  (V128DI "riscv_vector::vls_mode_valid_p (V128DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 1024")
  (V256DI "riscv_vector::vls_mode_valid_p (V256DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 2048")
  (V512DI "riscv_vector::vls_mode_valid_p (V512DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 4096")])

(define_mode_iterator VLSF [
  (V1HF "riscv_vector::vls_mode_valid_p (V1HFmode) && TARGET_ZVFH")
  (V2HF "riscv_vector::vls_mode_valid_p (V2HFmode) && TARGET_ZVFH")
  (V4HF "riscv_vector::vls_mode_valid_p (V4HFmode) && TARGET_ZVFH")
  (V8HF "riscv_vector::vls_mode_valid_p (V8HFmode) && TARGET_ZVFH")
  (V16HF "riscv_vector::vls_mode_valid_p (V16HFmode) && TARGET_ZVFH")
  (V32HF "riscv_vector::vls_mode_valid_p (V32HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 64")
  (V64HF "riscv_vector::vls_mode_valid_p (V64HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 128")
  (V128HF "riscv_vector::vls_mode_valid_p (V128HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 256")
  (V256HF "riscv_vector::vls_mode_valid_p (V256HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 512")
  (V512HF "riscv_vector::vls_mode_valid_p (V512HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 1024")
  (V1024HF "riscv_vector::vls_mode_valid_p (V1024HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 2048")
  (V2048HF "riscv_vector::vls_mode_valid_p (V2048HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 4096")
  (V1SF "riscv_vector::vls_mode_valid_p (V1SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V2SF "riscv_vector::vls_mode_valid_p (V2SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V4SF "riscv_vector::vls_mode_valid_p (V4SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V8SF "riscv_vector::vls_mode_valid_p (V8SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V16SF "riscv_vector::vls_mode_valid_p (V16SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 64")
  (V32SF "riscv_vector::vls_mode_valid_p (V32SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 128")
  (V64SF "riscv_vector::vls_mode_valid_p (V64SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 256")
  (V128SF "riscv_vector::vls_mode_valid_p (V128SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 512")
  (V256SF "riscv_vector::vls_mode_valid_p (V256SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 1024")
  (V512SF "riscv_vector::vls_mode_valid_p (V512SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 2048")
  (V1024SF "riscv_vector::vls_mode_valid_p (V1024SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 4096")
  (V1DF "riscv_vector::vls_mode_valid_p (V1DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V2DF "riscv_vector::vls_mode_valid_p (V2DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V4DF "riscv_vector::vls_mode_valid_p (V4DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V8DF "riscv_vector::vls_mode_valid_p (V8DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 64")
  (V16DF "riscv_vector::vls_mode_valid_p (V16DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 128")
  (V32DF "riscv_vector::vls_mode_valid_p (V32DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 256")
  (V64DF "riscv_vector::vls_mode_valid_p (V64DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 512")
  (V128DF "riscv_vector::vls_mode_valid_p (V128DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 1024")
  (V256DF "riscv_vector::vls_mode_valid_p (V256DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 2048")
  (V512DF "riscv_vector::vls_mode_valid_p (V512DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 4096")
])

(define_mode_iterator VLSF_ZVFHMIN [
  (V1HF "riscv_vector::vls_mode_valid_p (V1HFmode) && TARGET_VECTOR_ELEN_FP_16")
  (V2HF "riscv_vector::vls_mode_valid_p (V2HFmode) && TARGET_VECTOR_ELEN_FP_16")
  (V4HF "riscv_vector::vls_mode_valid_p (V4HFmode) && TARGET_VECTOR_ELEN_FP_16")
  (V8HF "riscv_vector::vls_mode_valid_p (V8HFmode) && TARGET_VECTOR_ELEN_FP_16")
  (V16HF "riscv_vector::vls_mode_valid_p (V16HFmode) && TARGET_VECTOR_ELEN_FP_16")
  (V32HF "riscv_vector::vls_mode_valid_p (V32HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 64")
  (V64HF "riscv_vector::vls_mode_valid_p (V64HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 128")
  (V128HF "riscv_vector::vls_mode_valid_p (V128HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 256")
  (V256HF "riscv_vector::vls_mode_valid_p (V256HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 512")
  (V512HF "riscv_vector::vls_mode_valid_p (V512HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 1024")
  (V1024HF "riscv_vector::vls_mode_valid_p (V1024HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 2048")
  (V2048HF "riscv_vector::vls_mode_valid_p (V2048HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 4096")
  (V1SF "riscv_vector::vls_mode_valid_p (V1SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V2SF "riscv_vector::vls_mode_valid_p (V2SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V4SF "riscv_vector::vls_mode_valid_p (V4SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V8SF "riscv_vector::vls_mode_valid_p (V8SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V16SF "riscv_vector::vls_mode_valid_p (V16SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 64")
  (V32SF "riscv_vector::vls_mode_valid_p (V32SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 128")
  (V64SF "riscv_vector::vls_mode_valid_p (V64SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 256")
  (V128SF "riscv_vector::vls_mode_valid_p (V128SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 512")
  (V256SF "riscv_vector::vls_mode_valid_p (V256SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 1024")
  (V512SF "riscv_vector::vls_mode_valid_p (V512SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 2048")
  (V1024SF "riscv_vector::vls_mode_valid_p (V1024SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 4096")
  (V1DF "riscv_vector::vls_mode_valid_p (V1DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V2DF "riscv_vector::vls_mode_valid_p (V2DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V4DF "riscv_vector::vls_mode_valid_p (V4DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V8DF "riscv_vector::vls_mode_valid_p (V8DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 64")
  (V16DF "riscv_vector::vls_mode_valid_p (V16DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 128")
  (V32DF "riscv_vector::vls_mode_valid_p (V32DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 256")
  (V64DF "riscv_vector::vls_mode_valid_p (V64DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 512")
  (V128DF "riscv_vector::vls_mode_valid_p (V128DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 1024")
  (V256DF "riscv_vector::vls_mode_valid_p (V256DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 2048")
  (V512DF "riscv_vector::vls_mode_valid_p (V512DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 4096")
])

(define_mode_iterator VEEWEXT2 [
  RVVM8HI RVVM4HI RVVM2HI RVVM1HI RVVMF2HI (RVVMF4HI "TARGET_VECTOR_ELEN_64")

  (RVVM8BF "TARGET_VECTOR_ELEN_BF_16") (RVVM4BF "TARGET_VECTOR_ELEN_BF_16") (RVVM2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM1BF "TARGET_VECTOR_ELEN_BF_16") (RVVMF2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF4BF "TARGET_VECTOR_ELEN_BF_16 && TARGET_VECTOR_ELEN_64")

  (RVVM8HF "TARGET_VECTOR_ELEN_FP_16") (RVVM4HF "TARGET_VECTOR_ELEN_FP_16") (RVVM2HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM1HF "TARGET_VECTOR_ELEN_FP_16") (RVVMF2HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVMF4HF "TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_64")

  RVVM8SI RVVM4SI RVVM2SI RVVM1SI (RVVMF2SI "TARGET_VECTOR_ELEN_64")

  (RVVM8SF "TARGET_VECTOR_ELEN_FP_32") (RVVM4SF "TARGET_VECTOR_ELEN_FP_32") (RVVM2SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM1SF "TARGET_VECTOR_ELEN_FP_32") (RVVMF2SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_VECTOR_ELEN_64")

  (RVVM8DI "TARGET_VECTOR_ELEN_64") (RVVM4DI "TARGET_VECTOR_ELEN_64")
  (RVVM2DI "TARGET_VECTOR_ELEN_64") (RVVM1DI "TARGET_VECTOR_ELEN_64")

  (RVVM8DF "TARGET_VECTOR_ELEN_FP_64") (RVVM4DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM2DF "TARGET_VECTOR_ELEN_FP_64") (RVVM1DF "TARGET_VECTOR_ELEN_FP_64")
])

(define_mode_iterator VEEWEXT4 [
  RVVM8SI RVVM4SI RVVM2SI RVVM1SI (RVVMF2SI "TARGET_VECTOR_ELEN_64")

  (RVVM8SF "TARGET_VECTOR_ELEN_FP_32") (RVVM4SF "TARGET_VECTOR_ELEN_FP_32") (RVVM2SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM1SF "TARGET_VECTOR_ELEN_FP_32") (RVVMF2SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_VECTOR_ELEN_64")

  (RVVM8DI "TARGET_VECTOR_ELEN_64") (RVVM4DI "TARGET_VECTOR_ELEN_64")
  (RVVM2DI "TARGET_VECTOR_ELEN_64") (RVVM1DI "TARGET_VECTOR_ELEN_64")

  (RVVM8DF "TARGET_VECTOR_ELEN_FP_64") (RVVM4DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM2DF "TARGET_VECTOR_ELEN_FP_64") (RVVM1DF "TARGET_VECTOR_ELEN_FP_64")
])

(define_mode_iterator VEEWEXT8 [
  (RVVM8DI "TARGET_VECTOR_ELEN_64") (RVVM4DI "TARGET_VECTOR_ELEN_64")
  (RVVM2DI "TARGET_VECTOR_ELEN_64") (RVVM1DI "TARGET_VECTOR_ELEN_64")

  (RVVM8DF "TARGET_VECTOR_ELEN_FP_64") (RVVM4DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM2DF "TARGET_VECTOR_ELEN_FP_64") (RVVM1DF "TARGET_VECTOR_ELEN_FP_64")
])

(define_mode_iterator VEEWTRUNC2 [
  RVVM4QI RVVM2QI RVVM1QI RVVMF2QI RVVMF4QI (RVVMF8QI "TARGET_VECTOR_ELEN_64")

  RVVM4HI RVVM2HI RVVM1HI RVVMF2HI (RVVMF4HI "TARGET_VECTOR_ELEN_64")

  (RVVM4BF "TARGET_VECTOR_ELEN_BF_16") (RVVM2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM1BF "TARGET_VECTOR_ELEN_BF_16") (RVVMF2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF4BF "TARGET_VECTOR_ELEN_BF_16 && TARGET_VECTOR_ELEN_64")

  (RVVM4HF "TARGET_VECTOR_ELEN_FP_16") (RVVM2HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM1HF "TARGET_VECTOR_ELEN_FP_16") (RVVMF2HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVMF4HF "TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_64")

  (RVVM4SI "TARGET_64BIT")
  (RVVM2SI "TARGET_64BIT")
  (RVVM1SI "TARGET_64BIT")
  (RVVMF2SI "TARGET_VECTOR_ELEN_64 && TARGET_64BIT")

  (RVVM4SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_64BIT")
  (RVVM2SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_64BIT")
  (RVVM1SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_64BIT")
  (RVVMF2SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_VECTOR_ELEN_64 && TARGET_64BIT")
])

(define_mode_iterator VEEWTRUNC4 [
  RVVM2QI RVVM1QI RVVMF2QI RVVMF4QI (RVVMF8QI "TARGET_VECTOR_ELEN_64")

  (RVVM2HI "TARGET_64BIT")
  (RVVM1HI "TARGET_64BIT")
  (RVVMF2HI "TARGET_64BIT")
  (RVVMF4HI "TARGET_VECTOR_ELEN_64 && TARGET_64BIT")

  (RVVM2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM1BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF4BF "TARGET_VECTOR_ELEN_BF_16 && TARGET_VECTOR_ELEN_64 && TARGET_64BIT")

  (RVVM2HF "TARGET_VECTOR_ELEN_FP_16 && TARGET_64BIT")
  (RVVM1HF "TARGET_VECTOR_ELEN_FP_16 && TARGET_64BIT")
  (RVVMF2HF "TARGET_VECTOR_ELEN_FP_16 && TARGET_64BIT")
  (RVVMF4HF "TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_64 && TARGET_64BIT")
])

(define_mode_iterator VEEWTRUNC8 [
  (RVVM1QI "TARGET_64BIT")
  (RVVMF2QI "TARGET_64BIT")
  (RVVMF4QI "TARGET_64BIT")
  (RVVMF8QI "TARGET_VECTOR_ELEN_64 && TARGET_64BIT")
])

(define_mode_iterator VEI16 [
  RVVM4QI RVVM2QI RVVM1QI RVVMF2QI RVVMF4QI (RVVMF8QI "TARGET_VECTOR_ELEN_64")

  RVVM8HI RVVM4HI RVVM2HI RVVM1HI RVVMF2HI (RVVMF4HI "TARGET_VECTOR_ELEN_64")

  (RVVM8HF "TARGET_VECTOR_ELEN_FP_16") (RVVM4HF "TARGET_VECTOR_ELEN_FP_16") (RVVM2HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM1HF "TARGET_VECTOR_ELEN_FP_16") (RVVMF2HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVMF4HF "TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_64")

  RVVM8SI RVVM4SI RVVM2SI RVVM1SI (RVVMF2SI "TARGET_VECTOR_ELEN_64")

  (RVVM8SF "TARGET_VECTOR_ELEN_FP_32") (RVVM4SF "TARGET_VECTOR_ELEN_FP_32") (RVVM2SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM1SF "TARGET_VECTOR_ELEN_FP_32") (RVVMF2SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_VECTOR_ELEN_64")

  (RVVM8DI "TARGET_VECTOR_ELEN_64") (RVVM4DI "TARGET_VECTOR_ELEN_64")
  (RVVM2DI "TARGET_VECTOR_ELEN_64") (RVVM1DI "TARGET_VECTOR_ELEN_64")

  (RVVM8DF "TARGET_VECTOR_ELEN_FP_64") (RVVM4DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM2DF "TARGET_VECTOR_ELEN_FP_64") (RVVM1DF "TARGET_VECTOR_ELEN_FP_64")

  (V1QI "riscv_vector::vls_mode_valid_p (V1QImode)")
  (V2QI "riscv_vector::vls_mode_valid_p (V2QImode)")
  (V4QI "riscv_vector::vls_mode_valid_p (V4QImode)")
  (V8QI "riscv_vector::vls_mode_valid_p (V8QImode)")
  (V16QI "riscv_vector::vls_mode_valid_p (V16QImode)")
  (V32QI "riscv_vector::vls_mode_valid_p (V32QImode) && TARGET_MIN_VLEN >= 64")
  (V64QI "riscv_vector::vls_mode_valid_p (V64QImode) && TARGET_MIN_VLEN >= 128")
  (V128QI "riscv_vector::vls_mode_valid_p (V128QImode) && TARGET_MIN_VLEN >= 256")
  (V256QI "riscv_vector::vls_mode_valid_p (V256QImode) && TARGET_MIN_VLEN >= 512")
  (V512QI "riscv_vector::vls_mode_valid_p (V512QImode) && TARGET_MIN_VLEN >= 1024")
  (V1024QI "riscv_vector::vls_mode_valid_p (V1024QImode) && TARGET_MIN_VLEN >= 2048")
  (V2048QI "riscv_vector::vls_mode_valid_p (V2048QImode) && TARGET_MIN_VLEN >= 4096")
  (V1HI "riscv_vector::vls_mode_valid_p (V1HImode)")
  (V2HI "riscv_vector::vls_mode_valid_p (V2HImode)")
  (V4HI "riscv_vector::vls_mode_valid_p (V4HImode)")
  (V8HI "riscv_vector::vls_mode_valid_p (V8HImode)")
  (V16HI "riscv_vector::vls_mode_valid_p (V16HImode)")
  (V32HI "riscv_vector::vls_mode_valid_p (V32HImode) && TARGET_MIN_VLEN >= 64")
  (V64HI "riscv_vector::vls_mode_valid_p (V64HImode) && TARGET_MIN_VLEN >= 128")
  (V128HI "riscv_vector::vls_mode_valid_p (V128HImode) && TARGET_MIN_VLEN >= 256")
  (V256HI "riscv_vector::vls_mode_valid_p (V256HImode) && TARGET_MIN_VLEN >= 512")
  (V512HI "riscv_vector::vls_mode_valid_p (V512HImode) && TARGET_MIN_VLEN >= 1024")
  (V1024HI "riscv_vector::vls_mode_valid_p (V1024HImode) && TARGET_MIN_VLEN >= 2048")
  (V2048HI "riscv_vector::vls_mode_valid_p (V2048HImode) && TARGET_MIN_VLEN >= 4096")
  (V1SI "riscv_vector::vls_mode_valid_p (V1SImode)")
  (V2SI "riscv_vector::vls_mode_valid_p (V2SImode)")
  (V4SI "riscv_vector::vls_mode_valid_p (V4SImode)")
  (V8SI "riscv_vector::vls_mode_valid_p (V8SImode)")
  (V16SI "riscv_vector::vls_mode_valid_p (V16SImode) && TARGET_MIN_VLEN >= 64")
  (V32SI "riscv_vector::vls_mode_valid_p (V32SImode) && TARGET_MIN_VLEN >= 128")
  (V64SI "riscv_vector::vls_mode_valid_p (V64SImode) && TARGET_MIN_VLEN >= 256")
  (V128SI "riscv_vector::vls_mode_valid_p (V128SImode) && TARGET_MIN_VLEN >= 512")
  (V256SI "riscv_vector::vls_mode_valid_p (V256SImode) && TARGET_MIN_VLEN >= 1024")
  (V512SI "riscv_vector::vls_mode_valid_p (V512SImode) && TARGET_MIN_VLEN >= 2048")
  (V1024SI "riscv_vector::vls_mode_valid_p (V1024SImode) && TARGET_MIN_VLEN >= 4096")
  (V1DI "riscv_vector::vls_mode_valid_p (V1DImode) && TARGET_VECTOR_ELEN_64")
  (V2DI "riscv_vector::vls_mode_valid_p (V2DImode) && TARGET_VECTOR_ELEN_64")
  (V4DI "riscv_vector::vls_mode_valid_p (V4DImode) && TARGET_VECTOR_ELEN_64")
  (V8DI "riscv_vector::vls_mode_valid_p (V8DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 64")
  (V16DI "riscv_vector::vls_mode_valid_p (V16DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 128")
  (V32DI "riscv_vector::vls_mode_valid_p (V32DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 256")
  (V64DI "riscv_vector::vls_mode_valid_p (V64DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 512")
  (V128DI "riscv_vector::vls_mode_valid_p (V128DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 1024")
  (V256DI "riscv_vector::vls_mode_valid_p (V256DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 2048")
  (V512DI "riscv_vector::vls_mode_valid_p (V512DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 4096")
  (V1HF "riscv_vector::vls_mode_valid_p (V1HFmode) && TARGET_VECTOR_ELEN_FP_16")
  (V2HF "riscv_vector::vls_mode_valid_p (V2HFmode) && TARGET_VECTOR_ELEN_FP_16")
  (V4HF "riscv_vector::vls_mode_valid_p (V4HFmode) && TARGET_VECTOR_ELEN_FP_16")
  (V8HF "riscv_vector::vls_mode_valid_p (V8HFmode) && TARGET_VECTOR_ELEN_FP_16")
  (V16HF "riscv_vector::vls_mode_valid_p (V16HFmode) && TARGET_VECTOR_ELEN_FP_16")
  (V32HF "riscv_vector::vls_mode_valid_p (V32HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 64")
  (V64HF "riscv_vector::vls_mode_valid_p (V64HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 128")
  (V128HF "riscv_vector::vls_mode_valid_p (V128HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 256")
  (V256HF "riscv_vector::vls_mode_valid_p (V256HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 512")
  (V512HF "riscv_vector::vls_mode_valid_p (V512HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 1024")
  (V1024HF "riscv_vector::vls_mode_valid_p (V1024HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 2048")
  (V2048HF "riscv_vector::vls_mode_valid_p (V2048HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 4096")
  (V1SF "riscv_vector::vls_mode_valid_p (V1SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V2SF "riscv_vector::vls_mode_valid_p (V2SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V4SF "riscv_vector::vls_mode_valid_p (V4SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V8SF "riscv_vector::vls_mode_valid_p (V8SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V16SF "riscv_vector::vls_mode_valid_p (V16SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 64")
  (V32SF "riscv_vector::vls_mode_valid_p (V32SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 128")
  (V64SF "riscv_vector::vls_mode_valid_p (V64SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 256")
  (V128SF "riscv_vector::vls_mode_valid_p (V128SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 512")
  (V256SF "riscv_vector::vls_mode_valid_p (V256SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 1024")
  (V512SF "riscv_vector::vls_mode_valid_p (V512SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 2048")
  (V1024SF "riscv_vector::vls_mode_valid_p (V1024SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 4096")
  (V1DF "riscv_vector::vls_mode_valid_p (V1DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V2DF "riscv_vector::vls_mode_valid_p (V2DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V4DF "riscv_vector::vls_mode_valid_p (V4DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V8DF "riscv_vector::vls_mode_valid_p (V8DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 64")
  (V16DF "riscv_vector::vls_mode_valid_p (V16DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 128")
  (V32DF "riscv_vector::vls_mode_valid_p (V32DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 256")
  (V64DF "riscv_vector::vls_mode_valid_p (V64DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 512")
  (V128DF "riscv_vector::vls_mode_valid_p (V128DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 1024")
  (V256DF "riscv_vector::vls_mode_valid_p (V256DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 2048")
  (V512DF "riscv_vector::vls_mode_valid_p (V512DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 4096")
])

(define_mode_iterator VFULLI [
  RVVM8QI RVVM4QI RVVM2QI RVVM1QI RVVMF2QI RVVMF4QI (RVVMF8QI "TARGET_VECTOR_ELEN_64")

  RVVM8HI RVVM4HI RVVM2HI RVVM1HI RVVMF2HI (RVVMF4HI "TARGET_VECTOR_ELEN_64")

  RVVM8SI RVVM4SI RVVM2SI RVVM1SI (RVVMF2SI "TARGET_VECTOR_ELEN_64")

  (RVVM8DI "TARGET_FULL_V") (RVVM4DI "TARGET_FULL_V") (RVVM2DI "TARGET_FULL_V") (RVVM1DI "TARGET_FULL_V")

  (V1QI "riscv_vector::vls_mode_valid_p (V1QImode)")
  (V2QI "riscv_vector::vls_mode_valid_p (V2QImode)")
  (V4QI "riscv_vector::vls_mode_valid_p (V4QImode)")
  (V8QI "riscv_vector::vls_mode_valid_p (V8QImode)")
  (V16QI "riscv_vector::vls_mode_valid_p (V16QImode)")
  (V32QI "riscv_vector::vls_mode_valid_p (V32QImode)")
  (V64QI "riscv_vector::vls_mode_valid_p (V64QImode) && TARGET_MIN_VLEN >= 64")
  (V128QI "riscv_vector::vls_mode_valid_p (V128QImode) && TARGET_MIN_VLEN >= 128")
  (V256QI "riscv_vector::vls_mode_valid_p (V256QImode) && TARGET_MIN_VLEN >= 256")
  (V512QI "riscv_vector::vls_mode_valid_p (V512QImode) && TARGET_MIN_VLEN >= 512")
  (V1024QI "riscv_vector::vls_mode_valid_p (V1024QImode) && TARGET_MIN_VLEN >= 1024")
  (V2048QI "riscv_vector::vls_mode_valid_p (V2048QImode) && TARGET_MIN_VLEN >= 2048")
  (V4096QI "riscv_vector::vls_mode_valid_p (V4096QImode) && TARGET_MIN_VLEN >= 4096")
  (V1HI "riscv_vector::vls_mode_valid_p (V1HImode)")
  (V2HI "riscv_vector::vls_mode_valid_p (V2HImode)")
  (V4HI "riscv_vector::vls_mode_valid_p (V4HImode)")
  (V8HI "riscv_vector::vls_mode_valid_p (V8HImode)")
  (V16HI "riscv_vector::vls_mode_valid_p (V16HImode)")
  (V32HI "riscv_vector::vls_mode_valid_p (V32HImode) && TARGET_MIN_VLEN >= 64")
  (V64HI "riscv_vector::vls_mode_valid_p (V64HImode) && TARGET_MIN_VLEN >= 128")
  (V128HI "riscv_vector::vls_mode_valid_p (V128HImode) && TARGET_MIN_VLEN >= 256")
  (V256HI "riscv_vector::vls_mode_valid_p (V256HImode) && TARGET_MIN_VLEN >= 512")
  (V512HI "riscv_vector::vls_mode_valid_p (V512HImode) && TARGET_MIN_VLEN >= 1024")
  (V1024HI "riscv_vector::vls_mode_valid_p (V1024HImode) && TARGET_MIN_VLEN >= 2048")
  (V2048HI "riscv_vector::vls_mode_valid_p (V2048HImode) && TARGET_MIN_VLEN >= 4096")
  (V1SI "riscv_vector::vls_mode_valid_p (V1SImode)")
  (V2SI "riscv_vector::vls_mode_valid_p (V2SImode)")
  (V4SI "riscv_vector::vls_mode_valid_p (V4SImode)")
  (V8SI "riscv_vector::vls_mode_valid_p (V8SImode)")
  (V16SI "riscv_vector::vls_mode_valid_p (V16SImode) && TARGET_MIN_VLEN >= 64")
  (V32SI "riscv_vector::vls_mode_valid_p (V32SImode) && TARGET_MIN_VLEN >= 128")
  (V64SI "riscv_vector::vls_mode_valid_p (V64SImode) && TARGET_MIN_VLEN >= 256")
  (V128SI "riscv_vector::vls_mode_valid_p (V128SImode) && TARGET_MIN_VLEN >= 512")
  (V256SI "riscv_vector::vls_mode_valid_p (V256SImode) && TARGET_MIN_VLEN >= 1024")
  (V512SI "riscv_vector::vls_mode_valid_p (V512SImode) && TARGET_MIN_VLEN >= 2048")
  (V1024SI "riscv_vector::vls_mode_valid_p (V1024SImode) && TARGET_MIN_VLEN >= 4096")
  (V1DI "riscv_vector::vls_mode_valid_p (V1DImode) && TARGET_FULL_V")
  (V2DI "riscv_vector::vls_mode_valid_p (V2DImode) && TARGET_FULL_V")
  (V4DI "riscv_vector::vls_mode_valid_p (V4DImode) && TARGET_FULL_V")
  (V8DI "riscv_vector::vls_mode_valid_p (V8DImode) && TARGET_FULL_V && TARGET_MIN_VLEN >= 64")
  (V16DI "riscv_vector::vls_mode_valid_p (V16DImode) && TARGET_FULL_V && TARGET_MIN_VLEN >= 128")
  (V32DI "riscv_vector::vls_mode_valid_p (V32DImode) && TARGET_FULL_V && TARGET_MIN_VLEN >= 256")
  (V64DI "riscv_vector::vls_mode_valid_p (V64DImode) && TARGET_FULL_V && TARGET_MIN_VLEN >= 512")
  (V128DI "riscv_vector::vls_mode_valid_p (V128DImode) && TARGET_FULL_V && TARGET_MIN_VLEN >= 1024")
  (V256DI "riscv_vector::vls_mode_valid_p (V256DImode) && TARGET_FULL_V && TARGET_MIN_VLEN >= 2048")
  (V512DI "riscv_vector::vls_mode_valid_p (V512DImode) && TARGET_FULL_V && TARGET_MIN_VLEN >= 4096")
])

(define_mode_iterator VI_QH [
  RVVM8QI RVVM4QI RVVM2QI RVVM1QI RVVMF2QI RVVMF4QI (RVVMF8QI "TARGET_VECTOR_ELEN_64")

  RVVM8HI RVVM4HI RVVM2HI RVVM1HI RVVMF2HI (RVVMF4HI "TARGET_VECTOR_ELEN_64")
])

(define_mode_iterator VI_QHS [
  RVVM8QI RVVM4QI RVVM2QI RVVM1QI RVVMF2QI RVVMF4QI (RVVMF8QI "TARGET_VECTOR_ELEN_64")

  RVVM8HI RVVM4HI RVVM2HI RVVM1HI RVVMF2HI (RVVMF4HI "TARGET_VECTOR_ELEN_64")

  RVVM8SI RVVM4SI RVVM2SI RVVM1SI (RVVMF2SI "TARGET_VECTOR_ELEN_64")

  (V1QI "riscv_vector::vls_mode_valid_p (V1QImode)")
  (V2QI "riscv_vector::vls_mode_valid_p (V2QImode)")
  (V4QI "riscv_vector::vls_mode_valid_p (V4QImode)")
  (V8QI "riscv_vector::vls_mode_valid_p (V8QImode)")
  (V16QI "riscv_vector::vls_mode_valid_p (V16QImode)")
  (V32QI "riscv_vector::vls_mode_valid_p (V32QImode)")
  (V64QI "riscv_vector::vls_mode_valid_p (V64QImode) && TARGET_MIN_VLEN >= 64")
  (V128QI "riscv_vector::vls_mode_valid_p (V128QImode) && TARGET_MIN_VLEN >= 128")
  (V256QI "riscv_vector::vls_mode_valid_p (V256QImode) && TARGET_MIN_VLEN >= 256")
  (V512QI "riscv_vector::vls_mode_valid_p (V512QImode) && TARGET_MIN_VLEN >= 512")
  (V1024QI "riscv_vector::vls_mode_valid_p (V1024QImode) && TARGET_MIN_VLEN >= 1024")
  (V2048QI "riscv_vector::vls_mode_valid_p (V2048QImode) && TARGET_MIN_VLEN >= 2048")
  (V4096QI "riscv_vector::vls_mode_valid_p (V4096QImode) && TARGET_MIN_VLEN >= 4096")
  (V1HI "riscv_vector::vls_mode_valid_p (V1HImode)")
  (V2HI "riscv_vector::vls_mode_valid_p (V2HImode)")
  (V4HI "riscv_vector::vls_mode_valid_p (V4HImode)")
  (V8HI "riscv_vector::vls_mode_valid_p (V8HImode)")
  (V16HI "riscv_vector::vls_mode_valid_p (V16HImode)")
  (V32HI "riscv_vector::vls_mode_valid_p (V32HImode) && TARGET_MIN_VLEN >= 64")
  (V64HI "riscv_vector::vls_mode_valid_p (V64HImode) && TARGET_MIN_VLEN >= 128")
  (V128HI "riscv_vector::vls_mode_valid_p (V128HImode) && TARGET_MIN_VLEN >= 256")
  (V256HI "riscv_vector::vls_mode_valid_p (V256HImode) && TARGET_MIN_VLEN >= 512")
  (V512HI "riscv_vector::vls_mode_valid_p (V512HImode) && TARGET_MIN_VLEN >= 1024")
  (V1024HI "riscv_vector::vls_mode_valid_p (V1024HImode) && TARGET_MIN_VLEN >= 2048")
  (V2048HI "riscv_vector::vls_mode_valid_p (V2048HImode) && TARGET_MIN_VLEN >= 4096")
  (V1SI "riscv_vector::vls_mode_valid_p (V1SImode)")
  (V2SI "riscv_vector::vls_mode_valid_p (V2SImode)")
  (V4SI "riscv_vector::vls_mode_valid_p (V4SImode)")
  (V8SI "riscv_vector::vls_mode_valid_p (V8SImode)")
  (V16SI "riscv_vector::vls_mode_valid_p (V16SImode) && TARGET_MIN_VLEN >= 64")
  (V32SI "riscv_vector::vls_mode_valid_p (V32SImode) && TARGET_MIN_VLEN >= 128")
  (V64SI "riscv_vector::vls_mode_valid_p (V64SImode) && TARGET_MIN_VLEN >= 256")
  (V128SI "riscv_vector::vls_mode_valid_p (V128SImode) && TARGET_MIN_VLEN >= 512")
  (V256SI "riscv_vector::vls_mode_valid_p (V256SImode) && TARGET_MIN_VLEN >= 1024")
  (V512SI "riscv_vector::vls_mode_valid_p (V512SImode) && TARGET_MIN_VLEN >= 2048")
  (V1024SI "riscv_vector::vls_mode_valid_p (V1024SImode) && TARGET_MIN_VLEN >= 4096")
])

(define_mode_iterator VI_QHS_NO_M8 [
  RVVM4QI RVVM2QI RVVM1QI RVVMF2QI RVVMF4QI (RVVMF8QI "TARGET_VECTOR_ELEN_64")

  RVVM4HI RVVM2HI RVVM1HI RVVMF2HI (RVVMF4HI "TARGET_VECTOR_ELEN_64")

  RVVM4SI RVVM2SI RVVM1SI (RVVMF2SI "TARGET_VECTOR_ELEN_64")

  (V1QI "riscv_vector::vls_mode_valid_p (V1QImode)")
  (V2QI "riscv_vector::vls_mode_valid_p (V2QImode)")
  (V4QI "riscv_vector::vls_mode_valid_p (V4QImode)")
  (V8QI "riscv_vector::vls_mode_valid_p (V8QImode)")
  (V16QI "riscv_vector::vls_mode_valid_p (V16QImode)")
  (V32QI "riscv_vector::vls_mode_valid_p (V32QImode)")
  (V64QI "riscv_vector::vls_mode_valid_p (V64QImode) && TARGET_MIN_VLEN >= 64")
  (V128QI "riscv_vector::vls_mode_valid_p (V128QImode) && TARGET_MIN_VLEN >= 128")
  (V256QI "riscv_vector::vls_mode_valid_p (V256QImode) && TARGET_MIN_VLEN >= 256")
  (V512QI "riscv_vector::vls_mode_valid_p (V512QImode) && TARGET_MIN_VLEN >= 512")
  (V1024QI "riscv_vector::vls_mode_valid_p (V1024QImode) && TARGET_MIN_VLEN >= 1024")
  (V2048QI "riscv_vector::vls_mode_valid_p (V2048QImode) && TARGET_MIN_VLEN >= 2048")
  (V1HI "riscv_vector::vls_mode_valid_p (V1HImode)")
  (V2HI "riscv_vector::vls_mode_valid_p (V2HImode)")
  (V4HI "riscv_vector::vls_mode_valid_p (V4HImode)")
  (V8HI "riscv_vector::vls_mode_valid_p (V8HImode)")
  (V16HI "riscv_vector::vls_mode_valid_p (V16HImode)")
  (V32HI "riscv_vector::vls_mode_valid_p (V32HImode) && TARGET_MIN_VLEN >= 64")
  (V64HI "riscv_vector::vls_mode_valid_p (V64HImode) && TARGET_MIN_VLEN >= 128")
  (V128HI "riscv_vector::vls_mode_valid_p (V128HImode) && TARGET_MIN_VLEN >= 256")
  (V256HI "riscv_vector::vls_mode_valid_p (V256HImode) && TARGET_MIN_VLEN >= 512")
  (V512HI "riscv_vector::vls_mode_valid_p (V512HImode) && TARGET_MIN_VLEN >= 1024")
  (V1024HI "riscv_vector::vls_mode_valid_p (V1024HImode) && TARGET_MIN_VLEN >= 2048")
  (V1SI "riscv_vector::vls_mode_valid_p (V1SImode)")
  (V2SI "riscv_vector::vls_mode_valid_p (V2SImode)")
  (V4SI "riscv_vector::vls_mode_valid_p (V4SImode)")
  (V8SI "riscv_vector::vls_mode_valid_p (V8SImode)")
  (V16SI "riscv_vector::vls_mode_valid_p (V16SImode) && TARGET_MIN_VLEN >= 64")
  (V32SI "riscv_vector::vls_mode_valid_p (V32SImode) && TARGET_MIN_VLEN >= 128")
  (V64SI "riscv_vector::vls_mode_valid_p (V64SImode) && TARGET_MIN_VLEN >= 256")
  (V128SI "riscv_vector::vls_mode_valid_p (V128SImode) && TARGET_MIN_VLEN >= 512")
  (V256SI "riscv_vector::vls_mode_valid_p (V256SImode) && TARGET_MIN_VLEN >= 1024")
  (V512SI "riscv_vector::vls_mode_valid_p (V512SImode) && TARGET_MIN_VLEN >= 2048")
])

(define_mode_iterator VF_HS [
  (RVVM8HF "TARGET_ZVFH") (RVVM4HF "TARGET_ZVFH") (RVVM2HF "TARGET_ZVFH")
  (RVVM1HF "TARGET_ZVFH") (RVVMF2HF "TARGET_ZVFH")
  (RVVMF4HF "TARGET_ZVFH && TARGET_VECTOR_ELEN_64")

  (RVVM8SF "TARGET_VECTOR_ELEN_FP_32") (RVVM4SF "TARGET_VECTOR_ELEN_FP_32") (RVVM2SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM1SF "TARGET_VECTOR_ELEN_FP_32") (RVVMF2SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_VECTOR_ELEN_64")

  (V1HF "riscv_vector::vls_mode_valid_p (V1HFmode) && TARGET_ZVFH")
  (V2HF "riscv_vector::vls_mode_valid_p (V2HFmode) && TARGET_ZVFH")
  (V4HF "riscv_vector::vls_mode_valid_p (V4HFmode) && TARGET_ZVFH")
  (V8HF "riscv_vector::vls_mode_valid_p (V8HFmode) && TARGET_ZVFH")
  (V16HF "riscv_vector::vls_mode_valid_p (V16HFmode) && TARGET_ZVFH")
  (V32HF "riscv_vector::vls_mode_valid_p (V32HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 64")
  (V64HF "riscv_vector::vls_mode_valid_p (V64HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 128")
  (V128HF "riscv_vector::vls_mode_valid_p (V128HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 256")
  (V256HF "riscv_vector::vls_mode_valid_p (V256HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 512")
  (V512HF "riscv_vector::vls_mode_valid_p (V512HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 1024")
  (V1024HF "riscv_vector::vls_mode_valid_p (V1024HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 2048")
  (V2048HF "riscv_vector::vls_mode_valid_p (V2048HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 4096")
  (V1SF "riscv_vector::vls_mode_valid_p (V1SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V2SF "riscv_vector::vls_mode_valid_p (V2SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V4SF "riscv_vector::vls_mode_valid_p (V4SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V8SF "riscv_vector::vls_mode_valid_p (V8SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V16SF "riscv_vector::vls_mode_valid_p (V16SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 64")
  (V32SF "riscv_vector::vls_mode_valid_p (V32SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 128")
  (V64SF "riscv_vector::vls_mode_valid_p (V64SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 256")
  (V128SF "riscv_vector::vls_mode_valid_p (V128SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 512")
  (V256SF "riscv_vector::vls_mode_valid_p (V256SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 1024")
  (V512SF "riscv_vector::vls_mode_valid_p (V512SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 2048")
  (V1024SF "riscv_vector::vls_mode_valid_p (V1024SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 4096")
])

(define_mode_iterator VF_HS_NO_M8 [
  (RVVM4HF "TARGET_ZVFH")
  (RVVM2HF "TARGET_ZVFH")
  (RVVM1HF "TARGET_ZVFH")
  (RVVMF2HF "TARGET_ZVFH")
  (RVVMF4HF "TARGET_ZVFH && TARGET_VECTOR_ELEN_64")
  (RVVM4SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM2SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM1SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVMF2SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_VECTOR_ELEN_64")

  (V1HF "riscv_vector::vls_mode_valid_p (V1HFmode) && TARGET_ZVFH")
  (V2HF "riscv_vector::vls_mode_valid_p (V2HFmode) && TARGET_ZVFH")
  (V4HF "riscv_vector::vls_mode_valid_p (V4HFmode) && TARGET_ZVFH")
  (V8HF "riscv_vector::vls_mode_valid_p (V8HFmode) && TARGET_ZVFH")
  (V16HF "riscv_vector::vls_mode_valid_p (V16HFmode) && TARGET_ZVFH")
  (V32HF "riscv_vector::vls_mode_valid_p (V32HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 64")
  (V64HF "riscv_vector::vls_mode_valid_p (V64HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 128")
  (V128HF "riscv_vector::vls_mode_valid_p (V128HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 256")
  (V256HF "riscv_vector::vls_mode_valid_p (V256HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 512")
  (V512HF "riscv_vector::vls_mode_valid_p (V512HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 1024")
  (V1024HF "riscv_vector::vls_mode_valid_p (V1024HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 2048")
  (V1SF "riscv_vector::vls_mode_valid_p (V1SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V2SF "riscv_vector::vls_mode_valid_p (V2SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V4SF "riscv_vector::vls_mode_valid_p (V4SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V8SF "riscv_vector::vls_mode_valid_p (V8SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V16SF "riscv_vector::vls_mode_valid_p (V16SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 64")
  (V32SF "riscv_vector::vls_mode_valid_p (V32SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 128")
  (V64SF "riscv_vector::vls_mode_valid_p (V64SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 256")
  (V128SF "riscv_vector::vls_mode_valid_p (V128SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 512")
  (V256SF "riscv_vector::vls_mode_valid_p (V256SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 1024")
  (V512SF "riscv_vector::vls_mode_valid_p (V512SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 2048")
])

(define_mode_iterator VF_HS_M8 [
  (RVVM8HF "TARGET_ZVFH")
  (RVVM8SF "TARGET_VECTOR_ELEN_FP_32")
])

(define_mode_iterator V_VLSI_QHS [
  RVVM8QI RVVM4QI RVVM2QI RVVM1QI RVVMF2QI RVVMF4QI (RVVMF8QI "TARGET_VECTOR_ELEN_64")

  RVVM8HI RVVM4HI RVVM2HI RVVM1HI RVVMF2HI (RVVMF4HI "TARGET_VECTOR_ELEN_64")

  RVVM8SI RVVM4SI RVVM2SI RVVM1SI (RVVMF2SI "TARGET_VECTOR_ELEN_64")

  (V1QI "riscv_vector::vls_mode_valid_p (V1QImode)")
  (V2QI "riscv_vector::vls_mode_valid_p (V2QImode)")
  (V4QI "riscv_vector::vls_mode_valid_p (V4QImode)")
  (V8QI "riscv_vector::vls_mode_valid_p (V8QImode)")
  (V16QI "riscv_vector::vls_mode_valid_p (V16QImode)")
  (V32QI "riscv_vector::vls_mode_valid_p (V32QImode)")
  (V64QI "riscv_vector::vls_mode_valid_p (V64QImode) && TARGET_MIN_VLEN >= 64")
  (V128QI "riscv_vector::vls_mode_valid_p (V128QImode) && TARGET_MIN_VLEN >= 128")
  (V256QI "riscv_vector::vls_mode_valid_p (V256QImode) && TARGET_MIN_VLEN >= 256")
  (V512QI "riscv_vector::vls_mode_valid_p (V512QImode) && TARGET_MIN_VLEN >= 512")
  (V1024QI "riscv_vector::vls_mode_valid_p (V1024QImode) && TARGET_MIN_VLEN >= 1024")
  (V2048QI "riscv_vector::vls_mode_valid_p (V2048QImode) && TARGET_MIN_VLEN >= 2048")
  (V4096QI "riscv_vector::vls_mode_valid_p (V4096QImode) && TARGET_MIN_VLEN >= 4096")
  (V1HI "riscv_vector::vls_mode_valid_p (V1HImode)")
  (V2HI "riscv_vector::vls_mode_valid_p (V2HImode)")
  (V4HI "riscv_vector::vls_mode_valid_p (V4HImode)")
  (V8HI "riscv_vector::vls_mode_valid_p (V8HImode)")
  (V16HI "riscv_vector::vls_mode_valid_p (V16HImode)")
  (V32HI "riscv_vector::vls_mode_valid_p (V32HImode) && TARGET_MIN_VLEN >= 64")
  (V64HI "riscv_vector::vls_mode_valid_p (V64HImode) && TARGET_MIN_VLEN >= 128")
  (V128HI "riscv_vector::vls_mode_valid_p (V128HImode) && TARGET_MIN_VLEN >= 256")
  (V256HI "riscv_vector::vls_mode_valid_p (V256HImode) && TARGET_MIN_VLEN >= 512")
  (V512HI "riscv_vector::vls_mode_valid_p (V512HImode) && TARGET_MIN_VLEN >= 1024")
  (V1024HI "riscv_vector::vls_mode_valid_p (V1024HImode) && TARGET_MIN_VLEN >= 2048")
  (V2048HI "riscv_vector::vls_mode_valid_p (V2048HImode) && TARGET_MIN_VLEN >= 4096")
  (V1SI "riscv_vector::vls_mode_valid_p (V1SImode)")
  (V2SI "riscv_vector::vls_mode_valid_p (V2SImode)")
  (V4SI "riscv_vector::vls_mode_valid_p (V4SImode)")
  (V8SI "riscv_vector::vls_mode_valid_p (V8SImode)")
  (V16SI "riscv_vector::vls_mode_valid_p (V16SImode) && TARGET_MIN_VLEN >= 64")
  (V32SI "riscv_vector::vls_mode_valid_p (V32SImode) && TARGET_MIN_VLEN >= 128")
  (V64SI "riscv_vector::vls_mode_valid_p (V64SImode) && TARGET_MIN_VLEN >= 256")
  (V128SI "riscv_vector::vls_mode_valid_p (V128SImode) && TARGET_MIN_VLEN >= 512")
  (V256SI "riscv_vector::vls_mode_valid_p (V256SImode) && TARGET_MIN_VLEN >= 1024")
  (V512SI "riscv_vector::vls_mode_valid_p (V512SImode) && TARGET_MIN_VLEN >= 2048")
  (V1024SI "riscv_vector::vls_mode_valid_p (V1024SImode) && TARGET_MIN_VLEN >= 4096")
])

(define_mode_iterator VI_D [
  (RVVM8DI "TARGET_VECTOR_ELEN_64") (RVVM4DI "TARGET_VECTOR_ELEN_64")
  (RVVM2DI "TARGET_VECTOR_ELEN_64") (RVVM1DI "TARGET_VECTOR_ELEN_64")
])

(define_mode_iterator V_VLSI_D [
  (RVVM8DI "TARGET_VECTOR_ELEN_64") (RVVM4DI "TARGET_VECTOR_ELEN_64")
  (RVVM2DI "TARGET_VECTOR_ELEN_64") (RVVM1DI "TARGET_VECTOR_ELEN_64")

  (V1DI "riscv_vector::vls_mode_valid_p (V1DImode) && TARGET_VECTOR_ELEN_64")
  (V2DI "riscv_vector::vls_mode_valid_p (V2DImode) && TARGET_VECTOR_ELEN_64")
  (V4DI "riscv_vector::vls_mode_valid_p (V4DImode) && TARGET_VECTOR_ELEN_64")
  (V8DI "riscv_vector::vls_mode_valid_p (V8DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 64")
  (V16DI "riscv_vector::vls_mode_valid_p (V16DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 128")
  (V32DI "riscv_vector::vls_mode_valid_p (V32DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 256")
  (V64DI "riscv_vector::vls_mode_valid_p (V64DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 512")
  (V128DI "riscv_vector::vls_mode_valid_p (V128DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 1024")
  (V256DI "riscv_vector::vls_mode_valid_p (V256DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 2048")
  (V512DI "riscv_vector::vls_mode_valid_p (V512DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 4096")
])

(define_mode_iterator VFULLI_D [
  (RVVM8DI "TARGET_FULL_V") (RVVM4DI "TARGET_FULL_V")
  (RVVM2DI "TARGET_FULL_V") (RVVM1DI "TARGET_FULL_V")
])

;; All RATIO mode iterators are used on gather/scatter vectorization.
;; RISC-V V Spec 18.3:
;; The V extension supports all vector load and store instructions (Section
;; Vector Loads and Stores), except the V extension does not support EEW=64
;; for index values when XLEN=32.
;; According to RVV ISA description above, all RATIO index DI mode need TARGET_64BIT.
;;
;; In gather/scatter expand, we need to sign/zero extend the index mode into vector
;; Pmode, so we need to check whether vector Pmode is available.
;; E.g. when index mode = RVVM8QImode and Pmode = SImode, if it is not zero_extend or
;; scalar != 1, such gather/scatter is not allowed since we don't have RVVM32SImode.
(define_mode_iterator RATIO64 [
  (RVVMF8QI "TARGET_VECTOR_ELEN_64")
  (RVVMF4HI "TARGET_VECTOR_ELEN_64")
  (RVVMF2SI "TARGET_VECTOR_ELEN_64")
  (RVVM1DI "TARGET_VECTOR_ELEN_64")
  (RVVMF4BF "TARGET_VECTOR_ELEN_BF_16 && TARGET_VECTOR_ELEN_64")
  (RVVMF4HF "TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_64")
  (RVVMF2SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_VECTOR_ELEN_64")
  (RVVM1DF "TARGET_VECTOR_ELEN_FP_64")
])

(define_mode_iterator RATIO32 [
  RVVMF4QI
  RVVMF2HI
  RVVM1SI
  (RVVM2DI "TARGET_VECTOR_ELEN_64")
  (RVVMF2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF2HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM1SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM2DF "TARGET_VECTOR_ELEN_FP_64")
])

(define_mode_iterator RATIO16 [
  RVVMF2QI
  RVVM1HI
  RVVM2SI
  (RVVM4DI "TARGET_VECTOR_ELEN_64")
  (RVVM1BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM1HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM2SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM4DF "TARGET_VECTOR_ELEN_FP_64")
])

(define_mode_iterator RATIO8 [
  RVVM1QI
  RVVM2HI
  RVVM4SI
  (RVVM8DI "TARGET_VECTOR_ELEN_64")
  (RVVM2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM2HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM4SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM8DF "TARGET_VECTOR_ELEN_FP_64")
])

(define_mode_iterator RATIO4 [
  RVVM2QI
  RVVM4HI
  RVVM8SI
  (RVVM4BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM4HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM8SF "TARGET_VECTOR_ELEN_FP_32")
])

(define_mode_iterator RATIO2 [
  RVVM4QI
  RVVM8HI
  (RVVM8BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM8HF "TARGET_VECTOR_ELEN_FP_16")
])

(define_mode_iterator RATIO1 [
  RVVM8QI
])

(define_mode_iterator RATIO64I [
  (RVVMF8QI "TARGET_VECTOR_ELEN_64")
  (RVVMF4HI "TARGET_VECTOR_ELEN_64")
  (RVVMF2SI "TARGET_VECTOR_ELEN_64")
  (RVVM1DI "TARGET_VECTOR_ELEN_64 && TARGET_64BIT")
])

(define_mode_iterator RATIO32I [
  RVVMF4QI
  RVVMF2HI
  RVVM1SI
  (RVVM2DI "TARGET_VECTOR_ELEN_64 && TARGET_64BIT")
])

(define_mode_iterator RATIO16I [
  RVVMF2QI
  RVVM1HI
  RVVM2SI
  (RVVM4DI "TARGET_VECTOR_ELEN_64 && TARGET_64BIT")
])

(define_mode_iterator RATIO8I [
  RVVM1QI
  RVVM2HI
  RVVM4SI
  (RVVM8DI "TARGET_VECTOR_ELEN_64 && TARGET_64BIT")
])

(define_mode_iterator RATIO4I [
  RVVM2QI
  RVVM4HI
  RVVM8SI
])

(define_mode_iterator RATIO2I [
  RVVM4QI
  RVVM8HI
])

(define_mode_iterator V_WHOLE [
  RVVM8QI RVVM4QI RVVM2QI RVVM1QI

  RVVM8HI RVVM4HI RVVM2HI RVVM1HI

  (RVVM8BF "TARGET_VECTOR_ELEN_BF_16") (RVVM4BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM2BF "TARGET_VECTOR_ELEN_BF_16") (RVVM1BF "TARGET_VECTOR_ELEN_BF_16")

  (RVVM8HF "TARGET_VECTOR_ELEN_FP_16") (RVVM4HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM2HF "TARGET_VECTOR_ELEN_FP_16") (RVVM1HF "TARGET_VECTOR_ELEN_FP_16")

  RVVM8SI RVVM4SI RVVM2SI RVVM1SI

  (RVVM8SF "TARGET_VECTOR_ELEN_FP_32") (RVVM4SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM2SF "TARGET_VECTOR_ELEN_FP_32") (RVVM1SF "TARGET_VECTOR_ELEN_FP_32")

  (RVVM8DI "TARGET_VECTOR_ELEN_64") (RVVM4DI "TARGET_VECTOR_ELEN_64")
  (RVVM2DI "TARGET_VECTOR_ELEN_64") (RVVM1DI "TARGET_VECTOR_ELEN_64")

  (RVVM8DF "TARGET_VECTOR_ELEN_FP_64") (RVVM4DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM2DF "TARGET_VECTOR_ELEN_FP_64") (RVVM1DF "TARGET_VECTOR_ELEN_FP_64")
])

(define_mode_iterator V_FRACT [
  RVVMF2QI RVVMF4QI (RVVMF8QI "TARGET_VECTOR_ELEN_64")

  RVVMF2HI (RVVMF4HI "TARGET_VECTOR_ELEN_64")

  (RVVMF2BF "TARGET_VECTOR_ELEN_BF_16") (RVVMF4BF "TARGET_VECTOR_ELEN_BF_16 && TARGET_VECTOR_ELEN_64")

  (RVVMF2HF "TARGET_VECTOR_ELEN_FP_16") (RVVMF4HF "TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_64")

  (RVVMF2SI "TARGET_VECTOR_ELEN_64")

  (RVVMF2SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_VECTOR_ELEN_64")
])

(define_mode_iterator VWEXTI [
  RVVM8HI RVVM4HI RVVM2HI RVVM1HI RVVMF2HI (RVVMF4HI "TARGET_VECTOR_ELEN_64")

  RVVM8SI RVVM4SI RVVM2SI RVVM1SI (RVVMF2SI "TARGET_VECTOR_ELEN_64")

  (RVVM8DI "TARGET_VECTOR_ELEN_64") (RVVM4DI "TARGET_VECTOR_ELEN_64")
  (RVVM2DI "TARGET_VECTOR_ELEN_64") (RVVM1DI "TARGET_VECTOR_ELEN_64")

  (V1HI "riscv_vector::vls_mode_valid_p (V1HImode)")
  (V2HI "riscv_vector::vls_mode_valid_p (V2HImode)")
  (V4HI "riscv_vector::vls_mode_valid_p (V4HImode)")
  (V8HI "riscv_vector::vls_mode_valid_p (V8HImode)")
  (V16HI "riscv_vector::vls_mode_valid_p (V16HImode)")
  (V32HI "riscv_vector::vls_mode_valid_p (V32HImode) && TARGET_MIN_VLEN >= 64")
  (V64HI "riscv_vector::vls_mode_valid_p (V64HImode) && TARGET_MIN_VLEN >= 128")
  (V128HI "riscv_vector::vls_mode_valid_p (V128HImode) && TARGET_MIN_VLEN >= 256")
  (V256HI "riscv_vector::vls_mode_valid_p (V256HImode) && TARGET_MIN_VLEN >= 512")
  (V512HI "riscv_vector::vls_mode_valid_p (V512HImode) && TARGET_MIN_VLEN >= 1024")
  (V1024HI "riscv_vector::vls_mode_valid_p (V1024HImode) && TARGET_MIN_VLEN >= 2048")
  (V2048HI "riscv_vector::vls_mode_valid_p (V2048HImode) && TARGET_MIN_VLEN >= 4096")
  (V1SI "riscv_vector::vls_mode_valid_p (V1SImode)")
  (V2SI "riscv_vector::vls_mode_valid_p (V2SImode)")
  (V4SI "riscv_vector::vls_mode_valid_p (V4SImode)")
  (V8SI "riscv_vector::vls_mode_valid_p (V8SImode)")
  (V16SI "riscv_vector::vls_mode_valid_p (V16SImode) && TARGET_MIN_VLEN >= 64")
  (V32SI "riscv_vector::vls_mode_valid_p (V32SImode) && TARGET_MIN_VLEN >= 128")
  (V64SI "riscv_vector::vls_mode_valid_p (V64SImode) && TARGET_MIN_VLEN >= 256")
  (V128SI "riscv_vector::vls_mode_valid_p (V128SImode) && TARGET_MIN_VLEN >= 512")
  (V256SI "riscv_vector::vls_mode_valid_p (V256SImode) && TARGET_MIN_VLEN >= 1024")
  (V512SI "riscv_vector::vls_mode_valid_p (V512SImode) && TARGET_MIN_VLEN >= 2048")
  (V1024SI "riscv_vector::vls_mode_valid_p (V1024SImode) && TARGET_MIN_VLEN >= 4096")
  (V1DI "riscv_vector::vls_mode_valid_p (V1DImode) && TARGET_VECTOR_ELEN_64")
  (V2DI "riscv_vector::vls_mode_valid_p (V2DImode) && TARGET_VECTOR_ELEN_64")
  (V4DI "riscv_vector::vls_mode_valid_p (V4DImode) && TARGET_VECTOR_ELEN_64")
  (V8DI "riscv_vector::vls_mode_valid_p (V8DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 64")
  (V16DI "riscv_vector::vls_mode_valid_p (V16DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 128")
  (V32DI "riscv_vector::vls_mode_valid_p (V32DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 256")
  (V64DI "riscv_vector::vls_mode_valid_p (V64DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 512")
  (V128DI "riscv_vector::vls_mode_valid_p (V128DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 1024")
  (V256DI "riscv_vector::vls_mode_valid_p (V256DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 2048")
  (V512DI "riscv_vector::vls_mode_valid_p (V512DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 4096")
])

;; Same iterator split reason as VF_ZVFHMIN and VF.
(define_mode_iterator VWEXTF_ZVFHMIN [
  (RVVM8SF "TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32")
  (RVVM4SF "TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32")
  (RVVM2SF "TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32")
  (RVVM1SF "TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32")
  (RVVMF2SF "TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_VECTOR_ELEN_64")

  (RVVM8DF "TARGET_VECTOR_ELEN_FP_64") (RVVM4DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM2DF "TARGET_VECTOR_ELEN_FP_64") (RVVM1DF "TARGET_VECTOR_ELEN_FP_64")

  (V1SF "riscv_vector::vls_mode_valid_p (V1SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32")
  (V2SF "riscv_vector::vls_mode_valid_p (V2SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32")
  (V4SF "riscv_vector::vls_mode_valid_p (V4SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32")
  (V8SF "riscv_vector::vls_mode_valid_p (V8SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32")
  (V16SF "riscv_vector::vls_mode_valid_p (V16SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 64")
  (V32SF "riscv_vector::vls_mode_valid_p (V32SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 128")
  (V64SF "riscv_vector::vls_mode_valid_p (V64SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 256")
  (V128SF "riscv_vector::vls_mode_valid_p (V128SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 512")
  (V256SF "riscv_vector::vls_mode_valid_p (V256SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 1024")
  (V512SF "riscv_vector::vls_mode_valid_p (V512SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 2048")
  (V1024SF "riscv_vector::vls_mode_valid_p (V1024SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 4096")
  (V1DF "riscv_vector::vls_mode_valid_p (V1DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V2DF "riscv_vector::vls_mode_valid_p (V2DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V4DF "riscv_vector::vls_mode_valid_p (V4DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V8DF "riscv_vector::vls_mode_valid_p (V8DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 64")
  (V16DF "riscv_vector::vls_mode_valid_p (V16DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 128")
  (V32DF "riscv_vector::vls_mode_valid_p (V32DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 256")
  (V64DF "riscv_vector::vls_mode_valid_p (V64DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 512")
  (V128DF "riscv_vector::vls_mode_valid_p (V128DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 1024")
  (V256DF "riscv_vector::vls_mode_valid_p (V256DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 2048")
  (V512DF "riscv_vector::vls_mode_valid_p (V512DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 4096")
])

(define_mode_iterator VWEXTF [
  (RVVM8SF "TARGET_ZVFH && TARGET_VECTOR_ELEN_FP_32")
  (RVVM4SF "TARGET_ZVFH && TARGET_VECTOR_ELEN_FP_32")
  (RVVM2SF "TARGET_ZVFH && TARGET_VECTOR_ELEN_FP_32")
  (RVVM1SF "TARGET_ZVFH && TARGET_VECTOR_ELEN_FP_32")
  (RVVMF2SF "TARGET_ZVFH && TARGET_VECTOR_ELEN_FP_32 && TARGET_VECTOR_ELEN_64")

  (RVVM8DF "TARGET_VECTOR_ELEN_FP_64") (RVVM4DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM2DF "TARGET_VECTOR_ELEN_FP_64") (RVVM1DF "TARGET_VECTOR_ELEN_FP_64")

  (V1SF "riscv_vector::vls_mode_valid_p (V1SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32")
  (V2SF "riscv_vector::vls_mode_valid_p (V2SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32")
  (V4SF "riscv_vector::vls_mode_valid_p (V4SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32")
  (V8SF "riscv_vector::vls_mode_valid_p (V8SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32")
  (V16SF "riscv_vector::vls_mode_valid_p (V16SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 64")
  (V32SF "riscv_vector::vls_mode_valid_p (V32SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 128")
  (V64SF "riscv_vector::vls_mode_valid_p (V64SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 256")
  (V128SF "riscv_vector::vls_mode_valid_p (V128SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 512")
  (V256SF "riscv_vector::vls_mode_valid_p (V256SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 1024")
  (V512SF "riscv_vector::vls_mode_valid_p (V512SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 2048")
  (V1024SF "riscv_vector::vls_mode_valid_p (V1024SFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 4096")
  (V1DF "riscv_vector::vls_mode_valid_p (V1DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V2DF "riscv_vector::vls_mode_valid_p (V2DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V4DF "riscv_vector::vls_mode_valid_p (V4DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V8DF "riscv_vector::vls_mode_valid_p (V8DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 64")
  (V16DF "riscv_vector::vls_mode_valid_p (V16DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 128")
  (V32DF "riscv_vector::vls_mode_valid_p (V32DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 256")
  (V64DF "riscv_vector::vls_mode_valid_p (V64DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 512")
  (V128DF "riscv_vector::vls_mode_valid_p (V128DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 1024")
  (V256DF "riscv_vector::vls_mode_valid_p (V256DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 2048")
  (V512DF "riscv_vector::vls_mode_valid_p (V512DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 4096")
])

(define_mode_iterator VWCONVERTI [
  (RVVM8SI "TARGET_ZVFH") (RVVM4SI "TARGET_ZVFH") (RVVM2SI "TARGET_ZVFH") (RVVM1SI "TARGET_ZVFH")
  (RVVMF2SI "TARGET_ZVFH")

  (RVVM8DI "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32")
  (RVVM4DI "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32")
  (RVVM2DI "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32")
  (RVVM1DI "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32")

  (V1SI "riscv_vector::vls_mode_valid_p (V1SImode) && TARGET_ZVFH")
  (V2SI "riscv_vector::vls_mode_valid_p (V2SImode) && TARGET_ZVFH")
  (V4SI "riscv_vector::vls_mode_valid_p (V4SImode) && TARGET_ZVFH")
  (V8SI "riscv_vector::vls_mode_valid_p (V8SImode) && TARGET_ZVFH")
  (V16SI "riscv_vector::vls_mode_valid_p (V16SImode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 64")
  (V32SI "riscv_vector::vls_mode_valid_p (V32SImode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 128")
  (V64SI "riscv_vector::vls_mode_valid_p (V64SImode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 256")
  (V128SI "riscv_vector::vls_mode_valid_p (V128SImode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 512")
  (V256SI "riscv_vector::vls_mode_valid_p (V256SImode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 1024")
  (V512SI "riscv_vector::vls_mode_valid_p (V512SImode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 2048")
  (V1024SI "riscv_vector::vls_mode_valid_p (V1024SImode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 4096")
  (V1DI "riscv_vector::vls_mode_valid_p (V1DImode) && TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32")
  (V2DI "riscv_vector::vls_mode_valid_p (V2DImode) && TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32")
  (V4DI "riscv_vector::vls_mode_valid_p (V4DImode) && TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32")
  (V8DI "riscv_vector::vls_mode_valid_p (V8DImode) && TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 64")
  (V16DI "riscv_vector::vls_mode_valid_p (V16DImode) && TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 128")
  (V32DI "riscv_vector::vls_mode_valid_p (V32DImode) && TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 256")
  (V64DI "riscv_vector::vls_mode_valid_p (V64DImode) && TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 512")
  (V128DI "riscv_vector::vls_mode_valid_p (V128DImode) && TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 1024")
  (V256DI "riscv_vector::vls_mode_valid_p (V256DImode) && TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 2048")
  (V512DI "riscv_vector::vls_mode_valid_p (V512DImode) && TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 4096")
])

(define_mode_iterator VWWCONVERTI [
  (RVVM8DI "TARGET_VECTOR_ELEN_64 && TARGET_ZVFH")
  (RVVM4DI "TARGET_VECTOR_ELEN_64 && TARGET_ZVFH")
  (RVVM2DI "TARGET_VECTOR_ELEN_64 && TARGET_ZVFH")
  (RVVM1DI "TARGET_VECTOR_ELEN_64 && TARGET_ZVFH")

  (V1DI "riscv_vector::vls_mode_valid_p (V1DImode) && TARGET_VECTOR_ELEN_64 && TARGET_ZVFH")
  (V2DI "riscv_vector::vls_mode_valid_p (V2DImode) && TARGET_VECTOR_ELEN_64 && TARGET_ZVFH")
  (V4DI "riscv_vector::vls_mode_valid_p (V4DImode) && TARGET_VECTOR_ELEN_64 && TARGET_ZVFH")
  (V8DI "riscv_vector::vls_mode_valid_p (V8DImode) && TARGET_VECTOR_ELEN_64 && TARGET_ZVFH && TARGET_MIN_VLEN >= 64")
  (V16DI "riscv_vector::vls_mode_valid_p (V16DImode) && TARGET_VECTOR_ELEN_64 && TARGET_ZVFH && TARGET_MIN_VLEN >= 128")
  (V32DI "riscv_vector::vls_mode_valid_p (V32DImode) && TARGET_VECTOR_ELEN_64 && TARGET_ZVFH && TARGET_MIN_VLEN >= 256")
  (V64DI "riscv_vector::vls_mode_valid_p (V64DImode) && TARGET_VECTOR_ELEN_64 && TARGET_ZVFH && TARGET_MIN_VLEN >= 512")
  (V128DI "riscv_vector::vls_mode_valid_p (V128DImode) && TARGET_VECTOR_ELEN_64 && TARGET_ZVFH && TARGET_MIN_VLEN >= 1024")
  (V256DI "riscv_vector::vls_mode_valid_p (V256DImode) && TARGET_VECTOR_ELEN_64 && TARGET_ZVFH && TARGET_MIN_VLEN >= 2048")
  (V512DI "riscv_vector::vls_mode_valid_p (V512DImode) && TARGET_VECTOR_ELEN_64 && TARGET_ZVFH && TARGET_MIN_VLEN >= 4096")
])

(define_mode_iterator VQEXTI [
  RVVM8SI RVVM4SI RVVM2SI RVVM1SI (RVVMF2SI "TARGET_VECTOR_ELEN_64")

  (RVVM8DI "TARGET_VECTOR_ELEN_64") (RVVM4DI "TARGET_VECTOR_ELEN_64")
  (RVVM2DI "TARGET_VECTOR_ELEN_64") (RVVM1DI "TARGET_VECTOR_ELEN_64")

  (V1SI "riscv_vector::vls_mode_valid_p (V1SImode)")
  (V2SI "riscv_vector::vls_mode_valid_p (V2SImode)")
  (V4SI "riscv_vector::vls_mode_valid_p (V4SImode)")
  (V8SI "riscv_vector::vls_mode_valid_p (V8SImode)")
  (V16SI "riscv_vector::vls_mode_valid_p (V16SImode) && TARGET_MIN_VLEN >= 64")
  (V32SI "riscv_vector::vls_mode_valid_p (V32SImode) && TARGET_MIN_VLEN >= 128")
  (V64SI "riscv_vector::vls_mode_valid_p (V64SImode) && TARGET_MIN_VLEN >= 256")
  (V128SI "riscv_vector::vls_mode_valid_p (V128SImode) && TARGET_MIN_VLEN >= 512")
  (V256SI "riscv_vector::vls_mode_valid_p (V256SImode) && TARGET_MIN_VLEN >= 1024")
  (V512SI "riscv_vector::vls_mode_valid_p (V512SImode) && TARGET_MIN_VLEN >= 2048")
  (V1024SI "riscv_vector::vls_mode_valid_p (V1024SImode) && TARGET_MIN_VLEN >= 4096")
  (V1DI "riscv_vector::vls_mode_valid_p (V1DImode) && TARGET_VECTOR_ELEN_64")
  (V2DI "riscv_vector::vls_mode_valid_p (V2DImode) && TARGET_VECTOR_ELEN_64")
  (V4DI "riscv_vector::vls_mode_valid_p (V4DImode) && TARGET_VECTOR_ELEN_64")
  (V8DI "riscv_vector::vls_mode_valid_p (V8DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 64")
  (V16DI "riscv_vector::vls_mode_valid_p (V16DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 128")
  (V32DI "riscv_vector::vls_mode_valid_p (V32DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 256")
  (V64DI "riscv_vector::vls_mode_valid_p (V64DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 512")
  (V128DI "riscv_vector::vls_mode_valid_p (V128DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 1024")
  (V256DI "riscv_vector::vls_mode_valid_p (V256DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 2048")
  (V512DI "riscv_vector::vls_mode_valid_p (V512DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 4096")
])

(define_mode_iterator VQEXTF [
  (RVVM8DF "TARGET_VECTOR_ELEN_FP_64") (RVVM4DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM2DF "TARGET_VECTOR_ELEN_FP_64") (RVVM1DF "TARGET_VECTOR_ELEN_FP_64")

  (V1DF "riscv_vector::vls_mode_valid_p (V1DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V2DF "riscv_vector::vls_mode_valid_p (V2DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V4DF "riscv_vector::vls_mode_valid_p (V4DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V8DF "riscv_vector::vls_mode_valid_p (V8DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 64")
  (V16DF "riscv_vector::vls_mode_valid_p (V16DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 128")
  (V32DF "riscv_vector::vls_mode_valid_p (V32DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 256")
  (V64DF "riscv_vector::vls_mode_valid_p (V64DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 512")
  (V128DF "riscv_vector::vls_mode_valid_p (V128DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 1024")
  (V256DF "riscv_vector::vls_mode_valid_p (V256DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 2048")
  (V512DF "riscv_vector::vls_mode_valid_p (V512DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 4096")
])

(define_mode_iterator VOEXTI [
  (RVVM8DI "TARGET_VECTOR_ELEN_64") (RVVM4DI "TARGET_VECTOR_ELEN_64")
  (RVVM2DI "TARGET_VECTOR_ELEN_64") (RVVM1DI "TARGET_VECTOR_ELEN_64")

  (V1DI "riscv_vector::vls_mode_valid_p (V1DImode) && TARGET_VECTOR_ELEN_64")
  (V2DI "riscv_vector::vls_mode_valid_p (V2DImode) && TARGET_VECTOR_ELEN_64")
  (V4DI "riscv_vector::vls_mode_valid_p (V4DImode) && TARGET_VECTOR_ELEN_64")
  (V8DI "riscv_vector::vls_mode_valid_p (V8DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 64")
  (V16DI "riscv_vector::vls_mode_valid_p (V16DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 128")
  (V32DI "riscv_vector::vls_mode_valid_p (V32DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 256")
  (V64DI "riscv_vector::vls_mode_valid_p (V64DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 512")
  (V128DI "riscv_vector::vls_mode_valid_p (V128DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 1024")
  (V256DI "riscv_vector::vls_mode_valid_p (V256DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 2048")
  (V512DI "riscv_vector::vls_mode_valid_p (V512DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 4096")
])

(define_mode_iterator V1T [
  (RVVMF8x2QI "TARGET_VECTOR_ELEN_64")
  (RVVMF8x3QI "TARGET_VECTOR_ELEN_64")
  (RVVMF8x4QI "TARGET_VECTOR_ELEN_64")
  (RVVMF8x5QI "TARGET_VECTOR_ELEN_64")
  (RVVMF8x6QI "TARGET_VECTOR_ELEN_64")
  (RVVMF8x7QI "TARGET_VECTOR_ELEN_64")
  (RVVMF8x8QI "TARGET_VECTOR_ELEN_64")
  (RVVMF4x2HI "TARGET_VECTOR_ELEN_64")
  (RVVMF4x3HI "TARGET_VECTOR_ELEN_64")
  (RVVMF4x4HI "TARGET_VECTOR_ELEN_64")
  (RVVMF4x5HI "TARGET_VECTOR_ELEN_64")
  (RVVMF4x6HI "TARGET_VECTOR_ELEN_64")
  (RVVMF4x7HI "TARGET_VECTOR_ELEN_64")
  (RVVMF4x8HI "TARGET_VECTOR_ELEN_64")
  (RVVMF2x2SI "TARGET_VECTOR_ELEN_64")
  (RVVMF2x3SI "TARGET_VECTOR_ELEN_64")
  (RVVMF2x4SI "TARGET_VECTOR_ELEN_64")
  (RVVMF2x5SI "TARGET_VECTOR_ELEN_64")
  (RVVMF2x6SI "TARGET_VECTOR_ELEN_64")
  (RVVMF2x7SI "TARGET_VECTOR_ELEN_64")
  (RVVMF2x8SI "TARGET_VECTOR_ELEN_64")
  (RVVM1x2DI "TARGET_VECTOR_ELEN_64")
  (RVVM1x3DI "TARGET_VECTOR_ELEN_64")
  (RVVM1x4DI "TARGET_VECTOR_ELEN_64")
  (RVVM1x5DI "TARGET_VECTOR_ELEN_64")
  (RVVM1x6DI "TARGET_VECTOR_ELEN_64")
  (RVVM1x7DI "TARGET_VECTOR_ELEN_64")
  (RVVM1x8DI "TARGET_VECTOR_ELEN_64")
  (RVVMF4x2BF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_BF_16")
  (RVVMF4x3BF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_BF_16")
  (RVVMF4x4BF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_BF_16")
  (RVVMF4x5BF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_BF_16")
  (RVVMF4x6BF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_BF_16")
  (RVVMF4x7BF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_BF_16")
  (RVVMF4x8BF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_BF_16")
  (RVVMF4x2HF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_16")
  (RVVMF4x3HF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_16")
  (RVVMF4x4HF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_16")
  (RVVMF4x5HF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_16")
  (RVVMF4x6HF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_16")
  (RVVMF4x7HF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_16")
  (RVVMF4x8HF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_16")
  (RVVMF2x2SF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32")
  (RVVMF2x3SF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32")
  (RVVMF2x4SF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32")
  (RVVMF2x5SF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32")
  (RVVMF2x6SF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32")
  (RVVMF2x7SF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32")
  (RVVMF2x8SF "TARGET_VECTOR_ELEN_64 && TARGET_VECTOR_ELEN_FP_32")
  (RVVM1x2DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM1x3DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM1x4DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM1x5DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM1x6DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM1x7DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM1x8DF "TARGET_VECTOR_ELEN_FP_64")
])

(define_mode_iterator V2T [
  RVVMF4x2QI
  RVVMF4x3QI
  RVVMF4x4QI
  RVVMF4x5QI
  RVVMF4x6QI
  RVVMF4x7QI
  RVVMF4x8QI
  RVVMF2x2HI
  RVVMF2x3HI
  RVVMF2x4HI
  RVVMF2x5HI
  RVVMF2x6HI
  RVVMF2x7HI
  RVVMF2x8HI
  RVVM1x2SI
  RVVM1x3SI
  RVVM1x4SI
  RVVM1x5SI
  RVVM1x6SI
  RVVM1x7SI
  RVVM1x8SI
  (RVVM2x2DI "TARGET_VECTOR_ELEN_64")
  (RVVM2x3DI "TARGET_VECTOR_ELEN_64")
  (RVVM2x4DI "TARGET_VECTOR_ELEN_64")
  (RVVMF2x2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF2x3BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF2x4BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF2x5BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF2x6BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF2x7BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF2x8BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF2x2HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVMF2x3HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVMF2x4HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVMF2x5HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVMF2x6HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVMF2x7HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVMF2x8HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM1x2SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM1x3SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM1x4SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM1x5SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM1x6SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM1x7SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM1x8SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM2x2DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM2x3DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM2x4DF "TARGET_VECTOR_ELEN_FP_64")
])

(define_mode_iterator V4T [
  RVVMF2x2QI
  RVVMF2x3QI
  RVVMF2x4QI
  RVVMF2x5QI
  RVVMF2x6QI
  RVVMF2x7QI
  RVVMF2x8QI
  RVVM1x2HI
  RVVM1x3HI
  RVVM1x4HI
  RVVM1x5HI
  RVVM1x6HI
  RVVM1x7HI
  RVVM1x8HI
  RVVM2x2SI
  RVVM2x3SI
  RVVM2x4SI
  (RVVM4x2DI "TARGET_VECTOR_ELEN_64")
  (RVVM1x2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM1x3BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM1x4BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM1x5BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM1x6BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM1x7BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM1x8BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM1x2HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM1x3HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM1x4HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM1x5HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM1x6HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM1x7HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM1x8HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM2x2SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM2x3SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM2x4SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM4x2DF "TARGET_VECTOR_ELEN_FP_64")
])

(define_mode_iterator V8T [
  RVVM1x2QI
  RVVM1x3QI
  RVVM1x4QI
  RVVM1x5QI
  RVVM1x6QI
  RVVM1x7QI
  RVVM1x8QI
  RVVM2x2HI
  RVVM2x3HI
  RVVM2x4HI
  RVVM4x2SI
  (RVVM2x2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM2x3BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM2x4BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM2x2HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM2x3HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM2x4HF "TARGET_VECTOR_ELEN_FP_16")
  (RVVM4x2SF "TARGET_VECTOR_ELEN_FP_32")
])

(define_mode_iterator V16T [
  RVVM2x2QI
  RVVM2x3QI
  RVVM2x4QI
  RVVM4x2HI
  (RVVM4x2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM4x2HF "TARGET_VECTOR_ELEN_FP_16")
])

(define_mode_iterator V32T [
  RVVM4x2QI
])

(define_mode_attr V_LMUL1 [
  (RVVM8QI "RVVM1QI") (RVVM4QI "RVVM1QI") (RVVM2QI "RVVM1QI") (RVVM1QI "RVVM1QI") (RVVMF2QI "RVVM1QI") (RVVMF4QI "RVVM1QI") (RVVMF8QI "RVVM1QI")

  (RVVM8HI "RVVM1HI") (RVVM4HI "RVVM1HI") (RVVM2HI "RVVM1HI") (RVVM1HI "RVVM1HI") (RVVMF2HI "RVVM1HI") (RVVMF4HI "RVVM1HI")

  (RVVM8SI "RVVM1SI") (RVVM4SI "RVVM1SI") (RVVM2SI "RVVM1SI") (RVVM1SI "RVVM1SI") (RVVMF2SI "RVVM1SI")

  (RVVM8DI "RVVM1DI") (RVVM4DI "RVVM1DI") (RVVM2DI "RVVM1DI") (RVVM1DI "RVVM1DI")

  (RVVM8HF "RVVM1HF") (RVVM4HF "RVVM1HF") (RVVM2HF "RVVM1HF") (RVVM1HF "RVVM1HF") (RVVMF2HF "RVVM1HF") (RVVMF4HF "RVVM1HF")

  (RVVM8SF "RVVM1SF") (RVVM4SF "RVVM1SF") (RVVM2SF "RVVM1SF") (RVVM1SF "RVVM1SF") (RVVMF2SF "RVVM1SF")

  (RVVM8DF "RVVM1DF") (RVVM4DF "RVVM1DF") (RVVM2DF "RVVM1DF") (RVVM1DF "RVVM1DF")

  (V1QI "RVVM1QI")
  (V2QI "RVVM1QI")
  (V4QI "RVVM1QI")
  (V8QI "RVVM1QI")
  (V16QI "RVVM1QI")
  (V32QI "RVVM1QI")
  (V64QI "RVVM1QI")
  (V128QI "RVVM1QI")
  (V256QI "RVVM1QI")
  (V512QI "RVVM1QI")
  (V1024QI "RVVM1QI")
  (V2048QI "RVVM1QI")
  (V4096QI "RVVM1QI")
  (V1HI "RVVM1HI")
  (V2HI "RVVM1HI")
  (V4HI "RVVM1HI")
  (V8HI "RVVM1HI")
  (V16HI "RVVM1HI")
  (V32HI "RVVM1HI")
  (V64HI "RVVM1HI")
  (V128HI "RVVM1HI")
  (V256HI "RVVM1HI")
  (V512HI "RVVM1HI")
  (V1024HI "RVVM1HI")
  (V2048HI "RVVM1HI")
  (V1SI "RVVM1SI")
  (V2SI "RVVM1SI")
  (V4SI "RVVM1SI")
  (V8SI "RVVM1SI")
  (V16SI "RVVM1SI")
  (V32SI "RVVM1SI")
  (V64SI "RVVM1SI")
  (V128SI "RVVM1SI")
  (V256SI "RVVM1SI")
  (V512SI "RVVM1SI")
  (V1024SI "RVVM1SI")
  (V1DI "RVVM1DI")
  (V2DI "RVVM1DI")
  (V4DI "RVVM1DI")
  (V8DI "RVVM1DI")
  (V16DI "RVVM1DI")
  (V32DI "RVVM1DI")
  (V64DI "RVVM1DI")
  (V128DI "RVVM1DI")
  (V256DI "RVVM1DI")
  (V512DI "RVVM1DI")
  (V1HF "RVVM1HF")
  (V2HF "RVVM1HF")
  (V4HF "RVVM1HF")
  (V8HF "RVVM1HF")
  (V16HF "RVVM1HF")
  (V32HF "RVVM1HF")
  (V64HF "RVVM1HF")
  (V128HF "RVVM1HF")
  (V256HF "RVVM1HF")
  (V512HF "RVVM1HF")
  (V1024HF "RVVM1HF")
  (V2048HF "RVVM1HF")
  (V1SF "RVVM1SF")
  (V2SF "RVVM1SF")
  (V4SF "RVVM1SF")
  (V8SF "RVVM1SF")
  (V16SF "RVVM1SF")
  (V32SF "RVVM1SF")
  (V64SF "RVVM1SF")
  (V128SF "RVVM1SF")
  (V256SF "RVVM1SF")
  (V512SF "RVVM1SF")
  (V1024SF "RVVM1SF")
  (V1DF "RVVM1DF")
  (V2DF "RVVM1DF")
  (V4DF "RVVM1DF")
  (V8DF "RVVM1DF")
  (V16DF "RVVM1DF")
  (V32DF "RVVM1DF")
  (V64DF "RVVM1DF")
  (V128DF "RVVM1DF")
  (V256DF "RVVM1DF")
  (V512DF "RVVM1DF")
])

(define_mode_attr V_EXT_LMUL1 [
  (RVVM8QI "RVVM1HI") (RVVM4QI "RVVM1HI") (RVVM2QI "RVVM1HI") (RVVM1QI "RVVM1HI") (RVVMF2QI "RVVM1HI") (RVVMF4QI "RVVM1HI") (RVVMF8QI "RVVM1HI")

  (RVVM8HI "RVVM1SI") (RVVM4HI "RVVM1SI") (RVVM2HI "RVVM1SI") (RVVM1HI "RVVM1SI") (RVVMF2HI "RVVM1SI") (RVVMF4HI "RVVM1SI")

  (RVVM8SI "RVVM1DI") (RVVM4SI "RVVM1DI") (RVVM2SI "RVVM1DI") (RVVM1SI "RVVM1DI") (RVVMF2SI "RVVM1DI")

  (RVVM8HF "RVVM1SF") (RVVM4HF "RVVM1SF") (RVVM2HF "RVVM1SF") (RVVM1HF "RVVM1SF") (RVVMF2HF "RVVM1SF") (RVVMF4HF "RVVM1SF")

  (RVVM8SF "RVVM1DF") (RVVM4SF "RVVM1DF") (RVVM2SF "RVVM1DF") (RVVM1SF "RVVM1DF") (RVVMF2SF "RVVM1DF")

  (V1QI "RVVM1HI")
  (V2QI "RVVM1HI")
  (V4QI "RVVM1HI")
  (V8QI "RVVM1HI")
  (V16QI "RVVM1HI")
  (V32QI "RVVM1HI")
  (V64QI "RVVM1HI")
  (V128QI "RVVM1HI")
  (V256QI "RVVM1HI")
  (V512QI "RVVM1HI")
  (V1024QI "RVVM1HI")
  (V2048QI "RVVM1HI")
  (V4096QI "RVVM1HI")
  (V1HI "RVVM1SI")
  (V2HI "RVVM1SI")
  (V4HI "RVVM1SI")
  (V8HI "RVVM1SI")
  (V16HI "RVVM1SI")
  (V32HI "RVVM1SI")
  (V64HI "RVVM1SI")
  (V128HI "RVVM1SI")
  (V256HI "RVVM1SI")
  (V512HI "RVVM1SI")
  (V1024HI "RVVM1SI")
  (V2048HI "RVVM1SI")
  (V1SI "RVVM1DI")
  (V2SI "RVVM1DI")
  (V4SI "RVVM1DI")
  (V8SI "RVVM1DI")
  (V16SI "RVVM1DI")
  (V32SI "RVVM1DI")
  (V64SI "RVVM1DI")
  (V128SI "RVVM1DI")
  (V256SI "RVVM1DI")
  (V512SI "RVVM1DI")
  (V1024SI "RVVM1DI")
  (V1HF "RVVM1SF")
  (V2HF "RVVM1SF")
  (V4HF "RVVM1SF")
  (V8HF "RVVM1SF")
  (V16HF "RVVM1SF")
  (V32HF "RVVM1SF")
  (V64HF "RVVM1SF")
  (V128HF "RVVM1SF")
  (V256HF "RVVM1SF")
  (V512HF "RVVM1SF")
  (V1024HF "RVVM1SF")
  (V2048HF "RVVM1SF")
  (V1SF "RVVM1DF")
  (V2SF "RVVM1DF")
  (V4SF "RVVM1DF")
  (V8SF "RVVM1DF")
  (V16SF "RVVM1DF")
  (V32SF "RVVM1DF")
  (V64SF "RVVM1DF")
  (V128SF "RVVM1DF")
  (V256SF "RVVM1DF")
  (V512SF "RVVM1DF")
  (V1024SF "RVVM1DF")
])

(define_mode_iterator VLSB [
  (V1BI "riscv_vector::vls_mode_valid_p (V1BImode)")
  (V2BI "riscv_vector::vls_mode_valid_p (V2BImode)")
  (V4BI "riscv_vector::vls_mode_valid_p (V4BImode)")
  (V8BI "riscv_vector::vls_mode_valid_p (V8BImode)")
  (V16BI "riscv_vector::vls_mode_valid_p (V16BImode)")
  (V32BI "riscv_vector::vls_mode_valid_p (V32BImode)")
  (V64BI "riscv_vector::vls_mode_valid_p (V64BImode) && TARGET_MIN_VLEN >= 64")
  (V128BI "riscv_vector::vls_mode_valid_p (V128BImode) && TARGET_MIN_VLEN >= 128")
  (V256BI "riscv_vector::vls_mode_valid_p (V256BImode) && TARGET_MIN_VLEN >= 256")
  (V512BI "riscv_vector::vls_mode_valid_p (V512BImode) && TARGET_MIN_VLEN >= 512")
  (V1024BI "riscv_vector::vls_mode_valid_p (V1024BImode) && TARGET_MIN_VLEN >= 1024")
  (V2048BI "riscv_vector::vls_mode_valid_p (V2048BImode) && TARGET_MIN_VLEN >= 2048")
  (V4096BI "riscv_vector::vls_mode_valid_p (V4096BImode) && TARGET_MIN_VLEN >= 4096")])

(define_mode_iterator VB [
  (RVVMF64BI "TARGET_VECTOR_ELEN_64") RVVMF32BI RVVMF16BI RVVMF8BI RVVMF4BI RVVMF2BI RVVM1BI
])

;; Iterator for indexed loads and stores.  We must disallow 64-bit indices on
;; XLEN=32 targets.  TODO:  Split iterators so more of them can be reused, i.e.
;; VI8, VI16, VI32, VI64 and then use
;; VINDEXED [VI8 VI16 VI32 (VI64 "TARGET_64BIT")].

(define_mode_iterator VINDEXED [
  RVVM8QI RVVM4QI RVVM2QI RVVM1QI RVVMF2QI RVVMF4QI (RVVMF8QI "TARGET_VECTOR_ELEN_64")

  RVVM8HI RVVM4HI RVVM2HI RVVM1HI RVVMF2HI (RVVMF4HI "TARGET_VECTOR_ELEN_64")

  RVVM8SI RVVM4SI RVVM2SI RVVM1SI (RVVMF2SI "TARGET_VECTOR_ELEN_64")

  (RVVM8DI "TARGET_VECTOR_ELEN_64 && TARGET_64BIT")
  (RVVM4DI "TARGET_VECTOR_ELEN_64 && TARGET_64BIT")
  (RVVM2DI "TARGET_VECTOR_ELEN_64 && TARGET_64BIT")
  (RVVM1DI "TARGET_VECTOR_ELEN_64 && TARGET_64BIT")

  (RVVM8BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM4BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVM1BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF2BF "TARGET_VECTOR_ELEN_BF_16")
  (RVVMF4BF "TARGET_VECTOR_ELEN_BF_16 && TARGET_VECTOR_ELEN_64")

  (RVVM8HF "TARGET_ZVFH") (RVVM4HF "TARGET_ZVFH") (RVVM2HF "TARGET_ZVFH")
  (RVVM1HF "TARGET_ZVFH") (RVVMF2HF "TARGET_ZVFH")
  (RVVMF4HF "TARGET_ZVFH && TARGET_VECTOR_ELEN_64")

  (RVVM8SF "TARGET_VECTOR_ELEN_FP_32") (RVVM4SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM2SF "TARGET_VECTOR_ELEN_FP_32") (RVVM1SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVMF2SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_VECTOR_ELEN_64")

  (RVVM8DF "TARGET_VECTOR_ELEN_FP_64 && TARGET_64BIT")
  (RVVM4DF "TARGET_VECTOR_ELEN_FP_64 && TARGET_64BIT")
  (RVVM2DF "TARGET_VECTOR_ELEN_FP_64 && TARGET_64BIT")
  (RVVM1DF "TARGET_VECTOR_ELEN_FP_64 && TARGET_64BIT")

  (V1QI "riscv_vector::vls_mode_valid_p (V1QImode)")
  (V2QI "riscv_vector::vls_mode_valid_p (V2QImode)")
  (V4QI "riscv_vector::vls_mode_valid_p (V4QImode)")
  (V8QI "riscv_vector::vls_mode_valid_p (V8QImode)")
  (V16QI "riscv_vector::vls_mode_valid_p (V16QImode)")
  (V32QI "riscv_vector::vls_mode_valid_p (V32QImode)")
  (V64QI "riscv_vector::vls_mode_valid_p (V64QImode) && TARGET_MIN_VLEN >= 64")
  (V128QI "riscv_vector::vls_mode_valid_p (V128QImode) && TARGET_MIN_VLEN >= 128")
  (V256QI "riscv_vector::vls_mode_valid_p (V256QImode) && TARGET_MIN_VLEN >= 256")
  (V512QI "riscv_vector::vls_mode_valid_p (V512QImode) && TARGET_MIN_VLEN >= 512")
  (V1024QI "riscv_vector::vls_mode_valid_p (V1024QImode) && TARGET_MIN_VLEN >= 1024")
  (V2048QI "riscv_vector::vls_mode_valid_p (V2048QImode) && TARGET_MIN_VLEN >= 2048")
  (V4096QI "riscv_vector::vls_mode_valid_p (V4096QImode) && TARGET_MIN_VLEN >= 4096")
  (V1HI "riscv_vector::vls_mode_valid_p (V1HImode)")
  (V2HI "riscv_vector::vls_mode_valid_p (V2HImode)")
  (V4HI "riscv_vector::vls_mode_valid_p (V4HImode)")
  (V8HI "riscv_vector::vls_mode_valid_p (V8HImode)")
  (V16HI "riscv_vector::vls_mode_valid_p (V16HImode)")
  (V32HI "riscv_vector::vls_mode_valid_p (V32HImode) && TARGET_MIN_VLEN >= 64")
  (V64HI "riscv_vector::vls_mode_valid_p (V64HImode) && TARGET_MIN_VLEN >= 128")
  (V128HI "riscv_vector::vls_mode_valid_p (V128HImode) && TARGET_MIN_VLEN >= 256")
  (V256HI "riscv_vector::vls_mode_valid_p (V256HImode) && TARGET_MIN_VLEN >= 512")
  (V512HI "riscv_vector::vls_mode_valid_p (V512HImode) && TARGET_MIN_VLEN >= 1024")
  (V1024HI "riscv_vector::vls_mode_valid_p (V1024HImode) && TARGET_MIN_VLEN >= 2048")
  (V2048HI "riscv_vector::vls_mode_valid_p (V2048HImode) && TARGET_MIN_VLEN >= 4096")
  (V1SI "riscv_vector::vls_mode_valid_p (V1SImode)")
  (V2SI "riscv_vector::vls_mode_valid_p (V2SImode)")
  (V4SI "riscv_vector::vls_mode_valid_p (V4SImode)")
  (V8SI "riscv_vector::vls_mode_valid_p (V8SImode)")
  (V16SI "riscv_vector::vls_mode_valid_p (V16SImode) && TARGET_MIN_VLEN >= 64")
  (V32SI "riscv_vector::vls_mode_valid_p (V32SImode) && TARGET_MIN_VLEN >= 128")
  (V64SI "riscv_vector::vls_mode_valid_p (V64SImode) && TARGET_MIN_VLEN >= 256")
  (V128SI "riscv_vector::vls_mode_valid_p (V128SImode) && TARGET_MIN_VLEN >= 512")
  (V256SI "riscv_vector::vls_mode_valid_p (V256SImode) && TARGET_MIN_VLEN >= 1024")
  (V512SI "riscv_vector::vls_mode_valid_p (V512SImode) && TARGET_MIN_VLEN >= 2048")
  (V1024SI "riscv_vector::vls_mode_valid_p (V1024SImode) && TARGET_MIN_VLEN >= 4096")
  (V1DI "riscv_vector::vls_mode_valid_p (V1DImode) && TARGET_VECTOR_ELEN_64 && TARGET_64BIT")
  (V2DI "riscv_vector::vls_mode_valid_p (V2DImode) && TARGET_VECTOR_ELEN_64 && TARGET_64BIT")
  (V4DI "riscv_vector::vls_mode_valid_p (V4DImode) && TARGET_VECTOR_ELEN_64 && TARGET_64BIT")
  (V8DI "riscv_vector::vls_mode_valid_p (V8DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 64 && TARGET_64BIT")
  (V16DI "riscv_vector::vls_mode_valid_p (V16DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 128 && TARGET_64BIT")
  (V32DI "riscv_vector::vls_mode_valid_p (V32DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 256 && TARGET_64BIT")
  (V64DI "riscv_vector::vls_mode_valid_p (V64DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 512 && TARGET_64BIT")
  (V128DI "riscv_vector::vls_mode_valid_p (V128DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 1024 && TARGET_64BIT")
  (V256DI "riscv_vector::vls_mode_valid_p (V256DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 2048 && TARGET_64BIT")
  (V512DI "riscv_vector::vls_mode_valid_p (V512DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 4096 && TARGET_64BIT")

  (V1HF "riscv_vector::vls_mode_valid_p (V1HFmode) && TARGET_ZVFH")
  (V2HF "riscv_vector::vls_mode_valid_p (V2HFmode) && TARGET_ZVFH")
  (V4HF "riscv_vector::vls_mode_valid_p (V4HFmode) && TARGET_ZVFH")
  (V8HF "riscv_vector::vls_mode_valid_p (V8HFmode) && TARGET_ZVFH")
  (V16HF "riscv_vector::vls_mode_valid_p (V16HFmode) && TARGET_ZVFH")
  (V32HF "riscv_vector::vls_mode_valid_p (V32HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 64")
  (V64HF "riscv_vector::vls_mode_valid_p (V64HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 128")
  (V128HF "riscv_vector::vls_mode_valid_p (V128HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 256")
  (V256HF "riscv_vector::vls_mode_valid_p (V256HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 512")
  (V512HF "riscv_vector::vls_mode_valid_p (V512HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 1024")
  (V1024HF "riscv_vector::vls_mode_valid_p (V1024HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 2048")
  (V2048HF "riscv_vector::vls_mode_valid_p (V2048HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 4096")
  (V1SF "riscv_vector::vls_mode_valid_p (V1SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V2SF "riscv_vector::vls_mode_valid_p (V2SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V4SF "riscv_vector::vls_mode_valid_p (V4SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V8SF "riscv_vector::vls_mode_valid_p (V8SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V16SF "riscv_vector::vls_mode_valid_p (V16SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 64")
  (V32SF "riscv_vector::vls_mode_valid_p (V32SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 128")
  (V64SF "riscv_vector::vls_mode_valid_p (V64SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 256")
  (V128SF "riscv_vector::vls_mode_valid_p (V128SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 512")
  (V256SF "riscv_vector::vls_mode_valid_p (V256SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 1024")
  (V512SF "riscv_vector::vls_mode_valid_p (V512SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 2048")
  (V1024SF "riscv_vector::vls_mode_valid_p (V1024SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 4096")
  (V1DF "riscv_vector::vls_mode_valid_p (V1DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_64BIT")
  (V2DF "riscv_vector::vls_mode_valid_p (V2DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_64BIT")
  (V4DF "riscv_vector::vls_mode_valid_p (V4DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_64BIT")
  (V8DF "riscv_vector::vls_mode_valid_p (V8DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 64 && TARGET_64BIT")
  (V16DF "riscv_vector::vls_mode_valid_p (V16DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 128 && TARGET_64BIT")
  (V32DF "riscv_vector::vls_mode_valid_p (V32DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 256 && TARGET_64BIT")
  (V64DF "riscv_vector::vls_mode_valid_p (V64DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 512 && TARGET_64BIT")
  (V128DF "riscv_vector::vls_mode_valid_p (V128DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 1024 && TARGET_64BIT")
  (V256DF "riscv_vector::vls_mode_valid_p (V256DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 2048 && TARGET_64BIT")
  (V512DF "riscv_vector::vls_mode_valid_p (V512DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 4096 && TARGET_64BIT")
])

(define_mode_iterator VB_VLS [VB VLSB])

(define_mode_iterator VLS [VLSI VLSF_ZVFHMIN])

(define_mode_iterator VLS_ZVFH [VLSI VLSF])

(define_mode_iterator V [VI VF_ZVFBF16 VF_ZVFHMIN])

(define_mode_iterator V_ZVFH [VI VF])

(define_mode_iterator V_VLS [V VLS])

(define_mode_iterator V_VLS_ZVFH [V_ZVFH VLS_ZVFH])

(define_mode_iterator V_VLSI [VI VLSI])

(define_mode_iterator V_VLSF [VF VLSF])

(define_mode_iterator V_VLSF_ZVFHMIN [VF_ZVFBF16 VF_ZVFHMIN VLSF_ZVFHMIN])

(define_mode_iterator VT [V1T V2T V4T V8T V16T V32T])

(define_int_iterator ANY_REDUC [
  UNSPEC_REDUC_SUM UNSPEC_REDUC_MAXU UNSPEC_REDUC_MAX UNSPEC_REDUC_MINU
  UNSPEC_REDUC_MIN UNSPEC_REDUC_AND UNSPEC_REDUC_OR UNSPEC_REDUC_XOR
])

(define_int_iterator ANY_REDUC_VL0_SAFE [
  UNSPEC_REDUC_SUM_VL0_SAFE UNSPEC_REDUC_MAXU_VL0_SAFE UNSPEC_REDUC_MAX_VL0_SAFE UNSPEC_REDUC_MINU_VL0_SAFE
  UNSPEC_REDUC_MIN_VL0_SAFE UNSPEC_REDUC_AND_VL0_SAFE UNSPEC_REDUC_OR_VL0_SAFE UNSPEC_REDUC_XOR_VL0_SAFE
])

(define_int_iterator ANY_WREDUC [
  UNSPEC_WREDUC_SUM UNSPEC_WREDUC_SUMU
])

(define_int_iterator ANY_WREDUC_VL0_SAFE [
  UNSPEC_WREDUC_SUM_VL0_SAFE UNSPEC_WREDUC_SUMU_VL0_SAFE
])

(define_int_iterator ANY_FREDUC [
  UNSPEC_REDUC_MAX UNSPEC_REDUC_MIN
])

(define_int_iterator ANY_FREDUC_VL0_SAFE [
  UNSPEC_REDUC_MAX_VL0_SAFE UNSPEC_REDUC_MIN_VL0_SAFE
])

(define_int_iterator ANY_FREDUC_SUM [
  UNSPEC_REDUC_SUM_ORDERED UNSPEC_REDUC_SUM_UNORDERED
])

(define_int_iterator ANY_FREDUC_SUM_VL0_SAFE [
  UNSPEC_REDUC_SUM_ORDERED_VL0_SAFE UNSPEC_REDUC_SUM_UNORDERED_VL0_SAFE
])

(define_int_iterator ANY_FWREDUC_SUM [
  UNSPEC_WREDUC_SUM_ORDERED UNSPEC_WREDUC_SUM_UNORDERED
])

(define_int_iterator ANY_FWREDUC_SUM_VL0_SAFE [
  UNSPEC_WREDUC_SUM_ORDERED_VL0_SAFE UNSPEC_WREDUC_SUM_UNORDERED_VL0_SAFE
])

(define_int_attr reduc_op_pat_name [
  (UNSPEC_REDUC_SUM "redsum")
  (UNSPEC_REDUC_SUM_VL0_SAFE "redsum_vl0s")
  (UNSPEC_REDUC_SUM_ORDERED "redosum") (UNSPEC_REDUC_SUM_UNORDERED "redusum")
  (UNSPEC_REDUC_SUM_ORDERED_VL0_SAFE "redosum_vl0s") (UNSPEC_REDUC_SUM_UNORDERED_VL0_SAFE "redusum_vl0s")
  (UNSPEC_REDUC_MAXU "redmaxu") (UNSPEC_REDUC_MAX "redmax") (UNSPEC_REDUC_MINU "redminu") (UNSPEC_REDUC_MIN "redmin")
  (UNSPEC_REDUC_MAXU_VL0_SAFE "redmaxu_vl0s") (UNSPEC_REDUC_MAX_VL0_SAFE "redmax_vl0s") (UNSPEC_REDUC_MINU_VL0_SAFE "redminu_vl0s") (UNSPEC_REDUC_MIN_VL0_SAFE "redmin_vl0s")
  (UNSPEC_REDUC_AND "redand") (UNSPEC_REDUC_OR "redor") (UNSPEC_REDUC_XOR "redxor")
  (UNSPEC_REDUC_AND_VL0_SAFE "redand_vl0s") (UNSPEC_REDUC_OR_VL0_SAFE "redor_vl0s") (UNSPEC_REDUC_XOR_VL0_SAFE "redxor_vl0s")
  (UNSPEC_WREDUC_SUM "wredsum") (UNSPEC_WREDUC_SUMU "wredsumu")
  (UNSPEC_WREDUC_SUM_VL0_SAFE "wredsum_vl0s") (UNSPEC_WREDUC_SUMU_VL0_SAFE "wredsumu_vl0s")
  (UNSPEC_WREDUC_SUM_ORDERED "wredosum") (UNSPEC_WREDUC_SUM_UNORDERED "wredusum")
  (UNSPEC_WREDUC_SUM_ORDERED_VL0_SAFE "wredosum_vl0s") (UNSPEC_WREDUC_SUM_UNORDERED_VL0_SAFE "wredusum_vl0s")
])

(define_int_attr reduc_op [
  (UNSPEC_REDUC_SUM "redsum")
  (UNSPEC_REDUC_SUM_VL0_SAFE "redsum")
  (UNSPEC_REDUC_SUM_ORDERED "redosum") (UNSPEC_REDUC_SUM_UNORDERED "redusum")
  (UNSPEC_REDUC_SUM_ORDERED_VL0_SAFE "redosum") (UNSPEC_REDUC_SUM_UNORDERED_VL0_SAFE "redusum")
  (UNSPEC_REDUC_MAXU "redmaxu") (UNSPEC_REDUC_MAX "redmax") (UNSPEC_REDUC_MINU "redminu") (UNSPEC_REDUC_MIN "redmin")
  (UNSPEC_REDUC_MAXU_VL0_SAFE "redmaxu") (UNSPEC_REDUC_MAX_VL0_SAFE "redmax") (UNSPEC_REDUC_MINU_VL0_SAFE "redminu") (UNSPEC_REDUC_MIN_VL0_SAFE "redmin")
  (UNSPEC_REDUC_AND "redand") (UNSPEC_REDUC_OR "redor") (UNSPEC_REDUC_XOR "redxor")
  (UNSPEC_REDUC_AND_VL0_SAFE "redand") (UNSPEC_REDUC_OR_VL0_SAFE "redor") (UNSPEC_REDUC_XOR_VL0_SAFE "redxor")
  (UNSPEC_WREDUC_SUM "wredsum") (UNSPEC_WREDUC_SUMU "wredsumu")
  (UNSPEC_WREDUC_SUM_VL0_SAFE "wredsum") (UNSPEC_WREDUC_SUMU_VL0_SAFE "wredsumu")
  (UNSPEC_WREDUC_SUM_ORDERED "wredosum") (UNSPEC_WREDUC_SUM_UNORDERED "wredusum")
  (UNSPEC_WREDUC_SUM_ORDERED_VL0_SAFE "wredosum") (UNSPEC_WREDUC_SUM_UNORDERED_VL0_SAFE "wredusum")
])

(define_code_attr WREDUC_UNSPEC [(zero_extend "UNSPEC_WREDUC_SUMU") (sign_extend "UNSPEC_WREDUC_SUM")])
(define_code_attr WREDUC_UNSPEC_VL0_SAFE [(zero_extend "UNSPEC_WREDUC_SUMU_VL0_SAFE") (sign_extend "UNSPEC_WREDUC_SUM_VL0_SAFE")])

(define_mode_attr VINDEX [
  (RVVM8QI "RVVM8QI") (RVVM4QI "RVVM4QI") (RVVM2QI "RVVM2QI") (RVVM1QI "RVVM1QI")
  (RVVMF2QI "RVVMF2QI") (RVVMF4QI "RVVMF4QI") (RVVMF8QI "RVVMF8QI")

  (RVVM8HI "RVVM8HI") (RVVM4HI "RVVM4HI") (RVVM2HI "RVVM2HI") (RVVM1HI "RVVM1HI") (RVVMF2HI "RVVMF2HI") (RVVMF4HI "RVVMF4HI")

  (RVVM8BF "RVVM8HI") (RVVM4BF "RVVM4HI") (RVVM2BF "RVVM2HI") (RVVM1BF "RVVM1HI") (RVVMF2BF "RVVMF2HI") (RVVMF4BF "RVVMF4HI")

  (RVVM8HF "RVVM8HI") (RVVM4HF "RVVM4HI") (RVVM2HF "RVVM2HI") (RVVM1HF "RVVM1HI") (RVVMF2HF "RVVMF2HI") (RVVMF4HF "RVVMF4HI")

  (RVVM8SI "RVVM8SI") (RVVM4SI "RVVM4SI") (RVVM2SI "RVVM2SI") (RVVM1SI "RVVM1SI") (RVVMF2SI "RVVMF2SI")

  (RVVM8SF "RVVM8SI") (RVVM4SF "RVVM4SI") (RVVM2SF "RVVM2SI") (RVVM1SF "RVVM1SI") (RVVMF2SF "RVVMF2SI")

  (RVVM8DI "RVVM8DI") (RVVM4DI "RVVM4DI") (RVVM2DI "RVVM2DI") (RVVM1DI "RVVM1DI")

  (RVVM8DF "RVVM8DI") (RVVM4DF "RVVM4DI") (RVVM2DF "RVVM2DI") (RVVM1DF "RVVM1DI")

  (V1QI "V1QI")
  (V2QI "V2QI")
  (V4QI "V4QI")
  (V8QI "V8QI")
  (V16QI "V16QI")
  (V32QI "V32QI")
  (V64QI "V64QI")
  (V128QI "V128QI")
  (V256QI "V256QI")
  (V512QI "V512QI")
  (V1024QI "V1024QI")
  (V2048QI "V2048QI")
  (V4096QI "V4096QI")
  (V1HI "V1HI")
  (V2HI "V2HI")
  (V4HI "V4HI")
  (V8HI "V8HI")
  (V16HI "V16HI")
  (V32HI "V32HI")
  (V64HI "V64HI")
  (V128HI "V128HI")
  (V256HI "V256HI")
  (V512HI "V512HI")
  (V1024HI "V1024HI")
  (V2048HI "V2048HI")
  (V1SI "V1SI")
  (V2SI "V2SI")
  (V4SI "V4SI")
  (V8SI "V8SI")
  (V16SI "V16SI")
  (V32SI "V32SI")
  (V64SI "V64SI")
  (V128SI "V128SI")
  (V256SI "V256SI")
  (V512SI "V512SI")
  (V1024SI "V1024SI")
  (V1DI "V1DI")
  (V2DI "V2DI")
  (V4DI "V4DI")
  (V8DI "V8DI")
  (V16DI "V16DI")
  (V32DI "V32DI")
  (V64DI "V64DI")
  (V128DI "V128DI")
  (V256DI "V256DI")
  (V512DI "V512DI")
  (V1HF "V1HI")
  (V2HF "V2HI")
  (V4HF "V4HI")
  (V8HF "V8HI")
  (V16HF "V16HI")
  (V32HF "V32HI")
  (V64HF "V64HI")
  (V128HF "V128HI")
  (V256HF "V256HI")
  (V512HF "V512HI")
  (V1024HF "V1024HI")
  (V2048HF "V2048HI")
  (V1SF "V1SI")
  (V2SF "V2SI")
  (V4SF "V4SI")
  (V8SF "V8SI")
  (V16SF "V16SI")
  (V32SF "V32SI")
  (V64SF "V64SI")
  (V128SF "V128SI")
  (V256SF "V256SI")
  (V512SF "V512SI")
  (V1024SF "V1024SI")
  (V1DF "V1DI")
  (V2DF "V2DI")
  (V4DF "V4DI")
  (V8DF "V8DI")
  (V16DF "V16DI")
  (V32DF "V32DI")
  (V64DF "V64DI")
  (V128DF "V128DI")
  (V256DF "V256DI")
  (V512DF "V512DI")
])

(define_mode_attr VINDEXEI16 [
  (RVVM4QI "RVVM8HI") (RVVM2QI "RVVM4HI") (RVVM1QI "RVVM2HI") (RVVMF2QI "RVVM1HI") (RVVMF4QI "RVVMF2HI") (RVVMF8QI "RVVMF4HI")

  (RVVM8HI "RVVM8HI") (RVVM4HI "RVVM4HI") (RVVM2HI "RVVM2HI") (RVVM1HI "RVVM1HI") (RVVMF2HI "RVVMF2HI") (RVVMF4HI "RVVMF4HI")

  (RVVM8SI "RVVM4HI") (RVVM4SI "RVVM2HI") (RVVM2SI "RVVM1HI") (RVVM1SI "RVVMF2HI") (RVVMF2SI "RVVMF4HI")

  (RVVM8DI "RVVM2HI") (RVVM4DI "RVVM1HI") (RVVM2DI "RVVMF2HI") (RVVM1DI "RVVMF4HI")

  (RVVM8HF "RVVM8HI") (RVVM4HF "RVVM4HI") (RVVM2HF "RVVM2HI") (RVVM1HF "RVVM1HI") (RVVMF2HF "RVVMF2HI") (RVVMF4HF "RVVMF4HI")

  (RVVM8SF "RVVM4HI") (RVVM4SF "RVVM2HI") (RVVM2SF "RVVM1HI") (RVVM1SF "RVVMF2HI") (RVVMF2SF "RVVMF4HI")

  (RVVM8DF "RVVM2HI") (RVVM4DF "RVVM1HI") (RVVM2DF "RVVMF2HI") (RVVM1DF "RVVMF4HI")

  (V1QI "V1HI")
  (V2QI "V2HI")
  (V4QI "V4HI")
  (V8QI "V8HI")
  (V16QI "V16HI")
  (V32QI "V32HI")
  (V64QI "V64HI")
  (V128QI "V128HI")
  (V256QI "V256HI")
  (V512QI "V512HI")
  (V1024QI "V1024HI")
  (V2048QI "V2048HI")
  (V1HI "V1HI")
  (V2HI "V2HI")
  (V4HI "V4HI")
  (V8HI "V8HI")
  (V16HI "V16HI")
  (V32HI "V32HI")
  (V64HI "V64HI")
  (V128HI "V128HI")
  (V256HI "V256HI")
  (V512HI "V512HI")
  (V1024HI "V1024HI")
  (V2048HI "V2048HI")
  (V1SI "V1HI")
  (V2SI "V2HI")
  (V4SI "V4HI")
  (V8SI "V8HI")
  (V16SI "V16HI")
  (V32SI "V32HI")
  (V64SI "V64HI")
  (V128SI "V128HI")
  (V256SI "V256HI")
  (V512SI "V512HI")
  (V1024SI "V1024HI")
  (V1DI "V1HI")
  (V2DI "V2HI")
  (V4DI "V4HI")
  (V8DI "V8HI")
  (V16DI "V16HI")
  (V32DI "V32HI")
  (V64DI "V64HI")
  (V128DI "V128HI")
  (V256DI "V256HI")
  (V512DI "V512HI")
  (V1HF "V1HI")
  (V2HF "V2HI")
  (V4HF "V4HI")
  (V8HF "V8HI")
  (V16HF "V16HF")
  (V32HF "V32HI")
  (V64HF "V64HI")
  (V128HF "V128HI")
  (V256HF "V256HI")
  (V512HF "V512HI")
  (V1024HF "V1024HI")
  (V2048HF "V2048HI")
  (V1SF "V1HI")
  (V2SF "V2HI")
  (V4SF "V4HI")
  (V8SF "V8HI")
  (V16SF "V16HI")
  (V32SF "V32HI")
  (V64SF "V64HI")
  (V128SF "V128HI")
  (V256SF "V256HI")
  (V512SF "V512HI")
  (V1024SF "V1024HI")
  (V1DF "V1HI")
  (V2DF "V2HI")
  (V4DF "V4HI")
  (V8DF "V8HI")
  (V16DF "V16HI")
  (V32DF "V32HI")
  (V64DF "V64HI")
  (V128DF "V128HI")
  (V256DF "V256HI")
  (V512DF "V512HI")
])

(define_mode_attr VM [
  (RVVM8QI "RVVM1BI") (RVVM4QI "RVVMF2BI") (RVVM2QI "RVVMF4BI") (RVVM1QI "RVVMF8BI") (RVVMF2QI "RVVMF16BI") (RVVMF4QI "RVVMF32BI") (RVVMF8QI "RVVMF64BI")

  (RVVM8HI "RVVMF2BI") (RVVM4HI "RVVMF4BI") (RVVM2HI "RVVMF8BI") (RVVM1HI "RVVMF16BI") (RVVMF2HI "RVVMF32BI") (RVVMF4HI "RVVMF64BI")

  (RVVM8BF "RVVMF2BI") (RVVM4BF "RVVMF4BI") (RVVM2BF "RVVMF8BI") (RVVM1BF "RVVMF16BI") (RVVMF2BF "RVVMF32BI") (RVVMF4BF "RVVMF64BI")

  (RVVM8HF "RVVMF2BI") (RVVM4HF "RVVMF4BI") (RVVM2HF "RVVMF8BI") (RVVM1HF "RVVMF16BI") (RVVMF2HF "RVVMF32BI") (RVVMF4HF "RVVMF64BI")

  (RVVM8SI "RVVMF4BI") (RVVM4SI "RVVMF8BI") (RVVM2SI "RVVMF16BI") (RVVM1SI "RVVMF32BI") (RVVMF2SI "RVVMF64BI")

  (RVVM8SF "RVVMF4BI") (RVVM4SF "RVVMF8BI") (RVVM2SF "RVVMF16BI") (RVVM1SF "RVVMF32BI") (RVVMF2SF "RVVMF64BI")

  (RVVM8DI "RVVMF8BI") (RVVM4DI "RVVMF16BI") (RVVM2DI "RVVMF32BI") (RVVM1DI "RVVMF64BI")

  (RVVM8DF "RVVMF8BI") (RVVM4DF "RVVMF16BI") (RVVM2DF "RVVMF32BI") (RVVM1DF "RVVMF64BI")

  (RVVM1x8QI "RVVMF8BI") (RVVMF2x8QI "RVVMF16BI") (RVVMF4x8QI "RVVMF32BI") (RVVMF8x8QI "RVVMF64BI")
  (RVVM1x7QI "RVVMF8BI") (RVVMF2x7QI "RVVMF16BI") (RVVMF4x7QI "RVVMF32BI") (RVVMF8x7QI "RVVMF64BI")
  (RVVM1x6QI "RVVMF8BI") (RVVMF2x6QI "RVVMF16BI") (RVVMF4x6QI "RVVMF32BI") (RVVMF8x6QI "RVVMF64BI")
  (RVVM1x5QI "RVVMF8BI") (RVVMF2x5QI "RVVMF16BI") (RVVMF4x5QI "RVVMF32BI") (RVVMF8x5QI "RVVMF64BI")
  (RVVM2x4QI "RVVMF4BI") (RVVM1x4QI "RVVMF8BI") (RVVMF2x4QI "RVVMF16BI") (RVVMF4x4QI "RVVMF32BI") (RVVMF8x4QI "RVVMF64BI")
  (RVVM2x3QI "RVVMF4BI") (RVVM1x3QI "RVVMF8BI") (RVVMF2x3QI "RVVMF16BI") (RVVMF4x3QI "RVVMF32BI") (RVVMF8x3QI "RVVMF64BI")
  (RVVM4x2QI "RVVMF2BI") (RVVM2x2QI "RVVMF4BI") (RVVM1x2QI "RVVMF8BI") (RVVMF2x2QI "RVVMF16BI") (RVVMF4x2QI "RVVMF32BI") (RVVMF8x2QI "RVVMF64BI")

  (RVVM1x8HI "RVVMF16BI") (RVVMF2x8HI "RVVMF32BI") (RVVMF4x8HI "RVVMF64BI")
  (RVVM1x7HI "RVVMF16BI") (RVVMF2x7HI "RVVMF32BI") (RVVMF4x7HI "RVVMF64BI")
  (RVVM1x6HI "RVVMF16BI") (RVVMF2x6HI "RVVMF32BI") (RVVMF4x6HI "RVVMF64BI")
  (RVVM1x5HI "RVVMF16BI") (RVVMF2x5HI "RVVMF32BI") (RVVMF4x5HI "RVVMF64BI")
  (RVVM2x4HI "RVVMF8BI") (RVVM1x4HI "RVVMF16BI") (RVVMF2x4HI "RVVMF32BI") (RVVMF4x4HI "RVVMF64BI")
  (RVVM2x3HI "RVVMF8BI") (RVVM1x3HI "RVVMF16BI") (RVVMF2x3HI "RVVMF32BI") (RVVMF4x3HI "RVVMF64BI")
  (RVVM4x2HI "RVVMF4BI") (RVVM2x2HI "RVVMF8BI") (RVVM1x2HI "RVVMF16BI") (RVVMF2x2HI "RVVMF32BI") (RVVMF4x2HI "RVVMF64BI")

  (RVVM1x8BF "RVVMF16BI") (RVVMF2x8BF "RVVMF32BI") (RVVMF4x8BF "RVVMF64BI")
  (RVVM1x7BF "RVVMF16BI") (RVVMF2x7BF "RVVMF32BI") (RVVMF4x7BF "RVVMF64BI")
  (RVVM1x6BF "RVVMF16BI") (RVVMF2x6BF "RVVMF32BI") (RVVMF4x6BF "RVVMF64BI")
  (RVVM1x5BF "RVVMF16BI") (RVVMF2x5BF "RVVMF32BI") (RVVMF4x5BF "RVVMF64BI")
  (RVVM2x4BF "RVVMF8BI") (RVVM1x4BF "RVVMF16BI") (RVVMF2x4BF "RVVMF32BI") (RVVMF4x4BF "RVVMF64BI")
  (RVVM2x3BF "RVVMF8BI") (RVVM1x3BF "RVVMF16BI") (RVVMF2x3BF "RVVMF32BI") (RVVMF4x3BF "RVVMF64BI")
  (RVVM4x2BF "RVVMF4BI") (RVVM2x2BF "RVVMF8BI") (RVVM1x2BF "RVVMF16BI") (RVVMF2x2BF "RVVMF32BI") (RVVMF4x2BF "RVVMF64BI")

  (RVVM1x8HF "RVVMF16BI") (RVVMF2x8HF "RVVMF32BI") (RVVMF4x8HF "RVVMF64BI")
  (RVVM1x7HF "RVVMF16BI") (RVVMF2x7HF "RVVMF32BI") (RVVMF4x7HF "RVVMF64BI")
  (RVVM1x6HF "RVVMF16BI") (RVVMF2x6HF "RVVMF32BI") (RVVMF4x6HF "RVVMF64BI")
  (RVVM1x5HF "RVVMF16BI") (RVVMF2x5HF "RVVMF32BI") (RVVMF4x5HF "RVVMF64BI")
  (RVVM2x4HF "RVVMF8BI") (RVVM1x4HF "RVVMF16BI") (RVVMF2x4HF "RVVMF32BI") (RVVMF4x4HF "RVVMF64BI")
  (RVVM2x3HF "RVVMF8BI") (RVVM1x3HF "RVVMF16BI") (RVVMF2x3HF "RVVMF32BI") (RVVMF4x3HF "RVVMF64BI")
  (RVVM4x2HF "RVVMF4BI") (RVVM2x2HF "RVVMF8BI") (RVVM1x2HF "RVVMF16BI") (RVVMF2x2HF "RVVMF32BI") (RVVMF4x2HF "RVVMF64BI")

  (RVVM1x8SI "RVVMF32BI") (RVVMF2x8SI "RVVMF64BI")
  (RVVM1x7SI "RVVMF32BI") (RVVMF2x7SI "RVVMF64BI")
  (RVVM1x6SI "RVVMF32BI") (RVVMF2x6SI "RVVMF64BI")
  (RVVM1x5SI "RVVMF32BI") (RVVMF2x5SI "RVVMF64BI")
  (RVVM2x4SI "RVVMF16BI") (RVVM1x4SI "RVVMF32BI") (RVVMF2x4SI "RVVMF64BI")
  (RVVM2x3SI "RVVMF16BI") (RVVM1x3SI "RVVMF32BI") (RVVMF2x3SI "RVVMF64BI")
  (RVVM4x2SI "RVVMF8BI") (RVVM2x2SI "RVVMF16BI") (RVVM1x2SI "RVVMF32BI") (RVVMF2x2SI "RVVMF64BI")

  (RVVM1x8SF "RVVMF32BI") (RVVMF2x8SF "RVVMF64BI")
  (RVVM1x7SF "RVVMF32BI") (RVVMF2x7SF "RVVMF64BI")
  (RVVM1x6SF "RVVMF32BI") (RVVMF2x6SF "RVVMF64BI")
  (RVVM1x5SF "RVVMF32BI") (RVVMF2x5SF "RVVMF64BI")
  (RVVM2x4SF "RVVMF16BI") (RVVM1x4SF "RVVMF32BI") (RVVMF2x4SF "RVVMF64BI")
  (RVVM2x3SF "RVVMF16BI") (RVVM1x3SF "RVVMF32BI") (RVVMF2x3SF "RVVMF64BI")
  (RVVM4x2SF "RVVMF8BI") (RVVM2x2SF "RVVMF16BI") (RVVM1x2SF "RVVMF32BI") (RVVMF2x2SF "RVVMF64BI")

  (RVVM1x8DI "RVVMF64BI")
  (RVVM1x7DI "RVVMF64BI")
  (RVVM1x6DI "RVVMF64BI")
  (RVVM1x5DI "RVVMF64BI")
  (RVVM2x4DI "RVVMF32BI")
  (RVVM1x4DI "RVVMF64BI")
  (RVVM2x3DI "RVVMF32BI")
  (RVVM1x3DI "RVVMF64BI")
  (RVVM4x2DI "RVVMF16BI")
  (RVVM2x2DI "RVVMF32BI")
  (RVVM1x2DI "RVVMF64BI")

  (RVVM1x8DF "RVVMF64BI")
  (RVVM1x7DF "RVVMF64BI")
  (RVVM1x6DF "RVVMF64BI")
  (RVVM1x5DF "RVVMF64BI")
  (RVVM2x4DF "RVVMF32BI")
  (RVVM1x4DF "RVVMF64BI")
  (RVVM2x3DF "RVVMF32BI")
  (RVVM1x3DF "RVVMF64BI")
  (RVVM4x2DF "RVVMF16BI")
  (RVVM2x2DF "RVVMF32BI")
  (RVVM1x2DF "RVVMF64BI")

  ;; VLS modes.
  (V1QI "V1BI") (V2QI "V2BI") (V4QI "V4BI") (V8QI "V8BI") (V16QI "V16BI") (V32QI "V32BI")
  (V64QI "V64BI") (V128QI "V128BI") (V256QI "V256BI") (V512QI "V512BI")
  (V1024QI "V1024BI") (V2048QI "V2048BI") (V4096QI "V4096BI")
  (V1HI "V1BI") (V2HI "V2BI") (V4HI "V4BI") (V8HI "V8BI") (V16HI "V16BI")
  (V32HI "V32BI") (V64HI "V64BI") (V128HI "V128BI") (V256HI "V256BI")
  (V512HI "V512BI") (V1024HI "V1024BI") (V2048HI "V2048BI")
  (V1SI "V1BI") (V2SI "V2BI") (V4SI "V4BI") (V8SI "V8BI")
  (V16SI "V16BI") (V32SI "V32BI") (V64SI "V64BI")
  (V128SI "V128BI") (V256SI "V256BI") (V512SI "V512BI") (V1024SI "V1024BI")
  (V1DI "V1BI") (V2DI "V2BI") (V4DI "V4BI") (V8DI "V8BI") (V16DI "V16BI") (V32DI "V32BI")
  (V64DI "V64BI") (V128DI "V128BI") (V256DI "V256BI") (V512DI "V512BI")
  (V1HF "V1BI") (V2HF "V2BI") (V4HF "V4BI") (V8HF "V8BI") (V16HF "V16BI")
  (V32HF "V32BI") (V64HF "V64BI") (V128HF "V128BI") (V256HF "V256BI")
  (V512HF "V512BI") (V1024HF "V1024BI") (V2048HF "V2048BI")
  (V1SF "V1BI") (V2SF "V2BI") (V4SF "V4BI") (V8SF "V8BI")
  (V16SF "V16BI") (V32SF "V32BI") (V64SF "V64BI")
  (V128SF "V128BI") (V256SF "V256BI") (V512SF "V512BI") (V1024SF "V1024BI")
  (V1DF "V1BI") (V2DF "V2BI") (V4DF "V4BI") (V8DF "V8BI") (V16DF "V16BI") (V32DF "V32BI")
  (V64DF "V64BI") (V128DF "V128BI") (V256DF "V256BI") (V512DF "V512BI")
])

(define_mode_attr vm [
  (RVVM8QI "rvvm1bi") (RVVM4QI "rvvmf2bi") (RVVM2QI "rvvmf4bi") (RVVM1QI "rvvmf8bi") (RVVMF2QI "rvvmf16bi") (RVVMF4QI "rvvmf32bi") (RVVMF8QI "rvvmf64bi")

  (RVVM8HI "rvvmf2bi") (RVVM4HI "rvvmf4bi") (RVVM2HI "rvvmf8bi") (RVVM1HI "rvvmf16bi") (RVVMF2HI "rvvmf32bi") (RVVMF4HI "rvvmf64bi")

  (RVVM8BF "rvvmf2bi") (RVVM4BF "rvvmf4bi") (RVVM2BF "rvvmf8bi") (RVVM1BF "rvvmf16bi") (RVVMF2BF "rvvmf32bi") (RVVMF4BF "rvvmf64bi")

  (RVVM8HF "rvvmf2bi") (RVVM4HF "rvvmf4bi") (RVVM2HF "rvvmf8bi") (RVVM1HF "rvvmf16bi") (RVVMF2HF "rvvmf32bi") (RVVMF4HF "rvvmf64bi")

  (RVVM8SI "rvvmf4bi") (RVVM4SI "rvvmf8bi") (RVVM2SI "rvvmf16bi") (RVVM1SI "rvvmf32bi") (RVVMF2SI "rvvmf64bi")

  (RVVM8SF "rvvmf4bi") (RVVM4SF "rvvmf8bi") (RVVM2SF "rvvmf16bi") (RVVM1SF "rvvmf32bi") (RVVMF2SF "rvvmf64bi")

  (RVVM8DI "rvvmf8bi") (RVVM4DI "rvvmf16bi") (RVVM2DI "rvvmf32bi") (RVVM1DI "rvvmf64bi")

  (RVVM8DF "rvvmf8bi") (RVVM4DF "rvvmf16bi") (RVVM2DF "rvvmf32bi") (RVVM1DF "rvvmf64bi")

  (RVVM1x8QI "rvvmf8bi") (RVVMF2x8QI "rvvmf16bi") (RVVMF4x8QI "rvvmf32bi") (RVVMF8x8QI "rvvmf64bi")
  (RVVM1x7QI "rvvmf8bi") (RVVMF2x7QI "rvvmf16bi") (RVVMF4x7QI "rvvmf32bi") (RVVMF8x7QI "rvvmf64bi")
  (RVVM1x6QI "rvvmf8bi") (RVVMF2x6QI "rvvmf16bi") (RVVMF4x6QI "rvvmf32bi") (RVVMF8x6QI "rvvmf64bi")
  (RVVM1x5QI "rvvmf8bi") (RVVMF2x5QI "rvvmf16bi") (RVVMF4x5QI "rvvmf32bi") (RVVMF8x5QI "rvvmf64bi")
  (RVVM2x4QI "rvvmf4bi") (RVVM1x4QI "rvvmf8bi") (RVVMF2x4QI "rvvmf16bi") (RVVMF4x4QI "rvvmf32bi") (RVVMF8x4QI "rvvmf64bi")
  (RVVM2x3QI "rvvmf4bi") (RVVM1x3QI "rvvmf8bi") (RVVMF2x3QI "rvvmf16bi") (RVVMF4x3QI "rvvmf32bi") (RVVMF8x3QI "rvvmf64bi")
  (RVVM4x2QI "rvvmf2bi") (RVVM2x2QI "rvvmf4bi") (RVVM1x2QI "rvvmf8bi") (RVVMF2x2QI "rvvmf16bi") (RVVMF4x2QI "rvvmf32bi") (RVVMF8x2QI "rvvmf64bi")

  (RVVM1x8HI "rvvmf16bi") (RVVMF2x8HI "rvvmf32bi") (RVVMF4x8HI "rvvmf64bi")
  (RVVM1x7HI "rvvmf16bi") (RVVMF2x7HI "rvvmf32bi") (RVVMF4x7HI "rvvmf64bi")
  (RVVM1x6HI "rvvmf16bi") (RVVMF2x6HI "rvvmf32bi") (RVVMF4x6HI "rvvmf64bi")
  (RVVM1x5HI "rvvmf16bi") (RVVMF2x5HI "rvvmf32bi") (RVVMF4x5HI "rvvmf64bi")
  (RVVM2x4HI "rvvmf8bi") (RVVM1x4HI "rvvmf16bi") (RVVMF2x4HI "rvvmf32bi") (RVVMF4x4HI "rvvmf64bi")
  (RVVM2x3HI "rvvmf8bi") (RVVM1x3HI "rvvmf16bi") (RVVMF2x3HI "rvvmf32bi") (RVVMF4x3HI "rvvmf64bi")
  (RVVM4x2HI "rvvmf4bi") (RVVM2x2HI "rvvmf8bi") (RVVM1x2HI "rvvmf16bi") (RVVMF2x2HI "rvvmf32bi") (RVVMF4x2HI "rvvmf64bi")

  (RVVM1x8BF "rvvmf16bi") (RVVMF2x8BF "rvvmf32bi") (RVVMF4x8BF "rvvmf64bi")
  (RVVM1x7BF "rvvmf16bi") (RVVMF2x7BF "rvvmf32bi") (RVVMF4x7BF "rvvmf64bi")
  (RVVM1x6BF "rvvmf16bi") (RVVMF2x6BF "rvvmf32bi") (RVVMF4x6BF "rvvmf64bi")
  (RVVM1x5BF "rvvmf16bi") (RVVMF2x5BF "rvvmf32bi") (RVVMF4x5BF "rvvmf64bi")
  (RVVM2x4BF "rvvmf8bi") (RVVM1x4BF "rvvmf16bi") (RVVMF2x4BF "rvvmf32bi") (RVVMF4x4BF "rvvmf64bi")
  (RVVM2x3BF "rvvmf8bi") (RVVM1x3BF "rvvmf16bi") (RVVMF2x3BF "rvvmf32bi") (RVVMF4x3BF "rvvmf64bi")
  (RVVM4x2BF "rvvmf4bi") (RVVM2x2BF "rvvmf8bi") (RVVM1x2BF "rvvmf16bi") (RVVMF2x2BF "rvvmf32bi") (RVVMF4x2BF "rvvmf64bi")

  (RVVM1x8HF "rvvmf16bi") (RVVMF2x8HF "rvvmf32bi") (RVVMF4x8HF "rvvmf64bi")
  (RVVM1x7HF "rvvmf16bi") (RVVMF2x7HF "rvvmf32bi") (RVVMF4x7HF "rvvmf64bi")
  (RVVM1x6HF "rvvmf16bi") (RVVMF2x6HF "rvvmf32bi") (RVVMF4x6HF "rvvmf64bi")
  (RVVM1x5HF "rvvmf16bi") (RVVMF2x5HF "rvvmf32bi") (RVVMF4x5HF "rvvmf64bi")
  (RVVM2x4HF "rvvmf8bi") (RVVM1x4HF "rvvmf16bi") (RVVMF2x4HF "rvvmf32bi") (RVVMF4x4HF "rvvmf64bi")
  (RVVM2x3HF "rvvmf8bi") (RVVM1x3HF "rvvmf16bi") (RVVMF2x3HF "rvvmf32bi") (RVVMF4x3HF "rvvmf64bi")
  (RVVM4x2HF "rvvmf4bi") (RVVM2x2HF "rvvmf8bi") (RVVM1x2HF "rvvmf16bi") (RVVMF2x2HF "rvvmf32bi") (RVVMF4x2HF "rvvmf64bi")

  (RVVM1x8SI "rvvmf32bi") (RVVMF2x8SI "rvvmf64bi")
  (RVVM1x7SI "rvvmf32bi") (RVVMF2x7SI "rvvmf64bi")
  (RVVM1x6SI "rvvmf32bi") (RVVMF2x6SI "rvvmf64bi")
  (RVVM1x5SI "rvvmf32bi") (RVVMF2x5SI "rvvmf64bi")
  (RVVM2x4SI "rvvmf16bi") (RVVM1x4SI "rvvmf32bi") (RVVMF2x4SI "rvvmf64bi")
  (RVVM2x3SI "rvvmf16bi") (RVVM1x3SI "rvvmf32bi") (RVVMF2x3SI "rvvmf64bi")
  (RVVM4x2SI "rvvmf4bi") (RVVM2x2SI "rvvmf16bi") (RVVM1x2SI "rvvmf32bi") (RVVMF2x2SI "rvvmf64bi")

  (RVVM1x8SF "rvvmf32bi") (RVVMF2x8SF "rvvmf64bi")
  (RVVM1x7SF "rvvmf32bi") (RVVMF2x7SF "rvvmf64bi")
  (RVVM1x6SF "rvvmf32bi") (RVVMF2x6SF "rvvmf64bi")
  (RVVM1x5SF "rvvmf32bi") (RVVMF2x5SF "rvvmf64bi")
  (RVVM2x4SF "rvvmf16bi") (RVVM1x4SF "rvvmf32bi") (RVVMF2x4SF "rvvmf64bi")
  (RVVM2x3SF "rvvmf16bi") (RVVM1x3SF "rvvmf32bi") (RVVMF2x3SF "rvvmf64bi")
  (RVVM4x2SF "rvvmf4bi") (RVVM2x2SF "rvvmf16bi") (RVVM1x2SF "rvvmf32bi") (RVVMF2x2SF "rvvmf64bi")

  (RVVM1x8DI "rvvmf64bi")
  (RVVM1x7DI "rvvmf64bi")
  (RVVM1x6DI "rvvmf64bi")
  (RVVM1x5DI "rvvmf64bi")
  (RVVM2x4DI "rvvmf32bi")
  (RVVM1x4DI "rvvmf64bi")
  (RVVM2x3DI "rvvmf32bi")
  (RVVM1x3DI "rvvmf64bi")
  (RVVM4x2DI "rvvmf16bi")
  (RVVM2x2DI "rvvmf32bi")
  (RVVM1x2DI "rvvmf64bi")

  (RVVM1x8DF "rvvmf64bi")
  (RVVM1x7DF "rvvmf64bi")
  (RVVM1x6DF "rvvmf64bi")
  (RVVM1x5DF "rvvmf64bi")
  (RVVM2x4DF "rvvmf32bi")
  (RVVM1x4DF "rvvmf64bi")
  (RVVM2x3DF "rvvmf32bi")
  (RVVM1x3DF "rvvmf64bi")
  (RVVM4x2DF "rvvmf16bi")
  (RVVM2x2DF "rvvmf32bi")
  (RVVM1x2DF "rvvmf64bi")

  ;; VLS modes.
  (V1QI "v1bi") (V2QI "v2bi") (V4QI "v4bi") (V8QI "v8bi") (V16QI "v16bi") (V32QI "v32bi")
  (V64QI "v64bi") (V128QI "v128bi") (V256QI "v256bi") (V512QI "v512bi")
  (V1024QI "v1024bi") (V2048QI "v2048bi") (V4096QI "v4096bi")
  (V1HI "v1bi") (V2HI "v2bi") (V4HI "v4bi") (V8HI "v8bi") (V16HI "v16bi")
  (V32HI "v32bi") (V64HI "v64bi") (V128HI "v128bi") (V256HI "v256bi")
  (V512HI "v512bi") (V1024HI "v1024bi") (V2048HI "v2048bi")
  (V1SI "v1bi") (V2SI "v2bi") (V4SI "v4bi") (V8SI "v8bi")
  (V16SI "v16bi") (V32SI "v32bi") (V64SI "v64bi")
  (V128SI "v128bi") (V256SI "v256bi") (V512SI "v512bi") (V1024SI "v1024bi")
  (V1DI "v1bi") (V2DI "v2bi") (V4DI "v4bi") (V8DI "v8bi") (V16DI "v16bi") (V32DI "v32bi")
  (V64DI "v64bi") (V128DI "v128bi") (V256DI "v256bi") (V512DI "v512bi")
  (V1HF "v1bi") (V2HF "v2bi") (V4HF "v4bi") (V8HF "v8bi") (V16HF "v16bi")
  (V32HF "v32bi") (V64HF "v64bi") (V128HF "v128bi") (V256HF "v256bi")
  (V512HF "v512bi") (V1024HF "v1024bi") (V2048HF "v2048bi")
  (V1SF "v1bi") (V2SF "v2bi") (V4SF "v4bi") (V8SF "v8bi")
  (V16SF "v16bi") (V32SF "v32bi") (V64SF "v64bi")
  (V128SF "v128bi") (V256SF "v256bi") (V512SF "v512bi") (V1024SF "v1024bi")
  (V1DF "v1bi") (V2DF "v2bi") (V4DF "v4bi") (V8DF "v8bi") (V16DF "v16bi") (V32DF "v32bi")
  (V64DF "v64bi") (V128DF "v128bi") (V256DF "v256bi") (V512DF "v512bi")
])

(define_mode_attr VEL [
  (RVVM8QI "QI") (RVVM4QI "QI") (RVVM2QI "QI") (RVVM1QI "QI") (RVVMF2QI "QI") (RVVMF4QI "QI") (RVVMF8QI "QI")

  (RVVM8HI "HI") (RVVM4HI "HI") (RVVM2HI "HI") (RVVM1HI "HI") (RVVMF2HI "HI") (RVVMF4HI "HI")

  (RVVM8BF "BF") (RVVM4BF "BF") (RVVM2BF "BF") (RVVM1BF "BF") (RVVMF2BF "BF") (RVVMF4BF "BF")

  (RVVM8HF "HF") (RVVM4HF "HF") (RVVM2HF "HF") (RVVM1HF "HF") (RVVMF2HF "HF") (RVVMF4HF "HF")

  (RVVM8SI "SI") (RVVM4SI "SI") (RVVM2SI "SI") (RVVM1SI "SI") (RVVMF2SI "SI")

  (RVVM8SF "SF") (RVVM4SF "SF") (RVVM2SF "SF") (RVVM1SF "SF") (RVVMF2SF "SF")

  (RVVM8DI "DI") (RVVM4DI "DI") (RVVM2DI "DI") (RVVM1DI "DI")

  (RVVM8DF "DF") (RVVM4DF "DF") (RVVM2DF "DF") (RVVM1DF "DF")

  ;; VLS modes.
  (V1QI "QI") (V2QI "QI") (V4QI "QI") (V8QI "QI") (V16QI "QI") (V32QI "QI") (V64QI "QI") (V128QI "QI") (V256QI "QI") (V512QI "QI")
  (V1024QI "QI") (V2048QI "QI") (V4096QI "QI")
  (V1HI "HI") (V2HI "HI") (V4HI "HI") (V8HI "HI") (V16HI "HI") (V32HI "HI") (V64HI "HI") (V128HI "HI") (V256HI "HI")
  (V512HI "HI") (V1024HI "HI") (V2048HI "HI")
  (V1SI "SI") (V2SI "SI") (V4SI "SI") (V8SI "SI") (V16SI "SI") (V32SI "SI") (V64SI "SI") (V128SI "SI") (V256SI "SI")
  (V512SI "SI") (V1024SI "SI")
  (V1DI "DI") (V2DI "DI") (V4DI "DI") (V8DI "DI") (V16DI "DI") (V32DI "DI") (V64DI "DI") (V128DI "DI") (V256DI "DI") (V512DI "DI")
  (V1HF "HF") (V2HF "HF") (V4HF "HF") (V8HF "HF") (V16HF "HF") (V32HF "HF") (V64HF "HF") (V128HF "HF") (V256HF "HF")
  (V512HF "HF") (V1024HF "HF") (V2048HF "HF")
  (V1SF "SF") (V2SF "SF") (V4SF "SF") (V8SF "SF") (V16SF "SF") (V32SF "SF") (V64SF "SF") (V128SF "SF") (V256SF "SF")
  (V512SF "SF") (V1024SF "SF")
  (V1DF "DF") (V2DF "DF") (V4DF "DF") (V8DF "DF") (V16DF "DF") (V32DF "DF") (V64DF "DF") (V128DF "DF") (V256DF "DF") (V512DF "DF")
])

(define_mode_attr V_DOUBLE_EXTEND_VEL [
  (RVVM4QI "HI") (RVVM2QI "HI") (RVVM1QI "HI") (RVVMF2QI "HI") (RVVMF4QI "HI") (RVVMF8QI "HI")

  (RVVM4HI "SI") (RVVM2HI "SI") (RVVM1HI "SI") (RVVMF2HI "SI") (RVVMF4HI "SI")

  (RVVM4SI "DI") (RVVM2SI "DI") (RVVM1SI "DI") (RVVMF2SI "DI")

  (RVVM4HF "SF") (RVVM2HF "SF") (RVVM1HF "SF") (RVVMF2HF "SF") (RVVMF4HF "SF")

  (RVVM4SF "DF") (RVVM2SF "DF") (RVVM1SF "DF") (RVVMF2SF "DF")

  (V1QI "HI")
  (V2QI "HI")
  (V4QI "HI")
  (V8QI "HI")
  (V16QI "HI")
  (V32QI "HI")
  (V64QI "HI")
  (V128QI "HI")
  (V256QI "HI")
  (V512QI "HI")
  (V1024QI "HI")
  (V2048QI "HI")
  (V1HI "SI")
  (V2HI "SI")
  (V4HI "SI")
  (V8HI "SI")
  (V16HI "SI")
  (V32HI "SI")
  (V64HI "SI")
  (V128HI "SI")
  (V256HI "SI")
  (V512HI "SI")
  (V1024HI "SI")
  (V1SI "SI")
  (V2SI "SI")
  (V4SI "SI")
  (V8SI "SI")
  (V16SI "SI")
  (V32SI "SI")
  (V64SI "SI")
  (V128SI "SI")
  (V256SI "SI")
  (V512SI "SI")
  (V1HF "SF")
  (V2HF "SF")
  (V4HF "SF")
  (V8HF "SF")
  (V16HF "SF")
  (V32HF "SF")
  (V64HF "SF")
  (V128HF "SF")
  (V256HF "SF")
  (V512HF "SF")
  (V1024HF "SF")
  (V1SF "DF")
  (V2SF "DF")
  (V4SF "DF")
  (V8SF "DF")
  (V16SF "DF")
  (V32SF "DF")
  (V64SF "DF")
  (V128SF "DF")
  (V256SF "DF")
  (V512SF "DF")
])

(define_mode_attr vel [
  (RVVM8QI "qi") (RVVM4QI "qi") (RVVM2QI "qi") (RVVM1QI "qi") (RVVMF2QI "qi") (RVVMF4QI "qi") (RVVMF8QI "qi")

  (RVVM8HI "hi") (RVVM4HI "hi") (RVVM2HI "hi") (RVVM1HI "hi") (RVVMF2HI "hi") (RVVMF4HI "hi")

  (RVVM8BF "bf") (RVVM4BF "bf") (RVVM2BF "bf") (RVVM1BF "bf") (RVVMF2BF "bf") (RVVMF4BF "bf")

  (RVVM8HF "hf") (RVVM4HF "hf") (RVVM2HF "hf") (RVVM1HF "hf") (RVVMF2HF "hf") (RVVMF4HF "hf")

  (RVVM8SI "si") (RVVM4SI "si") (RVVM2SI "si") (RVVM1SI "si") (RVVMF2SI "si")

  (RVVM8SF "sf") (RVVM4SF "sf") (RVVM2SF "sf") (RVVM1SF "sf") (RVVMF2SF "sf")

  (RVVM8DI "di") (RVVM4DI "di") (RVVM2DI "di") (RVVM1DI "di")

  (RVVM8DF "df") (RVVM4DF "df") (RVVM2DF "df") (RVVM1DF "df")

  ;; VLS modes.
  (V1QI "qi") (V2QI "qi") (V4QI "qi") (V8QI "qi") (V16QI "qi") (V32QI "qi") (V64QI "qi") (V128QI "qi") (V256QI "qi") (V512QI "qi")
  (V1024QI "qi") (V2048QI "qi") (V4096QI "qi")
  (V1HI "hi") (V2HI "hi") (V4HI "hi") (V8HI "hi") (V16HI "hi") (V32HI "hi") (V64HI "hi") (V128HI "hi") (V256HI "hi")
  (V512HI "hi") (V1024HI "hi") (V2048HI "hi")
  (V1SI "si") (V2SI "si") (V4SI "si") (V8SI "si") (V16SI "si") (V32SI "si") (V64SI "si") (V128SI "si") (V256SI "si")
  (V512SI "si") (V1024SI "si")
  (V1DI "di") (V2DI "di") (V4DI "di") (V8DI "di") (V16DI "di") (V32DI "di") (V64DI "di") (V128DI "di") (V256DI "di") (V512DI "di")
  (V1HF "hf") (V2HF "hf") (V4HF "hf") (V8HF "hf") (V16HF "hf") (V32HF "hf") (V64HF "hf") (V128HF "hf") (V256HF "hf")
  (V512HF "hf") (V1024HF "hf") (V2048HF "hf")
  (V1SF "sf") (V2SF "sf") (V4SF "sf") (V8SF "sf") (V16SF "sf") (V32SF "sf") (V64SF "sf") (V128SF "sf") (V256SF "sf")
  (V512SF "sf") (V1024SF "sf")
  (V1DF "df") (V2DF "df") (V4DF "df") (V8DF "df") (V16DF "df") (V32DF "df") (V64DF "df") (V128DF "df") (V256DF "df") (V512DF "df")
])

(define_mode_attr vsingle [
  (RVVM1x8QI "rvvm1qi") (RVVMF2x8QI "rvvmf2qi") (RVVMF4x8QI "rvvmf4qi") (RVVMF8x8QI "rvvmf8qi")
  (RVVM1x7QI "rvvm1qi") (RVVMF2x7QI "rvvmf2qi") (RVVMF4x7QI "rvvmf4qi") (RVVMF8x7QI "rvvmf8qi")
  (RVVM1x6QI "rvvm1qi") (RVVMF2x6QI "rvvmf2qi") (RVVMF4x6QI "rvvmf4qi") (RVVMF8x6QI "rvvmf8qi")
  (RVVM1x5QI "rvvm1qi") (RVVMF2x5QI "rvvmf2qi") (RVVMF4x5QI "rvvmf4qi") (RVVMF8x5QI "rvvmf8qi")
  (RVVM2x4QI "rvvm2qi") (RVVM1x4QI "rvvm1qi") (RVVMF2x4QI "rvvmf2qi") (RVVMF4x4QI "rvvmf4qi") (RVVMF8x4QI "rvvmf8qi")
  (RVVM2x3QI "rvvm2qi") (RVVM1x3QI "rvvm1qi") (RVVMF2x3QI "rvvmf2qi") (RVVMF4x3QI "rvvmf4qi") (RVVMF8x3QI "rvvmf8qi")
  (RVVM4x2QI "rvvm4qi") (RVVM2x2QI "rvvm2qi") (RVVM1x2QI "rvvm1qi") (RVVMF2x2QI "rvvmf2qi") (RVVMF4x2QI "rvvmf4qi") (RVVMF8x2QI "rvvmf8qi")

  (RVVM1x8HI "rvvm1hi") (RVVMF2x8HI "rvvmf2hi") (RVVMF4x8HI "rvvmf4hi")
  (RVVM1x7HI "rvvm1hi") (RVVMF2x7HI "rvvmf2hi") (RVVMF4x7HI "rvvmf4hi")
  (RVVM1x6HI "rvvm1hi") (RVVMF2x6HI "rvvmf2hi") (RVVMF4x6HI "rvvmf4hi")
  (RVVM1x5HI "rvvm1hi") (RVVMF2x5HI "rvvmf2hi") (RVVMF4x5HI "rvvmf4hi")
  (RVVM2x4HI "rvvm2hi") (RVVM1x4HI "rvvm1hi") (RVVMF2x4HI "rvvmf2hi") (RVVMF4x4HI "rvvmf4hi")
  (RVVM2x3HI "rvvm2hi") (RVVM1x3HI "rvvm1hi") (RVVMF2x3HI "rvvmf2hi") (RVVMF4x3HI "rvvmf4hi")
  (RVVM4x2HI "rvvm4hi") (RVVM2x2HI "rvvm2hi") (RVVM1x2HI "rvvm1hi") (RVVMF2x2HI "rvvmf2hi") (RVVMF4x2HI "rvvmf4hi")

  (RVVM1x8BF "rvvm1bf")
  (RVVMF2x8BF "rvvmf2bf")
  (RVVMF4x8BF "rvvmf4bf")
  (RVVM1x7BF "rvvm1bf")
  (RVVMF2x7BF "rvvmf2bf")
  (RVVMF4x7BF "rvvmf4bf")
  (RVVM1x6BF "rvvm1bf")
  (RVVMF2x6BF "rvvmf2bf")
  (RVVMF4x6BF "rvvmf4bf")
  (RVVM1x5BF "rvvm1bf")
  (RVVMF2x5BF "rvvmf2bf")
  (RVVMF4x5BF "rvvmf4bf")
  (RVVM2x4BF "rvvm2bf")
  (RVVM1x4BF "rvvm1bf")
  (RVVMF2x4BF "rvvmf2bf")
  (RVVMF4x4BF "rvvmf4bf")
  (RVVM2x3BF "rvvm2bf")
  (RVVM1x3BF "rvvm1bf")
  (RVVMF2x3BF "rvvmf2bf")
  (RVVMF4x3BF "rvvmf4bf")
  (RVVM4x2BF "rvvm4bf")
  (RVVM2x2BF "rvvm2bf")
  (RVVM1x2BF "rvvm1bf")
  (RVVMF2x2BF "rvvmf2bf")
  (RVVMF4x2BF "rvvmf4bf")

  (RVVM1x8HF "rvvm1hf")
  (RVVMF2x8HF "rvvmf2hf")
  (RVVMF4x8HF "rvvmf4hf")
  (RVVM1x7HF "rvvm1hf")
  (RVVMF2x7HF "rvvmf2hf")
  (RVVMF4x7HF "rvvmf4hf")
  (RVVM1x6HF "rvvm1hf")
  (RVVMF2x6HF "rvvmf2hf")
  (RVVMF4x6HF "rvvmf4hf")
  (RVVM1x5HF "rvvm1hf")
  (RVVMF2x5HF "rvvmf2hf")
  (RVVMF4x5HF "rvvmf4hf")
  (RVVM2x4HF "rvvm2hf")
  (RVVM1x4HF "rvvm1hf")
  (RVVMF2x4HF "rvvmf2hf")
  (RVVMF4x4HF "rvvmf4hf")
  (RVVM2x3HF "rvvm2hf")
  (RVVM1x3HF "rvvm1hf")
  (RVVMF2x3HF "rvvmf2hf")
  (RVVMF4x3HF "rvvmf4hf")
  (RVVM4x2HF "rvvm4hf")
  (RVVM2x2HF "rvvm2hf")
  (RVVM1x2HF "rvvm1hf")
  (RVVMF2x2HF "rvvmf2hf")
  (RVVMF4x2HF "rvvmf4hf")

  (RVVM1x8SI "rvvm1si") (RVVMF2x8SI "rvvmf2si")
  (RVVM1x7SI "rvvm1si") (RVVMF2x7SI "rvvmf2si")
  (RVVM1x6SI "rvvm1si") (RVVMF2x6SI "rvvmf2si")
  (RVVM1x5SI "rvvm1si") (RVVMF2x5SI "rvvmf2si")
  (RVVM2x4SI "rvvm2si") (RVVM1x4SI "rvvm1si") (RVVMF2x4SI "rvvmf2si")
  (RVVM2x3SI "rvvm2si") (RVVM1x3SI "rvvm1si") (RVVMF2x3SI "rvvmf2si")
  (RVVM4x2SI "rvvm4si") (RVVM2x2SI "rvvm2si") (RVVM1x2SI "rvvm1si") (RVVMF2x2SI "rvvmf2si")

  (RVVM1x8SF "rvvm1sf")
  (RVVMF2x8SF "rvvmf2sf")
  (RVVM1x7SF "rvvm1sf")
  (RVVMF2x7SF "rvvmf2sf")
  (RVVM1x6SF "rvvm1sf")
  (RVVMF2x6SF "rvvmf2sf")
  (RVVM1x5SF "rvvm1sf")
  (RVVMF2x5SF "rvvmf2sf")
  (RVVM2x4SF "rvvm2sf")
  (RVVM1x4SF "rvvm1sf")
  (RVVMF2x4SF "rvvmf2sf")
  (RVVM2x3SF "rvvm2sf")
  (RVVM1x3SF "rvvm1sf")
  (RVVMF2x3SF "rvvmf2sf")
  (RVVM4x2SF "rvvm4sf")
  (RVVM2x2SF "rvvm2sf")
  (RVVM1x2SF "rvvm1sf")
  (RVVMF2x2SF "rvvmf2sf")

  (RVVM1x8DI "rvvm1di")
  (RVVM1x7DI "rvvm1di")
  (RVVM1x6DI "rvvm1di")
  (RVVM1x5DI "rvvm1di")
  (RVVM2x4DI "rvvm2di")
  (RVVM1x4DI "rvvm1di")
  (RVVM2x3DI "rvvm2di")
  (RVVM1x3DI "rvvm1di")
  (RVVM4x2DI "rvvm4di")
  (RVVM2x2DI "rvvm2di")
  (RVVM1x2DI "rvvm1di")

  (RVVM1x8DF "rvvm1df")
  (RVVM1x7DF "rvvm1df")
  (RVVM1x6DF "rvvm1df")
  (RVVM1x5DF "rvvm1df")
  (RVVM2x4DF "rvvm2df")
  (RVVM1x4DF "rvvm1df")
  (RVVM2x3DF "rvvm2df")
  (RVVM1x3DF "rvvm1df")
  (RVVM4x2DF "rvvm4df")
  (RVVM2x2DF "rvvm2df")
  (RVVM1x2DF "rvvm1df")
])

(define_mode_attr VSUBEL [
  (RVVM8HI "QI") (RVVM4HI "QI") (RVVM2HI "QI") (RVVM1HI "QI") (RVVMF2HI "QI") (RVVMF4HI "QI")

  (RVVM8SI "HI") (RVVM4SI "HI") (RVVM2SI "HI") (RVVM1SI "HI") (RVVMF2SI "HI")

  (RVVM8SF "HF") (RVVM4SF "HF") (RVVM2SF "HF") (RVVM1SF "HF") (RVVMF2SF "HF")

  (RVVM8DI "SI") (RVVM4DI "SI") (RVVM2DI "SI") (RVVM1DI "SI")

  (RVVM8DF "SF") (RVVM4DF "SF") (RVVM2DF "SF") (RVVM1DF "SF")

  ;; VLS modes.
  (V1HI "QI") (V2HI "QI") (V4HI "QI") (V8HI "QI") (V16HI "QI") (V32HI "QI") (V64HI "QI") (V128HI "QI") (V256HI "QI")
  (V512HI "QI") (V1024HI "QI") (V2048HI "QI")
  (V1SI "HI") (V2SI "HI") (V4SI "HI") (V8SI "HI") (V16SI "HI") (V32SI "HI") (V64SI "HI") (V128SI "HI") (V256SI "HI")
  (V512SI "HI") (V1024SI "HI")
  (V1DI "SI") (V2DI "SI") (V4DI "SI") (V8DI "SI") (V16DI "SI") (V32DI "SI") (V64DI "SI") (V128DI "SI") (V256DI "SI") (V512DI "SI")

  (V1SF "HF")
  (V2SF "HF")
  (V4SF "HF")
  (V8SF "HF")
  (V16SF "HF")
  (V32SF "HF")
  (V64SF "HF")
  (V128SF "HF")
  (V256SF "HF")
  (V512SF "HF")
  (V1024SF "HF")
  (V1DF "SF")
  (V2DF "SF")
  (V4DF "SF")
  (V8DF "SF")
  (V16DF "SF")
  (V32DF "SF")
  (V64DF "SF")
  (V128DF "SF")
  (V256DF "SF")
  (V512DF "SF")
])

(define_mode_attr nf [
  (RVVM1x8QI "8") (RVVMF2x8QI "8") (RVVMF4x8QI "8") (RVVMF8x8QI "8")
  (RVVM1x7QI "7") (RVVMF2x7QI "7") (RVVMF4x7QI "7") (RVVMF8x7QI "7")
  (RVVM1x6QI "6") (RVVMF2x6QI "6") (RVVMF4x6QI "6") (RVVMF8x6QI "6")
  (RVVM1x5QI "5") (RVVMF2x5QI "5") (RVVMF4x5QI "5") (RVVMF8x5QI "5")
  (RVVM2x4QI "4") (RVVM1x4QI "4") (RVVMF2x4QI "4") (RVVMF4x4QI "4") (RVVMF8x4QI "4")
  (RVVM2x3QI "3") (RVVM1x3QI "3") (RVVMF2x3QI "3") (RVVMF4x3QI "3") (RVVMF8x3QI "3")
  (RVVM4x2QI "2") (RVVM2x2QI "2") (RVVM1x2QI "2") (RVVMF2x2QI "2") (RVVMF4x2QI "2") (RVVMF8x2QI "2")

  (RVVM1x8HI "8") (RVVMF2x8HI "8") (RVVMF4x8HI "8")
  (RVVM1x7HI "7") (RVVMF2x7HI "7") (RVVMF4x7HI "7")
  (RVVM1x6HI "6") (RVVMF2x6HI "6") (RVVMF4x6HI "6")
  (RVVM1x5HI "5") (RVVMF2x5HI "5") (RVVMF4x5HI "5")
  (RVVM2x4HI "4") (RVVM1x4HI "4") (RVVMF2x4HI "4") (RVVMF4x4HI "4")
  (RVVM2x3HI "3") (RVVM1x3HI "3") (RVVMF2x3HI "3") (RVVMF4x3HI "3")
  (RVVM4x2HI "2") (RVVM2x2HI "2") (RVVM1x2HI "2") (RVVMF2x2HI "2") (RVVMF4x2HI "2")

  (RVVM1x8BF "8") (RVVMF2x8BF "8") (RVVMF4x8BF "8")
  (RVVM1x7BF "7") (RVVMF2x7BF "7") (RVVMF4x7BF "7")
  (RVVM1x6BF "6") (RVVMF2x6BF "6") (RVVMF4x6BF "6")
  (RVVM1x5BF "5") (RVVMF2x5BF "5") (RVVMF4x5BF "5")
  (RVVM2x4BF "4") (RVVM1x4BF "4") (RVVMF2x4BF "4") (RVVMF4x4BF "4")
  (RVVM2x3BF "3") (RVVM1x3BF "3") (RVVMF2x3BF "3") (RVVMF4x3BF "3")
  (RVVM4x2BF "2") (RVVM2x2BF "2") (RVVM1x2BF "2") (RVVMF2x2BF "2") (RVVMF4x2BF "2")

  (RVVM1x8HF "8") (RVVMF2x8HF "8") (RVVMF4x8HF "8")
  (RVVM1x7HF "7") (RVVMF2x7HF "7") (RVVMF4x7HF "7")
  (RVVM1x6HF "6") (RVVMF2x6HF "6") (RVVMF4x6HF "6")
  (RVVM1x5HF "5") (RVVMF2x5HF "5") (RVVMF4x5HF "5")
  (RVVM2x4HF "4") (RVVM1x4HF "4") (RVVMF2x4HF "4") (RVVMF4x4HF "4")
  (RVVM2x3HF "3") (RVVM1x3HF "3") (RVVMF2x3HF "3") (RVVMF4x3HF "3")
  (RVVM4x2HF "2") (RVVM2x2HF "2") (RVVM1x2HF "2") (RVVMF2x2HF "2") (RVVMF4x2HF "2")

  (RVVM1x8SI "8") (RVVMF2x8SI "8")
  (RVVM1x7SI "7") (RVVMF2x7SI "7")
  (RVVM1x6SI "6") (RVVMF2x6SI "6")
  (RVVM1x5SI "5") (RVVMF2x5SI "5")
  (RVVM2x4SI "4") (RVVM1x4SI "4") (RVVMF2x4SI "4")
  (RVVM2x3SI "3") (RVVM1x3SI "3") (RVVMF2x3SI "3")
  (RVVM4x2SI "2") (RVVM2x2SI "2") (RVVM1x2SI "2") (RVVMF2x2SI "2")

  (RVVM1x8SF "8") (RVVMF2x8SF "8")
  (RVVM1x7SF "7") (RVVMF2x7SF "7")
  (RVVM1x6SF "6") (RVVMF2x6SF "6")
  (RVVM1x5SF "5") (RVVMF2x5SF "5")
  (RVVM2x4SF "4") (RVVM1x4SF "4") (RVVMF2x4SF "4")
  (RVVM2x3SF "3") (RVVM1x3SF "3") (RVVMF2x3SF "3")
  (RVVM4x2SF "2") (RVVM2x2SF "2") (RVVM1x2SF "2") (RVVMF2x2SF "2")

  (RVVM1x8DI "8")
  (RVVM1x7DI "7")
  (RVVM1x6DI "6")
  (RVVM1x5DI "5")
  (RVVM2x4DI "4")
  (RVVM1x4DI "4")
  (RVVM2x3DI "3")
  (RVVM1x3DI "3")
  (RVVM4x2DI "2")
  (RVVM2x2DI "2")
  (RVVM1x2DI "2")

  (RVVM1x8DF "8")
  (RVVM1x7DF "7")
  (RVVM1x6DF "6")
  (RVVM1x5DF "5")
  (RVVM2x4DF "4")
  (RVVM1x4DF "4")
  (RVVM2x3DF "3")
  (RVVM1x3DF "3")
  (RVVM4x2DF "2")
  (RVVM2x2DF "2")
  (RVVM1x2DF "2")
])

(define_mode_attr sew [
  (RVVM8QI "8") (RVVM4QI "8") (RVVM2QI "8") (RVVM1QI "8") (RVVMF2QI "8") (RVVMF4QI "8") (RVVMF8QI "8")

  (RVVM8HI "16") (RVVM4HI "16") (RVVM2HI "16") (RVVM1HI "16") (RVVMF2HI "16") (RVVMF4HI "16")

  (RVVM8BF "16") (RVVM4BF "16") (RVVM2BF "16") (RVVM1BF "16") (RVVMF2BF "16") (RVVMF4BF "16")

  (RVVM8HF "16") (RVVM4HF "16") (RVVM2HF "16") (RVVM1HF "16") (RVVMF2HF "16") (RVVMF4HF "16")

  (RVVM8SI "32") (RVVM4SI "32") (RVVM2SI "32") (RVVM1SI "32") (RVVMF2SI "32")

  (RVVM8SF "32") (RVVM4SF "32") (RVVM2SF "32") (RVVM1SF "32") (RVVMF2SF "32")

  (RVVM8DI "64") (RVVM4DI "64") (RVVM2DI "64") (RVVM1DI "64")

  (RVVM8DF "64") (RVVM4DF "64") (RVVM2DF "64") (RVVM1DF "64")

  (RVVM1x8QI "8") (RVVMF2x8QI "8") (RVVMF4x8QI "8") (RVVMF8x8QI "8")
  (RVVM1x7QI "8") (RVVMF2x7QI "8") (RVVMF4x7QI "8") (RVVMF8x7QI "8")
  (RVVM1x6QI "8") (RVVMF2x6QI "8") (RVVMF4x6QI "8") (RVVMF8x6QI "8")
  (RVVM1x5QI "8") (RVVMF2x5QI "8") (RVVMF4x5QI "8") (RVVMF8x5QI "8")
  (RVVM2x4QI "8") (RVVM1x4QI "8") (RVVMF2x4QI "8") (RVVMF4x4QI "8") (RVVMF8x4QI "8")
  (RVVM2x3QI "8") (RVVM1x3QI "8") (RVVMF2x3QI "8") (RVVMF4x3QI "8") (RVVMF8x3QI "8")
  (RVVM4x2QI "8") (RVVM2x2QI "8") (RVVM1x2QI "8") (RVVMF2x2QI "8") (RVVMF4x2QI "8") (RVVMF8x2QI "8")

  (RVVM1x8HI "16") (RVVMF2x8HI "16") (RVVMF4x8HI "16")
  (RVVM1x7HI "16") (RVVMF2x7HI "16") (RVVMF4x7HI "16")
  (RVVM1x6HI "16") (RVVMF2x6HI "16") (RVVMF4x6HI "16")
  (RVVM1x5HI "16") (RVVMF2x5HI "16") (RVVMF4x5HI "16")
  (RVVM2x4HI "16") (RVVM1x4HI "16") (RVVMF2x4HI "16") (RVVMF4x4HI "16")
  (RVVM2x3HI "16") (RVVM1x3HI "16") (RVVMF2x3HI "16") (RVVMF4x3HI "16")
  (RVVM4x2HI "16") (RVVM2x2HI "16") (RVVM1x2HI "16") (RVVMF2x2HI "16") (RVVMF4x2HI "16")

  (RVVM1x8BF "16") (RVVMF2x8BF "16") (RVVMF4x8BF "16")
  (RVVM1x7BF "16") (RVVMF2x7BF "16") (RVVMF4x7BF "16")
  (RVVM1x6BF "16") (RVVMF2x6BF "16") (RVVMF4x6BF "16")
  (RVVM1x5BF "16") (RVVMF2x5BF "16") (RVVMF4x5BF "16")
  (RVVM2x4BF "16") (RVVM1x4BF "16") (RVVMF2x4BF "16") (RVVMF4x4BF "16")
  (RVVM2x3BF "16") (RVVM1x3BF "16") (RVVMF2x3BF "16") (RVVMF4x3BF "16")
  (RVVM4x2BF "16") (RVVM2x2BF "16") (RVVM1x2BF "16") (RVVMF2x2BF "16") (RVVMF4x2BF "16")

  (RVVM1x8HF "16") (RVVMF2x8HF "16") (RVVMF4x8HF "16")
  (RVVM1x7HF "16") (RVVMF2x7HF "16") (RVVMF4x7HF "16")
  (RVVM1x6HF "16") (RVVMF2x6HF "16") (RVVMF4x6HF "16")
  (RVVM1x5HF "16") (RVVMF2x5HF "16") (RVVMF4x5HF "16")
  (RVVM2x4HF "16") (RVVM1x4HF "16") (RVVMF2x4HF "16") (RVVMF4x4HF "16")
  (RVVM2x3HF "16") (RVVM1x3HF "16") (RVVMF2x3HF "16") (RVVMF4x3HF "16")
  (RVVM4x2HF "16") (RVVM2x2HF "16") (RVVM1x2HF "16") (RVVMF2x2HF "16") (RVVMF4x2HF "16")

  (RVVM1x8SI "32") (RVVMF2x8SI "32")
  (RVVM1x7SI "32") (RVVMF2x7SI "32")
  (RVVM1x6SI "32") (RVVMF2x6SI "32")
  (RVVM1x5SI "32") (RVVMF2x5SI "32")
  (RVVM2x4SI "32") (RVVM1x4SI "32") (RVVMF2x4SI "32")
  (RVVM2x3SI "32") (RVVM1x3SI "32") (RVVMF2x3SI "32")
  (RVVM4x2SI "32") (RVVM2x2SI "32") (RVVM1x2SI "32") (RVVMF2x2SI "32")

  (RVVM1x8SF "32") (RVVMF2x8SF "32")
  (RVVM1x7SF "32") (RVVMF2x7SF "32")
  (RVVM1x6SF "32") (RVVMF2x6SF "32")
  (RVVM1x5SF "32") (RVVMF2x5SF "32")
  (RVVM2x4SF "32") (RVVM1x4SF "32") (RVVMF2x4SF "32")
  (RVVM2x3SF "32") (RVVM1x3SF "32") (RVVMF2x3SF "32")
  (RVVM4x2SF "32") (RVVM2x2SF "32") (RVVM1x2SF "32") (RVVMF2x2SF "32")

  (RVVM1x8DI "64")
  (RVVM1x7DI "64")
  (RVVM1x6DI "64")
  (RVVM1x5DI "64")
  (RVVM2x4DI "64")
  (RVVM1x4DI "64")
  (RVVM2x3DI "64")
  (RVVM1x3DI "64")
  (RVVM4x2DI "64")
  (RVVM2x2DI "64")
  (RVVM1x2DI "64")

  (RVVM1x8DF "64")
  (RVVM1x7DF "64")
  (RVVM1x6DF "64")
  (RVVM1x5DF "64")
  (RVVM2x4DF "64")
  (RVVM1x4DF "64")
  (RVVM2x3DF "64")
  (RVVM1x3DF "64")
  (RVVM4x2DF "64")
  (RVVM2x2DF "64")
  (RVVM1x2DF "64")

  ;; VLS modes.
  (V1QI "8") (V2QI "8") (V4QI "8") (V8QI "8") (V16QI "8") (V32QI "8") (V64QI "8") (V128QI "8") (V256QI "8") (V512QI "8")
  (V1024QI "8") (V2048QI "8") (V4096QI "8")
  (V1HI "16") (V2HI "16") (V4HI "16") (V8HI "16") (V16HI "16") (V32HI "16") (V64HI "16") (V128HI "16") (V256HI "16")
  (V512HI "16") (V1024HI "16") (V2048HI "16")
  (V1SI "32") (V2SI "32") (V4SI "32") (V8SI "32") (V16SI "32") (V32SI "32") (V64SI "32") (V128SI "32") (V256SI "32")
  (V512SI "32") (V1024SI "32")
  (V1DI "64") (V2DI "64") (V4DI "64") (V8DI "64") (V16DI "64") (V32DI "64") (V64DI "64") (V128DI "64") (V256DI "64") (V512DI "64")
  (V1HF "16") (V2HF "16") (V4HF "16") (V8HF "16") (V16HF "16") (V32HF "16") (V64HF "16") (V128HF "16") (V256HF "16")
  (V512HF "16") (V1024HF "16") (V2048HF "16")
  (V1SF "32") (V2SF "32") (V4SF "32") (V8SF "32") (V16SF "32") (V32SF "32") (V64SF "32") (V128SF "32") (V256SF "32")
  (V512SF "32") (V1024SF "32")
  (V1DF "64") (V2DF "64") (V4DF "64") (V8DF "64") (V16DF "64") (V32DF "64") (V64DF "64") (V128DF "64") (V256DF "64") (V512DF "64")
])

(define_mode_attr double_trunc_sew [
  (RVVM8HI "8") (RVVM4HI "8") (RVVM2HI "8") (RVVM1HI "8") (RVVMF2HI "8") (RVVMF4HI "8")

  (RVVM8BF "8") (RVVM4BF "8") (RVVM2BF "8") (RVVM1BF "8") (RVVMF2BF "8") (RVVMF4BF "8")

  (RVVM8HF "8") (RVVM4HF "8") (RVVM2HF "8") (RVVM1HF "8") (RVVMF2HF "8") (RVVMF4HF "8")

  (RVVM8SI "16") (RVVM4SI "16") (RVVM2SI "16") (RVVM1SI "16") (RVVMF2SI "16")

  (RVVM8SF "16") (RVVM4SF "16") (RVVM2SF "16") (RVVM1SF "16") (RVVMF2SF "16")

  (RVVM8DI "32") (RVVM4DI "32") (RVVM2DI "32") (RVVM1DI "32")

  (RVVM8DF "32") (RVVM4DF "32") (RVVM2DF "32") (RVVM1DF "32")
])

(define_mode_attr quad_trunc_sew [
  (RVVM8SI "8") (RVVM4SI "8") (RVVM2SI "8") (RVVM1SI "8") (RVVMF2SI "8")

  (RVVM8SF "8") (RVVM4SF "8") (RVVM2SF "8") (RVVM1SF "8") (RVVMF2SF "8")

  (RVVM8DI "16") (RVVM4DI "16") (RVVM2DI "16") (RVVM1DI "16")

  (RVVM8DF "16") (RVVM4DF "16") (RVVM2DF "16") (RVVM1DF "16")
])

(define_mode_attr oct_trunc_sew [
  (RVVM8DI "8") (RVVM4DI "8") (RVVM2DI "8") (RVVM1DI "8")

  (RVVM8DF "8") (RVVM4DF "8") (RVVM2DF "8") (RVVM1DF "8")
])

(define_mode_attr double_ext_sew [
  (RVVM4QI "16") (RVVM2QI "16") (RVVM1QI "16") (RVVMF2QI "16") (RVVMF4QI "16") (RVVMF8QI "16")

  (RVVM4HI "32") (RVVM2HI "32") (RVVM1HI "32") (RVVMF2HI "32") (RVVMF4HI "32")

  (RVVM4BF "32") (RVVM2BF "32") (RVVM1BF "32") (RVVMF2BF "32") (RVVMF4BF "32")

  (RVVM4HF "32") (RVVM2HF "32") (RVVM1HF "32") (RVVMF2HF "32") (RVVMF4HF "32")

  (RVVM4SI "64") (RVVM2SI "64") (RVVM1SI "64") (RVVMF2SI "64")

  (RVVM4SF "64") (RVVM2SF "64") (RVVM1SF "64") (RVVMF2SF "64")
])

(define_mode_attr quad_ext_sew [
  (RVVM2QI "32") (RVVM1QI "32") (RVVMF2QI "32") (RVVMF4QI "32") (RVVMF8QI "32")

  (RVVM2HI "64") (RVVM1HI "64") (RVVMF2HI "64") (RVVMF4HI "64")

  (RVVM2HF "64") (RVVM1HF "64") (RVVMF2HF "64") (RVVMF4HF "64")
])

(define_mode_attr oct_ext_sew [
  (RVVM1QI "64") (RVVMF2QI "64") (RVVMF4QI "64") (RVVMF8QI "64")
])

(define_mode_attr V_DOUBLE_EXTEND [
  (RVVM4QI "RVVM8HI") (RVVM2QI "RVVM4HI") (RVVM1QI "RVVM2HI") (RVVMF2QI "RVVM1HI") (RVVMF4QI "RVVMF2HI") (RVVMF8QI "RVVMF4HI")

  (RVVM4HI "RVVM8SI") (RVVM2HI "RVVM4SI") (RVVM1HI "RVVM2SI") (RVVMF2HI "RVVM1SI") (RVVMF4HI "RVVMF2SI")

  (RVVM4SI "RVVM8DI") (RVVM2SI "RVVM4DI") (RVVM1SI "RVVM2DI") (RVVMF2SI "RVVM1DI")

  (RVVM4HF "RVVM8SF") (RVVM2HF "RVVM4SF") (RVVM1HF "RVVM2SF") (RVVMF2HF "RVVM1SF") (RVVMF4HF "RVVMF2SF")

  (RVVM4SF "RVVM8DF") (RVVM2SF "RVVM4DF") (RVVM1SF "RVVM2DF") (RVVMF2SF "RVVM1DF")

  (V1QI "V1HI")
  (V2QI "V2HI")
  (V4QI "V4HI")
  (V8QI "V8HI")
  (V16QI "V16HI")
  (V32QI "V32HI")
  (V64QI "V64HI")
  (V128QI "V128HI")
  (V256QI "V256HI")
  (V512QI "V512HI")
  (V1024QI "V1024HI")
  (V2048QI "V2048HI")
  (V1HI "V1SI")
  (V2HI "V2SI")
  (V4HI "V4SI")
  (V8HI "V8SI")
  (V16HI "V16SI")
  (V32HI "V32SI")
  (V64HI "V64SI")
  (V128HI "V128SI")
  (V256HI "V256SI")
  (V512HI "V512SI")
  (V1024HI "V1024SI")
  (V1SI "V1SI")
  (V2SI "V2SI")
  (V4SI "V4SI")
  (V8SI "V8SI")
  (V16SI "V16SI")
  (V32SI "V32SI")
  (V64SI "V64SI")
  (V128SI "V128SI")
  (V256SI "V256SI")
  (V512SI "V512SI")
  (V1HF "V1SF")
  (V2HF "V2SF")
  (V4HF "V4SF")
  (V8HF "V8SF")
  (V16HF "V16SF")
  (V32HF "V32SF")
  (V64HF "V64SF")
  (V128HF "V128SF")
  (V256HF "V256SF")
  (V512HF "V512SF")
  (V1024HF "V1024SF")
  (V1SF "V1DF")
  (V2SF "V2DF")
  (V4SF "V4DF")
  (V8SF "V8DF")
  (V16SF "V16DF")
  (V32SF "V32DF")
  (V64SF "V64DF")
  (V128SF "V128DF")
  (V256SF "V256DF")
  (V512SF "V512DF")
])

(define_mode_attr V_DOUBLE_TRUNC [
  (RVVM8HI "RVVM4QI") (RVVM4HI "RVVM2QI") (RVVM2HI "RVVM1QI") (RVVM1HI "RVVMF2QI") (RVVMF2HI "RVVMF4QI") (RVVMF4HI "RVVMF8QI")

  (RVVM8SI "RVVM4HI") (RVVM4SI "RVVM2HI") (RVVM2SI "RVVM1HI") (RVVM1SI "RVVMF2HI") (RVVMF2SI "RVVMF4HI")

  (RVVM8SF "RVVM4HF") (RVVM4SF "RVVM2HF") (RVVM2SF "RVVM1HF") (RVVM1SF "RVVMF2HF") (RVVMF2SF "RVVMF4HF")

  (RVVM8DI "RVVM4SI") (RVVM4DI "RVVM2SI") (RVVM2DI "RVVM1SI") (RVVM1DI "RVVMF2SI")

  (RVVM8DF "RVVM4SF") (RVVM4DF "RVVM2SF") (RVVM2DF "RVVM1SF") (RVVM1DF "RVVMF2SF")

  (V1HI "V1QI")
  (V2HI "V2QI")
  (V4HI "V4QI")
  (V8HI "V8QI")
  (V16HI "V16QI")
  (V32HI "V32QI")
  (V64HI "V64QI")
  (V128HI "V128QI")
  (V256HI "V256QI")
  (V512HI "V512QI")
  (V1024HI "V1024QI")
  (V2048HI "V2048QI")
  (V1SI "V1HI")
  (V2SI "V2HI")
  (V4SI "V4HI")
  (V8SI "V8HI")
  (V16SI "V16HI")
  (V32SI "V32HI")
  (V64SI "V64HI")
  (V128SI "V128HI")
  (V256SI "V256HI")
  (V512SI "V512HI")
  (V1024SI "V1024HI")
  (V1DI "V1SI")
  (V2DI "V2SI")
  (V4DI "V4SI")
  (V8DI "V8SI")
  (V16DI "V16SI")
  (V32DI "V32SI")
  (V64DI "V64SI")
  (V128DI "V128SI")
  (V256DI "V256SI")
  (V512DI "V512SI")
  (V1SF "V1HF")
  (V2SF "V2HF")
  (V4SF "V4HF")
  (V8SF "V8HF")
  (V16SF "V16HF")
  (V32SF "V32HF")
  (V64SF "V64HF")
  (V128SF "V128HF")
  (V256SF "V256HF")
  (V512SF "V512HF")
  (V1024SF "V1024HF")
  (V1DF "V1SF")
  (V2DF "V2SF")
  (V4DF "V4SF")
  (V8DF "V8SF")
  (V16DF "V16SF")
  (V32DF "V32SF")
  (V64DF "V64SF")
  (V128DF "V128SF")
  (V256DF "V256SF")
  (V512DF "V512SF")
])

(define_mode_attr V_QUAD_TRUNC [
  (RVVM8SI "RVVM2QI") (RVVM4SI "RVVM1QI") (RVVM2SI "RVVMF2QI") (RVVM1SI "RVVMF4QI") (RVVMF2SI "RVVMF8QI")

  (RVVM8DI "RVVM2HI") (RVVM4DI "RVVM1HI") (RVVM2DI "RVVMF2HI") (RVVM1DI "RVVMF4HI")

  (RVVM8DF "RVVM2HF") (RVVM4DF "RVVM1HF") (RVVM2DF "RVVMF2HF") (RVVM1DF "RVVMF4HF")

  (V1SI "V1QI")
  (V2SI "V2QI")
  (V4SI "V4QI")
  (V8SI "V8QI")
  (V16SI "V16QI")
  (V32SI "V32QI")
  (V64SI "V64QI")
  (V128SI "V128QI")
  (V256SI "V256QI")
  (V512SI "V512QI")
  (V1024SI "V1024QI")
  (V1DI "V1HI")
  (V2DI "V2HI")
  (V4DI "V4HI")
  (V8DI "V8HI")
  (V16DI "V16HI")
  (V32DI "V32HI")
  (V64DI "V64HI")
  (V128DI "V128HI")
  (V256DI "V256HI")
  (V512DI "V512HI")
  (V1DF "V1HF")
  (V2DF "V2HF")
  (V4DF "V4HF")
  (V8DF "V8HF")
  (V16DF "V16HF")
  (V32DF "V32HF")
  (V64DF "V64HF")
  (V128DF "V128HF")
  (V256DF "V256HF")
  (V512DF "V512HF")
])

(define_mode_attr V_OCT_TRUNC [
  (RVVM8DI "RVVM1QI") (RVVM4DI "RVVMF2QI") (RVVM2DI "RVVMF4QI") (RVVM1DI "RVVMF8QI")

  (V1DI "V1QI")
  (V2DI "V2QI")
  (V4DI "V4QI")
  (V8DI "V8QI")
  (V16DI "V16QI")
  (V32DI "V32QI")
  (V64DI "V64QI")
  (V128DI "V128QI")
  (V256DI "V256QI")
  (V512DI "V512QI")
])

; Again in lower case.
(define_mode_attr v_double_trunc [
  (RVVM8HI "rvvm4qi") (RVVM4HI "rvvm2qi") (RVVM2HI "rvvm1qi") (RVVM1HI "rvvmf2qi") (RVVMF2HI "rvvmf4qi") (RVVMF4HI "rvvmf8qi")

  (RVVM8SI "rvvm4hi") (RVVM4SI "rvvm2hi") (RVVM2SI "rvvm1hi") (RVVM1SI "rvvmf2hi") (RVVMF2SI "rvvmf4hi")

  (RVVM8SF "rvvm4hf") (RVVM4SF "rvvm2hf") (RVVM2SF "rvvm1hf") (RVVM1SF "rvvmf2hf") (RVVMF2SF "rvvmf4hf")

  (RVVM8DI "rvvm4si") (RVVM4DI "rvvm2si") (RVVM2DI "rvvm1si") (RVVM1DI "rvvmf2si")

  (RVVM8DF "rvvm4sf") (RVVM4DF "rvvm2sf") (RVVM2DF "rvvm1sf") (RVVM1DF "rvvmf2sf")

  (V1HI "v1qi")
  (V2HI "v2qi")
  (V4HI "v4qi")
  (V8HI "v8qi")
  (V16HI "v16qi")
  (V32HI "v32qi")
  (V64HI "v64qi")
  (V128HI "v128qi")
  (V256HI "v256qi")
  (V512HI "v512qi")
  (V1024HI "v1024qi")
  (V2048HI "v2048qi")
  (V1SI "v1hi")
  (V2SI "v2hi")
  (V4SI "v4hi")
  (V8SI "v8hi")
  (V16SI "v16hi")
  (V32SI "v32hi")
  (V64SI "v64hi")
  (V128SI "v128hi")
  (V256SI "v256hi")
  (V512SI "v512hi")
  (V1024SI "v1024hi")
  (V1DI "v1si")
  (V2DI "v2si")
  (V4DI "v4si")
  (V8DI "v8si")
  (V16DI "v16si")
  (V32DI "v32si")
  (V64DI "v64si")
  (V128DI "v128si")
  (V256DI "v256si")
  (V512DI "v512si")
  (V1SF "v1hf")
  (V2SF "v2hf")
  (V4SF "v4hf")
  (V8SF "v8hf")
  (V16SF "v16hf")
  (V32SF "v32hf")
  (V64SF "v64hf")
  (V128SF "v128hf")
  (V256SF "v256hf")
  (V512SF "v512hf")
  (V1024SF "v1024hf")
  (V1DF "v1sf")
  (V2DF "v2sf")
  (V4DF "v4sf")
  (V8DF "v8sf")
  (V16DF "v16sf")
  (V32DF "v32sf")
  (V64DF "v64sf")
  (V128DF "v128sf")
  (V256DF "v256sf")
  (V512DF "v512sf")
])

(define_mode_attr v_quad_trunc [
  (RVVM8SI "rvvm2qi") (RVVM4SI "rvvm1qi") (RVVM2SI "rvvmf2qi") (RVVM1SI "rvvmf4qi") (RVVMF2SI "rvvmf8qi")

  (RVVM8DI "rvvm2hi") (RVVM4DI "rvvm1hi") (RVVM2DI "rvvmf2hi") (RVVM1DI "rvvmf4hi")

  (RVVM8DF "rvvm2hf") (RVVM4DF "rvvm1hf") (RVVM2DF "rvvmf2hf") (RVVM1DF "rvvmf4hf")

  (V1SI "v1qi")
  (V2SI "v2qi")
  (V4SI "v4qi")
  (V8SI "v8qi")
  (V16SI "v16qi")
  (V32SI "v32qi")
  (V64SI "v64qi")
  (V128SI "v128qi")
  (V256SI "v256qi")
  (V512SI "v512qi")
  (V1024SI "v1024qi")
  (V1DI "v1hi")
  (V2DI "v2hi")
  (V4DI "v4hi")
  (V8DI "v8hi")
  (V16DI "v16hi")
  (V32DI "v32hi")
  (V64DI "v64hi")
  (V128DI "v128hi")
  (V256DI "v256hi")
  (V512DI "v512hi")
  (V1DF "v1hf")
  (V2DF "v2hf")
  (V4DF "v4hf")
  (V8DF "v8hf")
  (V16DF "v16hf")
  (V32DF "v32hf")
  (V64DF "v64hf")
  (V128DF "v128hf")
  (V256DF "v256hf")
  (V512DF "v512hf")
])

(define_mode_attr v_oct_trunc [
  (RVVM8DI "rvvm1qi") (RVVM4DI "rvvmf2qi") (RVVM2DI "rvvmf4qi") (RVVM1DI "rvvmf8qi")

  (V1DI "v1qi")
  (V2DI "v2qi")
  (V4DI "v4qi")
  (V8DI "v8qi")
  (V16DI "v16qi")
  (V32DI "v32qi")
  (V64DI "v64qi")
  (V128DI "v128qi")
  (V256DI "v256qi")
  (V512DI "v512qi")
])

(define_mode_attr VINDEX_DOUBLE_TRUNC [
  (RVVM8HI "RVVM4QI") (RVVM4HI "RVVM2QI") (RVVM2HI "RVVM1QI") (RVVM1HI "RVVMF2QI") (RVVMF2HI "RVVMF4QI") (RVVMF4HI "RVVMF8QI")

  (RVVM8BF "RVVM4QI") (RVVM4BF "RVVM2QI") (RVVM2BF "RVVM1QI") (RVVM1BF "RVVMF2QI") (RVVMF2BF "RVVMF4QI") (RVVMF4BF "RVVMF8QI")

  (RVVM8HF "RVVM4QI") (RVVM4HF "RVVM2QI") (RVVM2HF "RVVM1QI") (RVVM1HF "RVVMF2QI") (RVVMF2HF "RVVMF4QI") (RVVMF4HF "RVVMF8QI")

  (RVVM8SI "RVVM4HI") (RVVM4SI "RVVM2HI") (RVVM2SI "RVVM1HI") (RVVM1SI "RVVMF2HI") (RVVMF2SI "RVVMF4HI")

  (RVVM8SF "RVVM4HI") (RVVM4SF "RVVM2HI") (RVVM2SF "RVVM1HI") (RVVM1SF "RVVMF2HI") (RVVMF2SF "RVVMF4HI")

  (RVVM8DI "RVVM4SI") (RVVM4DI "RVVM2SI") (RVVM2DI "RVVM1SI") (RVVM1DI "RVVMF2SI")

  (RVVM8DF "RVVM4SI") (RVVM4DF "RVVM2SI") (RVVM2DF "RVVM1SI") (RVVM1DF "RVVMF2SI")
])

(define_mode_attr VINDEX_QUAD_TRUNC [
  (RVVM8SI "RVVM2QI") (RVVM4SI "RVVM1QI") (RVVM2SI "RVVMF2QI") (RVVM1SI "RVVMF4QI") (RVVMF2SI "RVVMF8QI")

  (RVVM8SF "RVVM2QI") (RVVM4SF "RVVM1QI") (RVVM2SF "RVVMF2QI") (RVVM1SF "RVVMF4QI") (RVVMF2SF "RVVMF8QI")

  (RVVM8DI "RVVM2HI") (RVVM4DI "RVVM1HI") (RVVM2DI "RVVMF2HI") (RVVM1DI "RVVMF4HI")

  (RVVM8DF "RVVM2HI") (RVVM4DF "RVVM1HI") (RVVM2DF "RVVMF2HI") (RVVM1DF "RVVMF4HI")
])

(define_mode_attr VINDEX_OCT_TRUNC [
  (RVVM8DI "RVVM1QI") (RVVM4DI "RVVMF2QI") (RVVM2DI "RVVMF4QI") (RVVM1DI "RVVMF8QI")

  (RVVM8DF "RVVM1QI") (RVVM4DF "RVVMF2QI") (RVVM2DF "RVVMF4QI") (RVVM1DF "RVVMF8QI")
])

(define_mode_attr VINDEX_DOUBLE_EXT [
  (RVVM4QI "RVVM8HI") (RVVM2QI "RVVM4HI") (RVVM1QI "RVVM2HI") (RVVMF2QI "RVVM1HI") (RVVMF4QI "RVVMF2HI") (RVVMF8QI "RVVMF4HI")

  (RVVM4HI "RVVM8SI") (RVVM2HI "RVVM4SI") (RVVM1HI "RVVM2SI") (RVVMF2HI "RVVM1SI") (RVVMF4HI "RVVMF2SI")

  (RVVM4BF "RVVM8SI") (RVVM2BF "RVVM4SI") (RVVM1BF "RVVM2SI") (RVVMF2BF "RVVM1SI") (RVVMF4BF "RVVMF2SI")

  (RVVM4HF "RVVM8SI") (RVVM2HF "RVVM4SI") (RVVM1HF "RVVM2SI") (RVVMF2HF "RVVM1SI") (RVVMF4HF "RVVMF2SI")

  (RVVM4SI "RVVM8DI") (RVVM2SI "RVVM4DI") (RVVM1SI "RVVM2DI") (RVVMF2SI "RVVM1DI")

  (RVVM4SF "RVVM8DI") (RVVM2SF "RVVM4DI") (RVVM1SF "RVVM2DI") (RVVMF2SF "RVVM1DI")
])

(define_mode_attr VINDEX_QUAD_EXT [
  (RVVM2QI "RVVM8SI") (RVVM1QI "RVVM4SI") (RVVMF2QI "RVVM2SI") (RVVMF4QI "RVVM1SI") (RVVMF8QI "RVVMF2SI")

  (RVVM2HI "RVVM8DI") (RVVM1HI "RVVM4DI") (RVVMF2HI "RVVM2DI") (RVVMF4HI "RVVM1DI")

  (RVVM2BF "RVVM8DI") (RVVM1BF "RVVM4DI") (RVVMF2BF "RVVM2DI") (RVVMF4BF "RVVM1DI")

  (RVVM2HF "RVVM8DI") (RVVM1HF "RVVM4DI") (RVVMF2HF "RVVM2DI") (RVVMF4HF "RVVM1DI")
])

(define_mode_attr VINDEX_OCT_EXT [
  (RVVM1QI "RVVM8DI") (RVVMF2QI "RVVM4DI") (RVVMF4QI "RVVM2DI") (RVVMF8QI "RVVM1DI")
])

(define_mode_attr VCONVERT [
  (RVVM8HF "RVVM8HI") (RVVM4HF "RVVM4HI") (RVVM2HF "RVVM2HI") (RVVM1HF "RVVM1HI") (RVVMF2HF "RVVMF2HI") (RVVMF4HF "RVVMF4HI")
  (RVVM8SF "RVVM8SI") (RVVM4SF "RVVM4SI") (RVVM2SF "RVVM2SI") (RVVM1SF "RVVM1SI") (RVVMF2SF "RVVMF2SI")
  (RVVM8DF "RVVM8DI") (RVVM4DF "RVVM4DI") (RVVM2DF "RVVM2DI") (RVVM1DF "RVVM1DI")

  (V1HF "V1HI")
  (V2HF "V2HI")
  (V4HF "V4HI")
  (V8HF "V8HI")
  (V16HF "V16HI")
  (V32HF "V32HI")
  (V64HF "V64HI")
  (V128HF "V128HI")
  (V256HF "V256HI")
  (V512HF "V512HI")
  (V1024HF "V1024HI")
  (V2048HF "V2048HI")
  (V1SF "V1SI")
  (V2SF "V2SI")
  (V4SF "V4SI")
  (V8SF "V8SI")
  (V16SF "V16SI")
  (V32SF "V32SI")
  (V64SF "V64SI")
  (V128SF "V128SI")
  (V256SF "V256SI")
  (V512SF "V512SI")
  (V1024SF "V1024SI")
  (V1DF "V1DI")
  (V2DF "V2DI")
  (V4DF "V4DI")
  (V8DF "V8DI")
  (V16DF "V16DI")
  (V32DF "V32DI")
  (V64DF "V64DI")
  (V128DF "V128DI")
  (V256DF "V256DI")
  (V512DF "V512DI")
])

(define_mode_attr vconvert [
  (RVVM8HF "rvvm8hi") (RVVM4HF "rvvm4hi") (RVVM2HF "rvvm2hi") (RVVM1HF "rvvm1hi") (RVVMF2HF "rvvmf2hi") (RVVMF4HF "rvvmf4hi")
  (RVVM8SF "rvvm8si") (RVVM4SF "rvvm4si") (RVVM2SF "rvvm2si") (RVVM1SF "rvvm1si") (RVVMF2SF "rvvmf2si")
  (RVVM8DF "rvvm8di") (RVVM4DF "rvvm4di") (RVVM2DF "rvvm2di") (RVVM1DF "rvvm1di")

  (V1HF "v1hi")
  (V2HF "v2hi")
  (V4HF "v4hi")
  (V8HF "v8hi")
  (V16HF "v16hi")
  (V32HF "v32hi")
  (V64HF "v64hi")
  (V128HF "v128hi")
  (V256HF "v256hi")
  (V512HF "v512hi")
  (V1024HF "v1024hi")
  (V2048HF "v2048hi")
  (V1SF "v1si")
  (V2SF "v2si")
  (V4SF "v4si")
  (V8SF "v8si")
  (V16SF "v16si")
  (V32SF "v32si")
  (V64SF "v64si")
  (V128SF "v128si")
  (V256SF "v256si")
  (V512SF "v512si")
  (V1024SF "v1024si")
  (V1DF "v1di")
  (V2DF "v2di")
  (V4DF "v4di")
  (V8DF "v8di")
  (V16DF "v16di")
  (V32DF "v32di")
  (V64DF "v64di")
  (V128DF "v128di")
  (V256DF "v256di")
  (V512DF "v512di")
])

(define_mode_attr VNCONVERT [
  (RVVM8HF "RVVM4QI") (RVVM4HF "RVVM2QI") (RVVM2HF "RVVM1QI") (RVVM1HF "RVVMF2QI") (RVVMF2HF "RVVMF4QI") (RVVMF4HF "RVVMF8QI")

  (RVVM8SI "RVVM4HF") (RVVM4SI "RVVM2HF") (RVVM2SI "RVVM1HF") (RVVM1SI "RVVMF2HF") (RVVMF2SI "RVVMF4HF")
  (RVVM8SF "RVVM4HI") (RVVM4SF "RVVM2HI") (RVVM2SF "RVVM1HI") (RVVM1SF "RVVMF2HI") (RVVMF2SF "RVVMF4HI")

  (RVVM8DI "RVVM4SF") (RVVM4DI "RVVM2SF") (RVVM2DI "RVVM1SF") (RVVM1DI "RVVMF2SF")
  (RVVM8DF "RVVM4SI") (RVVM4DF "RVVM2SI") (RVVM2DF "RVVM1SI") (RVVM1DF "RVVMF2SI")

  (V1SI "V1HF")
  (V2SI "V2HF")
  (V4SI "V4HF")
  (V8SI "V8HF")
  (V16SI "V16HF")
  (V32SI "V32HF")
  (V64SI "V64HF")
  (V128SI "V128HF")
  (V256SI "V256HF")
  (V512SI "V512HF")
  (V1024SI "V1024HF")
  (V1DI "V1SF")
  (V2DI "V2SF")
  (V4DI "V4SF")
  (V8DI "V8SF")
  (V16DI "V16SF")
  (V32DI "V32SF")
  (V64DI "V64SF")
  (V128DI "V128SF")
  (V256DI "V256SF")
  (V512DI "V512SF")

  (V1HF "V1QI")
  (V2HF "V2QI")
  (V4HF "V4QI")
  (V8HF "V8QI")
  (V16HF "V16QI")
  (V32HF "V32QI")
  (V64HF "V64QI")
  (V128HF "V128QI")
  (V256HF "V256QI")
  (V512HF "V512QI")
  (V1024HF "V1024QI")
  (V2048HF "V2048QI")
  (V1SF "V1HI")
  (V2SF "V2HI")
  (V4SF "V4HI")
  (V8SF "V8HI")
  (V16SF "V16HI")
  (V32SF "V32HI")
  (V64SF "V64HI")
  (V128SF "V128HI")
  (V256SF "V256HI")
  (V512SF "V512HI")
  (V1024SF "V1024HI")
  (V1DF "V1SI")
  (V2DF "V2SI")
  (V4DF "V4SI")
  (V8DF "V8SI")
  (V16DF "V16SI")
  (V32DF "V32SI")
  (V64DF "V64SI")
  (V128DF "V128SI")
  (V256DF "V256SI")
  (V512DF "V512SI")
])

(define_mode_attr vnconvert [
  (RVVM8HF "rvvm4qi") (RVVM4HF "rvvm2qi") (RVVM2HF "rvvm1qi") (RVVM1HF "rvvmf2qi") (RVVMF2HF "rvvmf4qi") (RVVMF4HF "rvvmf8qi")

  (RVVM8SI "rvvm4hf") (RVVM4SI "rvvm2hf") (RVVM2SI "rvvm1hf") (RVVM1SI "rvvmf2hf") (RVVMF2SI "rvvmf4hf")
  (RVVM8SF "rvvm4hi") (RVVM4SF "rvvm2hi") (RVVM2SF "rvvm1hi") (RVVM1SF "rvvmf2hi") (RVVMF2SF "rvvmf4hi")

  (RVVM8DI "rvvm4sf") (RVVM4DI "rvvm2sf") (RVVM2DI "rvvm1sf") (RVVM1DI "rvvmf2sf")
  (RVVM8DF "rvvm4si") (RVVM4DF "rvvm2si") (RVVM2DF "rvvm1si") (RVVM1DF "rvvmf2si")

  (V1SI "v1hf")
  (V2SI "v2hf")
  (V4SI "v4hf")
  (V8SI "v8hf")
  (V16SI "v16hf")
  (V32SI "v32hf")
  (V64SI "v64hf")
  (V128SI "v128hf")
  (V256SI "v256hf")
  (V512SI "v512hf")
  (V1024SI "v1024hf")
  (V1DI "v1sf")
  (V2DI "v2sf")
  (V4DI "v4sf")
  (V8DI "v8sf")
  (V16DI "v16sf")
  (V32DI "v32sf")
  (V64DI "v64sf")
  (V128DI "v128sf")
  (V256DI "v256sf")
  (V512DI "v512sf")

  (V1HF "v1qi")
  (V2HF "v2qi")
  (V4HF "v4qi")
  (V8HF "v8qi")
  (V16HF "v16qi")
  (V32HF "v32qi")
  (V64HF "v64qi")
  (V128HF "v128qi")
  (V256HF "v256qi")
  (V512HF "v512qi")
  (V1024HF "v1024qi")
  (V2048HF "v2048qi")
  (V1SF "v1hi")
  (V2SF "v2hi")
  (V4SF "v4hi")
  (V8SF "v8hi")
  (V16SF "v16hi")
  (V32SF "v32hi")
  (V64SF "v64hi")
  (V128SF "v128hi")
  (V256SF "v256hi")
  (V512SF "v512hi")
  (V1024SF "v1024hi")
  (V1DF "v1si")
  (V2DF "v2si")
  (V4DF "v4si")
  (V8DF "v8si")
  (V16DF "v16si")
  (V32DF "v32si")
  (V64DF "v64si")
  (V128DF "v128si")
  (V256DF "v256si")
  (V512DF "v512si")
])

;; NN indicates narrow twice
(define_mode_attr VNNCONVERT [
  (RVVM8DI "RVVM2HF") (RVVM4DI "RVVM1HF") (RVVM2DI "RVVMF2HF")
  (RVVM1DI "RVVMF4HF")

  (V1DI "V1HF") (V2DI "V2HF") (V4DI "V4HF") (V8DI "V8HF") (V16DI "V16HF")
  (V32DI "V32HF") (V64DI "V64HF") (V128DI "V128HF") (V256DI "V256HF")
  (V512DI "V512HF")
])

;; nn indicates narrow twice
(define_mode_attr vnnconvert [
  (RVVM8DI "rvvm2hf")  (RVVM4DI "rvvm1hf") (RVVM2DI "rvvmf2hf")
  (RVVM1DI "rvvmf4hf")

  (V1DI "v1hf") (V2DI "v2hf") (V4DI "v4hf") (V8DI "v8hf") (V16DI "v16hf")
  (V32DI "v32hf") (V64DI "v64hf") (V128DI "v128hf") (V256DI "v256hf")
  (V512DI "v512hf")
])

;;
;; Convert float (VHF, VSF, VDF) to VSI/VDI.
;; The are sorts of rounding mode return integer (take rint as example)
;; - irint
;; - lrint
;; - llrint
;;
;; The long type has different bitsize in RV32 and RV64 makes them even
;; more complicated, details as below.
;; +------------+------------------+------------------+
;; | builtin    | RV32             | RV64             |
;; +------------+------------------+------------------+
;; | lrintf16   | HF => SI         | HF => DI         |
;; +------------+------------------+------------------+
;; | lrintf     | SF => SI         | SF => DI         |
;; +------------+------------------+------------------+
;; | lrint      | DF => SI         | DF => DI         |
;; +------------+------------------+------------------+
;; | llrintf16  | HF => DI         | Same as RV32     |
;; +------------+------------------+------------------+
;; | llrintf    | SF => DI         | Same as RV32     |
;; +------------+------------------+------------------+
;; | llrint     | DF => DI         | Same as RV32     |
;; +------------+------------------+------------------+
;; | irintf16   | HF => SI         | Same as RV32     |
;; +------------+------------------+------------------+
;; | irintf     | SF => SI         | Same as RV32     |
;; +------------+------------------+------------------+
;; | irint      | DF => SI         | Same as RV32     |
;; +------------+------------------+------------------+
;;
;; The [i/l/ll]rint share the same standard name lrint<m><n>,
;; and both the RV32 and RV64 has the cases to the SI and DI.
;; For example, both RV32 and RV64 has the below convert:
;;
;; HF => SI (RV32: lrintf16)  (RV64: irintf16)
;; HF => DI (RV32: llrintf16) (RV64: lrintf16)
;;
;; Due to we cannot define a mode_attr mapping one HF to both
;; the SI and DI, we use 2 different mode_atter to cover all
;; the combination as above, as well as the different iterator
;; for the lrint<m><n> patterns. Aka:
;;
;; V_F2SI_CONVERT: (HF, SF, DF) => SI
;; V_F2DI_CONVERT: (HF, SF, DF) => DI
;;
(define_mode_attr V_F2SI_CONVERT [
  (RVVM4HF "RVVM8SI") (RVVM2HF "RVVM4SI") (RVVM1HF "RVVM2SI")
  (RVVMF2HF "RVVM1SI") (RVVMF4HF "RVVMF2SI")

  (RVVM8SF "RVVM8SI") (RVVM4SF "RVVM4SI") (RVVM2SF "RVVM2SI")
  (RVVM1SF "RVVM1SI") (RVVMF2SF "RVVMF2SI")

  (RVVM8DF "RVVM4SI") (RVVM4DF "RVVM2SI") (RVVM2DF "RVVM1SI")
  (RVVM1DF "RVVMF2SI")

  (V1HF "V1SI") (V2HF "V2SI") (V4HF "V4SI") (V8HF "V8SI") (V16HF "V16SI")
  (V32HF "V32SI") (V64HF "V64SI") (V128HF "V128SI") (V256HF "V256SI")
  (V512HF "V512SI") (V1024HF "V1024SI")

  (V1SF "V1SI") (V2SF "V2SI") (V4SF "V4SI") (V8SF "V8SI") (V16SF "V16SI")
  (V32SF "V32SI") (V64SF "V64SI") (V128SF "V128SI") (V256SF "V256SI")
  (V512SF "V512SI") (V1024SF "V1024SI")

  (V1DF "V1SI") (V2DF "V2SI") (V4DF "V4SI") (V8DF "V8SI") (V16DF "V16SI")
  (V32DF "V32SI") (V64DF "V64SI") (V128DF "V128SI") (V256DF "V256SI")
  (V512DF "V512SI")
])

(define_mode_attr v_f2si_convert [
  (RVVM4HF "rvvm8si") (RVVM2HF "rvvm4si") (RVVM1HF "rvvm2si")
  (RVVMF2HF "rvvm1si") (RVVMF4HF "rvvmf2si")

  (RVVM8SF "rvvm8si") (RVVM4SF "rvvm4si") (RVVM2SF "rvvm2si")
  (RVVM1SF "rvvm1si") (RVVMF2SF "rvvmf2si")

  (RVVM8DF "rvvm4si") (RVVM4DF "rvvm2si") (RVVM2DF "rvvm1si")
  (RVVM1DF "rvvmf2si")

  (V1HF "v1si") (V2HF "v2si") (V4HF "v4si") (V8HF "v8si") (V16HF "v16si")
  (V32HF "v32si") (V64HF "v64si") (V128HF "v128si") (V256HF "v256si")
  (V512HF "v512si") (V1024HF "v1024si")

  (V1SF "v1si") (V2SF "v2si") (V4SF "v4si") (V8SF "v8si") (V16SF "v16si")
  (V32SF "v32si") (V64SF "v64si") (V128SF "v128si") (V256SF "v256si")
  (V512SF "v512si") (V1024SF "v1024si")

  (V1DF "v1si") (V2DF "v2si") (V4DF "v4si") (V8DF "v8si") (V16DF "v16si")
  (V32DF "v32si") (V64DF "v64si") (V128DF "v128si") (V256DF "v256si")
  (V512DF "v512si")
])

(define_mode_iterator V_VLS_F_CONVERT_SI [
  (RVVM4HF "TARGET_ZVFH") (RVVM2HF "TARGET_ZVFH") (RVVM1HF "TARGET_ZVFH")
  (RVVMF2HF "TARGET_ZVFH") (RVVMF4HF "TARGET_ZVFH && TARGET_VECTOR_ELEN_64")

  (RVVM8SF "TARGET_VECTOR_ELEN_FP_32") (RVVM4SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM2SF "TARGET_VECTOR_ELEN_FP_32") (RVVM1SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVMF2SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_VECTOR_ELEN_64")

  (RVVM8DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM4DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM2DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM1DF "TARGET_VECTOR_ELEN_FP_64")

  (V1HF "riscv_vector::vls_mode_valid_p (V1HFmode) && TARGET_ZVFH")
  (V2HF "riscv_vector::vls_mode_valid_p (V2HFmode) && TARGET_ZVFH")
  (V4HF "riscv_vector::vls_mode_valid_p (V4HFmode) && TARGET_ZVFH")
  (V8HF "riscv_vector::vls_mode_valid_p (V8HFmode) && TARGET_ZVFH")
  (V16HF "riscv_vector::vls_mode_valid_p (V16HFmode) && TARGET_ZVFH")
  (V32HF "riscv_vector::vls_mode_valid_p (V32HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 64")
  (V64HF "riscv_vector::vls_mode_valid_p (V64HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 128")
  (V128HF "riscv_vector::vls_mode_valid_p (V128HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 256")
  (V256HF "riscv_vector::vls_mode_valid_p (V256HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 512")
  (V512HF "riscv_vector::vls_mode_valid_p (V512HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 1024")
  (V1024HF "riscv_vector::vls_mode_valid_p (V1024HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 2048")

  (V1SF "riscv_vector::vls_mode_valid_p (V1SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V2SF "riscv_vector::vls_mode_valid_p (V2SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V4SF "riscv_vector::vls_mode_valid_p (V4SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V8SF "riscv_vector::vls_mode_valid_p (V8SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V16SF "riscv_vector::vls_mode_valid_p (V16SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 64")
  (V32SF "riscv_vector::vls_mode_valid_p (V32SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 128")
  (V64SF "riscv_vector::vls_mode_valid_p (V64SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 256")
  (V128SF "riscv_vector::vls_mode_valid_p (V128SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 512")
  (V256SF "riscv_vector::vls_mode_valid_p (V256SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 1024")
  (V512SF "riscv_vector::vls_mode_valid_p (V512SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 2048")
  (V1024SF "riscv_vector::vls_mode_valid_p (V1024SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 4096")

  (V1DF "riscv_vector::vls_mode_valid_p (V1DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V2DF "riscv_vector::vls_mode_valid_p (V2DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V4DF "riscv_vector::vls_mode_valid_p (V4DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V8DF "riscv_vector::vls_mode_valid_p (V8DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 64")
  (V16DF "riscv_vector::vls_mode_valid_p (V16DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 128")
  (V32DF "riscv_vector::vls_mode_valid_p (V32DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 256")
  (V64DF "riscv_vector::vls_mode_valid_p (V64DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 512")
  (V128DF "riscv_vector::vls_mode_valid_p (V128DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 1024")
  (V256DF "riscv_vector::vls_mode_valid_p (V256DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 2048")
  (V512DF "riscv_vector::vls_mode_valid_p (V512DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 4096")
])

(define_mode_attr V_F2DI_CONVERT [
  (RVVM2HF "RVVM8DI") (RVVM1HF "RVVM4DI") (RVVMF2HF "RVVM2DI")
  (RVVMF4HF "RVVM1DI")

  (RVVM4SF "RVVM8DI") (RVVM2SF "RVVM4DI") (RVVM1SF "RVVM2DI")
  (RVVMF2SF "RVVM1DI")

  (RVVM8DF "RVVM8DI") (RVVM4DF "RVVM4DI") (RVVM2DF "RVVM2DI")
  (RVVM1DF "RVVM1DI")

  (V1HF "V1DI") (V2HF "V2DI") (V4HF "V4DI") (V8HF "V8DI") (V16HF "V16DI")
  (V32HF "V32DI") (V64HF "V64DI") (V128HF "V128DI") (V256HF "V256DI")
  (V512HF "V512DI")

  (V1SF "V1DI") (V2SF "V2DI") (V4SF "V4DI") (V8SF "V8DI") (V16SF "V16DI")
  (V32SF "V32DI") (V64SF "V64DI") (V128SF "V128DI") (V256SF "V256DI")
  (V512SF "V512DI")

  (V1DF "V1DI") (V2DF "V2DI") (V4DF "V4DI") (V8DF "V8DI") (V16DF "V16DI")
  (V32DF "V32DI") (V64DF "V64DI") (V128DF "V128DI") (V256DF "V256DI")
  (V512DF "V512DI")
])

(define_mode_attr v_f2di_convert [
  (RVVM2HF "rvvm8di") (RVVM1HF "rvvm4di") (RVVMF2HF "rvvm2di")
  (RVVMF4HF "rvvm1di")

  (RVVM4SF "rvvm8di") (RVVM2SF "rvvm4di") (RVVM1SF "rvvm2di")
  (RVVMF2SF "rvvm1di")

  (RVVM8DF "rvvm8di") (RVVM4DF "rvvm4di") (RVVM2DF "rvvm2di")
  (RVVM1DF "rvvm1di")

  (V1HF "v1di") (V2HF "v2di") (V4HF "v4di") (V8HF "v8di") (V16HF "v16di")
  (V32HF "v32di") (V64HF "v64di") (V128HF "v128di") (V256HF "v256di")
  (V512HF "v512di")

  (V1SF "v1di") (V2SF "v2di") (V4SF "v4di") (V8SF "v8di") (V16SF "v16di")
  (V32SF "v32di") (V64SF "v64di") (V128SF "v128di") (V256SF "v256di")
  (V512SF "v512di")

  (V1DF "v1di") (V2DF "v2di") (V4DF "v4di") (V8DF "v8di") (V16DF "v16di")
  (V32DF "v32di") (V64DF "v64di") (V128DF "v128di") (V256DF "v256di")
  (V512DF "v512di")
])

(define_mode_attr V_F2DI_CONVERT_BRIDGE [
  (RVVM2HF "RVVM4SF") (RVVM1HF "RVVM2SF") (RVVMF2HF "RVVM1SF")
  (RVVMF4HF "RVVMF2SF")

  (RVVM4SF "VOID") (RVVM2SF "VOID") (RVVM1SF "VOID")
  (RVVMF2SF "VOID")

  (RVVM8DF "VOID") (RVVM4DF "VOID") (RVVM2DF "VOID")
  (RVVM1DF "VOID")

  (V1HF "V1SF") (V2HF "V2SF") (V4HF "V4SF") (V8HF "V8SF") (V16HF "V16SF")
  (V32HF "V32SF") (V64HF "V64SF") (V128HF "V128SF") (V256HF "V256SF")
  (V512HF "V512SF")

  (V1SF "VOID") (V2SF "VOID") (V4SF "VOID") (V8SF "VOID") (V16SF "VOID")
  (V32SF "VOID") (V64SF "VOID") (V128SF "VOID") (V256SF "VOID")
  (V512SF "VOID")

  (V1DF "VOID") (V2DF "VOID") (V4DF "VOID") (V8DF "VOID") (V16DF "VOID")
  (V32DF "VOID") (V64DF "VOID") (V128DF "VOID") (V256DF "VOID")
  (V512DF "VOID")
])

(define_mode_iterator V_VLS_F_CONVERT_DI [
  (RVVM2HF "TARGET_ZVFH") (RVVM1HF "TARGET_ZVFH") (RVVMF2HF "TARGET_ZVFH")
  (RVVMF4HF "TARGET_ZVFH && TARGET_VECTOR_ELEN_64")

  (RVVM4SF "TARGET_VECTOR_ELEN_FP_32") (RVVM2SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVM1SF "TARGET_VECTOR_ELEN_FP_32")
  (RVVMF2SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_VECTOR_ELEN_64")

  (RVVM8DF "TARGET_VECTOR_ELEN_FP_64") (RVVM4DF "TARGET_VECTOR_ELEN_FP_64")
  (RVVM2DF "TARGET_VECTOR_ELEN_FP_64") (RVVM1DF "TARGET_VECTOR_ELEN_FP_64")

  (V1HF "riscv_vector::vls_mode_valid_p (V1HFmode) && TARGET_ZVFH")
  (V2HF "riscv_vector::vls_mode_valid_p (V2HFmode) && TARGET_ZVFH")
  (V4HF "riscv_vector::vls_mode_valid_p (V4HFmode) && TARGET_ZVFH")
  (V8HF "riscv_vector::vls_mode_valid_p (V8HFmode) && TARGET_ZVFH")
  (V16HF "riscv_vector::vls_mode_valid_p (V16HFmode) && TARGET_ZVFH")
  (V32HF "riscv_vector::vls_mode_valid_p (V32HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 64")
  (V64HF "riscv_vector::vls_mode_valid_p (V64HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 128")
  (V128HF "riscv_vector::vls_mode_valid_p (V128HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 256")
  (V256HF "riscv_vector::vls_mode_valid_p (V256HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 512")
  (V512HF "riscv_vector::vls_mode_valid_p (V512HFmode) && TARGET_ZVFH && TARGET_MIN_VLEN >= 1024")

  (V1SF "riscv_vector::vls_mode_valid_p (V1SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V2SF "riscv_vector::vls_mode_valid_p (V2SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V4SF "riscv_vector::vls_mode_valid_p (V4SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V8SF "riscv_vector::vls_mode_valid_p (V8SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V16SF "riscv_vector::vls_mode_valid_p (V16SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 64")
  (V32SF "riscv_vector::vls_mode_valid_p (V32SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 128")
  (V64SF "riscv_vector::vls_mode_valid_p (V64SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 256")
  (V128SF "riscv_vector::vls_mode_valid_p (V128SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 512")
  (V256SF "riscv_vector::vls_mode_valid_p (V256SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 1024")
  (V512SF "riscv_vector::vls_mode_valid_p (V512SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 2048")

  (V1DF "riscv_vector::vls_mode_valid_p (V1DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V2DF "riscv_vector::vls_mode_valid_p (V2DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V4DF "riscv_vector::vls_mode_valid_p (V4DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V8DF "riscv_vector::vls_mode_valid_p (V8DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 64")
  (V16DF "riscv_vector::vls_mode_valid_p (V16DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 128")
  (V32DF "riscv_vector::vls_mode_valid_p (V32DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 256")
  (V64DF "riscv_vector::vls_mode_valid_p (V64DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 512")
  (V128DF "riscv_vector::vls_mode_valid_p (V128DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 1024")
  (V256DF "riscv_vector::vls_mode_valid_p (V256DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 2048")
  (V512DF "riscv_vector::vls_mode_valid_p (V512DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 4096")
])

(define_mode_attr stride_predicate [
  (RVVM8QI "vector_eew8_stride_operand") (RVVM4QI "vector_eew8_stride_operand")
  (RVVM2QI "vector_eew8_stride_operand") (RVVM1QI "vector_eew8_stride_operand")
  (RVVMF2QI "vector_eew8_stride_operand") (RVVMF4QI "vector_eew8_stride_operand")
  (RVVMF8QI "vector_eew8_stride_operand")

  (RVVM8HI "vector_eew16_stride_operand") (RVVM4HI "vector_eew16_stride_operand")
  (RVVM2HI "vector_eew16_stride_operand") (RVVM1HI "vector_eew16_stride_operand")
  (RVVMF2HI "vector_eew16_stride_operand") (RVVMF4HI "vector_eew16_stride_operand")

  (RVVM8BF "vector_eew16_stride_operand") (RVVM4BF "vector_eew16_stride_operand")
  (RVVM2BF "vector_eew16_stride_operand") (RVVM1BF "vector_eew16_stride_operand")
  (RVVMF2BF "vector_eew16_stride_operand") (RVVMF4BF "vector_eew16_stride_operand")

  (RVVM8HF "vector_eew16_stride_operand") (RVVM4HF "vector_eew16_stride_operand")
  (RVVM2HF "vector_eew16_stride_operand") (RVVM1HF "vector_eew16_stride_operand")
  (RVVMF2HF "vector_eew16_stride_operand") (RVVMF4HF "vector_eew16_stride_operand")

  (RVVM8SI "vector_eew32_stride_operand") (RVVM4SI "vector_eew32_stride_operand")
  (RVVM2SI "vector_eew32_stride_operand") (RVVM1SI "vector_eew32_stride_operand")
  (RVVMF2SI "vector_eew32_stride_operand")

  (RVVM8SF "vector_eew32_stride_operand") (RVVM4SF "vector_eew32_stride_operand")
  (RVVM2SF "vector_eew32_stride_operand") (RVVM1SF "vector_eew32_stride_operand")
  (RVVMF2SF "vector_eew32_stride_operand")

  (RVVM8DI "vector_eew64_stride_operand") (RVVM4DI "vector_eew64_stride_operand")
  (RVVM2DI "vector_eew64_stride_operand") (RVVM1DI "vector_eew64_stride_operand")

  (RVVM8DF "vector_eew64_stride_operand") (RVVM4DF "vector_eew64_stride_operand")
  (RVVM2DF "vector_eew64_stride_operand") (RVVM1DF "vector_eew64_stride_operand")

  (V1QI "vector_eew8_stride_operand")
  (V2QI "vector_eew8_stride_operand")
  (V4QI "vector_eew8_stride_operand")
  (V8QI "vector_eew8_stride_operand")
  (V16QI "vector_eew8_stride_operand")
  (V32QI "vector_eew8_stride_operand")
  (V64QI "vector_eew8_stride_operand")
  (V128QI "vector_eew8_stride_operand")
  (V256QI "vector_eew8_stride_operand")
  (V512QI "vector_eew8_stride_operand")
  (V1024QI "vector_eew8_stride_operand")
  (V2048QI "vector_eew8_stride_operand")
  (V4096QI "vector_eew8_stride_operand")
  (V1HI "vector_eew16_stride_operand")
  (V2HI "vector_eew16_stride_operand")
  (V4HI "vector_eew16_stride_operand")
  (V8HI "vector_eew16_stride_operand")
  (V16HI "vector_eew16_stride_operand")
  (V32HI "vector_eew16_stride_operand")
  (V64HI "vector_eew16_stride_operand")
  (V128HI "vector_eew16_stride_operand")
  (V256HI "vector_eew16_stride_operand")
  (V512HI "vector_eew16_stride_operand")
  (V1024HI "vector_eew16_stride_operand")
  (V2048HI "vector_eew16_stride_operand")
  (V1SI "vector_eew32_stride_operand")
  (V2SI "vector_eew32_stride_operand")
  (V4SI "vector_eew32_stride_operand")
  (V8SI "vector_eew32_stride_operand")
  (V16SI "vector_eew32_stride_operand")
  (V32SI "vector_eew32_stride_operand")
  (V64SI "vector_eew32_stride_operand")
  (V128SI "vector_eew32_stride_operand")
  (V256SI "vector_eew32_stride_operand")
  (V512SI "vector_eew32_stride_operand")
  (V1024SI "vector_eew32_stride_operand")
  (V1DI "vector_eew64_stride_operand")
  (V2DI "vector_eew64_stride_operand")
  (V4DI "vector_eew64_stride_operand")
  (V8DI "vector_eew64_stride_operand")
  (V16DI "vector_eew64_stride_operand")
  (V32DI "vector_eew64_stride_operand")
  (V64DI "vector_eew64_stride_operand")
  (V128DI "vector_eew64_stride_operand")
  (V256DI "vector_eew64_stride_operand")
  (V512DI "vector_eew64_stride_operand")

  (V1HF "vector_eew16_stride_operand")
  (V2HF "vector_eew16_stride_operand")
  (V4HF "vector_eew16_stride_operand")
  (V8HF "vector_eew16_stride_operand")
  (V16HF "vector_eew16_stride_operand")
  (V32HF "vector_eew16_stride_operand")
  (V64HF "vector_eew16_stride_operand")
  (V128HF "vector_eew16_stride_operand")
  (V256HF "vector_eew16_stride_operand")
  (V512HF "vector_eew16_stride_operand")
  (V1024HF "vector_eew16_stride_operand")
  (V2048HF "vector_eew16_stride_operand")
  (V1SF "vector_eew32_stride_operand")
  (V2SF "vector_eew32_stride_operand")
  (V4SF "vector_eew32_stride_operand")
  (V8SF "vector_eew32_stride_operand")
  (V16SF "vector_eew32_stride_operand")
  (V32SF "vector_eew32_stride_operand")
  (V64SF "vector_eew32_stride_operand")
  (V128SF "vector_eew32_stride_operand")
  (V256SF "vector_eew32_stride_operand")
  (V512SF "vector_eew32_stride_operand")
  (V1024SF "vector_eew32_stride_operand")
  (V1DF "vector_eew64_stride_operand")
  (V2DF "vector_eew64_stride_operand")
  (V4DF "vector_eew64_stride_operand")
  (V8DF "vector_eew64_stride_operand")
  (V16DF "vector_eew64_stride_operand")
  (V32DF "vector_eew64_stride_operand")
  (V64DF "vector_eew64_stride_operand")
  (V128DF "vector_eew64_stride_operand")
  (V256DF "vector_eew64_stride_operand")
  (V512DF "vector_eew64_stride_operand")
])

(define_mode_attr stride_load_constraint [
  (RVVM8QI "rJ,rJ,rJ,k01,k01,k01") (RVVM4QI "rJ,rJ,rJ,k01,k01,k01")
  (RVVM2QI "rJ,rJ,rJ,k01,k01,k01") (RVVM1QI "rJ,rJ,rJ,k01,k01,k01")
  (RVVMF2QI "rJ,rJ,rJ,k01,k01,k01") (RVVMF4QI "rJ,rJ,rJ,k01,k01,k01")
  (RVVMF8QI "rJ,rJ,rJ,k01,k01,k01")

  (RVVM8HI "rJ,rJ,rJ,k02,k02,k02") (RVVM4HI "rJ,rJ,rJ,k02,k02,k02")
  (RVVM2HI "rJ,rJ,rJ,k02,k02,k02") (RVVM1HI "rJ,rJ,rJ,k02,k02,k02")
  (RVVMF2HI "rJ,rJ,rJ,k02,k02,k02") (RVVMF4HI "rJ,rJ,rJ,k02,k02,k02")

  (RVVM8BF "rJ,rJ,rJ,k02,k02,k02") (RVVM4BF "rJ,rJ,rJ,k02,k02,k02")
  (RVVM2BF "rJ,rJ,rJ,k02,k02,k02") (RVVM1BF "rJ,rJ,rJ,k02,k02,k02")
  (RVVMF2BF "rJ,rJ,rJ,k02,k02,k02") (RVVMF4BF "rJ,rJ,rJ,k02,k02,k02")

  (RVVM8HF "rJ,rJ,rJ,k02,k02,k02") (RVVM4HF "rJ,rJ,rJ,k02,k02,k02")
  (RVVM2HF "rJ,rJ,rJ,k02,k02,k02") (RVVM1HF "rJ,rJ,rJ,k02,k02,k02")
  (RVVMF2HF "rJ,rJ,rJ,k02,k02,k02") (RVVMF4HF "rJ,rJ,rJ,k02,k02,k02")

  (RVVM8SI "rJ,rJ,rJ,k04,k04,k04") (RVVM4SI "rJ,rJ,rJ,k04,k04,k04")
  (RVVM2SI "rJ,rJ,rJ,k04,k04,k04") (RVVM1SI "rJ,rJ,rJ,k04,k04,k04")
  (RVVMF2SI "rJ,rJ,rJ,k04,k04,k04")

  (RVVM8SF "rJ,rJ,rJ,k04,k04,k04") (RVVM4SF "rJ,rJ,rJ,k04,k04,k04")
  (RVVM2SF "rJ,rJ,rJ,k04,k04,k04") (RVVM1SF "rJ,rJ,rJ,k04,k04,k04")
  (RVVMF2SF "rJ,rJ,rJ,k04,k04,k04")

  (RVVM8DI "rJ,rJ,rJ,k08,k08,k08") (RVVM4DI "rJ,rJ,rJ,k08,k08,k08")
  (RVVM2DI "rJ,rJ,rJ,k08,k08,k08") (RVVM1DI "rJ,rJ,rJ,k08,k08,k08")

  (RVVM8DF "rJ,rJ,rJ,k08,k08,k08") (RVVM4DF "rJ,rJ,rJ,k08,k08,k08")
  (RVVM2DF "rJ,rJ,rJ,k08,k08,k08") (RVVM1DF "rJ,rJ,rJ,k08,k08,k08")

  (V1QI "rJ,rJ,rJ,k01,k01,k01")
  (V2QI "rJ,rJ,rJ,k01,k01,k01")
  (V4QI "rJ,rJ,rJ,k01,k01,k01")
  (V8QI "rJ,rJ,rJ,k01,k01,k01")
  (V16QI "rJ,rJ,rJ,k01,k01,k01")
  (V32QI "rJ,rJ,rJ,k01,k01,k01")
  (V64QI "rJ,rJ,rJ,k01,k01,k01")
  (V128QI "rJ,rJ,rJ,k01,k01,k01")
  (V256QI "rJ,rJ,rJ,k01,k01,k01")
  (V512QI "rJ,rJ,rJ,k01,k01,k01")
  (V1024QI "rJ,rJ,rJ,k01,k01,k01")
  (V2048QI "rJ,rJ,rJ,k01,k01,k01")
  (V4096QI "rJ,rJ,rJ,k01,k01,k01")
  (V1HI "rJ,rJ,rJ,k02,k02,k02")
  (V2HI "rJ,rJ,rJ,k02,k02,k02")
  (V4HI "rJ,rJ,rJ,k02,k02,k02")
  (V8HI "rJ,rJ,rJ,k02,k02,k02")
  (V16HI "rJ,rJ,rJ,k02,k02,k02")
  (V32HI "rJ,rJ,rJ,k02,k02,k02")
  (V64HI "rJ,rJ,rJ,k02,k02,k02")
  (V128HI "rJ,rJ,rJ,k02,k02,k02")
  (V256HI "rJ,rJ,rJ,k02,k02,k02")
  (V512HI "rJ,rJ,rJ,k02,k02,k02")
  (V1024HI "rJ,rJ,rJ,k02,k02,k02")
  (V2048HI "rJ,rJ,rJ,k02,k02,k02")
  (V1SI "rJ,rJ,rJ,k04,k04,k04")
  (V2SI "rJ,rJ,rJ,k04,k04,k04")
  (V4SI "rJ,rJ,rJ,k04,k04,k04")
  (V8SI "rJ,rJ,rJ,k04,k04,k04")
  (V16SI "rJ,rJ,rJ,k04,k04,k04")
  (V32SI "rJ,rJ,rJ,k04,k04,k04")
  (V64SI "rJ,rJ,rJ,k04,k04,k04")
  (V128SI "rJ,rJ,rJ,k04,k04,k04")
  (V256SI "rJ,rJ,rJ,k04,k04,k04")
  (V512SI "rJ,rJ,rJ,k04,k04,k04")
  (V1024SI "rJ,rJ,rJ,k04,k04,k04")
  (V1DI "rJ,rJ,rJ,k08,k08,k08")
  (V2DI "rJ,rJ,rJ,k08,k08,k08")
  (V4DI "rJ,rJ,rJ,k08,k08,k08")
  (V8DI "rJ,rJ,rJ,k08,k08,k08")
  (V16DI "rJ,rJ,rJ,k08,k08,k08")
  (V32DI "rJ,rJ,rJ,k08,k08,k08")
  (V64DI "rJ,rJ,rJ,k08,k08,k08")
  (V128DI "rJ,rJ,rJ,k08,k08,k08")
  (V256DI "rJ,rJ,rJ,k08,k08,k08")
  (V512DI "rJ,rJ,rJ,k08,k08,k08")

  (V1HF "rJ,rJ,rJ,k02,k02,k02")
  (V2HF "rJ,rJ,rJ,k02,k02,k02")
  (V4HF "rJ,rJ,rJ,k02,k02,k02")
  (V8HF "rJ,rJ,rJ,k02,k02,k02")
  (V16HF "rJ,rJ,rJ,k02,k02,k02")
  (V32HF "rJ,rJ,rJ,k02,k02,k02")
  (V64HF "rJ,rJ,rJ,k02,k02,k02")
  (V128HF "rJ,rJ,rJ,k02,k02,k02")
  (V256HF "rJ,rJ,rJ,k02,k02,k02")
  (V512HF "rJ,rJ,rJ,k02,k02,k02")
  (V1024HF "rJ,rJ,rJ,k02,k02,k02")
  (V2048HF "rJ,rJ,rJ,k02,k02,k02")
  (V1SF "rJ,rJ,rJ,k04,k04,k04")
  (V2SF "rJ,rJ,rJ,k04,k04,k04")
  (V4SF "rJ,rJ,rJ,k04,k04,k04")
  (V8SF "rJ,rJ,rJ,k04,k04,k04")
  (V16SF "rJ,rJ,rJ,k04,k04,k04")
  (V32SF "rJ,rJ,rJ,k04,k04,k04")
  (V64SF "rJ,rJ,rJ,k04,k04,k04")
  (V128SF "rJ,rJ,rJ,k04,k04,k04")
  (V256SF "rJ,rJ,rJ,k04,k04,k04")
  (V512SF "rJ,rJ,rJ,k04,k04,k04")
  (V1024SF "rJ,rJ,rJ,k04,k04,k04")
  (V1DF "rJ,rJ,rJ,k08,k08,k08")
  (V2DF "rJ,rJ,rJ,k08,k08,k08")
  (V4DF "rJ,rJ,rJ,k08,k08,k08")
  (V8DF "rJ,rJ,rJ,k08,k08,k08")
  (V16DF "rJ,rJ,rJ,k08,k08,k08")
  (V32DF "rJ,rJ,rJ,k08,k08,k08")
  (V64DF "rJ,rJ,rJ,k08,k08,k08")
  (V128DF "rJ,rJ,rJ,k08,k08,k08")
  (V256DF "rJ,rJ,rJ,k08,k08,k08")
  (V512DF "rJ,rJ,rJ,k08,k08,k08")
])

(define_mode_attr stride_store_constraint [
  (RVVM8QI "rJ,k01") (RVVM4QI "rJ,k01")
  (RVVM2QI "rJ,k01") (RVVM1QI "rJ,k01")
  (RVVMF2QI "rJ,k01") (RVVMF4QI "rJ,k01")
  (RVVMF8QI "rJ,k01")

  (RVVM8HI "rJ,k02") (RVVM4HI "rJ,k02")
  (RVVM2HI "rJ,k02") (RVVM1HI "rJ,k02")
  (RVVMF2HI "rJ,k02") (RVVMF4HI "rJ,k02")

  (RVVM8BF "rJ,k02") (RVVM4BF "rJ,k02")
  (RVVM2BF "rJ,k02") (RVVM1BF "rJ,k02")
  (RVVMF2BF "rJ,k02") (RVVMF4BF "rJ,k02")

  (RVVM8HF "rJ,k02") (RVVM4HF "rJ,k02")
  (RVVM2HF "rJ,k02") (RVVM1HF "rJ,k02")
  (RVVMF2HF "rJ,k02") (RVVMF4HF "rJ,k02")

  (RVVM8SI "rJ,k04") (RVVM4SI "rJ,k04")
  (RVVM2SI "rJ,k04") (RVVM1SI "rJ,k04")
  (RVVMF2SI "rJ,k04")

  (RVVM8SF "rJ,k04") (RVVM4SF "rJ,k04")
  (RVVM2SF "rJ,k04") (RVVM1SF "rJ,k04")
  (RVVMF2SF "rJ,k04")

  (RVVM8DI "rJ,k08") (RVVM4DI "rJ,k08")
  (RVVM2DI "rJ,k08") (RVVM1DI "rJ,k08")

  (RVVM8DF "rJ,k08") (RVVM4DF "rJ,k08")
  (RVVM2DF "rJ,k08") (RVVM1DF "rJ,k08")

  (V1QI "rJ,k01")
  (V2QI "rJ,k01")
  (V4QI "rJ,k01")
  (V8QI "rJ,k01")
  (V16QI "rJ,k01")
  (V32QI "rJ,k01")
  (V64QI "rJ,k01")
  (V128QI "rJ,k01")
  (V256QI "rJ,k01")
  (V512QI "rJ,k01")
  (V1024QI "rJ,k01")
  (V2048QI "rJ,k01")
  (V4096QI "rJ,k01")
  (V1HI "rJ,k02")
  (V2HI "rJ,k02")
  (V4HI "rJ,k02")
  (V8HI "rJ,k02")
  (V16HI "rJ,k02")
  (V32HI "rJ,k02")
  (V64HI "rJ,k02")
  (V128HI "rJ,k02")
  (V256HI "rJ,k02")
  (V512HI "rJ,k02")
  (V1024HI "rJ,k02")
  (V2048HI "rJ,k02")
  (V1SI "rJ,k04")
  (V2SI "rJ,k04")
  (V4SI "rJ,k04")
  (V8SI "rJ,k04")
  (V16SI "rJ,k04")
  (V32SI "rJ,k04")
  (V64SI "rJ,k04")
  (V128SI "rJ,k04")
  (V256SI "rJ,k04")
  (V512SI "rJ,k04")
  (V1024SI "rJ,k04")
  (V1DI "rJ,k08")
  (V2DI "rJ,k08")
  (V4DI "rJ,k08")
  (V8DI "rJ,k08")
  (V16DI "rJ,k08")
  (V32DI "rJ,k08")
  (V64DI "rJ,k08")
  (V128DI "rJ,k08")
  (V256DI "rJ,k08")
  (V512DI "rJ,k08")

  (V1HF "rJ,k02")
  (V2HF "rJ,k02")
  (V4HF "rJ,k02")
  (V8HF "rJ,k02")
  (V16HF "rJ,k02")
  (V32HF "rJ,k02")
  (V64HF "rJ,k02")
  (V128HF "rJ,k02")
  (V256HF "rJ,k02")
  (V512HF "rJ,k02")
  (V1024HF "rJ,k02")
  (V2048HF "rJ,k02")
  (V1SF "rJ,k04")
  (V2SF "rJ,k04")
  (V4SF "rJ,k04")
  (V8SF "rJ,k04")
  (V16SF "rJ,k04")
  (V32SF "rJ,k04")
  (V64SF "rJ,k04")
  (V128SF "rJ,k04")
  (V256SF "rJ,k04")
  (V512SF "rJ,k04")
  (V1024SF "rJ,k04")
  (V1DF "rJ,k08")
  (V2DF "rJ,k08")
  (V4DF "rJ,k08")
  (V8DF "rJ,k08")
  (V16DF "rJ,k08")
  (V32DF "rJ,k08")
  (V64DF "rJ,k08")
  (V128DF "rJ,k08")
  (V256DF "rJ,k08")
  (V512DF "rJ,k08")
])

(define_mode_attr gs_extension [
  (RVVM8QI "const_1_operand") (RVVM4QI "const_1_operand")
  (RVVM2QI "vector_gs_extension_operand") (RVVM1QI "immediate_operand") (RVVMF2QI "immediate_operand")
  (RVVMF4QI "immediate_operand") (RVVMF8QI "immediate_operand")

  (RVVM8HI "const_1_operand") (RVVM4HI "vector_gs_extension_operand")
  (RVVM2HI "immediate_operand") (RVVM1HI "immediate_operand")
  (RVVMF2HI "immediate_operand") (RVVMF4HI "immediate_operand")

  (RVVM8BF "const_1_operand") (RVVM4BF "vector_gs_extension_operand")
  (RVVM2BF "immediate_operand") (RVVM1BF "immediate_operand")
  (RVVMF2BF "immediate_operand") (RVVMF4BF "immediate_operand")

  (RVVM8HF "const_1_operand") (RVVM4HF "vector_gs_extension_operand")
  (RVVM2HF "immediate_operand") (RVVM1HF "immediate_operand")
  (RVVMF2HF "immediate_operand") (RVVMF4HF "immediate_operand")

  (RVVM8SI "vector_gs_extension_operand") (RVVM4SI "immediate_operand") (RVVM2SI "immediate_operand")
  (RVVM1SI "immediate_operand") (RVVMF2SI "immediate_operand")

  (RVVM8SF "vector_gs_extension_operand") (RVVM4SF "immediate_operand") (RVVM2SF "immediate_operand")
  (RVVM1SF "immediate_operand") (RVVMF2SF "immediate_operand")

  (RVVM8DI "immediate_operand") (RVVM4DI "immediate_operand")
  (RVVM2DI "immediate_operand") (RVVM1DI "immediate_operand")

  (RVVM8DF "immediate_operand") (RVVM4DF "immediate_operand")
  (RVVM2DF "immediate_operand") (RVVM1DF "immediate_operand")
])

(define_mode_attr gs_scale [
  (RVVM8QI "const_1_operand") (RVVM4QI "const_1_operand")
  (RVVM2QI "const_1_operand") (RVVM1QI "const_1_operand") (RVVMF2QI "const_1_operand")
  (RVVMF4QI "const_1_operand") (RVVMF8QI "const_1_operand")

  (RVVM8HI "const_1_operand") (RVVM4HI "vector_gs_scale_operand_16_rv32")
  (RVVM2HI "const_1_or_2_operand") (RVVM1HI "const_1_or_2_operand")
  (RVVMF2HI "const_1_or_2_operand") (RVVMF4HI "const_1_or_2_operand")

  (RVVM8BF "const_1_operand") (RVVM4BF "vector_gs_scale_operand_16_rv32")
  (RVVM2BF "const_1_or_2_operand") (RVVM1BF "const_1_or_2_operand")
  (RVVMF2BF "const_1_or_2_operand") (RVVMF4BF "const_1_or_2_operand")

  (RVVM8HF "const_1_operand") (RVVM4HF "vector_gs_scale_operand_16_rv32")
  (RVVM2HF "const_1_or_2_operand") (RVVM1HF "const_1_or_2_operand")
  (RVVMF2HF "const_1_or_2_operand") (RVVMF4HF "const_1_or_2_operand")

  (RVVM8SI "vector_gs_scale_operand_32_rv32") (RVVM4SI "const_1_or_4_operand") (RVVM2SI "const_1_or_4_operand")
  (RVVM1SI "const_1_or_4_operand") (RVVMF2SI "const_1_or_4_operand")

  (RVVM8SF "vector_gs_scale_operand_32_rv32") (RVVM4SF "const_1_or_4_operand") (RVVM2SF "const_1_or_4_operand")
  (RVVM1SF "const_1_or_4_operand") (RVVMF2SF "const_1_or_4_operand")

  (RVVM8DI "const_1_or_8_operand") (RVVM4DI "const_1_or_8_operand")
  (RVVM2DI "const_1_or_8_operand") (RVVM1DI "const_1_or_8_operand")

  (RVVM8DF "const_1_or_8_operand") (RVVM4DF "const_1_or_8_operand")
  (RVVM2DF "const_1_or_8_operand") (RVVM1DF "const_1_or_8_operand")
])

(define_int_iterator ORDER [UNSPEC_ORDERED UNSPEC_UNORDERED])

(define_int_iterator VMULH [UNSPEC_VMULHS UNSPEC_VMULHU UNSPEC_VMULHSU])

(define_int_iterator VNCLIP [UNSPEC_VNCLIP UNSPEC_VNCLIPU])

(define_int_iterator SF_VFNRCLIP [UNSPEC_SF_VFNRCLIP UNSPEC_SF_VFNRCLIPU])

(define_int_iterator VSLIDES [UNSPEC_VSLIDEUP UNSPEC_VSLIDEDOWN])
(define_int_iterator VSLIDES1 [UNSPEC_VSLIDE1UP UNSPEC_VSLIDE1DOWN])
(define_int_iterator VFSLIDES1 [UNSPEC_VFSLIDE1UP UNSPEC_VFSLIDE1DOWN])

(define_int_iterator VSAT_OP [UNSPEC_VAADDU UNSPEC_VAADD
			      UNSPEC_VASUBU UNSPEC_VASUB UNSPEC_VSMUL
			      UNSPEC_VSSRL UNSPEC_VSSRA])

(define_int_iterator VSAT_ARITH_OP [UNSPEC_VAADDU UNSPEC_VAADD
			      	    UNSPEC_VASUBU UNSPEC_VASUB UNSPEC_VSMUL])
(define_int_iterator VSAT_SHIFT_OP [UNSPEC_VSSRL UNSPEC_VSSRA])

(define_int_iterator VMISC [UNSPEC_VMSBF UNSPEC_VMSIF UNSPEC_VMSOF])

(define_int_iterator VFMISC [UNSPEC_VFRSQRT7])

(define_int_iterator VFMISC_FRM [UNSPEC_VFREC7])

(define_int_iterator VFCVTS [UNSPEC_VFCVT UNSPEC_UNSIGNED_VFCVT])

(define_int_attr order [
  (UNSPEC_ORDERED "o") (UNSPEC_UNORDERED "u")
  (UNSPEC_REDUC_SUM_ORDERED "o") (UNSPEC_REDUC_SUM_UNORDERED "u")
  (UNSPEC_WREDUC_SUM_ORDERED "o") (UNSPEC_WREDUC_SUM_UNORDERED "u")
  (UNSPEC_REDUC_SUM_ORDERED_VL0_SAFE "o") (UNSPEC_REDUC_SUM_UNORDERED_VL0_SAFE "u")
  (UNSPEC_WREDUC_SUM_ORDERED_VL0_SAFE "o") (UNSPEC_WREDUC_SUM_UNORDERED_VL0_SAFE "u")
])

(define_int_attr v_su [(UNSPEC_VMULHS "") (UNSPEC_VMULHU "u") (UNSPEC_VMULHSU "su")
		       (UNSPEC_VNCLIP "") (UNSPEC_VNCLIPU "u")
		       (UNSPEC_VFCVT "") (UNSPEC_UNSIGNED_VFCVT "u")
		       (UNSPEC_SF_VFNRCLIP "") (UNSPEC_SF_VFNRCLIPU "u")])
(define_int_attr sat_op [(UNSPEC_VAADDU "aaddu") (UNSPEC_VAADD "aadd")
			 (UNSPEC_VASUBU "asubu") (UNSPEC_VASUB "asub")
			 (UNSPEC_VSMUL "smul") (UNSPEC_VSSRL "ssrl")
			 (UNSPEC_VSSRA "ssra")])
(define_int_attr sat_insn_type [(UNSPEC_VAADDU "vaalu") (UNSPEC_VAADD "vaalu")
			 	(UNSPEC_VASUBU "vaalu") (UNSPEC_VASUB "vaalu")
			 	(UNSPEC_VSMUL "vsmul") (UNSPEC_VSSRL "vsshift")
			 	(UNSPEC_VSSRA "vsshift") (UNSPEC_VNCLIP "vnclip")
				(UNSPEC_VNCLIPU "vnclip")])

(define_int_attr misc_op [(UNSPEC_VMSBF "sbf") (UNSPEC_VMSIF "sif") (UNSPEC_VMSOF "sof")
			  (UNSPEC_VFRSQRT7 "rsqrt7")])

(define_int_attr misc_frm_op [(UNSPEC_VFREC7 "rec7")])

(define_int_attr float_insn_type [(UNSPEC_VFRSQRT7 "vfsqrt")])

(define_int_attr float_frm_insn_type [(UNSPEC_VFREC7 "vfrecp")])

(define_int_iterator VCOPYSIGNS [UNSPEC_VCOPYSIGN UNSPEC_VXORSIGN])

(define_int_attr copysign [(UNSPEC_VCOPYSIGN "copysign") (UNSPEC_VXORSIGN "xorsign")])

(define_int_attr nx [(UNSPEC_VCOPYSIGN "") (UNSPEC_VXORSIGN "x")])

(define_int_attr ud [(UNSPEC_VSLIDEUP "up") (UNSPEC_VSLIDEDOWN "down")
		     (UNSPEC_VSLIDE1UP "1up") (UNSPEC_VSLIDE1DOWN "1down")
		     (UNSPEC_VFSLIDE1UP "1up") (UNSPEC_VFSLIDE1DOWN "1down")])

(define_int_attr ud_constraint [(UNSPEC_VSLIDEUP "=&vr,&vr,&vr,&vr") (UNSPEC_VSLIDEDOWN "=vd,vd,vr,vr")
				(UNSPEC_VSLIDE1UP "=&vr,&vr,&vr,&vr") (UNSPEC_VSLIDE1DOWN "=vd,vd,vr,vr")
				(UNSPEC_VFSLIDE1UP "=&vr,&vr,&vr,&vr") (UNSPEC_VFSLIDE1DOWN "=vd,vd,vr,vr")])

(define_int_attr UNSPEC [(UNSPEC_VSLIDE1UP "UNSPEC_VSLIDE1UP")
			 (UNSPEC_VSLIDE1DOWN "UNSPEC_VSLIDE1DOWN")])

(define_int_iterator UNSPEC_VFMAXMIN [UNSPEC_VFMAX UNSPEC_VFMIN])

(define_int_attr ieee_fmaxmin_op [(UNSPEC_VFMAX "fmax") (UNSPEC_VFMIN "fmin")])
(define_int_attr IEEE_FMAXMIN_OP [(UNSPEC_VFMAX "UNSPEC_VFMAX") (UNSPEC_VFMIN "UNSPEC_VFMIN")])

(define_code_iterator any_int_binop [plus minus and ior xor ashift ashiftrt lshiftrt
  smax umax smin umin mult div udiv mod umod
])

(define_code_iterator any_int_unop [neg not])

(define_code_iterator any_commutative_binop [plus and ior xor
  smax umax smin umin mult
])

(define_code_iterator any_non_commutative_binop [minus div udiv mod umod])

(define_code_iterator any_int_binop_no_shift
 [plus minus and ior xor smax umax smin umin mult div udiv mod umod
])

(define_code_iterator any_sat_int_binop [ss_plus ss_minus us_plus us_minus])
(define_code_iterator sat_int_plus_binop [ss_plus us_plus])
(define_code_iterator sat_int_minus_binop [ss_minus us_minus])

(define_code_iterator mulh [smul_highpart umul_highpart])
(define_code_attr mulh_table [(smul_highpart "smul") (umul_highpart "umul")])
(define_code_attr MULH_UNSPEC [(smul_highpart "UNSPEC_VMULHS") (umul_highpart "UNSPEC_VMULHU")])

(define_code_iterator any_widen_binop [plus minus mult])
(define_code_iterator plus_minus [plus minus])

(define_code_attr madd_msub [(plus "madd") (minus "msub")])
(define_code_attr macc_msac [(plus "macc") (minus "msac")])
(define_code_attr nmsub_nmadd [(plus "nmsub") (minus "nmadd")])
(define_code_attr nmsac_nmacc [(plus "nmsac") (minus "nmacc")])

(define_code_iterator and_ior [and ior])

(define_code_iterator any_float_binop [plus mult minus div])
(define_code_iterator any_float_binop_nofrm [smax smin])
(define_code_iterator commutative_float_binop [plus mult])
(define_code_iterator commutative_float_binop_nofrm [smax smin])
(define_code_iterator non_commutative_float_binop [minus div])
(define_code_iterator any_float_unop [sqrt])
(define_code_iterator any_float_unop_nofrm [neg abs])

(define_code_iterator any_fix [fix unsigned_fix])
(define_code_iterator any_float [float unsigned_float])

(define_code_attr fix_cvt [(fix "fix_trunc") (unsigned_fix "fixuns_trunc")])
(define_code_attr float_cvt [(float "float") (unsigned_float "floatuns")])

(define_code_attr ninsn [(and "nand") (ior "nor") (xor "xnor")])

(define_code_attr binop_rhs1_predicate [
			(plus "register_operand")
			(minus "vector_arith_operand")
			(ior "register_operand")
			(xor "register_operand")
			(and "register_operand")
			(ashift "register_operand")
			(ashiftrt "register_operand")
			(lshiftrt "register_operand")
			(smin "register_operand")
			(smax "register_operand")
			(umin "register_operand")
			(umax "register_operand")
			(mult "register_operand")
			(div "register_operand")
			(mod "register_operand")
			(udiv "register_operand")
			(umod "register_operand")
			(ss_plus "register_operand")
			(us_plus "register_operand")
			(ss_minus "register_operand")
			(us_minus "register_operand")])

(define_code_attr binop_rhs2_predicate [
			(plus "vector_arith_operand")
			(minus "vector_neg_arith_operand")
			(ior "vector_arith_operand")
			(xor "vector_arith_operand")
			(and "vector_arith_operand")
			(ashift "vector_shift_operand")
			(ashiftrt "vector_shift_operand")
			(lshiftrt "vector_shift_operand")
			(smin "register_operand")
			(smax "register_operand")
			(umin "register_operand")
			(umax "register_operand")
			(mult "register_operand")
			(div "register_operand")
			(mod "register_operand")
			(udiv "register_operand")
			(umod "register_operand")
			(ss_plus "vector_arith_operand")
			(us_plus "vector_arith_operand")
			(ss_minus "vector_neg_arith_operand")
			(us_minus "register_operand")])

(define_code_attr binop_rhs1_constraint [
			(plus "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(minus "vr,vr,vr,vr,vr,vr,vr,vr,vi,vi,vi,vi")
			(ior "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(xor "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(and "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(ashift "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(ashiftrt "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(lshiftrt "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(smin "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(smax "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(umin "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(umax "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(mult "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(div "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(mod "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(udiv "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(umod "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")])

(define_code_attr binop_rhs2_constraint [
			(plus "vr,vr,vr,vr,vi,vi,vi,vi,vr,vr,vr,vr")
			(minus "vr,vr,vr,vr,vj,vj,vj,vj,vr,vr,vr,vr")
			(ior "vr,vr,vr,vr,vi,vi,vi,vi,vr,vr,vr,vr")
			(xor "vr,vr,vr,vr,vi,vi,vi,vi,vr,vr,vr,vr")
			(and "vr,vr,vr,vr,vi,vi,vi,vi,vr,vr,vr,vr")
			(ashift "vr,vr,vr,vr,vk,vk,vk,vk,vr,vr,vr,vr")
			(ashiftrt "vr,vr,vr,vr,vk,vk,vk,vk,vr,vr,vr,vr")
			(lshiftrt "vr,vr,vr,vr,vk,vk,vk,vk,vr,vr,vr,vr")
			(smin "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(smax "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(umin "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(umax "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(mult "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(div "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(mod "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(udiv "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(umod "vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr,vr")
			(ss_plus "vr,vr,vr,vr,vi,vi,vi,vi")
			(us_plus "vr,vr,vr,vr,vi,vi,vi,vi")
			(ss_minus "vr,vr,vr,vr,vj,vj,vj,vj")
			(us_minus "vr,vr,vr,vr,vr,vr,vr,vr")])

(define_code_attr int_binop_insn_type [
			(plus "vialu")
			(minus "vialu")
			(ior "vialu")
			(xor "vialu")
			(and "vialu")
			(ashift "vshift")
			(ashiftrt "vshift")
			(lshiftrt "vshift")
			(smin "viminmax")
			(smax "viminmax")
			(umin "viminmax")
			(umax "viminmax")
			(mult "vimul")
			(div "vidiv")
			(mod "vidiv")
			(udiv "vidiv")
			(umod "vidiv")
			(ss_plus "vsalu")
			(us_plus "vsalu")
			(ss_minus "vsalu")
			(us_minus "vsalu")])

(define_code_attr widen_binop_insn_type [
			(plus "walu")
			(minus "walu")
			(mult "wmul")])

(define_code_attr float_insn_type [
			(plus "vfalu")
			(mult "vfmul")
			(smax "vfminmax")
			(smin "vfminmax")
			(minus "vfalu")
			(div "vfdiv")
			(neg "vfsgnj")
			(abs "vfsgnj")
			(sqrt "vfsqrt")])

;; <binop_vi_variant_insn> expands to the insn name of binop matching constraint rhs1 is immediate.
;; minus is negated as vadd and ss_minus is negated as vsadd, others remain <insn>.
(define_code_attr binop_vi_variant_insn [(ashift "sll.vi")
			       (ashiftrt "sra.vi")
			       (lshiftrt "srl.vi")
			       (div "div.vv")
			       (mod "rem.vv")
			       (udiv "divu.vv")
			       (umod "remu.vv")
			       (ior "or.vi")
			       (xor "xor.vi")
			       (and "and.vi")
			       (plus "add.vi")
			       (minus "add.vi")
			       (smin "min.vv")
			       (smax "max.vv")
			       (umin "minu.vv")
			       (umax "maxu.vv")
			       (mult "mul.vv")
			       (ss_plus "sadd.vi")
			       (us_plus "saddu.vi")
			       (ss_minus "sadd.vi")
			       (us_minus "ssubu.vv")])

;; <binop_reverse_vi_variant_insn> expands to the insn name of binop matching constraint rhs2 is immediate.
;; minus is reversed as vrsub, others remain <insn>.
(define_code_attr binop_reverse_vi_variant_insn [(ashift "sll.vv")
			       (ashiftrt "sra.vv")
			       (lshiftrt "srl.vv")
			       (div "div.vv")
			       (mod "rem.vv")
			       (udiv "divu.vv")
			       (umod "remu.vv")
			       (ior "or.vv")
			       (xor "xor.vv")
			       (and "and.vv")
			       (plus "add.vv")
			       (minus "rsub.vi")
			       (smin "min.vv")
			       (smax "max.vv")
			       (umin "minu.vv")
			       (umax "maxu.vv")
			       (mult "mul.vv")])

(define_code_attr binop_vi_variant_op [(ashift "%3,%v4")
			     (ashiftrt "%3,%v4")
			     (lshiftrt "%3,%v4")
			     (div "%3,%4")
			     (mod "%3,%4")
			     (udiv "%3,%4")
			     (umod "%3,%4")
			     (ior "%3,%v4")
			     (xor "%3,%v4")
			     (and "%3,%v4")
			     (plus "%3,%v4")
			     (minus "%3,%V4")
			     (smin "%3,%4")
			     (smax "%3,%4")
			     (umin "%3,%4")
			     (umax "%3,%4")
			     (mult "%3,%4")
			     (ss_plus "%3,%v4")
			     (us_plus "%3,%v4")
			     (ss_minus "%3,%V4")
			     (us_minus "%3,%4")])

(define_code_attr binop_reverse_vi_variant_op [(ashift "%3,%4")
			      (ashiftrt "%3,%4")
			      (lshiftrt "%3,%4")
			      (div "%3,%4")
			      (mod "%3,%4")
			      (udiv "%3,%4")
			      (umod "%3,%4")
			      (ior "%3,%4")
			      (xor "%3,%4")
			      (and "%3,%4")
			      (plus "%3,%4")
			      (minus "%4,%v3")
			      (smin "%3,%4")
			      (smax "%3,%4")
			      (umin "%3,%4")
			      (umax "%3,%4")
			      (mult "%3,%4")])

(define_code_attr sz [(sign_extend "s") (zero_extend "z")])

;; VLS modes that has NUNITS < 32.
(define_mode_iterator VLS_AVL_IMM [
  (V1QI "riscv_vector::vls_mode_valid_p (V1QImode)")
  (V2QI "riscv_vector::vls_mode_valid_p (V2QImode)")
  (V4QI "riscv_vector::vls_mode_valid_p (V4QImode)")
  (V8QI "riscv_vector::vls_mode_valid_p (V8QImode)")
  (V16QI "riscv_vector::vls_mode_valid_p (V16QImode)")
  (V1HI "riscv_vector::vls_mode_valid_p (V1HImode)")
  (V2HI "riscv_vector::vls_mode_valid_p (V2HImode)")
  (V4HI "riscv_vector::vls_mode_valid_p (V4HImode)")
  (V8HI "riscv_vector::vls_mode_valid_p (V8HImode)")
  (V16HI "riscv_vector::vls_mode_valid_p (V16HImode)")
  (V1SI "riscv_vector::vls_mode_valid_p (V1SImode)")
  (V2SI "riscv_vector::vls_mode_valid_p (V2SImode)")
  (V4SI "riscv_vector::vls_mode_valid_p (V4SImode)")
  (V8SI "riscv_vector::vls_mode_valid_p (V8SImode)")
  (V16SI "riscv_vector::vls_mode_valid_p (V16SImode) && TARGET_MIN_VLEN >= 64")
  (V1DI "riscv_vector::vls_mode_valid_p (V1DImode) && TARGET_VECTOR_ELEN_64")
  (V2DI "riscv_vector::vls_mode_valid_p (V2DImode) && TARGET_VECTOR_ELEN_64")
  (V4DI "riscv_vector::vls_mode_valid_p (V4DImode) && TARGET_VECTOR_ELEN_64")
  (V8DI "riscv_vector::vls_mode_valid_p (V8DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 64")
  (V16DI "riscv_vector::vls_mode_valid_p (V16DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 128")
  (V1HF "riscv_vector::vls_mode_valid_p (V1HFmode) && TARGET_VECTOR_ELEN_FP_16")
  (V2HF "riscv_vector::vls_mode_valid_p (V2HFmode) && TARGET_VECTOR_ELEN_FP_16")
  (V4HF "riscv_vector::vls_mode_valid_p (V4HFmode) && TARGET_VECTOR_ELEN_FP_16")
  (V8HF "riscv_vector::vls_mode_valid_p (V8HFmode) && TARGET_VECTOR_ELEN_FP_16")
  (V16HF "riscv_vector::vls_mode_valid_p (V16HFmode) && TARGET_VECTOR_ELEN_FP_16")
  (V1SF "riscv_vector::vls_mode_valid_p (V1SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V2SF "riscv_vector::vls_mode_valid_p (V2SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V4SF "riscv_vector::vls_mode_valid_p (V4SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V8SF "riscv_vector::vls_mode_valid_p (V8SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V16SF "riscv_vector::vls_mode_valid_p (V16SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 64")
  (V1DF "riscv_vector::vls_mode_valid_p (V1DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V2DF "riscv_vector::vls_mode_valid_p (V2DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V4DF "riscv_vector::vls_mode_valid_p (V4DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V8DF "riscv_vector::vls_mode_valid_p (V8DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 64")
  (V16DF "riscv_vector::vls_mode_valid_p (V16DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 128")

  (V1BI "riscv_vector::vls_mode_valid_p (V1BImode)")
  (V2BI "riscv_vector::vls_mode_valid_p (V2BImode)")
  (V4BI "riscv_vector::vls_mode_valid_p (V4BImode)")
  (V8BI "riscv_vector::vls_mode_valid_p (V8BImode)")
  (V16BI "riscv_vector::vls_mode_valid_p (V16BImode)")])

;; VLS modes that has NUNITS >= 32.
(define_mode_iterator VLS_AVL_REG [
  (V32QI "riscv_vector::vls_mode_valid_p (V32QImode)")
  (V64QI "riscv_vector::vls_mode_valid_p (V64QImode) && TARGET_MIN_VLEN >= 64")
  (V128QI "riscv_vector::vls_mode_valid_p (V128QImode) && TARGET_MIN_VLEN >= 128")
  (V256QI "riscv_vector::vls_mode_valid_p (V256QImode) && TARGET_MIN_VLEN >= 256")
  (V512QI "riscv_vector::vls_mode_valid_p (V512QImode) && TARGET_MIN_VLEN >= 512")
  (V1024QI "riscv_vector::vls_mode_valid_p (V1024QImode) && TARGET_MIN_VLEN >= 1024")
  (V2048QI "riscv_vector::vls_mode_valid_p (V2048QImode) && TARGET_MIN_VLEN >= 2048")
  (V4096QI "riscv_vector::vls_mode_valid_p (V4096QImode) && TARGET_MIN_VLEN >= 4096")
  (V32HI "riscv_vector::vls_mode_valid_p (V32HImode) && TARGET_MIN_VLEN >= 64")
  (V64HI "riscv_vector::vls_mode_valid_p (V64HImode) && TARGET_MIN_VLEN >= 128")
  (V128HI "riscv_vector::vls_mode_valid_p (V128HImode) && TARGET_MIN_VLEN >= 256")
  (V256HI "riscv_vector::vls_mode_valid_p (V256HImode) && TARGET_MIN_VLEN >= 512")
  (V512HI "riscv_vector::vls_mode_valid_p (V512HImode) && TARGET_MIN_VLEN >= 1024")
  (V1024HI "riscv_vector::vls_mode_valid_p (V1024HImode) && TARGET_MIN_VLEN >= 2048")
  (V2048HI "riscv_vector::vls_mode_valid_p (V2048HImode) && TARGET_MIN_VLEN >= 4096")
  (V32SI "riscv_vector::vls_mode_valid_p (V32SImode) && TARGET_MIN_VLEN >= 128")
  (V64SI "riscv_vector::vls_mode_valid_p (V64SImode) && TARGET_MIN_VLEN >= 256")
  (V128SI "riscv_vector::vls_mode_valid_p (V128SImode) && TARGET_MIN_VLEN >= 512")
  (V256SI "riscv_vector::vls_mode_valid_p (V256SImode) && TARGET_MIN_VLEN >= 1024")
  (V512SI "riscv_vector::vls_mode_valid_p (V512SImode) && TARGET_MIN_VLEN >= 2048")
  (V1024SI "riscv_vector::vls_mode_valid_p (V1024SImode) && TARGET_MIN_VLEN >= 4096")
  (V32DI "riscv_vector::vls_mode_valid_p (V32DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 256")
  (V64DI "riscv_vector::vls_mode_valid_p (V64DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 512")
  (V128DI "riscv_vector::vls_mode_valid_p (V128DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 1024")
  (V256DI "riscv_vector::vls_mode_valid_p (V256DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 2048")
  (V512DI "riscv_vector::vls_mode_valid_p (V512DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 4096")
  (V32HF "riscv_vector::vls_mode_valid_p (V32HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 64")
  (V64HF "riscv_vector::vls_mode_valid_p (V64HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 128")
  (V128HF "riscv_vector::vls_mode_valid_p (V128HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 256")
  (V256HF "riscv_vector::vls_mode_valid_p (V256HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 512")
  (V512HF "riscv_vector::vls_mode_valid_p (V512HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 1024")
  (V1024HF "riscv_vector::vls_mode_valid_p (V1024HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 2048")
  (V2048HF "riscv_vector::vls_mode_valid_p (V2048HFmode) && TARGET_VECTOR_ELEN_FP_16 && TARGET_MIN_VLEN >= 4096")
  (V32SF "riscv_vector::vls_mode_valid_p (V32SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 128")
  (V64SF "riscv_vector::vls_mode_valid_p (V64SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 256")
  (V128SF "riscv_vector::vls_mode_valid_p (V128SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 512")
  (V256SF "riscv_vector::vls_mode_valid_p (V256SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 1024")
  (V512SF "riscv_vector::vls_mode_valid_p (V512SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 2048")
  (V1024SF "riscv_vector::vls_mode_valid_p (V1024SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 4096")
  (V32DF "riscv_vector::vls_mode_valid_p (V32DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 256")
  (V64DF "riscv_vector::vls_mode_valid_p (V64DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 512")
  (V128DF "riscv_vector::vls_mode_valid_p (V128DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 1024")
  (V256DF "riscv_vector::vls_mode_valid_p (V256DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 2048")
  (V512DF "riscv_vector::vls_mode_valid_p (V512DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 4096")

  (V32BI "riscv_vector::vls_mode_valid_p (V32BImode)")
  (V64BI "riscv_vector::vls_mode_valid_p (V64BImode) && TARGET_MIN_VLEN >= 64")
  (V128BI "riscv_vector::vls_mode_valid_p (V128BImode) && TARGET_MIN_VLEN >= 128")
  (V256BI "riscv_vector::vls_mode_valid_p (V256BImode) && TARGET_MIN_VLEN >= 256")
  (V512BI "riscv_vector::vls_mode_valid_p (V512BImode) && TARGET_MIN_VLEN >= 512")
  (V1024BI "riscv_vector::vls_mode_valid_p (V1024BImode) && TARGET_MIN_VLEN >= 1024")
  (V2048BI "riscv_vector::vls_mode_valid_p (V2048BImode) && TARGET_MIN_VLEN >= 2048")
  (V4096BI "riscv_vector::vls_mode_valid_p (V4096BImode) && TARGET_MIN_VLEN >= 4096")])

(define_mode_iterator VSI [
  RVVM8SI RVVM4SI RVVM2SI RVVM1SI (RVVMF2SI "TARGET_VECTOR_ELEN_64")
])

(define_mode_iterator VLMULX2_SI [
  RVVM4SI RVVM2SI RVVM1SI (RVVMF2SI "TARGET_VECTOR_ELEN_64")
])

(define_mode_iterator VLMULX4_SI [
  RVVM2SI RVVM1SI (RVVMF2SI "TARGET_VECTOR_ELEN_64")
])

(define_mode_iterator VLMULX8_SI [
  RVVM1SI (RVVMF2SI "TARGET_VECTOR_ELEN_64")
])

(define_mode_iterator VLMULX16_SI [
  (RVVMF2SI "TARGET_VECTOR_ELEN_64")
])

(define_mode_attr VSIX2 [
  (RVVM8SI "RVVM8SI") (RVVM4SI "RVVM8SI") (RVVM2SI "RVVM4SI") (RVVM1SI "RVVM2SI") (RVVMF2SI "RVVM1SI")
])

(define_mode_attr VSIX4 [
  (RVVM2SI "RVVM8SI") (RVVM1SI "RVVM4SI") (RVVMF2SI "RVVM2SI")
])

(define_mode_attr VSIX8 [
  (RVVM1SI "RVVM8SI") (RVVMF2SI "RVVM4SI")
])

(define_mode_attr VSIX16 [
  (RVVMF2SI "RVVM8SI")
])

(define_mode_iterator VLS_HAS_HALF [
  (V2QI "riscv_vector::vls_mode_valid_p (V2QImode)")
  (V4QI "riscv_vector::vls_mode_valid_p (V4QImode)")
  (V8QI "riscv_vector::vls_mode_valid_p (V8QImode)")
  (V16QI "riscv_vector::vls_mode_valid_p (V16QImode)")
  (V2HI "riscv_vector::vls_mode_valid_p (V2HImode)")
  (V4HI "riscv_vector::vls_mode_valid_p (V4HImode)")
  (V8HI "riscv_vector::vls_mode_valid_p (V8HImode)")
  (V16HI "riscv_vector::vls_mode_valid_p (V16HImode)")
  (V2SI "riscv_vector::vls_mode_valid_p (V2SImode)")
  (V4SI "riscv_vector::vls_mode_valid_p (V4SImode)")
  (V8SI "riscv_vector::vls_mode_valid_p (V8SImode)")
  (V16SI "riscv_vector::vls_mode_valid_p (V16SImode) && TARGET_MIN_VLEN >= 64")
  (V2DI "riscv_vector::vls_mode_valid_p (V2DImode) && TARGET_VECTOR_ELEN_64")
  (V4DI "riscv_vector::vls_mode_valid_p (V4DImode) && TARGET_VECTOR_ELEN_64")
  (V8DI "riscv_vector::vls_mode_valid_p (V8DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 64")
  (V16DI "riscv_vector::vls_mode_valid_p (V16DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 128")
  (V2SF "riscv_vector::vls_mode_valid_p (V2SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V4SF "riscv_vector::vls_mode_valid_p (V4SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V8SF "riscv_vector::vls_mode_valid_p (V8SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V16SF "riscv_vector::vls_mode_valid_p (V16SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 64")
  (V2DF "riscv_vector::vls_mode_valid_p (V2DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V4DF "riscv_vector::vls_mode_valid_p (V4DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V8DF "riscv_vector::vls_mode_valid_p (V8DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 64")
  (V16DF "riscv_vector::vls_mode_valid_p (V16DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 128")
  (V32QI "riscv_vector::vls_mode_valid_p (V32QImode)")
  (V64QI "riscv_vector::vls_mode_valid_p (V64QImode) && TARGET_MIN_VLEN >= 64")
  (V128QI "riscv_vector::vls_mode_valid_p (V128QImode) && TARGET_MIN_VLEN >= 128")
  (V256QI "riscv_vector::vls_mode_valid_p (V256QImode) && TARGET_MIN_VLEN >= 256")
  (V512QI "riscv_vector::vls_mode_valid_p (V512QImode) && TARGET_MIN_VLEN >= 512")
  (V1024QI "riscv_vector::vls_mode_valid_p (V1024QImode) && TARGET_MIN_VLEN >= 1024")
  (V2048QI "riscv_vector::vls_mode_valid_p (V2048QImode) && TARGET_MIN_VLEN >= 2048")
  (V4096QI "riscv_vector::vls_mode_valid_p (V4096QImode) && TARGET_MIN_VLEN >= 4096")
  (V32HI "riscv_vector::vls_mode_valid_p (V32HImode) && TARGET_MIN_VLEN >= 64")
  (V64HI "riscv_vector::vls_mode_valid_p (V64HImode) && TARGET_MIN_VLEN >= 128")
  (V128HI "riscv_vector::vls_mode_valid_p (V128HImode) && TARGET_MIN_VLEN >= 256")
  (V256HI "riscv_vector::vls_mode_valid_p (V256HImode) && TARGET_MIN_VLEN >= 512")
  (V512HI "riscv_vector::vls_mode_valid_p (V512HImode) && TARGET_MIN_VLEN >= 1024")
  (V1024HI "riscv_vector::vls_mode_valid_p (V1024HImode) && TARGET_MIN_VLEN >= 2048")
  (V2048HI "riscv_vector::vls_mode_valid_p (V2048HImode) && TARGET_MIN_VLEN >= 4096")
  (V32SI "riscv_vector::vls_mode_valid_p (V32SImode) && TARGET_MIN_VLEN >= 128")
  (V64SI "riscv_vector::vls_mode_valid_p (V64SImode) && TARGET_MIN_VLEN >= 256")
  (V128SI "riscv_vector::vls_mode_valid_p (V128SImode) && TARGET_MIN_VLEN >= 512")
  (V256SI "riscv_vector::vls_mode_valid_p (V256SImode) && TARGET_MIN_VLEN >= 1024")
  (V512SI "riscv_vector::vls_mode_valid_p (V512SImode) && TARGET_MIN_VLEN >= 2048")
  (V1024SI "riscv_vector::vls_mode_valid_p (V1024SImode) && TARGET_MIN_VLEN >= 4096")
  (V32DI "riscv_vector::vls_mode_valid_p (V32DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 256")
  (V64DI "riscv_vector::vls_mode_valid_p (V64DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 512")
  (V128DI "riscv_vector::vls_mode_valid_p (V128DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 1024")
  (V256DI "riscv_vector::vls_mode_valid_p (V256DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 2048")
  (V512DI "riscv_vector::vls_mode_valid_p (V512DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 4096")
  (V32SF "riscv_vector::vls_mode_valid_p (V32SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 128")
  (V64SF "riscv_vector::vls_mode_valid_p (V64SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 256")
  (V128SF "riscv_vector::vls_mode_valid_p (V128SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 512")
  (V256SF "riscv_vector::vls_mode_valid_p (V256SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 1024")
  (V512SF "riscv_vector::vls_mode_valid_p (V512SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 2048")
  (V1024SF "riscv_vector::vls_mode_valid_p (V1024SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 4096")
  (V32DF "riscv_vector::vls_mode_valid_p (V32DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 256")
  (V64DF "riscv_vector::vls_mode_valid_p (V64DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 512")
  (V128DF "riscv_vector::vls_mode_valid_p (V128DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 1024")
  (V256DF "riscv_vector::vls_mode_valid_p (V256DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 2048")
  (V512DF "riscv_vector::vls_mode_valid_p (V512DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 4096")
])

(define_mode_attr VLS_HALF [
  (V2QI "V1QI")
  (V4QI "V2QI")
  (V8QI "V4QI")
  (V16QI "V8QI")
  (V32QI "V16QI")
  (V64QI "V32QI")
  (V128QI "V64QI")
  (V256QI "V128QI")
  (V512QI "V256QI")
  (V1024QI "V512QI")
  (V2048QI "V1024QI")
  (V4096QI "V2048QI")

  (V2HI "V1HI")
  (V4HI "V2HI")
  (V8HI "V4HI")
  (V16HI "V8HI")
  (V32HI "V16HI")
  (V64HI "V32HI")
  (V128HI "V64HI")
  (V256HI "V128HI")
  (V512HI "V256HI")
  (V1024HI "V512HI")
  (V2048HI "V1024HI")

  (V2SI "V1SI")
  (V4SI "V2SI")
  (V8SI "V4SI")
  (V16SI "V8SI")
  (V32SI "V16SI")
  (V64SI "V32SI")
  (V128SI "V64SI")
  (V256SI "V128SI")
  (V512SI "V256SI")
  (V1024SI "V512SI")

  (V2DI "V1DI")
  (V4DI "V2DI")
  (V8DI "V4DI")
  (V16DI "V8DI")
  (V32DI "V16DI")
  (V64DI "V32DI")
  (V128DI "V64DI")
  (V256DI "V128DI")
  (V512DI "V256DI")

  (V2SF "V1SF")
  (V4SF "V2SF")
  (V8SF "V4SF")
  (V16SF "V8SF")
  (V32SF "V16SF")
  (V64SF "V32SF")
  (V128SF "V64SF")
  (V256SF "V128SF")
  (V512SF "V256SF")
  (V1024SF "V512SF")

  (V2DF "V1DF")
  (V4DF "V2DF")
  (V8DF "V4DF")
  (V16DF "V8DF")
  (V32DF "V16DF")
  (V64DF "V32DF")
  (V128DF "V64DF")
  (V256DF "V128DF")
  (V512DF "V256DF")
])

(define_mode_attr vls_half [
  (V2QI "v1qi")
  (V4QI "v2qi")
  (V8QI "v4qi")
  (V16QI "v8qi")
  (V32QI "v16qi")
  (V64QI "v32qi")
  (V128QI "v64qi")
  (V256QI "v128qi")
  (V512QI "v256qi")
  (V1024QI "v512qi")
  (V2048QI "v1024qi")
  (V4096QI "v2048qi")

  (V2HI "v1hi")
  (V4HI "v2hi")
  (V8HI "v4hi")
  (V16HI "v8hi")
  (V32HI "v16hi")
  (V64HI "v32hi")
  (V128HI "v64hi")
  (V256HI "v128hi")
  (V512HI "v256hi")
  (V1024HI "v512hi")
  (V2048HI "v1024hi")

  (V2SI "v1si")
  (V4SI "v2si")
  (V8SI "v4si")
  (V16SI "v8si")
  (V32SI "v16si")
  (V64SI "v32si")
  (V128SI "v64si")
  (V256SI "v128si")
  (V512SI "v256si")
  (V1024SI "v512si")

  (V2DI "v1di")
  (V4DI "v2di")
  (V8DI "v4di")
  (V16DI "v8di")
  (V32DI "v16di")
  (V64DI "v32di")
  (V128DI "v64di")
  (V256DI "v128di")
  (V512DI "v256di")

  (V2SF "v1sf")
  (V4SF "v2sf")
  (V8SF "v4sf")
  (V16SF "v8sf")
  (V32SF "v16sf")
  (V64SF "v32sf")
  (V128SF "v64sf")
  (V256SF "v128sf")
  (V512SF "v256sf")
  (V1024SF "v512sf")

  (V2DF "v1df")
  (V4DF "v2df")
  (V8DF "v4df")
  (V16DF "v8df")
  (V32DF "v16df")
  (V64DF "v32df")
  (V128DF "v64df")
  (V256DF "v128df")
  (V512DF "v256df")
])

(define_mode_iterator VLS_HAS_QUARTER [
  (V4QI "riscv_vector::vls_mode_valid_p (V4QImode)")
  (V8QI "riscv_vector::vls_mode_valid_p (V8QImode)")
  (V16QI "riscv_vector::vls_mode_valid_p (V16QImode)")
  (V4HI "riscv_vector::vls_mode_valid_p (V4HImode)")
  (V8HI "riscv_vector::vls_mode_valid_p (V8HImode)")
  (V16HI "riscv_vector::vls_mode_valid_p (V16HImode)")
  (V4SI "riscv_vector::vls_mode_valid_p (V4SImode)")
  (V8SI "riscv_vector::vls_mode_valid_p (V8SImode)")
  (V16SI "riscv_vector::vls_mode_valid_p (V16SImode) && TARGET_MIN_VLEN >= 64")
  (V4DI "riscv_vector::vls_mode_valid_p (V4DImode) && TARGET_VECTOR_ELEN_64")
  (V8DI "riscv_vector::vls_mode_valid_p (V8DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 64")
  (V16DI "riscv_vector::vls_mode_valid_p (V16DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 128")
  (V4SF "riscv_vector::vls_mode_valid_p (V4SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V8SF "riscv_vector::vls_mode_valid_p (V8SFmode) && TARGET_VECTOR_ELEN_FP_32")
  (V16SF "riscv_vector::vls_mode_valid_p (V16SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 64")
  (V4DF "riscv_vector::vls_mode_valid_p (V4DFmode) && TARGET_VECTOR_ELEN_FP_64")
  (V8DF "riscv_vector::vls_mode_valid_p (V8DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 64")
  (V16DF "riscv_vector::vls_mode_valid_p (V16DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 128")
  (V32QI "riscv_vector::vls_mode_valid_p (V32QImode)")
  (V64QI "riscv_vector::vls_mode_valid_p (V64QImode) && TARGET_MIN_VLEN >= 64")
  (V128QI "riscv_vector::vls_mode_valid_p (V128QImode) && TARGET_MIN_VLEN >= 128")
  (V256QI "riscv_vector::vls_mode_valid_p (V256QImode) && TARGET_MIN_VLEN >= 256")
  (V512QI "riscv_vector::vls_mode_valid_p (V512QImode) && TARGET_MIN_VLEN >= 512")
  (V1024QI "riscv_vector::vls_mode_valid_p (V1024QImode) && TARGET_MIN_VLEN >= 1024")
  (V2048QI "riscv_vector::vls_mode_valid_p (V2048QImode) && TARGET_MIN_VLEN >= 2048")
  (V4096QI "riscv_vector::vls_mode_valid_p (V4096QImode) && TARGET_MIN_VLEN >= 4096")
  (V32HI "riscv_vector::vls_mode_valid_p (V32HImode) && TARGET_MIN_VLEN >= 64")
  (V64HI "riscv_vector::vls_mode_valid_p (V64HImode) && TARGET_MIN_VLEN >= 128")
  (V128HI "riscv_vector::vls_mode_valid_p (V128HImode) && TARGET_MIN_VLEN >= 256")
  (V256HI "riscv_vector::vls_mode_valid_p (V256HImode) && TARGET_MIN_VLEN >= 512")
  (V512HI "riscv_vector::vls_mode_valid_p (V512HImode) && TARGET_MIN_VLEN >= 1024")
  (V1024HI "riscv_vector::vls_mode_valid_p (V1024HImode) && TARGET_MIN_VLEN >= 2048")
  (V2048HI "riscv_vector::vls_mode_valid_p (V2048HImode) && TARGET_MIN_VLEN >= 4096")
  (V32SI "riscv_vector::vls_mode_valid_p (V32SImode) && TARGET_MIN_VLEN >= 128")
  (V64SI "riscv_vector::vls_mode_valid_p (V64SImode) && TARGET_MIN_VLEN >= 256")
  (V128SI "riscv_vector::vls_mode_valid_p (V128SImode) && TARGET_MIN_VLEN >= 512")
  (V256SI "riscv_vector::vls_mode_valid_p (V256SImode) && TARGET_MIN_VLEN >= 1024")
  (V512SI "riscv_vector::vls_mode_valid_p (V512SImode) && TARGET_MIN_VLEN >= 2048")
  (V1024SI "riscv_vector::vls_mode_valid_p (V1024SImode) && TARGET_MIN_VLEN >= 4096")
  (V32DI "riscv_vector::vls_mode_valid_p (V32DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 256")
  (V64DI "riscv_vector::vls_mode_valid_p (V64DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 512")
  (V128DI "riscv_vector::vls_mode_valid_p (V128DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 1024")
  (V256DI "riscv_vector::vls_mode_valid_p (V256DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 2048")
  (V512DI "riscv_vector::vls_mode_valid_p (V512DImode) && TARGET_VECTOR_ELEN_64 && TARGET_MIN_VLEN >= 4096")
  (V32SF "riscv_vector::vls_mode_valid_p (V32SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 128")
  (V64SF "riscv_vector::vls_mode_valid_p (V64SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 256")
  (V128SF "riscv_vector::vls_mode_valid_p (V128SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 512")
  (V256SF "riscv_vector::vls_mode_valid_p (V256SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 1024")
  (V512SF "riscv_vector::vls_mode_valid_p (V512SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 2048")
  (V1024SF "riscv_vector::vls_mode_valid_p (V1024SFmode) && TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN >= 4096")
  (V32DF "riscv_vector::vls_mode_valid_p (V32DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 256")
  (V64DF "riscv_vector::vls_mode_valid_p (V64DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 512")
  (V128DF "riscv_vector::vls_mode_valid_p (V128DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 1024")
  (V256DF "riscv_vector::vls_mode_valid_p (V256DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 2048")
  (V512DF "riscv_vector::vls_mode_valid_p (V512DFmode) && TARGET_VECTOR_ELEN_FP_64 && TARGET_MIN_VLEN >= 4096")
])

(define_mode_attr VLS_QUARTER [
  (V4QI "V1QI")
  (V8QI "V2QI")
  (V16QI "V4QI")
  (V32QI "V8QI")
  (V64QI "V16QI")
  (V128QI "V32QI")
  (V256QI "V64QI")
  (V512QI "V128QI")
  (V1024QI "V256QI")
  (V2048QI "V512QI")
  (V4096QI "V1024QI")

  (V4HI "V1HI")
  (V8HI "V2HI")
  (V16HI "V4HI")
  (V32HI "V8HI")
  (V64HI "V16HI")
  (V128HI "V32HI")
  (V256HI "V64HI")
  (V512HI "V128HI")
  (V1024HI "V256HI")
  (V2048HI "V512HI")

  (V4SI "V1SI")
  (V8SI "V2SI")
  (V16SI "V4SI")
  (V32SI "V8SI")
  (V64SI "V16SI")
  (V128SI "V32SI")
  (V256SI "V64SI")
  (V512SI "V128SI")
  (V1024SI "V256SI")

  (V4DI "V1DI")
  (V8DI "V2DI")
  (V16DI "V4DI")
  (V32DI "V8DI")
  (V64DI "V16DI")
  (V128DI "V32DI")
  (V256DI "V64DI")
  (V512DI "V128DI")

  (V4SF "V1SF")
  (V8SF "V2SF")
  (V16SF "V4SF")
  (V32SF "V8SF")
  (V64SF "V16SF")
  (V128SF "V32SF")
  (V256SF "V64SF")
  (V512SF "V128SF")
  (V1024SF "V256SF")

  (V4DF "V1DF")
  (V8DF "V2DF")
  (V16DF "V4DF")
  (V32DF "V8DF")
  (V64DF "V16DF")
  (V128DF "V32DF")
  (V256DF "V64DF")
  (V512DF "V128DF")
])

(define_mode_attr vls_quarter [
  (V4QI "v1qi")
  (V8QI "v2qi")
  (V16QI "v4qi")
  (V32QI "v8qi")
  (V64QI "v16qi")
  (V128QI "v32qi")
  (V256QI "v64qi")
  (V512QI "v128qi")
  (V1024QI "v256qi")
  (V2048QI "v512qi")
  (V4096QI "v1024qi")

  (V4HI "v1hi")
  (V8HI "v2hi")
  (V16HI "v4hi")
  (V32HI "v8hi")
  (V64HI "v16hi")
  (V128HI "v32hi")
  (V256HI "v64hi")
  (V512HI "v128hi")
  (V1024HI "v256hi")
  (V2048HI "v512hi")

  (V4SI "v1si")
  (V8SI "v2si")
  (V16SI "v4si")
  (V32SI "v8si")
  (V64SI "v16si")
  (V128SI "v32si")
  (V256SI "v64si")
  (V512SI "v128si")
  (V1024SI "v256si")

  (V4DI "v1di")
  (V8DI "v2di")
  (V16DI "v4di")
  (V32DI "v8di")
  (V64DI "v16di")
  (V128DI "v32di")
  (V256DI "v64di")
  (V512DI "v128di")

  (V4SF "v1sf")
  (V8SF "v2sf")
  (V16SF "v4sf")
  (V32SF "v8sf")
  (V64SF "v16sf")
  (V128SF "v32sf")
  (V256SF "v64sf")
  (V512SF "v128sf")
  (V1024SF "v256sf")

  (V4DF "v1df")
  (V8DF "v2df")
  (V16DF "v4df")
  (V32DF "v8df")
  (V64DF "v16df")
  (V128DF "v32df")
  (V256DF "v64df")
  (V512DF "v128df")
])

(define_mode_iterator SF_VSI [
  RVVM8SI RVVM4SI RVVM2SI RVVM1SI
])

(define_mode_attr SF_VQMACC_QOQ [
  (RVVM8SI "RVVM4QI")
  (RVVM4SI "RVVM2QI")
  (RVVM2SI "RVVM1QI")
  (RVVM1SI "RVVMF2QI")
])

(define_mode_attr sf_vqmacc_qoq [
  (RVVM8SI "rvvm4qi")
  (RVVM4SI "rvvm2qi")
  (RVVM2SI "rvvm1qi")
  (RVVM1SI "rvvmf2qi")
])

(define_mode_attr SF_VQMACC_DOD [
  (RVVM8SI "RVVM8QI")
  (RVVM4SI "RVVM4QI")
  (RVVM2SI "RVVM2QI")
  (RVVM1SI "RVVM1QI")
])

(define_mode_attr sf_vqmacc_dod [
  (RVVM8SI "rvvm8qi")
  (RVVM4SI "rvvm4qi")
  (RVVM2SI "rvvm2qi")
  (RVVM1SI "rvvm1qi")
])

(define_mode_iterator SF_XF [
  RVVM2QI RVVM1QI RVVMF2QI RVVMF4QI (RVVMF8QI "TARGET_VECTOR_ELEN_64")
])


(define_mode_attr SF_XFQF [
  (RVVMF8QI "RVVMF2SF")
  (RVVMF4QI "RVVM1SF")
  (RVVMF2QI "RVVM2SF")
  (RVVM1QI  "RVVM4SF")
  (RVVM2QI  "RVVM8SF")
])

(define_mode_attr sf_xfqf [
  (RVVMF8QI "rvvmf2sf")
  (RVVMF4QI "rvvm1sf")
  (RVVMF2QI "rvvm2sf")
  (RVVM1QI  "rvvm4sf")
  (RVVM2QI  "rvvm8sf")
])
