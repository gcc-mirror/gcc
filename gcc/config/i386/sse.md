;; GCC machine description for SSE instructions
;; Copyright (C) 2005-2018 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_c_enum "unspec" [
  ;; SSE
  UNSPEC_MOVNT

  ;; SSE3
  UNSPEC_LDDQU

  ;; SSSE3
  UNSPEC_PSHUFB
  UNSPEC_PSIGN
  UNSPEC_PALIGNR

  ;; For SSE4A support
  UNSPEC_EXTRQI
  UNSPEC_EXTRQ
  UNSPEC_INSERTQI
  UNSPEC_INSERTQ

  ;; For SSE4.1 support
  UNSPEC_BLENDV
  UNSPEC_INSERTPS
  UNSPEC_DP
  UNSPEC_MOVNTDQA
  UNSPEC_MPSADBW
  UNSPEC_PHMINPOSUW
  UNSPEC_PTEST

  ;; For SSE4.2 support
  UNSPEC_PCMPESTR
  UNSPEC_PCMPISTR

  ;; For FMA4 support
  UNSPEC_FMADDSUB
  UNSPEC_XOP_UNSIGNED_CMP
  UNSPEC_XOP_TRUEFALSE
  UNSPEC_XOP_PERMUTE
  UNSPEC_FRCZ

  ;; For AES support
  UNSPEC_AESENC
  UNSPEC_AESENCLAST
  UNSPEC_AESDEC
  UNSPEC_AESDECLAST
  UNSPEC_AESIMC
  UNSPEC_AESKEYGENASSIST

  ;; For PCLMUL support
  UNSPEC_PCLMUL

  ;; For AVX support
  UNSPEC_PCMP
  UNSPEC_VPERMIL
  UNSPEC_VPERMIL2
  UNSPEC_VPERMIL2F128
  UNSPEC_CAST
  UNSPEC_VTESTP
  UNSPEC_VCVTPH2PS
  UNSPEC_VCVTPS2PH

  ;; For AVX2 support
  UNSPEC_VPERMVAR
  UNSPEC_VPERMTI
  UNSPEC_GATHER
  UNSPEC_VSIBADDR

  ;; For AVX512F support
  UNSPEC_VPERMT2
  UNSPEC_UNSIGNED_FIX_NOTRUNC
  UNSPEC_UNSIGNED_PCMP
  UNSPEC_TESTM
  UNSPEC_TESTNM
  UNSPEC_SCATTER
  UNSPEC_RCP14
  UNSPEC_RSQRT14
  UNSPEC_FIXUPIMM
  UNSPEC_SCALEF
  UNSPEC_VTERNLOG
  UNSPEC_GETEXP
  UNSPEC_GETMANT
  UNSPEC_ALIGN
  UNSPEC_CONFLICT
  UNSPEC_COMPRESS
  UNSPEC_COMPRESS_STORE
  UNSPEC_EXPAND
  UNSPEC_MASKED_EQ
  UNSPEC_MASKED_GT

  ;; Mask operations
  UNSPEC_MASKOP
  UNSPEC_KORTEST
  UNSPEC_KTEST

  ;; For embed. rounding feature
  UNSPEC_EMBEDDED_ROUNDING

  ;; For AVX512PF support
  UNSPEC_GATHER_PREFETCH
  UNSPEC_SCATTER_PREFETCH

  ;; For AVX512ER support
  UNSPEC_EXP2
  UNSPEC_RCP28
  UNSPEC_RSQRT28

  ;; For SHA support
  UNSPEC_SHA1MSG1
  UNSPEC_SHA1MSG2
  UNSPEC_SHA1NEXTE
  UNSPEC_SHA1RNDS4
  UNSPEC_SHA256MSG1
  UNSPEC_SHA256MSG2
  UNSPEC_SHA256RNDS2

  ;; For AVX512BW support
  UNSPEC_DBPSADBW
  UNSPEC_PMADDUBSW512
  UNSPEC_PMADDWD512
  UNSPEC_PSHUFHW
  UNSPEC_PSHUFLW
  UNSPEC_CVTINT2MASK

  ;; For AVX512DQ support
  UNSPEC_REDUCE
  UNSPEC_FPCLASS
  UNSPEC_RANGE

  ;; For AVX512IFMA support
  UNSPEC_VPMADD52LUQ
  UNSPEC_VPMADD52HUQ

  ;; For AVX512VBMI support
  UNSPEC_VPMULTISHIFT

  ;; For AVX5124FMAPS/AVX5124VNNIW support
  UNSPEC_VP4FMADD
  UNSPEC_VP4FNMADD
  UNSPEC_VP4DPWSSD
  UNSPEC_VP4DPWSSDS

  ;; For GFNI support
  UNSPEC_GF2P8AFFINEINV
  UNSPEC_GF2P8AFFINE
  UNSPEC_GF2P8MUL

  ;; For AVX512VBMI2 support
  UNSPEC_VPSHLD
  UNSPEC_VPSHRD
  UNSPEC_VPSHRDV
  UNSPEC_VPSHLDV

  ;; For AVX512VNNI support
  UNSPEC_VPMADDUBSWACCD
  UNSPEC_VPMADDUBSWACCSSD
  UNSPEC_VPMADDWDACCD
  UNSPEC_VPMADDWDACCSSD

  ;; For VAES support
  UNSPEC_VAESDEC
  UNSPEC_VAESDECLAST
  UNSPEC_VAESENC
  UNSPEC_VAESENCLAST

  ;; For VPCLMULQDQ support
  UNSPEC_VPCLMULQDQ

  ;; For AVX512BITALG support
  UNSPEC_VPSHUFBIT
])

(define_c_enum "unspecv" [
  UNSPECV_LDMXCSR
  UNSPECV_STMXCSR
  UNSPECV_CLFLUSH
  UNSPECV_MONITOR
  UNSPECV_MWAIT
  UNSPECV_VZEROALL
  UNSPECV_VZEROUPPER
])

;; All vector modes including V?TImode, used in move patterns.
(define_mode_iterator VMOVE
  [(V64QI "TARGET_AVX512F") (V32QI "TARGET_AVX") V16QI
   (V32HI "TARGET_AVX512F") (V16HI "TARGET_AVX") V8HI
   (V16SI "TARGET_AVX512F") (V8SI "TARGET_AVX") V4SI
   (V8DI "TARGET_AVX512F")  (V4DI "TARGET_AVX") V2DI
   (V4TI "TARGET_AVX512F") (V2TI "TARGET_AVX") V1TI
   (V16SF "TARGET_AVX512F") (V8SF "TARGET_AVX") V4SF
   (V8DF "TARGET_AVX512F")  (V4DF "TARGET_AVX") V2DF])

;; All AVX-512{F,VL} vector modes. Supposed TARGET_AVX512F baseline.
(define_mode_iterator V48_AVX512VL
  [V16SI (V8SI "TARGET_AVX512VL") (V4SI "TARGET_AVX512VL")
   V8DI  (V4DI "TARGET_AVX512VL") (V2DI "TARGET_AVX512VL")
   V16SF (V8SF "TARGET_AVX512VL") (V4SF "TARGET_AVX512VL")
   V8DF  (V4DF "TARGET_AVX512VL") (V2DF "TARGET_AVX512VL")])

;; 1,2 byte AVX-512{BW,VL} vector modes. Supposed TARGET_AVX512BW baseline.
(define_mode_iterator VI12_AVX512VL
  [V64QI (V16QI "TARGET_AVX512VL") (V32QI "TARGET_AVX512VL")
   V32HI (V16HI "TARGET_AVX512VL") (V8HI "TARGET_AVX512VL")])

;; Same iterator, but without supposed TARGET_AVX512BW
(define_mode_iterator VI12_AVX512VLBW
  [(V64QI "TARGET_AVX512BW") (V16QI "TARGET_AVX512VL")
   (V32QI "TARGET_AVX512VL && TARGET_AVX512BW") (V32HI "TARGET_AVX512BW")
   (V16HI "TARGET_AVX512VL") (V8HI "TARGET_AVX512VL")])

(define_mode_iterator VI1_AVX512VL
  [V64QI (V16QI "TARGET_AVX512VL") (V32QI "TARGET_AVX512VL")])

;; All vector modes
(define_mode_iterator V
  [(V64QI "TARGET_AVX512F") (V32QI "TARGET_AVX") V16QI
   (V32HI "TARGET_AVX512F") (V16HI "TARGET_AVX") V8HI
   (V16SI "TARGET_AVX512F") (V8SI "TARGET_AVX") V4SI
   (V8DI "TARGET_AVX512F")  (V4DI "TARGET_AVX") V2DI
   (V16SF "TARGET_AVX512F") (V8SF "TARGET_AVX") V4SF
   (V8DF "TARGET_AVX512F")  (V4DF "TARGET_AVX") (V2DF "TARGET_SSE2")])

;; All 128bit vector modes
(define_mode_iterator V_128
  [V16QI V8HI V4SI V2DI V4SF (V2DF "TARGET_SSE2")])

;; All 256bit vector modes
(define_mode_iterator V_256
  [V32QI V16HI V8SI V4DI V8SF V4DF])

;; All 128bit and 256bit vector modes
(define_mode_iterator V_128_256
  [V32QI V16QI V16HI V8HI V8SI V4SI V4DI V2DI V8SF V4SF V4DF V2DF])

;; All 512bit vector modes
(define_mode_iterator V_512 [V64QI V32HI V16SI V8DI V16SF V8DF])

;; All 256bit and 512bit vector modes
(define_mode_iterator V_256_512
  [V32QI V16HI V8SI V4DI V8SF V4DF
   (V64QI "TARGET_AVX512F") (V32HI "TARGET_AVX512F") (V16SI "TARGET_AVX512F")
   (V8DI "TARGET_AVX512F") (V16SF "TARGET_AVX512F") (V8DF "TARGET_AVX512F")])

;; All vector float modes
(define_mode_iterator VF
  [(V16SF "TARGET_AVX512F") (V8SF "TARGET_AVX") V4SF
   (V8DF "TARGET_AVX512F") (V4DF "TARGET_AVX") (V2DF "TARGET_SSE2")])

;; 128- and 256-bit float vector modes
(define_mode_iterator VF_128_256
  [(V8SF "TARGET_AVX") V4SF
   (V4DF "TARGET_AVX") (V2DF "TARGET_SSE2")])

;; All SFmode vector float modes
(define_mode_iterator VF1
  [(V16SF "TARGET_AVX512F") (V8SF "TARGET_AVX") V4SF])

;; 128- and 256-bit SF vector modes
(define_mode_iterator VF1_128_256
  [(V8SF "TARGET_AVX") V4SF])

(define_mode_iterator VF1_128_256VL
  [V8SF (V4SF "TARGET_AVX512VL")])

;; All DFmode vector float modes
(define_mode_iterator VF2
  [(V8DF "TARGET_AVX512F") (V4DF "TARGET_AVX") V2DF])

;; 128- and 256-bit DF vector modes
(define_mode_iterator VF2_128_256
  [(V4DF "TARGET_AVX") V2DF])

(define_mode_iterator VF2_512_256
  [(V8DF "TARGET_AVX512F") V4DF])

(define_mode_iterator VF2_512_256VL
  [V8DF (V4DF "TARGET_AVX512VL")])

;; All 128bit vector float modes
(define_mode_iterator VF_128
  [V4SF (V2DF "TARGET_SSE2")])

;; All 256bit vector float modes
(define_mode_iterator VF_256
  [V8SF V4DF])

;; All 512bit vector float modes
(define_mode_iterator VF_512
  [V16SF V8DF])

(define_mode_iterator VI48_AVX512VL
  [V16SI (V8SI  "TARGET_AVX512VL") (V4SI  "TARGET_AVX512VL")
   V8DI  (V4DI  "TARGET_AVX512VL") (V2DI  "TARGET_AVX512VL")])

(define_mode_iterator VF_AVX512VL
  [V16SF (V8SF "TARGET_AVX512VL") (V4SF "TARGET_AVX512VL")
   V8DF (V4DF "TARGET_AVX512VL") (V2DF "TARGET_AVX512VL")])

(define_mode_iterator VF2_AVX512VL
  [V8DF (V4DF "TARGET_AVX512VL") (V2DF "TARGET_AVX512VL")])

(define_mode_iterator VF1_AVX512VL
  [V16SF (V8SF "TARGET_AVX512VL") (V4SF "TARGET_AVX512VL")])

;; All vector integer modes
(define_mode_iterator VI
  [(V16SI "TARGET_AVX512F") (V8DI "TARGET_AVX512F")
   (V64QI "TARGET_AVX512BW") (V32QI "TARGET_AVX") V16QI
   (V32HI "TARGET_AVX512BW") (V16HI "TARGET_AVX") V8HI
   (V8SI "TARGET_AVX") V4SI
   (V4DI "TARGET_AVX") V2DI])

(define_mode_iterator VI_AVX2
  [(V64QI "TARGET_AVX512BW") (V32QI "TARGET_AVX2") V16QI
   (V32HI "TARGET_AVX512BW") (V16HI "TARGET_AVX2") V8HI
   (V16SI "TARGET_AVX512F") (V8SI "TARGET_AVX2") V4SI
   (V8DI "TARGET_AVX512F") (V4DI "TARGET_AVX2") V2DI])

;; All QImode vector integer modes
(define_mode_iterator VI1
  [(V32QI "TARGET_AVX") V16QI])

;; All DImode vector integer modes
(define_mode_iterator V_AVX
  [V16QI V8HI V4SI V2DI V4SF V2DF
   (V32QI "TARGET_AVX") (V16HI "TARGET_AVX")
   (V8SI "TARGET_AVX") (V4DI "TARGET_AVX")
   (V8SF "TARGET_AVX") (V4DF"TARGET_AVX")])

(define_mode_iterator VI48_AVX
 [V4SI V2DI
  (V8SI "TARGET_AVX") (V4DI "TARGET_AVX")])

(define_mode_iterator VI8
  [(V8DI "TARGET_AVX512F") (V4DI "TARGET_AVX") V2DI])

(define_mode_iterator VI8_FVL
  [(V8DI "TARGET_AVX512F") V4DI (V2DI "TARGET_AVX512VL")])

(define_mode_iterator VI8_AVX512VL
  [V8DI (V4DI "TARGET_AVX512VL") (V2DI "TARGET_AVX512VL")])

(define_mode_iterator VI8_256_512
  [V8DI (V4DI "TARGET_AVX512VL")])

(define_mode_iterator VI1_AVX2
  [(V32QI "TARGET_AVX2") V16QI])

(define_mode_iterator VI1_AVX512
  [(V64QI "TARGET_AVX512BW") (V32QI "TARGET_AVX2") V16QI])

(define_mode_iterator VI1_AVX512F
  [(V64QI "TARGET_AVX512F") (V32QI "TARGET_AVX") V16QI])

(define_mode_iterator VI2_AVX2
  [(V32HI "TARGET_AVX512BW") (V16HI "TARGET_AVX2") V8HI])

(define_mode_iterator VI2_AVX512F
  [(V32HI "TARGET_AVX512F") (V16HI "TARGET_AVX2") V8HI])

(define_mode_iterator VI4_AVX
  [(V8SI "TARGET_AVX") V4SI])

(define_mode_iterator VI4_AVX2
  [(V8SI "TARGET_AVX2") V4SI])

(define_mode_iterator VI4_AVX512F
  [(V16SI "TARGET_AVX512F") (V8SI "TARGET_AVX2") V4SI])

(define_mode_iterator VI4_AVX512VL
  [V16SI (V8SI "TARGET_AVX512VL") (V4SI "TARGET_AVX512VL")])

(define_mode_iterator VI48_AVX512F_AVX512VL
  [V4SI V8SI (V16SI "TARGET_AVX512F")
   (V2DI "TARGET_AVX512VL") (V4DI "TARGET_AVX512VL") (V8DI "TARGET_AVX512F")])

(define_mode_iterator VI2_AVX512VL
  [(V8HI "TARGET_AVX512VL") (V16HI "TARGET_AVX512VL") V32HI])

(define_mode_iterator VI1_AVX512VL_F
  [V32QI (V16QI "TARGET_AVX512VL") (V64QI "TARGET_AVX512F")])

(define_mode_iterator VI8_AVX2_AVX512BW
  [(V8DI "TARGET_AVX512BW") (V4DI "TARGET_AVX2") V2DI])

(define_mode_iterator VI8_AVX2
  [(V4DI "TARGET_AVX2") V2DI])

(define_mode_iterator VI8_AVX2_AVX512F
  [(V8DI "TARGET_AVX512F") (V4DI "TARGET_AVX2") V2DI])

(define_mode_iterator VI8_AVX_AVX512F
  [(V8DI "TARGET_AVX512F") (V4DI "TARGET_AVX")])

(define_mode_iterator VI4_128_8_256
  [V4SI V4DI])

;; All V8D* modes
(define_mode_iterator V8FI
  [V8DF V8DI])

;; All V16S* modes
(define_mode_iterator V16FI
  [V16SF V16SI])

;; ??? We should probably use TImode instead.
(define_mode_iterator VIMAX_AVX2_AVX512BW
  [(V4TI "TARGET_AVX512BW") (V2TI "TARGET_AVX2") V1TI])

;; Suppose TARGET_AVX512BW as baseline
(define_mode_iterator VIMAX_AVX512VL
  [V4TI (V2TI "TARGET_AVX512VL") (V1TI "TARGET_AVX512VL")])

(define_mode_iterator VIMAX_AVX2
  [(V2TI "TARGET_AVX2") V1TI])

;; ??? This should probably be dropped in favor of VIMAX_AVX2_AVX512BW.
(define_mode_iterator SSESCALARMODE
  [(V4TI "TARGET_AVX512BW") (V2TI "TARGET_AVX2") TI])

(define_mode_iterator VI12_AVX2
  [(V64QI "TARGET_AVX512BW") (V32QI "TARGET_AVX2") V16QI
   (V32HI "TARGET_AVX512BW") (V16HI "TARGET_AVX2") V8HI])

(define_mode_iterator VI24_AVX2
  [(V16HI "TARGET_AVX2") V8HI
   (V8SI "TARGET_AVX2") V4SI])

(define_mode_iterator VI124_AVX2_24_AVX512F_1_AVX512BW
  [(V64QI "TARGET_AVX512BW") (V32QI "TARGET_AVX2") V16QI
   (V32HI "TARGET_AVX512F") (V16HI "TARGET_AVX2") V8HI
   (V16SI "TARGET_AVX512F") (V8SI "TARGET_AVX2") V4SI])

(define_mode_iterator VI124_AVX2
  [(V32QI "TARGET_AVX2") V16QI
   (V16HI "TARGET_AVX2") V8HI
   (V8SI "TARGET_AVX2") V4SI])

(define_mode_iterator VI2_AVX2_AVX512BW
  [(V32HI "TARGET_AVX512BW") (V16HI "TARGET_AVX2") V8HI])

(define_mode_iterator VI248_AVX512VL
  [V32HI V16SI V8DI
   (V16HI "TARGET_AVX512VL") (V8SI "TARGET_AVX512VL")
   (V4DI "TARGET_AVX512VL") (V8HI "TARGET_AVX512VL")
   (V4SI "TARGET_AVX512VL") (V2DI "TARGET_AVX512VL")])

(define_mode_iterator VI48_AVX2
  [(V8SI "TARGET_AVX2") V4SI
   (V4DI "TARGET_AVX2") V2DI])

(define_mode_iterator VI248_AVX2
  [(V16HI "TARGET_AVX2") V8HI
   (V8SI "TARGET_AVX2") V4SI
   (V4DI "TARGET_AVX2") V2DI])

(define_mode_iterator VI248_AVX2_8_AVX512F_24_AVX512BW
  [(V32HI "TARGET_AVX512BW") (V16HI "TARGET_AVX2") V8HI
   (V16SI "TARGET_AVX512BW") (V8SI "TARGET_AVX2") V4SI
   (V8DI "TARGET_AVX512F") (V4DI "TARGET_AVX2") V2DI])

(define_mode_iterator VI248_AVX512BW
  [(V32HI "TARGET_AVX512BW") V16SI V8DI])

(define_mode_iterator VI248_AVX512BW_AVX512VL
  [(V32HI "TARGET_AVX512BW") 
   (V4DI "TARGET_AVX512VL") V16SI V8DI])

;; Suppose TARGET_AVX512VL as baseline
(define_mode_iterator VI248_AVX512BW_1
 [(V16HI "TARGET_AVX512BW") (V8HI "TARGET_AVX512BW")
  V8SI V4SI
  V2DI])
   
(define_mode_iterator VI248_AVX512BW_2
 [(V16HI "TARGET_AVX512BW") (V8HI "TARGET_AVX512BW")
  V8SI V4SI
  V4DI V2DI])
   
(define_mode_iterator VI48_AVX512F
  [(V16SI "TARGET_AVX512F") V8SI V4SI
   (V8DI "TARGET_AVX512F") V4DI V2DI])

(define_mode_iterator VI48_AVX_AVX512F
  [(V16SI "TARGET_AVX512F") (V8SI "TARGET_AVX") V4SI
   (V8DI "TARGET_AVX512F") (V4DI "TARGET_AVX") V2DI])

(define_mode_iterator VI12_AVX_AVX512F
  [ (V64QI "TARGET_AVX512F") (V32QI "TARGET_AVX") V16QI
    (V32HI "TARGET_AVX512F") (V16HI "TARGET_AVX") V8HI])

(define_mode_iterator V48_AVX2
  [V4SF V2DF
   V8SF V4DF
   (V4SI "TARGET_AVX2") (V2DI "TARGET_AVX2")
   (V8SI "TARGET_AVX2") (V4DI "TARGET_AVX2")])

(define_mode_iterator VI1_AVX512VLBW
  [(V64QI "TARGET_AVX512BW") (V32QI  "TARGET_AVX512VL")
	(V16QI  "TARGET_AVX512VL")])

(define_mode_attr avx512
  [(V16QI "avx512vl") (V32QI "avx512vl") (V64QI "avx512bw")
   (V8HI  "avx512vl") (V16HI  "avx512vl") (V32HI "avx512bw")
   (V4SI  "avx512vl") (V8SI  "avx512vl") (V16SI "avx512f")
   (V2DI  "avx512vl") (V4DI  "avx512vl") (V8DI "avx512f")
   (V4SF "avx512vl") (V8SF "avx512vl") (V16SF "avx512f")
   (V2DF "avx512vl") (V4DF "avx512vl") (V8DF "avx512f")])

(define_mode_attr sse2_avx_avx512f
  [(V16QI "sse2") (V32QI "avx") (V64QI "avx512f")
   (V8HI  "avx512vl") (V16HI  "avx512vl") (V32HI "avx512bw")
   (V4SI  "sse2") (V8SI  "avx") (V16SI "avx512f")
   (V2DI  "avx512vl") (V4DI  "avx512vl") (V8DI "avx512f")
   (V16SF "avx512f") (V8SF "avx") (V4SF "avx")
   (V8DF "avx512f") (V4DF "avx") (V2DF "avx")])

(define_mode_attr sse2_avx2
  [(V16QI "sse2") (V32QI "avx2") (V64QI "avx512bw")
   (V8HI "sse2") (V16HI "avx2") (V32HI "avx512bw")
   (V4SI "sse2") (V8SI "avx2") (V16SI "avx512f")
   (V2DI "sse2") (V4DI "avx2") (V8DI "avx512f")
   (V1TI "sse2") (V2TI "avx2") (V4TI "avx512bw")])

(define_mode_attr ssse3_avx2
   [(V16QI "ssse3") (V32QI "avx2") (V64QI "avx512bw")
    (V4HI "ssse3") (V8HI "ssse3") (V16HI "avx2") (V32HI "avx512bw")
    (V4SI "ssse3") (V8SI "avx2")
    (V2DI "ssse3") (V4DI "avx2")
    (TI "ssse3") (V2TI "avx2") (V4TI "avx512bw")])

(define_mode_attr sse4_1_avx2
   [(V16QI "sse4_1") (V32QI "avx2") (V64QI "avx512bw")
    (V8HI "sse4_1") (V16HI "avx2") (V32HI "avx512bw")
    (V4SI "sse4_1") (V8SI "avx2") (V16SI "avx512f")
    (V2DI "sse4_1") (V4DI "avx2") (V8DI "avx512dq")])

(define_mode_attr avx_avx2
  [(V4SF "avx") (V2DF "avx")
   (V8SF "avx") (V4DF "avx")
   (V4SI "avx2") (V2DI "avx2")
   (V8SI "avx2") (V4DI "avx2")])

(define_mode_attr vec_avx2
  [(V16QI "vec") (V32QI "avx2")
   (V8HI "vec") (V16HI "avx2")
   (V4SI "vec") (V8SI "avx2")
   (V2DI "vec") (V4DI "avx2")])

(define_mode_attr avx2_avx512
  [(V4SI "avx2") (V8SI "avx2") (V16SI "avx512f")
   (V2DI "avx2") (V4DI "avx2") (V8DI "avx512f")
   (V4SF "avx2") (V8SF "avx2") (V16SF "avx512f")
   (V2DF "avx2") (V4DF "avx2") (V8DF "avx512f")
   (V8HI "avx512vl") (V16HI "avx512vl") (V32HI "avx512bw")])

(define_mode_attr shuffletype
  [(V16SF "f") (V16SI "i") (V8DF "f") (V8DI "i")
  (V8SF "f") (V8SI "i") (V4DF "f") (V4DI "i")
  (V4SF "f") (V4SI "i") (V2DF "f") (V2DI "i")
  (V32HI "i") (V16HI "i") (V8HI "i")
  (V64QI "i") (V32QI "i") (V16QI "i")
  (V4TI "i") (V2TI "i") (V1TI "i")])

(define_mode_attr ssequartermode
  [(V16SF "V4SF") (V8DF "V2DF") (V16SI "V4SI") (V8DI "V2DI")])

(define_mode_attr ssequarterinsnmode
  [(V16SF "V4SF") (V8DF "V2DF") (V16SI "TI") (V8DI "TI")])

(define_mode_attr ssedoublemodelower
  [(V16QI "v16hi") (V32QI "v32hi") (V64QI "v64hi")
   (V8HI "v8si")   (V16HI "v16si") (V32HI "v32si")
   (V4SI "v4di")   (V8SI "v8di")   (V16SI "v16di")])

(define_mode_attr ssedoublemode
  [(V4SF "V8SF") (V8SF "V16SF") (V16SF "V32SF")
   (V2DF "V4DF") (V4DF "V8DF") (V8DF "V16DF")
   (V16QI "V16HI") (V32QI "V32HI") (V64QI "V64HI")
   (V4HI "V4SI") (V8HI "V8SI") (V16HI "V16SI") (V32HI "V32SI")
   (V4SI "V4DI") (V8SI "V16SI") (V16SI "V32SI")
   (V4DI "V8DI") (V8DI "V16DI")])

(define_mode_attr ssebytemode
  [(V8DI "V64QI") (V4DI "V32QI") (V2DI "V16QI")])

;; All 128bit vector integer modes
(define_mode_iterator VI_128 [V16QI V8HI V4SI V2DI])

;; All 256bit vector integer modes
(define_mode_iterator VI_256 [V32QI V16HI V8SI V4DI])

;; Various 128bit vector integer mode combinations
(define_mode_iterator VI12_128 [V16QI V8HI])
(define_mode_iterator VI14_128 [V16QI V4SI])
(define_mode_iterator VI124_128 [V16QI V8HI V4SI])
(define_mode_iterator VI24_128 [V8HI V4SI])
(define_mode_iterator VI248_128 [V8HI V4SI V2DI])
(define_mode_iterator VI48_128 [V4SI V2DI])

;; Various 256bit and 512 vector integer mode combinations
(define_mode_iterator VI124_256 [V32QI V16HI V8SI])
(define_mode_iterator VI124_256_AVX512F_AVX512BW
  [V32QI V16HI V8SI
   (V64QI "TARGET_AVX512BW")
   (V32HI "TARGET_AVX512BW")
   (V16SI "TARGET_AVX512F")])
(define_mode_iterator VI48_256 [V8SI V4DI])
(define_mode_iterator VI48_512 [V16SI V8DI])
(define_mode_iterator VI4_256_8_512 [V8SI V8DI])
(define_mode_iterator VI_AVX512BW
  [V16SI V8DI (V32HI "TARGET_AVX512BW") (V64QI "TARGET_AVX512BW")])

;; Int-float size matches
(define_mode_iterator VI4F_128 [V4SI V4SF])
(define_mode_iterator VI8F_128 [V2DI V2DF])
(define_mode_iterator VI4F_256 [V8SI V8SF])
(define_mode_iterator VI8F_256 [V4DI V4DF])
(define_mode_iterator VI4F_256_512
  [V8SI V8SF
   (V16SI "TARGET_AVX512F") (V16SF "TARGET_AVX512F")])
(define_mode_iterator VI48F_256_512
  [V8SI V8SF
  (V16SI "TARGET_AVX512F") (V16SF "TARGET_AVX512F")
  (V8DI  "TARGET_AVX512F") (V8DF  "TARGET_AVX512F")
  (V4DI  "TARGET_AVX512VL") (V4DF  "TARGET_AVX512VL")])
(define_mode_iterator VF48_I1248
  [V16SI V16SF V8DI V8DF V32HI V64QI])
(define_mode_iterator VI48F
  [V16SI V16SF V8DI V8DF
   (V8SI "TARGET_AVX512VL") (V8SF "TARGET_AVX512VL")
   (V4DI "TARGET_AVX512VL") (V4DF "TARGET_AVX512VL")
   (V4SI "TARGET_AVX512VL") (V4SF "TARGET_AVX512VL")
   (V2DI "TARGET_AVX512VL") (V2DF "TARGET_AVX512VL")])
(define_mode_iterator VI48F_256 [V8SI V8SF V4DI V4DF])

;; Mapping from float mode to required SSE level
(define_mode_attr sse
  [(SF "sse") (DF "sse2")
   (V4SF "sse") (V2DF "sse2")
   (V16SF "avx512f") (V8SF "avx")
   (V8DF "avx512f") (V4DF "avx")])

(define_mode_attr sse2
  [(V16QI "sse2") (V32QI "avx") (V64QI "avx512f")
   (V2DI "sse2") (V4DI "avx") (V8DI "avx512f")])

(define_mode_attr sse3
  [(V16QI "sse3") (V32QI "avx")])

(define_mode_attr sse4_1
  [(V4SF "sse4_1") (V2DF "sse4_1")
   (V8SF "avx") (V4DF "avx")
   (V8DF "avx512f")
   (V4DI "avx") (V2DI "sse4_1")
   (V8SI "avx") (V4SI "sse4_1")
   (V16QI "sse4_1") (V32QI "avx")
   (V8HI "sse4_1") (V16HI "avx")])

(define_mode_attr avxsizesuffix
  [(V64QI "512") (V32HI "512") (V16SI "512") (V8DI "512")
   (V32QI "256") (V16HI "256") (V8SI "256") (V4DI "256")
   (V16QI "") (V8HI "") (V4SI "") (V2DI "")
   (V16SF "512") (V8DF "512")
   (V8SF "256") (V4DF "256")
   (V4SF "") (V2DF "")])

;; SSE instruction mode
(define_mode_attr sseinsnmode
  [(V64QI "XI") (V32HI "XI") (V16SI "XI") (V8DI "XI") (V4TI "XI")
   (V32QI "OI") (V16HI "OI") (V8SI "OI") (V4DI "OI") (V2TI "OI")
   (V16QI "TI") (V8HI "TI") (V4SI "TI") (V2DI "TI") (V1TI "TI")
   (V16SF "V16SF") (V8DF "V8DF")
   (V8SF "V8SF") (V4DF "V4DF")
   (V4SF "V4SF") (V2DF "V2DF")
   (TI "TI")])

;; Mapping of vector modes to corresponding mask size
(define_mode_attr avx512fmaskmode
  [(V64QI "DI") (V32QI "SI") (V16QI "HI")
   (V32HI "SI") (V16HI "HI") (V8HI  "QI") (V4HI "QI")
   (V16SI "HI") (V8SI  "QI") (V4SI  "QI")
   (V8DI  "QI") (V4DI  "QI") (V2DI  "QI")
   (V16SF "HI") (V8SF  "QI") (V4SF  "QI")
   (V8DF  "QI") (V4DF  "QI") (V2DF  "QI")])

;; Mapping of vector modes to corresponding mask size
(define_mode_attr avx512fmaskmodelower
  [(V64QI "di") (V32QI "si") (V16QI "hi")
   (V32HI "si") (V16HI "hi") (V8HI  "qi") (V4HI "qi")
   (V16SI "hi") (V8SI  "qi") (V4SI  "qi")
   (V8DI  "qi") (V4DI  "qi") (V2DI  "qi")
   (V16SF "hi") (V8SF  "qi") (V4SF  "qi")
   (V8DF  "qi") (V4DF  "qi") (V2DF  "qi")])

;; Mapping of vector float modes to an integer mode of the same size
(define_mode_attr sseintvecmode
  [(V16SF "V16SI") (V8DF  "V8DI")
   (V8SF  "V8SI")  (V4DF  "V4DI")
   (V4SF  "V4SI")  (V2DF  "V2DI")
   (V16SI "V16SI") (V8DI  "V8DI")
   (V8SI  "V8SI")  (V4DI  "V4DI")
   (V4SI  "V4SI")  (V2DI  "V2DI")
   (V16HI "V16HI") (V8HI  "V8HI")
   (V32HI "V32HI") (V64QI "V64QI")
   (V32QI "V32QI") (V16QI "V16QI")])

(define_mode_attr sseintvecmode2
  [(V8DF "XI") (V4DF "OI") (V2DF "TI")
   (V8SF "OI") (V4SF "TI")])

(define_mode_attr sseintvecmodelower
  [(V16SF "v16si") (V8DF "v8di")
   (V8SF "v8si") (V4DF "v4di")
   (V4SF "v4si") (V2DF "v2di")
   (V8SI "v8si") (V4DI "v4di")
   (V4SI "v4si") (V2DI "v2di")
   (V16HI "v16hi") (V8HI "v8hi")
   (V32QI "v32qi") (V16QI "v16qi")])

;; Mapping of vector modes to a vector mode of double size
(define_mode_attr ssedoublevecmode
  [(V32QI "V64QI") (V16HI "V32HI") (V8SI "V16SI") (V4DI "V8DI")
   (V16QI "V32QI") (V8HI "V16HI") (V4SI "V8SI") (V2DI "V4DI")
   (V8SF "V16SF") (V4DF "V8DF")
   (V4SF "V8SF") (V2DF "V4DF")])

;; Mapping of vector modes to a vector mode of half size
(define_mode_attr ssehalfvecmode
  [(V64QI "V32QI") (V32HI "V16HI") (V16SI "V8SI") (V8DI "V4DI") (V4TI "V2TI")
   (V32QI "V16QI") (V16HI  "V8HI") (V8SI  "V4SI") (V4DI "V2DI")
   (V16QI  "V8QI") (V8HI   "V4HI") (V4SI  "V2SI")
   (V16SF "V8SF") (V8DF "V4DF")
   (V8SF  "V4SF") (V4DF "V2DF")
   (V4SF  "V2SF")])

(define_mode_attr ssehalfvecmodelower
  [(V64QI "v32qi") (V32HI "v16hi") (V16SI "v8si") (V8DI "v4di") (V4TI "v2ti")
   (V32QI "v16qi") (V16HI  "v8hi") (V8SI  "v4si") (V4DI "v2di")
   (V16QI  "v8qi") (V8HI   "v4hi") (V4SI  "v2si")
   (V16SF "v8sf") (V8DF "v4df")
   (V8SF  "v4sf") (V4DF "v2df")
   (V4SF  "v2sf")])

;; Mapping of vector modes ti packed single mode of the same size
(define_mode_attr ssePSmode
  [(V16SI "V16SF") (V8DF "V16SF")
   (V16SF "V16SF") (V8DI "V16SF")
   (V64QI "V16SF") (V32QI "V8SF") (V16QI "V4SF")
   (V32HI "V16SF") (V16HI "V8SF") (V8HI "V4SF")
   (V8SI "V8SF") (V4SI "V4SF")
   (V4DI "V8SF") (V2DI "V4SF")
   (V4TI "V16SF") (V2TI "V8SF") (V1TI "V4SF")
   (V8SF "V8SF") (V4SF "V4SF")
   (V4DF "V8SF") (V2DF "V4SF")])

(define_mode_attr ssePSmode2
  [(V8DI "V8SF") (V4DI "V4SF")])

;; Mapping of vector modes back to the scalar modes
(define_mode_attr ssescalarmode
  [(V64QI "QI") (V32QI "QI") (V16QI "QI")
   (V32HI "HI") (V16HI "HI") (V8HI "HI")
   (V16SI "SI") (V8SI "SI")  (V4SI "SI")
   (V8DI "DI")  (V4DI "DI")  (V2DI "DI")
   (V16SF "SF") (V8SF "SF")  (V4SF "SF")
   (V8DF "DF")  (V4DF "DF")  (V2DF "DF")
   (V4TI "TI")  (V2TI "TI")])

;; Mapping of vector modes back to the scalar modes
(define_mode_attr ssescalarmodelower
  [(V64QI "qi") (V32QI "qi") (V16QI "qi")
   (V32HI "hi") (V16HI "hi") (V8HI "hi")
   (V16SI "si") (V8SI "si")  (V4SI "si")
   (V8DI "di")  (V4DI "di")  (V2DI "di")
   (V16SF "sf") (V8SF "sf")  (V4SF "sf")
   (V8DF "df")  (V4DF "df")  (V2DF "df")
   (V4TI "ti")  (V2TI "ti")])

;; Mapping of vector modes to the 128bit modes
(define_mode_attr ssexmmmode
  [(V64QI "V16QI") (V32QI "V16QI") (V16QI "V16QI")
   (V32HI "V8HI")  (V16HI "V8HI") (V8HI "V8HI")
   (V16SI "V4SI")  (V8SI "V4SI")  (V4SI "V4SI")
   (V8DI "V2DI")   (V4DI "V2DI")  (V2DI "V2DI")
   (V16SF "V4SF")  (V8SF "V4SF")  (V4SF "V4SF")
   (V8DF "V2DF")   (V4DF "V2DF")  (V2DF "V2DF")])

;; Pointer size override for scalar modes (Intel asm dialect)
(define_mode_attr iptr
  [(V64QI "b") (V32HI "w") (V16SI "k") (V8DI "q")
   (V32QI "b") (V16HI "w") (V8SI "k") (V4DI "q")
   (V16QI "b") (V8HI "w") (V4SI "k") (V2DI "q")
   (V16SF "k") (V8DF "q")
   (V8SF "k") (V4DF "q")
   (V4SF "k") (V2DF "q")
   (SF "k") (DF "q")])

;; Number of scalar elements in each vector type
(define_mode_attr ssescalarnum
  [(V64QI "64") (V16SI "16") (V8DI "8")
   (V32QI "32") (V16HI "16") (V8SI "8") (V4DI "4")
   (V16QI "16") (V8HI "8") (V4SI "4") (V2DI "2")
   (V16SF "16") (V8DF "8")
   (V8SF "8") (V4DF "4")
   (V4SF "4") (V2DF "2")])

;; Mask of scalar elements in each vector type
(define_mode_attr ssescalarnummask
  [(V32QI "31") (V16HI "15") (V8SI "7") (V4DI "3")
   (V16QI "15") (V8HI "7") (V4SI "3") (V2DI "1")
   (V8SF "7") (V4DF "3")
   (V4SF "3") (V2DF "1")])

(define_mode_attr ssescalarsize
  [(V4TI  "64") (V2TI  "64") (V1TI  "64")
   (V8DI  "64") (V4DI  "64") (V2DI  "64")
   (V64QI "8") (V32QI "8") (V16QI "8")
   (V32HI "16") (V16HI "16") (V8HI "16")
   (V16SI "32") (V8SI "32") (V4SI "32")
   (V16SF "32") (V8SF "32") (V4SF "32")
   (V8DF "64") (V4DF "64") (V2DF "64")])

;; SSE prefix for integer vector modes
(define_mode_attr sseintprefix
  [(V2DI  "p") (V2DF  "")
   (V4DI  "p") (V4DF  "")
   (V8DI  "p") (V8DF  "")
   (V4SI  "p") (V4SF  "")
   (V8SI  "p") (V8SF  "")
   (V16SI "p") (V16SF "")
   (V16QI "p") (V8HI "p")
   (V32QI "p") (V16HI "p")
   (V64QI "p") (V32HI "p")])

;; SSE scalar suffix for vector modes
(define_mode_attr ssescalarmodesuffix
  [(SF "ss") (DF "sd")
   (V16SF "ss") (V8DF "sd")
   (V8SF "ss") (V4DF "sd")
   (V4SF "ss") (V2DF "sd")
   (V16SI "d") (V8DI "q")
   (V8SI "d") (V4DI "q")
   (V4SI "d") (V2DI "q")])

;; Pack/unpack vector modes
(define_mode_attr sseunpackmode
  [(V16QI "V8HI") (V8HI "V4SI") (V4SI "V2DI")
   (V32QI "V16HI") (V16HI "V8SI") (V8SI "V4DI")
   (V32HI "V16SI") (V64QI "V32HI") (V16SI "V8DI")])

(define_mode_attr ssepackmode
  [(V8HI "V16QI") (V4SI "V8HI") (V2DI "V4SI")
   (V16HI "V32QI") (V8SI "V16HI") (V4DI "V8SI")
   (V32HI "V64QI") (V16SI "V32HI") (V8DI "V16SI")])

;; Mapping of the max integer size for xop rotate immediate constraint
(define_mode_attr sserotatemax
  [(V16QI "7") (V8HI "15") (V4SI "31") (V2DI "63")])

;; Mapping of mode to cast intrinsic name
(define_mode_attr castmode
 [(V8SI "si") (V8SF "ps") (V4DF "pd")
  (V16SI "si") (V16SF "ps") (V8DF "pd")])

;; Instruction suffix for sign and zero extensions.
(define_code_attr extsuffix [(sign_extend "sx") (zero_extend "zx")])

;; i128 for integer vectors and TARGET_AVX2, f128 otherwise.
;; i64x4 or f64x4 for 512bit modes.
(define_mode_attr i128
  [(V16SF "f64x4") (V8SF "f128") (V8DF "f64x4") (V4DF "f128")
   (V64QI "i64x4") (V32QI "%~128") (V32HI "i64x4") (V16HI "%~128")
   (V16SI "i64x4") (V8SI "%~128") (V8DI "i64x4") (V4DI "%~128")])

;; For 256-bit modes for TARGET_AVX512VL && TARGET_AVX512DQ
;; i32x4, f32x4, i64x2 or f64x2 suffixes.
(define_mode_attr i128vldq
  [(V8SF "f32x4") (V4DF "f64x2")
   (V32QI "i32x4") (V16HI "i32x4") (V8SI "i32x4") (V4DI "i64x2")])

;; Mix-n-match
(define_mode_iterator AVX256MODE2P [V8SI V8SF V4DF])
(define_mode_iterator AVX512MODE2P [V16SI V16SF V8DF])

;; Mapping for dbpsabbw modes
(define_mode_attr dbpsadbwmode
  [(V32HI "V64QI") (V16HI "V32QI") (V8HI "V16QI")])

;; Mapping suffixes for broadcast
(define_mode_attr bcstscalarsuff
  [(V64QI "b")  (V32QI "b") (V16QI "b")
   (V32HI "w")  (V16HI "w") (V8HI "w")
   (V16SI "d")  (V8SI "d")  (V4SI "d")
   (V8DI "q")   (V4DI "q")  (V2DI "q")
   (V16SF "ss") (V8SF "ss") (V4SF "ss")
   (V8DF "sd")  (V4DF "sd") (V2DF "sd")])

;; Tie mode of assembler operand to mode iterator
(define_mode_attr xtg_mode
  [(V16QI "x") (V8HI "x") (V4SI "x") (V2DI "x") (V4SF "x") (V2DF "x")
   (V32QI "t") (V16HI "t") (V8SI "t") (V4DI "t") (V8SF "t") (V4DF "t")
   (V64QI "g") (V32HI "g") (V16SI "g") (V8DI "g") (V16SF "g") (V8DF "g")])

;; Half mask mode for unpacks
(define_mode_attr HALFMASKMODE
  [(DI "SI") (SI "HI")])

;; Double mask mode for packs
(define_mode_attr DOUBLEMASKMODE
  [(HI "SI") (SI "DI")])


;; Include define_subst patterns for instructions with mask
(include "subst.md")

;; Patterns whose name begins with "sse{,2,3}_" are invoked by intrinsics.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Move patterns
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All of these patterns are enabled for SSE1 as well as SSE2.
;; This is essential for maintaining stable calling conventions.

(define_expand "mov<mode>"
  [(set (match_operand:VMOVE 0 "nonimmediate_operand")
	(match_operand:VMOVE 1 "nonimmediate_operand"))]
  "TARGET_SSE"
{
  ix86_expand_vector_move (<MODE>mode, operands);
  DONE;
})

(define_insn "mov<mode>_internal"
  [(set (match_operand:VMOVE 0 "nonimmediate_operand"
	 "=v,v ,v ,m")
	(match_operand:VMOVE 1 "nonimmediate_or_sse_const_operand"
	 " C,BC,vm,v"))]
  "TARGET_SSE
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
{
  switch (get_attr_type (insn))
    {
    case TYPE_SSELOG1:
      return standard_sse_constant_opcode (insn, operands);

    case TYPE_SSEMOV:
      /* There is no evex-encoded vmov* for sizes smaller than 64-bytes
	 in avx512f, so we need to use workarounds, to access sse registers
	 16-31, which are evex-only. In avx512vl we don't need workarounds.  */
      if (TARGET_AVX512F && <MODE_SIZE> < 64 && !TARGET_AVX512VL
	  && (EXT_REX_SSE_REG_P (operands[0])
	      || EXT_REX_SSE_REG_P (operands[1])))
	{
	  if (memory_operand (operands[0], <MODE>mode))
	    {
	      if (<MODE_SIZE> == 32)
		return "vextract<shuffletype>64x4\t{$0x0, %g1, %0|%0, %g1, 0x0}";
	      else if (<MODE_SIZE> == 16)
		return "vextract<shuffletype>32x4\t{$0x0, %g1, %0|%0, %g1, 0x0}";
	      else
		gcc_unreachable ();
	    }
	  else if (memory_operand (operands[1], <MODE>mode))
	    {
	      if (<MODE_SIZE> == 32)
		return "vbroadcast<shuffletype>64x4\t{%1, %g0|%g0, %1}";
	      else if (<MODE_SIZE> == 16)
		return "vbroadcast<shuffletype>32x4\t{%1, %g0|%g0, %1}";
	      else
		gcc_unreachable ();
	    }
	  else
	    /* Reg -> reg move is always aligned.  Just use wider move.  */
	    switch (get_attr_mode (insn))
	      {
	      case MODE_V8SF:
	      case MODE_V4SF:
		return "vmovaps\t{%g1, %g0|%g0, %g1}";
	      case MODE_V4DF:
	      case MODE_V2DF:
		return "vmovapd\t{%g1, %g0|%g0, %g1}";
	      case MODE_OI:
	      case MODE_TI:
		return "vmovdqa64\t{%g1, %g0|%g0, %g1}";
	      default:
		gcc_unreachable ();
	      }
	}

      switch (get_attr_mode (insn))
	{
	case MODE_V16SF:
	case MODE_V8SF:
	case MODE_V4SF:
	  if (misaligned_operand (operands[0], <MODE>mode)
	      || misaligned_operand (operands[1], <MODE>mode))
	    return "%vmovups\t{%1, %0|%0, %1}";
	  else
	    return "%vmovaps\t{%1, %0|%0, %1}";

	case MODE_V8DF:
	case MODE_V4DF:
	case MODE_V2DF:
	  if (misaligned_operand (operands[0], <MODE>mode)
	      || misaligned_operand (operands[1], <MODE>mode))
	    return "%vmovupd\t{%1, %0|%0, %1}";
	  else
	    return "%vmovapd\t{%1, %0|%0, %1}";

	case MODE_OI:
	case MODE_TI:
	  if (misaligned_operand (operands[0], <MODE>mode)
	      || misaligned_operand (operands[1], <MODE>mode))
	    return TARGET_AVX512VL
		   && (<MODE>mode == V4SImode
		       || <MODE>mode == V2DImode
		       || <MODE>mode == V8SImode
		       || <MODE>mode == V4DImode
		       || TARGET_AVX512BW)
		   ? "vmovdqu<ssescalarsize>\t{%1, %0|%0, %1}"
		   : "%vmovdqu\t{%1, %0|%0, %1}";
	  else
	    return TARGET_AVX512VL ? "vmovdqa64\t{%1, %0|%0, %1}"
				   : "%vmovdqa\t{%1, %0|%0, %1}";
	case MODE_XI:
	  if (misaligned_operand (operands[0], <MODE>mode)
	      || misaligned_operand (operands[1], <MODE>mode))
	    return (<MODE>mode == V16SImode
		    || <MODE>mode == V8DImode
		    || TARGET_AVX512BW)
		   ? "vmovdqu<ssescalarsize>\t{%1, %0|%0, %1}"
		   : "vmovdqu64\t{%1, %0|%0, %1}";
	  else
	    return "vmovdqa64\t{%1, %0|%0, %1}";

	default:
	  gcc_unreachable ();
	}

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "sselog1,sselog1,ssemov,ssemov")
   (set_attr "prefix" "maybe_vex")
   (set (attr "mode")
	(cond [(and (eq_attr "alternative" "1")
		    (match_test "TARGET_AVX512VL"))
		 (const_string "<sseinsnmode>")
	       (and (match_test "<MODE_SIZE> == 16")
		    (ior (match_test "TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL")
			 (and (eq_attr "alternative" "3")
			      (match_test "TARGET_SSE_TYPELESS_STORES"))))
		 (const_string "<ssePSmode>")
	       (match_test "TARGET_AVX")
		 (const_string "<sseinsnmode>")
	       (ior (not (match_test "TARGET_SSE2"))
		    (match_test "optimize_function_for_size_p (cfun)"))
		 (const_string "V4SF")
	       (and (eq_attr "alternative" "0")
		    (match_test "TARGET_SSE_LOAD0_BY_PXOR"))
		 (const_string "TI")
	      ]
	      (const_string "<sseinsnmode>")))
   (set (attr "enabled")
        (cond [(and (match_test "<MODE_SIZE> == 16")
		    (eq_attr "alternative" "1"))
		 (symbol_ref "TARGET_SSE2")
	       (and (match_test "<MODE_SIZE> == 32")
		    (eq_attr "alternative" "1"))
		 (symbol_ref "TARGET_AVX2")
	      ]
	      (symbol_ref "true")))])

(define_insn "<avx512>_load<mode>_mask"
  [(set (match_operand:V48_AVX512VL 0 "register_operand" "=v,v")
	(vec_merge:V48_AVX512VL
	  (match_operand:V48_AVX512VL 1 "nonimmediate_operand" "v,m")
	  (match_operand:V48_AVX512VL 2 "nonimm_or_0_operand" "0C,0C")
	  (match_operand:<avx512fmaskmode> 3 "register_operand" "Yk,Yk")))]
  "TARGET_AVX512F"
{
  if (FLOAT_MODE_P (GET_MODE_INNER (<MODE>mode)))
    {
      if (misaligned_operand (operands[1], <MODE>mode))
	return "vmovu<ssemodesuffix>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}";
      else
	return "vmova<ssemodesuffix>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}";
    }
  else
    {
      if (misaligned_operand (operands[1], <MODE>mode))
	return "vmovdqu<ssescalarsize>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}";
      else
	return "vmovdqa<ssescalarsize>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}";
    }
}
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "memory" "none,load")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_load<mode>_mask"
  [(set (match_operand:VI12_AVX512VL 0 "register_operand" "=v,v")
	(vec_merge:VI12_AVX512VL
	  (match_operand:VI12_AVX512VL 1 "nonimmediate_operand" "v,m")
	  (match_operand:VI12_AVX512VL 2 "nonimm_or_0_operand" "0C,0C")
	  (match_operand:<avx512fmaskmode> 3 "register_operand" "Yk,Yk")))]
  "TARGET_AVX512BW"
  "vmovdqu<ssescalarsize>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "memory" "none,load")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_blendm<mode>"
  [(set (match_operand:V48_AVX512VL 0 "register_operand" "=v")
	(vec_merge:V48_AVX512VL
	  (match_operand:V48_AVX512VL 2 "nonimmediate_operand" "vm")
	  (match_operand:V48_AVX512VL 1 "register_operand" "v")
	  (match_operand:<avx512fmaskmode> 3 "register_operand" "Yk")))]
  "TARGET_AVX512F"
  "v<sseintprefix>blendm<ssemodesuffix>\t{%2, %1, %0%{%3%}|%0%{%3%}, %1, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_blendm<mode>"
  [(set (match_operand:VI12_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI12_AVX512VL
	  (match_operand:VI12_AVX512VL 2 "nonimmediate_operand" "vm")
	  (match_operand:VI12_AVX512VL 1 "register_operand" "v")
	  (match_operand:<avx512fmaskmode> 3 "register_operand" "Yk")))]
  "TARGET_AVX512BW"
  "vpblendm<ssemodesuffix>\t{%2, %1, %0%{%3%}|%0%{%3%}, %1, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_store<mode>_mask"
  [(set (match_operand:V48_AVX512VL 0 "memory_operand" "=m")
	(vec_merge:V48_AVX512VL
	  (match_operand:V48_AVX512VL 1 "register_operand" "v")
	  (match_dup 0)
	  (match_operand:<avx512fmaskmode> 2 "register_operand" "Yk")))]
  "TARGET_AVX512F"
{
  if (FLOAT_MODE_P (GET_MODE_INNER (<MODE>mode)))
    {
      if (misaligned_operand (operands[0], <MODE>mode))
	return "vmovu<ssemodesuffix>\t{%1, %0%{%2%}|%0%{%2%}, %1}";
      else
	return "vmova<ssemodesuffix>\t{%1, %0%{%2%}|%0%{%2%}, %1}";
    }
  else
    {
      if (misaligned_operand (operands[0], <MODE>mode))
	return "vmovdqu<ssescalarsize>\t{%1, %0%{%2%}|%0%{%2%}, %1}";
      else
	return "vmovdqa<ssescalarsize>\t{%1, %0%{%2%}|%0%{%2%}, %1}";
    }
}
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "memory" "store")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_store<mode>_mask"
  [(set (match_operand:VI12_AVX512VL 0 "memory_operand" "=m")
	(vec_merge:VI12_AVX512VL
	  (match_operand:VI12_AVX512VL 1 "register_operand" "v")
	  (match_dup 0)
	  (match_operand:<avx512fmaskmode> 2 "register_operand" "Yk")))]
  "TARGET_AVX512BW"
  "vmovdqu<ssescalarsize>\t{%1, %0%{%2%}|%0%{%2%}, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "memory" "store")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "sse2_movq128"
  [(set (match_operand:V2DI 0 "register_operand" "=v")
	(vec_concat:V2DI
	  (vec_select:DI
	    (match_operand:V2DI 1 "nonimmediate_operand" "vm")
	    (parallel [(const_int 0)]))
	  (const_int 0)))]
  "TARGET_SSE2"
  "%vmovq\t{%1, %0|%0, %q1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

;; Move a DI from a 32-bit register pair (e.g. %edx:%eax) to an xmm.
;; We'd rather avoid this entirely; if the 32-bit reg pair was loaded
;; from memory, we'd prefer to load the memory directly into the %xmm
;; register.  To facilitate this happy circumstance, this pattern won't
;; split until after register allocation.  If the 64-bit value didn't
;; come from memory, this is the best we can do.  This is much better
;; than storing %edx:%eax into a stack temporary and loading an %xmm
;; from there.

(define_insn_and_split "movdi_to_sse"
  [(parallel
    [(set (match_operand:V4SI 0 "register_operand" "=?x,x")
	  (subreg:V4SI (match_operand:DI 1 "nonimmediate_operand" "r,m") 0))
     (clobber (match_scratch:V4SI 2 "=&x,X"))])]
  "!TARGET_64BIT && TARGET_SSE2 && TARGET_INTER_UNIT_MOVES_TO_VEC"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
 if (register_operand (operands[1], DImode))
   {
      /* The DImode arrived in a pair of integral registers (e.g. %edx:%eax).
	 Assemble the 64-bit DImode value in an xmm register.  */
      emit_insn (gen_sse2_loadld (operands[0], CONST0_RTX (V4SImode),
				  gen_lowpart (SImode, operands[1])));
      emit_insn (gen_sse2_loadld (operands[2], CONST0_RTX (V4SImode),
				  gen_highpart (SImode, operands[1])));
      emit_insn (gen_vec_interleave_lowv4si (operands[0], operands[0],
					     operands[2]));
   }
 else if (memory_operand (operands[1], DImode))
   emit_insn (gen_vec_concatv2di (gen_lowpart (V2DImode, operands[0]),
				  operands[1], const0_rtx));
 else
   gcc_unreachable ();
 DONE;
})

(define_split
  [(set (match_operand:V4SF 0 "register_operand")
	(match_operand:V4SF 1 "zero_extended_scalar_load_operand"))]
  "TARGET_SSE && reload_completed"
  [(set (match_dup 0)
	(vec_merge:V4SF
	  (vec_duplicate:V4SF (match_dup 1))
	  (match_dup 2)
	  (const_int 1)))]
{
  operands[1] = gen_lowpart (SFmode, operands[1]);
  operands[2] = CONST0_RTX (V4SFmode);
})

(define_split
  [(set (match_operand:V2DF 0 "register_operand")
	(match_operand:V2DF 1 "zero_extended_scalar_load_operand"))]
  "TARGET_SSE2 && reload_completed"
  [(set (match_dup 0) (vec_concat:V2DF (match_dup 1) (match_dup 2)))]
{
  operands[1] = gen_lowpart (DFmode, operands[1]);
  operands[2] = CONST0_RTX (DFmode);
})

(define_expand "movmisalign<mode>"
  [(set (match_operand:VMOVE 0 "nonimmediate_operand")
	(match_operand:VMOVE 1 "nonimmediate_operand"))]
  "TARGET_SSE"
{
  ix86_expand_vector_move_misalign (<MODE>mode, operands);
  DONE;
})

;; Merge movsd/movhpd to movupd for TARGET_SSE_UNALIGNED_LOAD_OPTIMAL targets.
(define_peephole2
  [(set (match_operand:V2DF 0 "sse_reg_operand")
	(vec_concat:V2DF (match_operand:DF 1 "memory_operand")
			 (match_operand:DF 4 "const0_operand")))
   (set (match_operand:V2DF 2 "sse_reg_operand")
	(vec_concat:V2DF (vec_select:DF (match_dup 2)
					(parallel [(const_int 0)]))
			 (match_operand:DF 3 "memory_operand")))]
  "TARGET_SSE2 && TARGET_SSE_UNALIGNED_LOAD_OPTIMAL
   && ix86_operands_ok_for_move_multiple (operands, true, DFmode)"
  [(set (match_dup 2) (match_dup 5))]
  "operands[5] = adjust_address (operands[1], V2DFmode, 0);")

(define_peephole2
  [(set (match_operand:DF 0 "sse_reg_operand")
	(match_operand:DF 1 "memory_operand"))
   (set (match_operand:V2DF 2 "sse_reg_operand")
	(vec_concat:V2DF (match_operand:DF 4 "sse_reg_operand")
			 (match_operand:DF 3 "memory_operand")))]
  "TARGET_SSE2 && TARGET_SSE_UNALIGNED_LOAD_OPTIMAL
   && REGNO (operands[4]) == REGNO (operands[2])
   && ix86_operands_ok_for_move_multiple (operands, true, DFmode)"
  [(set (match_dup 2) (match_dup 5))]
  "operands[5] = adjust_address (operands[1], V2DFmode, 0);")

;; Merge movlpd/movhpd to movupd for TARGET_SSE_UNALIGNED_STORE_OPTIMAL targets.
(define_peephole2
  [(set (match_operand:DF 0 "memory_operand")
	(vec_select:DF (match_operand:V2DF 1 "sse_reg_operand")
		       (parallel [(const_int 0)])))
   (set (match_operand:DF 2 "memory_operand")
	(vec_select:DF (match_operand:V2DF 3 "sse_reg_operand")
		       (parallel [(const_int 1)])))]
  "TARGET_SSE2 && TARGET_SSE_UNALIGNED_STORE_OPTIMAL
   && ix86_operands_ok_for_move_multiple (operands, false, DFmode)"
  [(set (match_dup 4) (match_dup 1))]
  "operands[4] = adjust_address (operands[0], V2DFmode, 0);")

(define_insn "<sse3>_lddqu<avxsizesuffix>"
  [(set (match_operand:VI1 0 "register_operand" "=x")
	(unspec:VI1 [(match_operand:VI1 1 "memory_operand" "m")]
		    UNSPEC_LDDQU))]
  "TARGET_SSE3"
  "%vlddqu\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "movu" "1")
   (set (attr "prefix_data16")
     (if_then_else
       (match_test "TARGET_AVX")
     (const_string "*")
     (const_string "0")))
   (set (attr "prefix_rep")
     (if_then_else
       (match_test "TARGET_AVX")
     (const_string "*")
     (const_string "1")))
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "sse2_movnti<mode>"
  [(set (match_operand:SWI48 0 "memory_operand" "=m")
	(unspec:SWI48 [(match_operand:SWI48 1 "register_operand" "r")]
		      UNSPEC_MOVNT))]
  "TARGET_SSE2"
  "movnti\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_data16" "0")
   (set_attr "mode" "<MODE>")])

(define_insn "<sse>_movnt<mode>"
  [(set (match_operand:VF 0 "memory_operand" "=m")
	(unspec:VF
	  [(match_operand:VF 1 "register_operand" "v")]
	  UNSPEC_MOVNT))]
  "TARGET_SSE"
  "%vmovnt<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

(define_insn "<sse2>_movnt<mode>"
  [(set (match_operand:VI8 0 "memory_operand" "=m")
	(unspec:VI8 [(match_operand:VI8 1 "register_operand" "v")]
		    UNSPEC_MOVNT))]
  "TARGET_SSE2"
  "%vmovntdq\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set (attr "prefix_data16")
     (if_then_else
       (match_test "TARGET_AVX")
     (const_string "*")
     (const_string "1")))
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<sseinsnmode>")])

; Expand patterns for non-temporal stores.  At the moment, only those
; that directly map to insns are defined; it would be possible to
; define patterns for other modes that would expand to several insns.

;; Modes handled by storent patterns.
(define_mode_iterator STORENT_MODE
  [(DI "TARGET_SSE2 && TARGET_64BIT") (SI "TARGET_SSE2")
   (SF "TARGET_SSE4A") (DF "TARGET_SSE4A")
   (V8DI "TARGET_AVX512F") (V4DI "TARGET_AVX") (V2DI "TARGET_SSE2")
   (V16SF "TARGET_AVX512F") (V8SF "TARGET_AVX") V4SF
   (V8DF "TARGET_AVX512F") (V4DF "TARGET_AVX") (V2DF "TARGET_SSE2")])

(define_expand "storent<mode>"
  [(set (match_operand:STORENT_MODE 0 "memory_operand")
	(unspec:STORENT_MODE
	  [(match_operand:STORENT_MODE 1 "register_operand")]
	  UNSPEC_MOVNT))]
  "TARGET_SSE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mask operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All integer modes with AVX512BW/DQ.
(define_mode_iterator SWI1248_AVX512BWDQ
  [(QI "TARGET_AVX512DQ") HI (SI "TARGET_AVX512BW") (DI "TARGET_AVX512BW")])

;; All integer modes with AVX512BW, where HImode operation
;; can be used instead of QImode.
(define_mode_iterator SWI1248_AVX512BW
  [QI HI (SI "TARGET_AVX512BW") (DI "TARGET_AVX512BW")])

;; All integer modes with AVX512BW/DQ, even HImode requires DQ.
(define_mode_iterator SWI1248_AVX512BWDQ2
  [(QI "TARGET_AVX512DQ") (HI "TARGET_AVX512DQ")
   (SI "TARGET_AVX512BW") (DI "TARGET_AVX512BW")])

(define_expand "kmov<mskmodesuffix>"
  [(set (match_operand:SWI1248_AVX512BWDQ 0 "nonimmediate_operand")
	(match_operand:SWI1248_AVX512BWDQ 1 "nonimmediate_operand"))]
  "TARGET_AVX512F
   && !(MEM_P (operands[0]) && MEM_P (operands[1]))")

(define_insn "k<code><mode>"
  [(set (match_operand:SWI1248_AVX512BW 0 "register_operand" "=k")
	(any_logic:SWI1248_AVX512BW
	  (match_operand:SWI1248_AVX512BW 1 "register_operand" "k")
	  (match_operand:SWI1248_AVX512BW 2 "register_operand" "k")))
   (unspec [(const_int 0)] UNSPEC_MASKOP)]
  "TARGET_AVX512F"
{
  if (get_attr_mode (insn) == MODE_HI)
    return "k<logic>w\t{%2, %1, %0|%0, %1, %2}";
  else
    return "k<logic><mskmodesuffix>\t{%2, %1, %0|%0, %1, %2}";
}
  [(set_attr "type" "msklog")
   (set_attr "prefix" "vex")
   (set (attr "mode")
     (cond [(and (match_test "<MODE>mode == QImode")
		 (not (match_test "TARGET_AVX512DQ")))
	       (const_string "HI")
	   ]
	   (const_string "<MODE>")))])

(define_insn "kandn<mode>"
  [(set (match_operand:SWI1248_AVX512BW 0 "register_operand" "=k")
	(and:SWI1248_AVX512BW
	  (not:SWI1248_AVX512BW
	    (match_operand:SWI1248_AVX512BW 1 "register_operand" "k"))
	  (match_operand:SWI1248_AVX512BW 2 "register_operand" "k")))
   (unspec [(const_int 0)] UNSPEC_MASKOP)]
  "TARGET_AVX512F"
{
  if (get_attr_mode (insn) == MODE_HI)
    return "kandnw\t{%2, %1, %0|%0, %1, %2}";
  else
    return "kandn<mskmodesuffix>\t{%2, %1, %0|%0, %1, %2}";
}
  [(set_attr "type" "msklog")
   (set_attr "prefix" "vex")
   (set (attr "mode")
     (cond [(and (match_test "<MODE>mode == QImode")
		 (not (match_test "TARGET_AVX512DQ")))
	      (const_string "HI")
	   ]
	   (const_string "<MODE>")))])

(define_insn "kxnor<mode>"
  [(set (match_operand:SWI1248_AVX512BW 0 "register_operand" "=k")
	(not:SWI1248_AVX512BW
	  (xor:SWI1248_AVX512BW
	    (match_operand:SWI1248_AVX512BW 1 "register_operand" "k")
	    (match_operand:SWI1248_AVX512BW 2 "register_operand" "k"))))
   (unspec [(const_int 0)] UNSPEC_MASKOP)]
  "TARGET_AVX512F"
{
  if (get_attr_mode (insn) == MODE_HI)
    return "kxnorw\t{%2, %1, %0|%0, %1, %2}";
  else
    return "kxnor<mskmodesuffix>\t{%2, %1, %0|%0, %1, %2}";
}
  [(set_attr "type" "msklog")
   (set_attr "prefix" "vex")
   (set (attr "mode")
     (cond [(and (match_test "<MODE>mode == QImode")
		 (not (match_test "TARGET_AVX512DQ")))
	      (const_string "HI")
	   ]
	   (const_string "<MODE>")))])

(define_insn "knot<mode>"
  [(set (match_operand:SWI1248_AVX512BW 0 "register_operand" "=k")
	(not:SWI1248_AVX512BW
	  (match_operand:SWI1248_AVX512BW 1 "register_operand" "k")))
   (unspec [(const_int 0)] UNSPEC_MASKOP)]
  "TARGET_AVX512F"
{
  if (get_attr_mode (insn) == MODE_HI)
    return "knotw\t{%1, %0|%0, %1}";
  else
    return "knot<mskmodesuffix>\t{%1, %0|%0, %1}";
}
  [(set_attr "type" "msklog")
   (set_attr "prefix" "vex")
   (set (attr "mode")
     (cond [(and (match_test "<MODE>mode == QImode")
		 (not (match_test "TARGET_AVX512DQ")))
	       (const_string "HI")
	   ]
	   (const_string "<MODE>")))])

(define_insn "kadd<mode>"
  [(set (match_operand:SWI1248_AVX512BWDQ2 0 "register_operand" "=k")
	(plus:SWI1248_AVX512BWDQ2
	  (match_operand:SWI1248_AVX512BWDQ2 1 "register_operand" "k")
	  (match_operand:SWI1248_AVX512BWDQ2 2 "register_operand" "k")))
   (unspec [(const_int 0)] UNSPEC_MASKOP)]
  "TARGET_AVX512F"
  "kadd<mskmodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "msklog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

;; Mask variant shift mnemonics
(define_code_attr mshift [(ashift "shiftl") (lshiftrt "shiftr")])

(define_insn "k<code><mode>"
  [(set (match_operand:SWI1248_AVX512BWDQ 0 "register_operand" "=k")
	(any_lshift:SWI1248_AVX512BWDQ
	  (match_operand:SWI1248_AVX512BWDQ 1 "register_operand" "k")
	  (match_operand:QI 2 "immediate_operand" "n")))
   (unspec [(const_int 0)] UNSPEC_MASKOP)]
  "TARGET_AVX512F"
  "k<mshift><mskmodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "msklog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

(define_insn "ktest<mode>"
  [(set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_operand:SWI1248_AVX512BWDQ2 0 "register_operand" "k")
	   (match_operand:SWI1248_AVX512BWDQ2 1 "register_operand" "k")]
	  UNSPEC_KTEST))]
  "TARGET_AVX512F"
  "ktest<mskmodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "mode" "<MODE>")
   (set_attr "type" "msklog")
   (set_attr "prefix" "vex")])

(define_insn "kortest<mode>"
  [(set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_operand:SWI1248_AVX512BWDQ 0 "register_operand" "k")
	   (match_operand:SWI1248_AVX512BWDQ 1 "register_operand" "k")]
	  UNSPEC_KORTEST))]
  "TARGET_AVX512F"
  "kortest<mskmodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "mode" "<MODE>")
   (set_attr "type" "msklog")
   (set_attr "prefix" "vex")])

(define_insn "kunpckhi"
  [(set (match_operand:HI 0 "register_operand" "=k")
	(ior:HI
	  (ashift:HI
	    (zero_extend:HI (match_operand:QI 1 "register_operand" "k"))
	    (const_int 8))
	  (zero_extend:HI (match_operand:QI 2 "register_operand" "k"))))]
  "TARGET_AVX512F"
  "kunpckbw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "mode" "HI")
   (set_attr "type" "msklog")
   (set_attr "prefix" "vex")])

(define_insn "kunpcksi"
  [(set (match_operand:SI 0 "register_operand" "=k")
	(ior:SI
	  (ashift:SI
	    (zero_extend:SI (match_operand:HI 1 "register_operand" "k"))
	    (const_int 16))
	  (zero_extend:SI (match_operand:HI 2 "register_operand" "k"))))]
  "TARGET_AVX512BW"
  "kunpckwd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "mode" "SI")])

(define_insn "kunpckdi"
  [(set (match_operand:DI 0 "register_operand" "=k")
	(ior:DI
	  (ashift:DI
	    (zero_extend:DI (match_operand:SI 1 "register_operand" "k"))
	    (const_int 32))
	  (zero_extend:DI (match_operand:SI 2 "register_operand" "k"))))]
  "TARGET_AVX512BW"
  "kunpckdq\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "mode" "DI")])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel floating point arithmetic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "<code><mode>2"
  [(set (match_operand:VF 0 "register_operand")
	(absneg:VF
	  (match_operand:VF 1 "register_operand")))]
  "TARGET_SSE"
  "ix86_expand_fp_absneg_operator (<CODE>, <MODE>mode, operands); DONE;")

(define_insn_and_split "*absneg<mode>2"
  [(set (match_operand:VF 0 "register_operand" "=x,x,v,v")
	(match_operator:VF 3 "absneg_operator"
	  [(match_operand:VF 1 "vector_operand" "0,  xBm,v, m")]))
   (use (match_operand:VF 2 "vector_operand"    "xBm,0,  vm,v"))]
  "TARGET_SSE"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  enum rtx_code absneg_op;
  rtx op1, op2;
  rtx t;

  if (TARGET_AVX)
    {
      if (MEM_P (operands[1]))
	op1 = operands[2], op2 = operands[1];
      else
	op1 = operands[1], op2 = operands[2];
    }
  else
    {
      op1 = operands[0];
      if (rtx_equal_p (operands[0], operands[1]))
	op2 = operands[2];
      else
	op2 = operands[1];
    }

  absneg_op = GET_CODE (operands[3]) == NEG ? XOR : AND;
  t = gen_rtx_fmt_ee (absneg_op, <MODE>mode, op1, op2);
  t = gen_rtx_SET (operands[0], t);
  emit_insn (t);
  DONE;
}
  [(set_attr "isa" "noavx,noavx,avx,avx")])

(define_expand "<plusminus_insn><mode>3<mask_name><round_name>"
  [(set (match_operand:VF 0 "register_operand")
	(plusminus:VF
	  (match_operand:VF 1 "<round_nimm_predicate>")
	  (match_operand:VF 2 "<round_nimm_predicate>")))]
  "TARGET_SSE && <mask_mode512bit_condition> && <round_mode512bit_condition>"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*<plusminus_insn><mode>3<mask_name><round_name>"
  [(set (match_operand:VF 0 "register_operand" "=x,v")
	(plusminus:VF
	  (match_operand:VF 1 "<round_nimm_predicate>" "<comm>0,v")
	  (match_operand:VF 2 "<round_nimm_predicate>" "xBm,<round_constraint>")))]
  "TARGET_SSE && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)
   && <mask_mode512bit_condition> && <round_mode512bit_condition>"
  "@
   <plusminus_mnemonic><ssemodesuffix>\t{%2, %0|%0, %2}
   v<plusminus_mnemonic><ssemodesuffix>\t{<round_mask_op3>%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2<round_mask_op3>}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseadd")
   (set_attr "prefix" "<mask_prefix3>")
   (set_attr "mode" "<MODE>")])

(define_insn "<sse>_vm<plusminus_insn><mode>3<mask_scalar_name><round_scalar_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=x,v")
	(vec_merge:VF_128
	  (plusminus:VF_128
	    (match_operand:VF_128 1 "register_operand" "0,v")
	    (match_operand:VF_128 2 "vector_operand" "xBm,<round_scalar_constraint>"))
	  (match_dup 1)
	  (const_int 1)))]
  "TARGET_SSE"
  "@
   <plusminus_mnemonic><ssescalarmodesuffix>\t{%2, %0|%0, %<iptr>2}
   v<plusminus_mnemonic><ssescalarmodesuffix>\t{<round_scalar_mask_op3>%2, %1, %0<mask_scalar_operand3>|%0<mask_scalar_operand3>, %1, %<iptr>2<round_scalar_mask_op3>}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseadd")
   (set_attr "prefix" "<round_scalar_prefix>")
   (set_attr "mode" "<ssescalarmode>")])

(define_expand "mul<mode>3<mask_name><round_name>"
  [(set (match_operand:VF 0 "register_operand")
	(mult:VF
	  (match_operand:VF 1 "<round_nimm_predicate>")
	  (match_operand:VF 2 "<round_nimm_predicate>")))]
  "TARGET_SSE && <mask_mode512bit_condition> && <round_mode512bit_condition>"
  "ix86_fixup_binary_operands_no_copy (MULT, <MODE>mode, operands);")

(define_insn "*mul<mode>3<mask_name><round_name>"
  [(set (match_operand:VF 0 "register_operand" "=x,v")
	(mult:VF
	  (match_operand:VF 1 "<round_nimm_predicate>" "%0,v")
	  (match_operand:VF 2 "<round_nimm_predicate>" "xBm,<round_constraint>")))]
  "TARGET_SSE
   && !(MEM_P (operands[1]) && MEM_P (operands[2]))
   && <mask_mode512bit_condition> && <round_mode512bit_condition>"
  "@
   mul<ssemodesuffix>\t{%2, %0|%0, %2}
   vmul<ssemodesuffix>\t{<round_mask_op3>%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2<round_mask_op3>}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "ssemul")
   (set_attr "prefix" "<mask_prefix3>")
   (set_attr "btver2_decode" "direct,double")
   (set_attr "mode" "<MODE>")])

(define_insn "<sse>_vm<multdiv_mnemonic><mode>3<mask_scalar_name><round_scalar_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=x,v")
	(vec_merge:VF_128
	  (multdiv:VF_128
	    (match_operand:VF_128 1 "register_operand" "0,v")
	    (match_operand:VF_128 2 "vector_operand" "xBm,<round_scalar_constraint>"))
	  (match_dup 1)
	  (const_int 1)))]
  "TARGET_SSE"
  "@
   <multdiv_mnemonic><ssescalarmodesuffix>\t{%2, %0|%0, %<iptr>2}
   v<multdiv_mnemonic><ssescalarmodesuffix>\t{<round_scalar_mask_op3>%2, %1, %0<mask_scalar_operand3>|%0<mask_scalar_operand3>, %1, %<iptr>2<round_scalar_mask_op3>}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sse<multdiv_mnemonic>")
   (set_attr "prefix" "<round_scalar_prefix>")
   (set_attr "btver2_decode" "direct,double")
   (set_attr "mode" "<ssescalarmode>")])

(define_expand "div<mode>3"
  [(set (match_operand:VF2 0 "register_operand")
	(div:VF2 (match_operand:VF2 1 "register_operand")
		 (match_operand:VF2 2 "vector_operand")))]
  "TARGET_SSE2"
  "ix86_fixup_binary_operands_no_copy (DIV, <MODE>mode, operands);")

(define_expand "div<mode>3"
  [(set (match_operand:VF1 0 "register_operand")
	(div:VF1 (match_operand:VF1 1 "register_operand")
		 (match_operand:VF1 2 "vector_operand")))]
  "TARGET_SSE"
{
  ix86_fixup_binary_operands_no_copy (DIV, <MODE>mode, operands);

  if (TARGET_SSE_MATH
      && TARGET_RECIP_VEC_DIV
      && !optimize_insn_for_size_p ()
      && flag_finite_math_only && !flag_trapping_math
      && flag_unsafe_math_optimizations)
    {
      ix86_emit_swdivsf (operands[0], operands[1], operands[2], <MODE>mode);
      DONE;
    }
})

(define_insn "<sse>_div<mode>3<mask_name><round_name>"
  [(set (match_operand:VF 0 "register_operand" "=x,v")
	(div:VF
	  (match_operand:VF 1 "register_operand" "0,v")
	  (match_operand:VF 2 "<round_nimm_predicate>" "xBm,<round_constraint>")))]
  "TARGET_SSE && <mask_mode512bit_condition> && <round_mode512bit_condition>"
  "@
   div<ssemodesuffix>\t{%2, %0|%0, %2}
   vdiv<ssemodesuffix>\t{<round_mask_op3>%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2<round_mask_op3>}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "ssediv")
   (set_attr "prefix" "<mask_prefix3>")
   (set_attr "mode" "<MODE>")])

(define_insn "<sse>_rcp<mode>2"
  [(set (match_operand:VF1_128_256 0 "register_operand" "=x")
	(unspec:VF1_128_256
	  [(match_operand:VF1_128_256 1 "vector_operand" "xBm")] UNSPEC_RCP))]
  "TARGET_SSE"
  "%vrcpps\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "atom_sse_attr" "rcp")
   (set_attr "btver2_sse_attr" "rcp")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

(define_insn "sse_vmrcpv4sf2"
  [(set (match_operand:V4SF 0 "register_operand" "=x,x")
	(vec_merge:V4SF
	  (unspec:V4SF [(match_operand:V4SF 1 "nonimmediate_operand" "xm,xm")]
		       UNSPEC_RCP)
	  (match_operand:V4SF 2 "register_operand" "0,x")
	  (const_int 1)))]
  "TARGET_SSE"
  "@
   rcpss\t{%1, %0|%0, %k1}
   vrcpss\t{%1, %2, %0|%0, %2, %k1}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sse")
   (set_attr "atom_sse_attr" "rcp")
   (set_attr "btver2_sse_attr" "rcp")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "SF")])

(define_insn "<mask_codefor>rcp14<mode><mask_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
	(unspec:VF_AVX512VL
	  [(match_operand:VF_AVX512VL 1 "nonimmediate_operand" "vm")]
	  UNSPEC_RCP14))]
  "TARGET_AVX512F"
  "vrcp14<ssemodesuffix>\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "srcp14<mode>"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
	  (unspec:VF_128
	    [(match_operand:VF_128 1 "nonimmediate_operand" "vm")]
	    UNSPEC_RCP14)
	  (match_operand:VF_128 2 "register_operand" "v")
	  (const_int 1)))]
  "TARGET_AVX512F"
  "vrcp14<ssescalarmodesuffix>\t{%1, %2, %0|%0, %2, %<iptr>1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "srcp14<mode>_mask"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
	  (vec_merge:VF_128
	    (unspec:VF_128
	      [(match_operand:VF_128 1 "nonimmediate_operand" "vm")]
	    UNSPEC_RCP14)
	      (match_operand:VF_128 3 "nonimm_or_0_operand" "0C")
	    (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk"))
	  (match_operand:VF_128 2 "register_operand" "v")
	  (const_int 1)))]
  "TARGET_AVX512F"
  "vrcp14<ssescalarmodesuffix>\t{%1, %2, %0%{%4%}%N3|%0%{%4%}%N3, %2, %<iptr>1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_expand "sqrt<mode>2"
  [(set (match_operand:VF2 0 "register_operand")
	(sqrt:VF2 (match_operand:VF2 1 "vector_operand")))]
  "TARGET_SSE2")

(define_expand "sqrt<mode>2"
  [(set (match_operand:VF1 0 "register_operand")
	(sqrt:VF1 (match_operand:VF1 1 "vector_operand")))]
  "TARGET_SSE"
{
  if (TARGET_SSE_MATH
      && TARGET_RECIP_VEC_SQRT
      && !optimize_insn_for_size_p ()
      && flag_finite_math_only && !flag_trapping_math
      && flag_unsafe_math_optimizations)
    {
      ix86_emit_swsqrtsf (operands[0], operands[1], <MODE>mode, false);
      DONE;
    }
})

(define_insn "<sse>_sqrt<mode>2<mask_name><round_name>"
  [(set (match_operand:VF 0 "register_operand" "=x,v")
	(sqrt:VF (match_operand:VF 1 "<round_nimm_predicate>" "xBm,<round_constraint>")))]
  "TARGET_SSE && <mask_mode512bit_condition> && <round_mode512bit_condition>"
  "@
   sqrt<ssemodesuffix>\t{%1, %0|%0, %1}
   vsqrt<ssemodesuffix>\t{<round_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_mask_op2>}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sse")
   (set_attr "atom_sse_attr" "sqrt")
   (set_attr "btver2_sse_attr" "sqrt")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

(define_insn "<sse>_vmsqrt<mode>2<mask_scalar_name><round_scalar_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=x,v")
	(vec_merge:VF_128
	  (sqrt:VF_128
	    (match_operand:VF_128 1 "vector_operand" "xBm,<round_scalar_constraint>"))
	  (match_operand:VF_128 2 "register_operand" "0,v")
	  (const_int 1)))]
  "TARGET_SSE"
  "@
   sqrt<ssescalarmodesuffix>\t{%1, %0|%0, %<iptr>1}
   vsqrt<ssescalarmodesuffix>\t{<round_scalar_mask_op3>%1, %2, %0<mask_scalar_operand3>|%0<mask_scalar_operand3>, %2, %<iptr>1<round_scalar_mask_op3>}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sse")
   (set_attr "atom_sse_attr" "sqrt")
   (set_attr "prefix" "<round_scalar_prefix>")
   (set_attr "btver2_sse_attr" "sqrt")
   (set_attr "mode" "<ssescalarmode>")])

(define_expand "rsqrt<mode>2"
  [(set (match_operand:VF1_128_256 0 "register_operand")
	(unspec:VF1_128_256
	  [(match_operand:VF1_128_256 1 "vector_operand")] UNSPEC_RSQRT))]
  "TARGET_SSE_MATH"
{
  ix86_emit_swsqrtsf (operands[0], operands[1], <MODE>mode, true);
  DONE;
})

(define_expand "rsqrtv16sf2"
  [(set (match_operand:V16SF 0 "register_operand")
	(unspec:V16SF
	  [(match_operand:V16SF 1 "vector_operand")]
	  UNSPEC_RSQRT28))]
  "TARGET_SSE_MATH && TARGET_AVX512ER"
{
  ix86_emit_swsqrtsf (operands[0], operands[1], V16SFmode, true);
  DONE;
})

(define_insn "<sse>_rsqrt<mode>2"
  [(set (match_operand:VF1_128_256 0 "register_operand" "=x")
	(unspec:VF1_128_256
	  [(match_operand:VF1_128_256 1 "vector_operand" "xBm")] UNSPEC_RSQRT))]
  "TARGET_SSE"
  "%vrsqrtps\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

(define_insn "<mask_codefor>rsqrt14<mode><mask_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
	(unspec:VF_AVX512VL
	  [(match_operand:VF_AVX512VL 1 "nonimmediate_operand" "vm")]
	  UNSPEC_RSQRT14))]
  "TARGET_AVX512F"
  "vrsqrt14<ssemodesuffix>\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "rsqrt14<mode>"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
	  (unspec:VF_128
	    [(match_operand:VF_128 1 "nonimmediate_operand" "vm")]
	    UNSPEC_RSQRT14)
	  (match_operand:VF_128 2 "register_operand" "v")
	  (const_int 1)))]
  "TARGET_AVX512F"
  "vrsqrt14<ssescalarmodesuffix>\t{%1, %2, %0|%0, %2, %<iptr>1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "rsqrt14_<mode>_mask"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
	  (vec_merge:VF_128
	    (unspec:VF_128
	      [(match_operand:VF_128 1 "nonimmediate_operand" "vm")]
	      UNSPEC_RSQRT14)
	      (match_operand:VF_128 3 "nonimm_or_0_operand" "0C")
	      (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk"))
	  (match_operand:VF_128 2 "register_operand" "v")
	  (const_int 1)))]
  "TARGET_AVX512F"
  "vrsqrt14<ssescalarmodesuffix>\t{%1, %2, %0%{%4%}%N3|%0%{%4%}%N3, %2, %<iptr>1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "sse_vmrsqrtv4sf2"
  [(set (match_operand:V4SF 0 "register_operand" "=x,x")
	(vec_merge:V4SF
	  (unspec:V4SF [(match_operand:V4SF 1 "nonimmediate_operand" "xm,xm")]
		       UNSPEC_RSQRT)
	  (match_operand:V4SF 2 "register_operand" "0,x")
	  (const_int 1)))]
  "TARGET_SSE"
  "@
   rsqrtss\t{%1, %0|%0, %k1}
   vrsqrtss\t{%1, %2, %0|%0, %2, %k1}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sse")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "SF")])

(define_expand "<code><mode>3<mask_name><round_saeonly_name>"
  [(set (match_operand:VF 0 "register_operand")
	(smaxmin:VF
	  (match_operand:VF 1 "<round_saeonly_nimm_predicate>")
	  (match_operand:VF 2 "<round_saeonly_nimm_predicate>")))]
  "TARGET_SSE && <mask_mode512bit_condition> && <round_saeonly_mode512bit_condition>"
{
  if (!flag_finite_math_only || flag_signed_zeros)
    {
      operands[1] = force_reg (<MODE>mode, operands[1]);
      emit_insn (gen_ieee_<maxmin_float><mode>3<mask_name><round_saeonly_name>
		 (operands[0], operands[1], operands[2]
		  <mask_operand_arg34>
		  <round_saeonly_mask_arg3>));
      DONE;
    }
  else
    ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);
})

;; These versions of the min/max patterns are intentionally ignorant of
;; their behavior wrt -0.0 and NaN (via the commutative operand mark).
;; Since both the tree-level MAX_EXPR and the rtl-level SMAX operator
;; are undefined in this condition, we're certain this is correct.

(define_insn "*<code><mode>3<mask_name><round_saeonly_name>"
  [(set (match_operand:VF 0 "register_operand" "=x,v")
	(smaxmin:VF
	  (match_operand:VF 1 "<round_saeonly_nimm_predicate>" "%0,v")
	  (match_operand:VF 2 "<round_saeonly_nimm_predicate>" "xBm,<round_saeonly_constraint>")))]
  "TARGET_SSE
   && !(MEM_P (operands[1]) && MEM_P (operands[2]))
   && <mask_mode512bit_condition> && <round_saeonly_mode512bit_condition>"
  "@
   <maxmin_float><ssemodesuffix>\t{%2, %0|%0, %2}
   v<maxmin_float><ssemodesuffix>\t{<round_saeonly_mask_op3>%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2<round_saeonly_mask_op3>}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseadd")
   (set_attr "btver2_sse_attr" "maxmin")
   (set_attr "prefix" "<mask_prefix3>")
   (set_attr "mode" "<MODE>")])

;; These versions of the min/max patterns implement exactly the operations
;;   min = (op1 < op2 ? op1 : op2)
;;   max = (!(op1 < op2) ? op1 : op2)
;; Their operands are not commutative, and thus they may be used in the
;; presence of -0.0 and NaN.

(define_insn "ieee_<ieee_maxmin><mode>3<mask_name><round_saeonly_name>"
  [(set (match_operand:VF 0 "register_operand" "=x,v")
	(unspec:VF
	  [(match_operand:VF 1 "register_operand" "0,v")
	   (match_operand:VF 2 "<round_saeonly_nimm_predicate>" "xBm,<round_saeonly_constraint>")]
	  IEEE_MAXMIN))]
  "TARGET_SSE
   && <mask_mode512bit_condition> && <round_saeonly_mode512bit_condition>"
  "@
   <ieee_maxmin><ssemodesuffix>\t{%2, %0|%0, %2}
   v<ieee_maxmin><ssemodesuffix>\t{<round_saeonly_mask_op3>%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2<round_saeonly_mask_op3>}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseadd")
   (set_attr "btver2_sse_attr" "maxmin")
   (set_attr "prefix" "<mask_prefix3>")
   (set_attr "mode" "<MODE>")])

(define_insn "<sse>_vm<code><mode>3<mask_scalar_name><round_saeonly_scalar_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=x,v")
	(vec_merge:VF_128
	  (smaxmin:VF_128
	    (match_operand:VF_128 1 "register_operand" "0,v")
	    (match_operand:VF_128 2 "vector_operand" "xBm,<round_saeonly_scalar_constraint>"))
	 (match_dup 1)
	 (const_int 1)))]
  "TARGET_SSE"
  "@
   <maxmin_float><ssescalarmodesuffix>\t{%2, %0|%0, %<iptr>2}
   v<maxmin_float><ssescalarmodesuffix>\t{<round_saeonly_scalar_mask_op3>%2, %1, %0<mask_scalar_operand3>|%0<mask_scalar_operand3>, %1, %<iptr>2<round_saeonly_scalar_mask_op3>}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sse")
   (set_attr "btver2_sse_attr" "maxmin")
   (set_attr "prefix" "<round_saeonly_scalar_prefix>")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "avx_addsubv4df3"
  [(set (match_operand:V4DF 0 "register_operand" "=x")
	(vec_merge:V4DF
	  (minus:V4DF
	    (match_operand:V4DF 1 "register_operand" "x")
	    (match_operand:V4DF 2 "nonimmediate_operand" "xm"))
	  (plus:V4DF (match_dup 1) (match_dup 2))
	  (const_int 5)))]
  "TARGET_AVX"
  "vaddsubpd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_insn "sse3_addsubv2df3"
  [(set (match_operand:V2DF 0 "register_operand" "=x,x")
	(vec_merge:V2DF
	  (minus:V2DF
	    (match_operand:V2DF 1 "register_operand" "0,x")
	    (match_operand:V2DF 2 "vector_operand" "xBm,xm"))
	  (plus:V2DF (match_dup 1) (match_dup 2))
	  (const_int 1)))]
  "TARGET_SSE3"
  "@
   addsubpd\t{%2, %0|%0, %2}
   vaddsubpd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "V2DF")])

(define_insn "avx_addsubv8sf3"
  [(set (match_operand:V8SF 0 "register_operand" "=x")
	(vec_merge:V8SF
	  (minus:V8SF
	    (match_operand:V8SF 1 "register_operand" "x")
	    (match_operand:V8SF 2 "nonimmediate_operand" "xm"))
	  (plus:V8SF (match_dup 1) (match_dup 2))
	  (const_int 85)))]
  "TARGET_AVX"
  "vaddsubps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "sse3_addsubv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=x,x")
	(vec_merge:V4SF
	  (minus:V4SF
	    (match_operand:V4SF 1 "register_operand" "0,x")
	    (match_operand:V4SF 2 "vector_operand" "xBm,xm"))
	  (plus:V4SF (match_dup 1) (match_dup 2))
	  (const_int 5)))]
  "TARGET_SSE3"
  "@
   addsubps\t{%2, %0|%0, %2}
   vaddsubps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseadd")
   (set_attr "prefix" "orig,vex")
   (set_attr "prefix_rep" "1,*")
   (set_attr "mode" "V4SF")])

(define_split
  [(set (match_operand:VF_128_256 0 "register_operand")
	(match_operator:VF_128_256 6 "addsub_vm_operator"
	  [(minus:VF_128_256
	     (match_operand:VF_128_256 1 "register_operand")
	     (match_operand:VF_128_256 2 "vector_operand"))
	   (plus:VF_128_256
	     (match_operand:VF_128_256 3 "vector_operand")
	     (match_operand:VF_128_256 4 "vector_operand"))
	   (match_operand 5 "const_int_operand")]))]
  "TARGET_SSE3
   && can_create_pseudo_p ()
   && ((rtx_equal_p (operands[1], operands[3])
	&& rtx_equal_p (operands[2], operands[4]))
       || (rtx_equal_p (operands[1], operands[4])
	   && rtx_equal_p (operands[2], operands[3])))"
  [(set (match_dup 0)
	(vec_merge:VF_128_256
	  (minus:VF_128_256 (match_dup 1) (match_dup 2))
	  (plus:VF_128_256 (match_dup 1) (match_dup 2))
	  (match_dup 5)))])

(define_split
  [(set (match_operand:VF_128_256 0 "register_operand")
	(match_operator:VF_128_256 6 "addsub_vm_operator"
	  [(plus:VF_128_256
	     (match_operand:VF_128_256 1 "vector_operand")
	     (match_operand:VF_128_256 2 "vector_operand"))
	   (minus:VF_128_256
	     (match_operand:VF_128_256 3 "register_operand")
	     (match_operand:VF_128_256 4 "vector_operand"))
	   (match_operand 5 "const_int_operand")]))]
  "TARGET_SSE3
   && can_create_pseudo_p ()
   && ((rtx_equal_p (operands[1], operands[3])
	&& rtx_equal_p (operands[2], operands[4]))
       || (rtx_equal_p (operands[1], operands[4])
	   && rtx_equal_p (operands[2], operands[3])))"
  [(set (match_dup 0)
	(vec_merge:VF_128_256
	  (minus:VF_128_256 (match_dup 3) (match_dup 4))
	  (plus:VF_128_256 (match_dup 3) (match_dup 4))
	  (match_dup 5)))]
{
  /* Negate mask bits to compensate for swapped PLUS and MINUS RTXes.  */
  operands[5]
    = GEN_INT (~INTVAL (operands[5])
	       & ((HOST_WIDE_INT_1U << GET_MODE_NUNITS (<MODE>mode)) - 1));
})

(define_split
  [(set (match_operand:VF_128_256 0 "register_operand")
	(match_operator:VF_128_256 7 "addsub_vs_operator"
	  [(vec_concat:<ssedoublemode>
	     (minus:VF_128_256
	       (match_operand:VF_128_256 1 "register_operand")
	       (match_operand:VF_128_256 2 "vector_operand"))
	     (plus:VF_128_256
	       (match_operand:VF_128_256 3 "vector_operand")
	       (match_operand:VF_128_256 4 "vector_operand")))
	   (match_parallel 5 "addsub_vs_parallel"
	     [(match_operand 6 "const_int_operand")])]))]
  "TARGET_SSE3
   && can_create_pseudo_p ()
   && ((rtx_equal_p (operands[1], operands[3])
	&& rtx_equal_p (operands[2], operands[4]))
       || (rtx_equal_p (operands[1], operands[4])
	   && rtx_equal_p (operands[2], operands[3])))"
  [(set (match_dup 0)
	(vec_merge:VF_128_256
	  (minus:VF_128_256 (match_dup 1) (match_dup 2))
	  (plus:VF_128_256 (match_dup 1) (match_dup 2))
	  (match_dup 5)))]
{
  int i, nelt = XVECLEN (operands[5], 0);
  HOST_WIDE_INT ival = 0;

  for (i = 0; i < nelt; i++)
    if (INTVAL (XVECEXP (operands[5], 0, i)) < GET_MODE_NUNITS (<MODE>mode))
      ival |= HOST_WIDE_INT_1 << i;

  operands[5] = GEN_INT (ival);
})

(define_split
  [(set (match_operand:VF_128_256 0 "register_operand")
	(match_operator:VF_128_256 7 "addsub_vs_operator"
	  [(vec_concat:<ssedoublemode>
	     (plus:VF_128_256
	       (match_operand:VF_128_256 1 "vector_operand")
	       (match_operand:VF_128_256 2 "vector_operand"))
	     (minus:VF_128_256
	       (match_operand:VF_128_256 3 "register_operand")
	       (match_operand:VF_128_256 4 "vector_operand")))
	   (match_parallel 5 "addsub_vs_parallel"
	     [(match_operand 6 "const_int_operand")])]))]
  "TARGET_SSE3
   && can_create_pseudo_p ()
   && ((rtx_equal_p (operands[1], operands[3])
	&& rtx_equal_p (operands[2], operands[4]))
       || (rtx_equal_p (operands[1], operands[4])
	   && rtx_equal_p (operands[2], operands[3])))"
  [(set (match_dup 0)
	(vec_merge:VF_128_256
	  (minus:VF_128_256 (match_dup 3) (match_dup 4))
	  (plus:VF_128_256 (match_dup 3) (match_dup 4))
	  (match_dup 5)))]
{
  int i, nelt = XVECLEN (operands[5], 0);
  HOST_WIDE_INT ival = 0;

  for (i = 0; i < nelt; i++)
    if (INTVAL (XVECEXP (operands[5], 0, i)) >= GET_MODE_NUNITS (<MODE>mode))
      ival |= HOST_WIDE_INT_1 << i;

  operands[5] = GEN_INT (ival);
})

(define_insn "avx_h<plusminus_insn>v4df3"
  [(set (match_operand:V4DF 0 "register_operand" "=x")
	(vec_concat:V4DF
	  (vec_concat:V2DF
	    (plusminus:DF
	      (vec_select:DF
		(match_operand:V4DF 1 "register_operand" "x")
		(parallel [(const_int 0)]))
	      (vec_select:DF (match_dup 1) (parallel [(const_int 1)])))
	    (plusminus:DF
	      (vec_select:DF
		(match_operand:V4DF 2 "nonimmediate_operand" "xm")
		(parallel [(const_int 0)]))
	      (vec_select:DF (match_dup 2) (parallel [(const_int 1)]))))
	  (vec_concat:V2DF
	    (plusminus:DF
	      (vec_select:DF (match_dup 1) (parallel [(const_int 2)]))
	      (vec_select:DF (match_dup 1) (parallel [(const_int 3)])))
	    (plusminus:DF
	      (vec_select:DF (match_dup 2) (parallel [(const_int 2)]))
	      (vec_select:DF (match_dup 2) (parallel [(const_int 3)]))))))]
  "TARGET_AVX"
  "vh<plusminus_mnemonic>pd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_expand "sse3_haddv2df3"
  [(set (match_operand:V2DF 0 "register_operand")
	(vec_concat:V2DF
	  (plus:DF
	    (vec_select:DF
	      (match_operand:V2DF 1 "register_operand")
	      (parallel [(const_int 0)]))
	    (vec_select:DF (match_dup 1) (parallel [(const_int 1)])))
	  (plus:DF
	    (vec_select:DF
	      (match_operand:V2DF 2 "vector_operand")
	      (parallel [(const_int 0)]))
	    (vec_select:DF (match_dup 2) (parallel [(const_int 1)])))))]
  "TARGET_SSE3")

(define_insn "*sse3_haddv2df3"
  [(set (match_operand:V2DF 0 "register_operand" "=x,x")
	(vec_concat:V2DF
	  (plus:DF
	    (vec_select:DF
	      (match_operand:V2DF 1 "register_operand" "0,x")
	      (parallel [(match_operand:SI 3 "const_0_to_1_operand")]))
	    (vec_select:DF
	      (match_dup 1)
	      (parallel [(match_operand:SI 4 "const_0_to_1_operand")])))
	  (plus:DF
	    (vec_select:DF
	      (match_operand:V2DF 2 "vector_operand" "xBm,xm")
	      (parallel [(match_operand:SI 5 "const_0_to_1_operand")]))
	    (vec_select:DF
	      (match_dup 2)
	      (parallel [(match_operand:SI 6 "const_0_to_1_operand")])))))]
  "TARGET_SSE3
   && INTVAL (operands[3]) != INTVAL (operands[4])
   && INTVAL (operands[5]) != INTVAL (operands[6])"
  "@
   haddpd\t{%2, %0|%0, %2}
   vhaddpd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseadd")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "V2DF")])

(define_insn "sse3_hsubv2df3"
  [(set (match_operand:V2DF 0 "register_operand" "=x,x")
	(vec_concat:V2DF
	  (minus:DF
	    (vec_select:DF
	      (match_operand:V2DF 1 "register_operand" "0,x")
	      (parallel [(const_int 0)]))
	    (vec_select:DF (match_dup 1) (parallel [(const_int 1)])))
	  (minus:DF
	    (vec_select:DF
	      (match_operand:V2DF 2 "vector_operand" "xBm,xm")
	      (parallel [(const_int 0)]))
	    (vec_select:DF (match_dup 2) (parallel [(const_int 1)])))))]
  "TARGET_SSE3"
  "@
   hsubpd\t{%2, %0|%0, %2}
   vhsubpd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseadd")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "V2DF")])

(define_insn "*sse3_haddv2df3_low"
  [(set (match_operand:DF 0 "register_operand" "=x,x")
	(plus:DF
	  (vec_select:DF
	    (match_operand:V2DF 1 "register_operand" "0,x")
	    (parallel [(match_operand:SI 2 "const_0_to_1_operand")]))
	  (vec_select:DF
	    (match_dup 1)
	    (parallel [(match_operand:SI 3 "const_0_to_1_operand")]))))]
  "TARGET_SSE3
   && INTVAL (operands[2]) != INTVAL (operands[3])"
  "@
   haddpd\t{%0, %0|%0, %0}
   vhaddpd\t{%1, %1, %0|%0, %1, %1}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseadd1")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "V2DF")])

(define_insn "*sse3_hsubv2df3_low"
  [(set (match_operand:DF 0 "register_operand" "=x,x")
	(minus:DF
	  (vec_select:DF
	    (match_operand:V2DF 1 "register_operand" "0,x")
	    (parallel [(const_int 0)]))
	  (vec_select:DF
	    (match_dup 1)
	    (parallel [(const_int 1)]))))]
  "TARGET_SSE3"
  "@
   hsubpd\t{%0, %0|%0, %0}
   vhsubpd\t{%1, %1, %0|%0, %1, %1}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseadd1")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "V2DF")])

(define_insn "avx_h<plusminus_insn>v8sf3"
  [(set (match_operand:V8SF 0 "register_operand" "=x")
	(vec_concat:V8SF
	  (vec_concat:V4SF
	    (vec_concat:V2SF
	      (plusminus:SF
		(vec_select:SF
		  (match_operand:V8SF 1 "register_operand" "x")
		  (parallel [(const_int 0)]))
		(vec_select:SF (match_dup 1) (parallel [(const_int 1)])))
	      (plusminus:SF
		(vec_select:SF (match_dup 1) (parallel [(const_int 2)]))
		(vec_select:SF (match_dup 1) (parallel [(const_int 3)]))))
	    (vec_concat:V2SF
	      (plusminus:SF
		(vec_select:SF
		  (match_operand:V8SF 2 "nonimmediate_operand" "xm")
		  (parallel [(const_int 0)]))
		(vec_select:SF (match_dup 2) (parallel [(const_int 1)])))
	      (plusminus:SF
		(vec_select:SF (match_dup 2) (parallel [(const_int 2)]))
		(vec_select:SF (match_dup 2) (parallel [(const_int 3)])))))
	  (vec_concat:V4SF
	    (vec_concat:V2SF
	      (plusminus:SF
		(vec_select:SF (match_dup 1) (parallel [(const_int 4)]))
		(vec_select:SF (match_dup 1) (parallel [(const_int 5)])))
	      (plusminus:SF
		(vec_select:SF (match_dup 1) (parallel [(const_int 6)]))
		(vec_select:SF (match_dup 1) (parallel [(const_int 7)]))))
	    (vec_concat:V2SF
	      (plusminus:SF
		(vec_select:SF (match_dup 2) (parallel [(const_int 4)]))
		(vec_select:SF (match_dup 2) (parallel [(const_int 5)])))
	      (plusminus:SF
		(vec_select:SF (match_dup 2) (parallel [(const_int 6)]))
		(vec_select:SF (match_dup 2) (parallel [(const_int 7)])))))))]
  "TARGET_AVX"
  "vh<plusminus_mnemonic>ps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "sse3_h<plusminus_insn>v4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=x,x")
	(vec_concat:V4SF
	  (vec_concat:V2SF
	    (plusminus:SF
	      (vec_select:SF
		(match_operand:V4SF 1 "register_operand" "0,x")
		(parallel [(const_int 0)]))
	      (vec_select:SF (match_dup 1) (parallel [(const_int 1)])))
	    (plusminus:SF
	      (vec_select:SF (match_dup 1) (parallel [(const_int 2)]))
	      (vec_select:SF (match_dup 1) (parallel [(const_int 3)]))))
	  (vec_concat:V2SF
	    (plusminus:SF
	      (vec_select:SF
		(match_operand:V4SF 2 "vector_operand" "xBm,xm")
		(parallel [(const_int 0)]))
	      (vec_select:SF (match_dup 2) (parallel [(const_int 1)])))
	    (plusminus:SF
	      (vec_select:SF (match_dup 2) (parallel [(const_int 2)]))
	      (vec_select:SF (match_dup 2) (parallel [(const_int 3)]))))))]
  "TARGET_SSE3"
  "@
   h<plusminus_mnemonic>ps\t{%2, %0|%0, %2}
   vh<plusminus_mnemonic>ps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix" "orig,vex")
   (set_attr "prefix_rep" "1,*")
   (set_attr "mode" "V4SF")])

(define_expand "reduc_plus_scal_v8df"
  [(match_operand:DF 0 "register_operand")
   (match_operand:V8DF 1 "register_operand")]
  "TARGET_AVX512F"
{
  rtx tmp = gen_reg_rtx (V8DFmode);
  ix86_expand_reduc (gen_addv8df3, tmp, operands[1]);
  emit_insn (gen_vec_extractv8dfdf (operands[0], tmp, const0_rtx));
  DONE;
})

(define_expand "reduc_plus_scal_v4df"
  [(match_operand:DF 0 "register_operand")
   (match_operand:V4DF 1 "register_operand")]
  "TARGET_AVX"
{
  rtx tmp = gen_reg_rtx (V2DFmode);
  emit_insn (gen_vec_extract_hi_v4df (tmp, operands[1]));
  rtx tmp2 = gen_reg_rtx (V2DFmode);
  emit_insn (gen_addv2df3 (tmp2, tmp, gen_lowpart (V2DFmode, operands[1])));
  rtx tmp3 = gen_reg_rtx (V2DFmode);
  emit_insn (gen_vec_interleave_highv2df (tmp3, tmp2, tmp2));
  emit_insn (gen_adddf3 (operands[0],
			 gen_lowpart (DFmode, tmp2),
			 gen_lowpart (DFmode, tmp3)));
  DONE;
})

(define_expand "reduc_plus_scal_v2df"
  [(match_operand:DF 0 "register_operand")
   (match_operand:V2DF 1 "register_operand")]
  "TARGET_SSE2"
{
  rtx tmp = gen_reg_rtx (V2DFmode);
  emit_insn (gen_vec_interleave_highv2df (tmp, operands[1], operands[1]));
  emit_insn (gen_adddf3 (operands[0],
			 gen_lowpart (DFmode, tmp),
			 gen_lowpart (DFmode, operands[1])));
  DONE;
})

(define_expand "reduc_plus_scal_v16sf"
  [(match_operand:SF 0 "register_operand")
   (match_operand:V16SF 1 "register_operand")]
  "TARGET_AVX512F"
{
  rtx tmp = gen_reg_rtx (V16SFmode);
  ix86_expand_reduc (gen_addv16sf3, tmp, operands[1]);
  emit_insn (gen_vec_extractv16sfsf (operands[0], tmp, const0_rtx));
  DONE;
})

(define_expand "reduc_plus_scal_v8sf"
  [(match_operand:SF 0 "register_operand")
   (match_operand:V8SF 1 "register_operand")]
  "TARGET_AVX"
{
  rtx tmp = gen_reg_rtx (V8SFmode);
  rtx tmp2 = gen_reg_rtx (V8SFmode);
  rtx vec_res = gen_reg_rtx (V8SFmode);
  emit_insn (gen_avx_haddv8sf3 (tmp, operands[1], operands[1]));
  emit_insn (gen_avx_haddv8sf3 (tmp2, tmp, tmp));
  emit_insn (gen_avx_vperm2f128v8sf3 (tmp, tmp2, tmp2, GEN_INT (1)));
  emit_insn (gen_addv8sf3 (vec_res, tmp, tmp2));
  emit_insn (gen_vec_extractv8sfsf (operands[0], vec_res, const0_rtx));
  DONE;
})

(define_expand "reduc_plus_scal_v4sf"
  [(match_operand:SF 0 "register_operand")
   (match_operand:V4SF 1 "register_operand")]
  "TARGET_SSE"
{
  rtx vec_res = gen_reg_rtx (V4SFmode);
  if (TARGET_SSE3)
    {
      rtx tmp = gen_reg_rtx (V4SFmode);
      emit_insn (gen_sse3_haddv4sf3 (tmp, operands[1], operands[1]));
      emit_insn (gen_sse3_haddv4sf3 (vec_res, tmp, tmp));
    }
  else
    ix86_expand_reduc (gen_addv4sf3, vec_res, operands[1]);
  emit_insn (gen_vec_extractv4sfsf (operands[0], vec_res, const0_rtx));
  DONE;
})

;; Modes handled by reduc_sm{in,ax}* patterns.
(define_mode_iterator REDUC_SMINMAX_MODE
  [(V32QI "TARGET_AVX2") (V16HI "TARGET_AVX2")
   (V8SI "TARGET_AVX2") (V4DI "TARGET_AVX2")
   (V8SF "TARGET_AVX") (V4DF "TARGET_AVX")
   (V4SF "TARGET_SSE") (V64QI "TARGET_AVX512BW")
   (V32HI "TARGET_AVX512BW") (V16SI "TARGET_AVX512F")
   (V8DI "TARGET_AVX512F") (V16SF "TARGET_AVX512F")
   (V8DF "TARGET_AVX512F")])

(define_expand "reduc_<code>_scal_<mode>"
  [(smaxmin:REDUC_SMINMAX_MODE
     (match_operand:<ssescalarmode> 0 "register_operand")
     (match_operand:REDUC_SMINMAX_MODE 1 "register_operand"))]
  ""
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  ix86_expand_reduc (gen_<code><mode>3, tmp, operands[1]);
  emit_insn (gen_vec_extract<mode><ssescalarmodelower> (operands[0], tmp,
							const0_rtx));
  DONE;
})

(define_expand "reduc_<code>_scal_<mode>"
  [(umaxmin:VI_AVX512BW
     (match_operand:<ssescalarmode> 0 "register_operand")
     (match_operand:VI_AVX512BW 1 "register_operand"))]
  "TARGET_AVX512F"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  ix86_expand_reduc (gen_<code><mode>3, tmp, operands[1]);
  emit_insn (gen_vec_extract<mode><ssescalarmodelower> (operands[0], tmp,
  							const0_rtx));
  DONE;
})

(define_expand "reduc_<code>_scal_<mode>"
  [(umaxmin:VI_256
     (match_operand:<ssescalarmode> 0 "register_operand")
     (match_operand:VI_256 1 "register_operand"))]
  "TARGET_AVX2"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  ix86_expand_reduc (gen_<code><mode>3, tmp, operands[1]);
  emit_insn (gen_vec_extract<mode><ssescalarmodelower> (operands[0], tmp,
							const0_rtx));
  DONE;
})

(define_expand "reduc_umin_scal_v8hi"
  [(umin:V8HI
     (match_operand:HI 0 "register_operand")
     (match_operand:V8HI 1 "register_operand"))]
  "TARGET_SSE4_1"
{
  rtx tmp = gen_reg_rtx (V8HImode);
  ix86_expand_reduc (gen_uminv8hi3, tmp, operands[1]);
  emit_insn (gen_vec_extractv8hihi (operands[0], tmp, const0_rtx));
  DONE;
})

(define_insn "<mask_codefor>reducep<mode><mask_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
	(unspec:VF_AVX512VL
	  [(match_operand:VF_AVX512VL 1 "nonimmediate_operand" "vm")
	   (match_operand:SI 2 "const_0_to_255_operand")]
	  UNSPEC_REDUCE))]
  "TARGET_AVX512DQ"
  "vreduce<ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "reduces<mode><mask_scalar_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
	  (unspec:VF_128
	    [(match_operand:VF_128 1 "register_operand" "v")
	     (match_operand:VF_128 2 "nonimmediate_operand" "vm")
	     (match_operand:SI 3 "const_0_to_255_operand")]
	    UNSPEC_REDUCE)
	  (match_dup 1)
	  (const_int 1)))]
  "TARGET_AVX512DQ"
  "vreduce<ssescalarmodesuffix>\t{%3, %2, %1, %0<mask_scalar_operand4>|%0<mask_scalar_operand4>, %1, %<iptr>2, %3}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel floating point comparisons
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "avx_cmp<mode>3"
  [(set (match_operand:VF_128_256 0 "register_operand" "=x")
	(unspec:VF_128_256
	  [(match_operand:VF_128_256 1 "register_operand" "x")
	   (match_operand:VF_128_256 2 "nonimmediate_operand" "xm")
	   (match_operand:SI 3 "const_0_to_31_operand" "n")]
	  UNSPEC_PCMP))]
  "TARGET_AVX"
  "vcmp<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

(define_insn "avx_vmcmp<mode>3"
  [(set (match_operand:VF_128 0 "register_operand" "=x")
	(vec_merge:VF_128
	  (unspec:VF_128
	    [(match_operand:VF_128 1 "register_operand" "x")
	     (match_operand:VF_128 2 "nonimmediate_operand" "xm")
	     (match_operand:SI 3 "const_0_to_31_operand" "n")]
	    UNSPEC_PCMP)
	 (match_dup 1)
	 (const_int 1)))]
  "TARGET_AVX"
  "vcmp<ssescalarmodesuffix>\t{%3, %2, %1, %0|%0, %1, %<iptr>2, %3}"
  [(set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "*<sse>_maskcmp<mode>3_comm"
  [(set (match_operand:VF_128_256 0 "register_operand" "=x,x")
	(match_operator:VF_128_256 3 "sse_comparison_operator"
	  [(match_operand:VF_128_256 1 "register_operand" "%0,x")
	   (match_operand:VF_128_256 2 "vector_operand" "xBm,xm")]))]
  "TARGET_SSE
   && GET_RTX_CLASS (GET_CODE (operands[3])) == RTX_COMM_COMPARE"
  "@
   cmp%D3<ssemodesuffix>\t{%2, %0|%0, %2}
   vcmp%D3<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "<MODE>")])

(define_insn "<sse>_maskcmp<mode>3"
  [(set (match_operand:VF_128_256 0 "register_operand" "=x,x")
	(match_operator:VF_128_256 3 "sse_comparison_operator"
	  [(match_operand:VF_128_256 1 "register_operand" "0,x")
	   (match_operand:VF_128_256 2 "vector_operand" "xBm,xm")]))]
  "TARGET_SSE"
  "@
   cmp%D3<ssemodesuffix>\t{%2, %0|%0, %2}
   vcmp%D3<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "<MODE>")])

(define_insn "<sse>_vmmaskcmp<mode>3"
  [(set (match_operand:VF_128 0 "register_operand" "=x,x")
	(vec_merge:VF_128
	 (match_operator:VF_128 3 "sse_comparison_operator"
	   [(match_operand:VF_128 1 "register_operand" "0,x")
	    (match_operand:VF_128 2 "vector_operand" "xBm,xm")])
	 (match_dup 1)
	 (const_int 1)))]
  "TARGET_SSE"
  "@
   cmp%D3<ssescalarmodesuffix>\t{%2, %0|%0, %<iptr>2}
   vcmp%D3<ssescalarmodesuffix>\t{%2, %1, %0|%0, %1, %<iptr>2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1,*")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "<ssescalarmode>")])

(define_mode_attr cmp_imm_predicate
  [(V16SF "const_0_to_31_operand")  (V8DF "const_0_to_31_operand")
   (V16SI "const_0_to_7_operand")   (V8DI "const_0_to_7_operand")
   (V8SF "const_0_to_31_operand")   (V4DF "const_0_to_31_operand")
   (V8SI "const_0_to_7_operand")    (V4DI "const_0_to_7_operand")
   (V4SF "const_0_to_31_operand")   (V2DF "const_0_to_31_operand")
   (V4SI "const_0_to_7_operand")    (V2DI "const_0_to_7_operand")
   (V32HI "const_0_to_7_operand")   (V64QI "const_0_to_7_operand")
   (V16HI "const_0_to_7_operand")   (V32QI "const_0_to_7_operand")
   (V8HI "const_0_to_7_operand")    (V16QI "const_0_to_7_operand")])

(define_insn "<avx512>_cmp<mode>3<mask_scalar_merge_name><round_saeonly_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(unspec:<avx512fmaskmode>
	  [(match_operand:V48_AVX512VL 1 "register_operand" "v")
	   (match_operand:V48_AVX512VL 2 "nonimmediate_operand" "<round_saeonly_constraint>")
	   (match_operand:SI 3 "<cmp_imm_predicate>" "n")]
	  UNSPEC_PCMP))]
  "TARGET_AVX512F && <round_saeonly_mode512bit_condition>"
  "v<sseintprefix>cmp<ssemodesuffix>\t{%3, <round_saeonly_mask_scalar_merge_op4>%2, %1, %0<mask_scalar_merge_operand4>|%0<mask_scalar_merge_operand4>, %1, %2<round_saeonly_mask_scalar_merge_op4>, %3}"
  [(set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_cmp<mode>3<mask_scalar_merge_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(unspec:<avx512fmaskmode>
	  [(match_operand:VI12_AVX512VL 1 "register_operand" "v")
	   (match_operand:VI12_AVX512VL 2 "nonimmediate_operand" "vm")
	   (match_operand:SI 3 "<cmp_imm_predicate>" "n")]
	  UNSPEC_PCMP))]
  "TARGET_AVX512BW"
  "vpcmp<ssemodesuffix>\t{%3, %2, %1, %0<mask_scalar_merge_operand4>|%0<mask_scalar_merge_operand4>, %1, %2, %3}"
  [(set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_ucmp<mode>3<mask_scalar_merge_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(unspec:<avx512fmaskmode>
	  [(match_operand:VI12_AVX512VL 1 "register_operand" "v")
	   (match_operand:VI12_AVX512VL 2 "nonimmediate_operand" "vm")
	   (match_operand:SI 3 "const_0_to_7_operand" "n")]
	  UNSPEC_UNSIGNED_PCMP))]
  "TARGET_AVX512BW"
  "vpcmpu<ssemodesuffix>\t{%3, %2, %1, %0<mask_scalar_merge_operand4>|%0<mask_scalar_merge_operand4>, %1, %2, %3}"
  [(set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_ucmp<mode>3<mask_scalar_merge_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(unspec:<avx512fmaskmode>
	  [(match_operand:VI48_AVX512VL 1 "register_operand" "v")
	   (match_operand:VI48_AVX512VL 2 "nonimmediate_operand" "vm")
	   (match_operand:SI 3 "const_0_to_7_operand" "n")]
	  UNSPEC_UNSIGNED_PCMP))]
  "TARGET_AVX512F"
  "vpcmpu<ssemodesuffix>\t{%3, %2, %1, %0<mask_scalar_merge_operand4>|%0<mask_scalar_merge_operand4>, %1, %2, %3}"
  [(set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "avx512f_vmcmp<mode>3<round_saeonly_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(and:<avx512fmaskmode>
	  (unspec:<avx512fmaskmode>
	    [(match_operand:VF_128 1 "register_operand" "v")
	     (match_operand:VF_128 2 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")
	     (match_operand:SI 3 "const_0_to_31_operand" "n")]
	    UNSPEC_PCMP)
	  (const_int 1)))]
  "TARGET_AVX512F"
  "vcmp<ssescalarmodesuffix>\t{%3, <round_saeonly_op4>%2, %1, %0|%0, %1, %<iptr>2<round_saeonly_op4>, %3}"
  [(set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "avx512f_vmcmp<mode>3_mask<round_saeonly_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(and:<avx512fmaskmode>
	  (unspec:<avx512fmaskmode>
	    [(match_operand:VF_128 1 "register_operand" "v")
	     (match_operand:VF_128 2 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")
	     (match_operand:SI 3 "const_0_to_31_operand" "n")]
	    UNSPEC_PCMP)
	  (and:<avx512fmaskmode>
	    (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")
	    (const_int 1))))]
  "TARGET_AVX512F"
  "vcmp<ssescalarmodesuffix>\t{%3, <round_saeonly_op5>%2, %1, %0%{%4%}|%0%{%4%}, %1, %<iptr>2<round_saeonly_op5>, %3}"
  [(set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "avx512f_maskcmp<mode>3"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(match_operator:<avx512fmaskmode> 3 "sse_comparison_operator"
	  [(match_operand:VF 1 "register_operand" "v")
	   (match_operand:VF 2 "nonimmediate_operand" "vm")]))]
  "TARGET_AVX512F"
  "vcmp%D3<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<sse>_<unord>comi<round_saeonly_name>"
  [(set (reg:CCFP FLAGS_REG)
	(compare:CCFP
	  (vec_select:MODEF
	    (match_operand:<ssevecmode> 0 "register_operand" "v")
	    (parallel [(const_int 0)]))
	  (vec_select:MODEF
	    (match_operand:<ssevecmode> 1 "<round_saeonly_nimm_scalar_predicate>" "<round_saeonly_constraint>")
	    (parallel [(const_int 0)]))))]
  "SSE_FLOAT_MODE_P (<MODE>mode)"
  "%v<unord>comi<ssemodesuffix>\t{<round_saeonly_op2>%1, %0|%0, %<iptr>1<round_saeonly_op2>}"
  [(set_attr "type" "ssecomi")
   (set_attr "prefix" "maybe_vex")
   (set_attr "prefix_rep" "0")
   (set (attr "prefix_data16")
	(if_then_else (eq_attr "mode" "DF")
		      (const_string "1")
		      (const_string "0")))
   (set_attr "mode" "<MODE>")])

(define_expand "vec_cmp<mode><avx512fmaskmodelower>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand")
	(match_operator:<avx512fmaskmode> 1 ""
	  [(match_operand:V48_AVX512VL 2 "register_operand")
	   (match_operand:V48_AVX512VL 3 "nonimmediate_operand")]))]
  "TARGET_AVX512F"
{
  bool ok = ix86_expand_mask_vec_cmp (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vec_cmp<mode><avx512fmaskmodelower>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand")
	(match_operator:<avx512fmaskmode> 1 ""
	  [(match_operand:VI12_AVX512VL 2 "register_operand")
	   (match_operand:VI12_AVX512VL 3 "nonimmediate_operand")]))]
  "TARGET_AVX512BW"
{
  bool ok = ix86_expand_mask_vec_cmp (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vec_cmp<mode><sseintvecmodelower>"
  [(set (match_operand:<sseintvecmode> 0 "register_operand")
	(match_operator:<sseintvecmode> 1 ""
	  [(match_operand:VI_256 2 "register_operand")
	   (match_operand:VI_256 3 "nonimmediate_operand")]))]
  "TARGET_AVX2"
{
  bool ok = ix86_expand_int_vec_cmp (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vec_cmp<mode><sseintvecmodelower>"
  [(set (match_operand:<sseintvecmode> 0 "register_operand")
	(match_operator:<sseintvecmode> 1 ""
	  [(match_operand:VI124_128 2 "register_operand")
	   (match_operand:VI124_128 3 "vector_operand")]))]
  "TARGET_SSE2"
{
  bool ok = ix86_expand_int_vec_cmp (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vec_cmpv2div2di"
  [(set (match_operand:V2DI 0 "register_operand")
	(match_operator:V2DI 1 ""
	  [(match_operand:V2DI 2 "register_operand")
	   (match_operand:V2DI 3 "vector_operand")]))]
  "TARGET_SSE4_2"
{
  bool ok = ix86_expand_int_vec_cmp (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vec_cmp<mode><sseintvecmodelower>"
  [(set (match_operand:<sseintvecmode> 0 "register_operand")
	(match_operator:<sseintvecmode> 1 ""
	  [(match_operand:VF_256 2 "register_operand")
	   (match_operand:VF_256 3 "nonimmediate_operand")]))]
  "TARGET_AVX"
{
  bool ok = ix86_expand_fp_vec_cmp (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vec_cmp<mode><sseintvecmodelower>"
  [(set (match_operand:<sseintvecmode> 0 "register_operand")
	(match_operator:<sseintvecmode> 1 ""
	  [(match_operand:VF_128 2 "register_operand")
	   (match_operand:VF_128 3 "vector_operand")]))]
  "TARGET_SSE"
{
  bool ok = ix86_expand_fp_vec_cmp (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vec_cmpu<mode><avx512fmaskmodelower>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand")
	(match_operator:<avx512fmaskmode> 1 ""
	  [(match_operand:VI48_AVX512VL 2 "register_operand")
	   (match_operand:VI48_AVX512VL 3 "nonimmediate_operand")]))]
  "TARGET_AVX512F"
{
  bool ok = ix86_expand_mask_vec_cmp (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vec_cmpu<mode><avx512fmaskmodelower>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand")
	(match_operator:<avx512fmaskmode> 1 ""
	  [(match_operand:VI12_AVX512VL 2 "register_operand")
	   (match_operand:VI12_AVX512VL 3 "nonimmediate_operand")]))]
  "TARGET_AVX512BW"
{
  bool ok = ix86_expand_mask_vec_cmp (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vec_cmpu<mode><sseintvecmodelower>"
  [(set (match_operand:<sseintvecmode> 0 "register_operand")
	(match_operator:<sseintvecmode> 1 ""
	  [(match_operand:VI_256 2 "register_operand")
	   (match_operand:VI_256 3 "nonimmediate_operand")]))]
  "TARGET_AVX2"
{
  bool ok = ix86_expand_int_vec_cmp (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vec_cmpu<mode><sseintvecmodelower>"
  [(set (match_operand:<sseintvecmode> 0 "register_operand")
	(match_operator:<sseintvecmode> 1 ""
	  [(match_operand:VI124_128 2 "register_operand")
	   (match_operand:VI124_128 3 "vector_operand")]))]
  "TARGET_SSE2"
{
  bool ok = ix86_expand_int_vec_cmp (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vec_cmpuv2div2di"
  [(set (match_operand:V2DI 0 "register_operand")
	(match_operator:V2DI 1 ""
	  [(match_operand:V2DI 2 "register_operand")
	   (match_operand:V2DI 3 "vector_operand")]))]
  "TARGET_SSE4_2"
{
  bool ok = ix86_expand_int_vec_cmp (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vec_cmpeqv2div2di"
  [(set (match_operand:V2DI 0 "register_operand")
	(match_operator:V2DI 1 ""
	  [(match_operand:V2DI 2 "register_operand")
	   (match_operand:V2DI 3 "vector_operand")]))]
  "TARGET_SSE4_1"
{
  bool ok = ix86_expand_int_vec_cmp (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vcond<V_512:mode><VF_512:mode>"
  [(set (match_operand:V_512 0 "register_operand")
	(if_then_else:V_512
	  (match_operator 3 ""
	    [(match_operand:VF_512 4 "nonimmediate_operand")
	     (match_operand:VF_512 5 "nonimmediate_operand")])
	  (match_operand:V_512 1 "general_operand")
	  (match_operand:V_512 2 "general_operand")))]
  "TARGET_AVX512F
   && (GET_MODE_NUNITS (<V_512:MODE>mode)
       == GET_MODE_NUNITS (<VF_512:MODE>mode))"
{
  bool ok = ix86_expand_fp_vcond (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vcond<V_256:mode><VF_256:mode>"
  [(set (match_operand:V_256 0 "register_operand")
	(if_then_else:V_256
	  (match_operator 3 ""
	    [(match_operand:VF_256 4 "nonimmediate_operand")
	     (match_operand:VF_256 5 "nonimmediate_operand")])
	  (match_operand:V_256 1 "general_operand")
	  (match_operand:V_256 2 "general_operand")))]
  "TARGET_AVX
   && (GET_MODE_NUNITS (<V_256:MODE>mode)
       == GET_MODE_NUNITS (<VF_256:MODE>mode))"
{
  bool ok = ix86_expand_fp_vcond (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vcond<V_128:mode><VF_128:mode>"
  [(set (match_operand:V_128 0 "register_operand")
	(if_then_else:V_128
	  (match_operator 3 ""
	    [(match_operand:VF_128 4 "vector_operand")
	     (match_operand:VF_128 5 "vector_operand")])
	  (match_operand:V_128 1 "general_operand")
	  (match_operand:V_128 2 "general_operand")))]
  "TARGET_SSE
   && (GET_MODE_NUNITS (<V_128:MODE>mode)
       == GET_MODE_NUNITS (<VF_128:MODE>mode))"
{
  bool ok = ix86_expand_fp_vcond (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vcond_mask_<mode><avx512fmaskmodelower>"
  [(set (match_operand:V48_AVX512VL 0 "register_operand")
	(vec_merge:V48_AVX512VL
	  (match_operand:V48_AVX512VL 1 "nonimmediate_operand")
	  (match_operand:V48_AVX512VL 2 "nonimm_or_0_operand")
	  (match_operand:<avx512fmaskmode> 3 "register_operand")))]
  "TARGET_AVX512F")

(define_expand "vcond_mask_<mode><avx512fmaskmodelower>"
  [(set (match_operand:VI12_AVX512VL 0 "register_operand")
	(vec_merge:VI12_AVX512VL
	  (match_operand:VI12_AVX512VL 1 "nonimmediate_operand")
	  (match_operand:VI12_AVX512VL 2 "nonimm_or_0_operand")
	  (match_operand:<avx512fmaskmode> 3 "register_operand")))]
  "TARGET_AVX512BW")

(define_expand "vcond_mask_<mode><sseintvecmodelower>"
  [(set (match_operand:VI_256 0 "register_operand")
	(vec_merge:VI_256
	  (match_operand:VI_256 1 "nonimmediate_operand")
	  (match_operand:VI_256 2 "nonimm_or_0_operand")
	  (match_operand:<sseintvecmode> 3 "register_operand")))]
  "TARGET_AVX2"
{
  ix86_expand_sse_movcc (operands[0], operands[3],
			 operands[1], operands[2]);
  DONE;
})

(define_expand "vcond_mask_<mode><sseintvecmodelower>"
  [(set (match_operand:VI124_128 0 "register_operand")
	(vec_merge:VI124_128
	  (match_operand:VI124_128 1 "vector_operand")
	  (match_operand:VI124_128 2 "nonimm_or_0_operand")
	  (match_operand:<sseintvecmode> 3 "register_operand")))]
  "TARGET_SSE2"
{
  ix86_expand_sse_movcc (operands[0], operands[3],
			 operands[1], operands[2]);
  DONE;
})

(define_expand "vcond_mask_v2div2di"
  [(set (match_operand:V2DI 0 "register_operand")
	(vec_merge:V2DI
	  (match_operand:V2DI 1 "vector_operand")
	  (match_operand:V2DI 2 "nonimm_or_0_operand")
	  (match_operand:V2DI 3 "register_operand")))]
  "TARGET_SSE4_2"
{
  ix86_expand_sse_movcc (operands[0], operands[3],
			 operands[1], operands[2]);
  DONE;
})

(define_expand "vcond_mask_<mode><sseintvecmodelower>"
  [(set (match_operand:VF_256 0 "register_operand")
	(vec_merge:VF_256
	  (match_operand:VF_256 1 "nonimmediate_operand")
	  (match_operand:VF_256 2 "nonimm_or_0_operand")
	  (match_operand:<sseintvecmode> 3 "register_operand")))]
  "TARGET_AVX"
{
  ix86_expand_sse_movcc (operands[0], operands[3],
			 operands[1], operands[2]);
  DONE;
})

(define_expand "vcond_mask_<mode><sseintvecmodelower>"
  [(set (match_operand:VF_128 0 "register_operand")
	(vec_merge:VF_128
	  (match_operand:VF_128 1 "vector_operand")
	  (match_operand:VF_128 2 "nonimm_or_0_operand")
	  (match_operand:<sseintvecmode> 3 "register_operand")))]
  "TARGET_SSE"
{
  ix86_expand_sse_movcc (operands[0], operands[3],
			 operands[1], operands[2]);
  DONE;
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel floating point logical operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "<sse>_andnot<mode>3<mask_name>"
  [(set (match_operand:VF_128_256 0 "register_operand" "=x,x,v,v")
	(and:VF_128_256
	  (not:VF_128_256
	    (match_operand:VF_128_256 1 "register_operand" "0,x,v,v"))
	  (match_operand:VF_128_256 2 "vector_operand" "xBm,xm,vm,vm")))]
  "TARGET_SSE && <mask_avx512vl_condition>"
{
  static char buf[128];
  const char *ops;
  const char *suffix;

  switch (which_alternative)
    {
    case 0:
      ops = "andn%s\t{%%2, %%0|%%0, %%2}";
      break;
    case 1:
    case 2:
    case 3:
      ops = "vandn%s\t{%%2, %%1, %%0<mask_operand3_1>|%%0<mask_operand3_1>, %%1, %%2}";
      break;
    default:
      gcc_unreachable ();
    }

  switch (get_attr_mode (insn))
    {
    case MODE_V8SF:
    case MODE_V4SF:
      suffix = "ps";
      break;
    case MODE_OI:
    case MODE_TI:
      /* There is no vandnp[sd] in avx512f.  Use vpandn[qd].  */
      suffix = GET_MODE_INNER (<MODE>mode) == DFmode ? "q" : "d";
      ops = "vpandn%s\t{%%2, %%1, %%0<mask_operand3_1>|%%0<mask_operand3_1>, %%1, %%2}";
      break;
    default:
      suffix = "<ssemodesuffix>";
    }

  snprintf (buf, sizeof (buf), ops, suffix);
  return buf;
}
  [(set_attr "isa" "noavx,avx,avx512dq,avx512f")
   (set_attr "type" "sselog")
   (set_attr "prefix" "orig,maybe_vex,evex,evex")
   (set (attr "mode")
	(cond [(and (match_test "<mask_applied>")
		    (and (eq_attr "alternative" "1")
			 (match_test "!TARGET_AVX512DQ")))
		 (const_string "<sseintvecmode2>")
	       (eq_attr "alternative" "3")
		 (const_string "<sseintvecmode2>")
	       (and (match_test "<MODE_SIZE> == 16")
		    (match_test "TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL"))
		 (const_string "<ssePSmode>")
	       (match_test "TARGET_AVX")
		 (const_string "<MODE>")
	       (match_test "optimize_function_for_size_p (cfun)")
		 (const_string "V4SF")
	       ]
	       (const_string "<MODE>")))])


(define_insn "<sse>_andnot<mode>3<mask_name>"
  [(set (match_operand:VF_512 0 "register_operand" "=v")
	(and:VF_512
	  (not:VF_512
	    (match_operand:VF_512 1 "register_operand" "v"))
	  (match_operand:VF_512 2 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512F"
{
  static char buf[128];
  const char *ops;
  const char *suffix;

  suffix = "<ssemodesuffix>";
  ops = "";

  /* There is no vandnp[sd] in avx512f.  Use vpandn[qd].  */
  if (!TARGET_AVX512DQ)
    {
      suffix = GET_MODE_INNER (<MODE>mode) == DFmode ? "q" : "d";
      ops = "p";
    }

  snprintf (buf, sizeof (buf),
	    "v%sandn%s\t{%%2, %%1, %%0<mask_operand3_1>|%%0<mask_operand3_1>, %%1, %%2}",
	    ops, suffix);
  return buf;
}
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set (attr "mode")
        (if_then_else (match_test "TARGET_AVX512DQ")
		      (const_string "<sseinsnmode>")
		      (const_string "XI")))])

(define_expand "<code><mode>3<mask_name>"
  [(set (match_operand:VF_128_256 0 "register_operand")
       (any_logic:VF_128_256
         (match_operand:VF_128_256 1 "vector_operand")
         (match_operand:VF_128_256 2 "vector_operand")))]
  "TARGET_SSE && <mask_avx512vl_condition>"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_expand "<code><mode>3<mask_name>"
  [(set (match_operand:VF_512 0 "register_operand")
       (any_logic:VF_512
         (match_operand:VF_512 1 "nonimmediate_operand")
         (match_operand:VF_512 2 "nonimmediate_operand")))]
  "TARGET_AVX512F"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*<code><mode>3<mask_name>"
  [(set (match_operand:VF_128_256 0 "register_operand" "=x,x,v,v")
	(any_logic:VF_128_256
	  (match_operand:VF_128_256 1 "vector_operand" "%0,x,v,v")
	  (match_operand:VF_128_256 2 "vector_operand" "xBm,xm,vm,vm")))]
  "TARGET_SSE && <mask_avx512vl_condition>
   && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
{
  static char buf[128];
  const char *ops;
  const char *suffix;

  switch (which_alternative)
    {
    case 0:
      ops = "<logic>%s\t{%%2, %%0|%%0, %%2}";
      break;
    case 1:
    case 2:
    case 3:
      ops = "v<logic>%s\t{%%2, %%1, %%0<mask_operand3_1>|%%0<mask_operand3_1>, %%1, %%2}";
      break;
    default:
      gcc_unreachable ();
    }

  switch (get_attr_mode (insn))
    {
    case MODE_V8SF:
    case MODE_V4SF:
      suffix = "ps";
      break;
    case MODE_OI:
    case MODE_TI:
      /* There is no v<logic>p[sd] in avx512f.  Use vp<logic>[qd].  */
      suffix = GET_MODE_INNER (<MODE>mode) == DFmode ? "q" : "d";
      ops = "vp<logic>%s\t{%%2, %%1, %%0<mask_operand3_1>|%%0<mask_operand3_1>, %%1, %%2}";
      break;
    default:
      suffix = "<ssemodesuffix>";
    }

  snprintf (buf, sizeof (buf), ops, suffix);
  return buf;
}
  [(set_attr "isa" "noavx,avx,avx512dq,avx512f")
   (set_attr "type" "sselog")
   (set_attr "prefix" "orig,maybe_evex,evex,evex")
   (set (attr "mode")
	(cond [(and (match_test "<mask_applied>")
		    (and (eq_attr "alternative" "1")
			 (match_test "!TARGET_AVX512DQ")))
		 (const_string "<sseintvecmode2>")
	       (eq_attr "alternative" "3")
		 (const_string "<sseintvecmode2>")
	       (and (match_test "<MODE_SIZE> == 16")
		    (match_test "TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL"))
		 (const_string "<ssePSmode>")
	       (match_test "TARGET_AVX")
		 (const_string "<MODE>")
	       (match_test "optimize_function_for_size_p (cfun)")
		 (const_string "V4SF")
	       ]
	       (const_string "<MODE>")))])

(define_insn "*<code><mode>3<mask_name>"
  [(set (match_operand:VF_512 0 "register_operand" "=v")
	(any_logic:VF_512
	  (match_operand:VF_512 1 "nonimmediate_operand" "%v")
	  (match_operand:VF_512 2 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512F && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
{
  static char buf[128];
  const char *ops;
  const char *suffix;

  suffix = "<ssemodesuffix>";
  ops = "";

  /* There is no v<logic>p[sd] in avx512f.  Use vp<logic>[dq].  */
  if (!TARGET_AVX512DQ)
    {
      suffix = GET_MODE_INNER (<MODE>mode) == DFmode ? "q" : "d";
      ops = "p";
    }

  snprintf (buf, sizeof (buf),
	   "v%s<logic>%s\t{%%2, %%1, %%0<mask_operand3_1>|%%0<mask_operand3_1>, %%1, %%2}",
	   ops, suffix);
  return buf;
}
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set (attr "mode")
        (if_then_else (match_test "TARGET_AVX512DQ")
		      (const_string "<sseinsnmode>")
		      (const_string "XI")))])

(define_expand "copysign<mode>3"
  [(set (match_dup 4)
	(and:VF
	  (not:VF (match_dup 3))
	  (match_operand:VF 1 "vector_operand")))
   (set (match_dup 5)
	(and:VF (match_dup 3)
		(match_operand:VF 2 "vector_operand")))
   (set (match_operand:VF 0 "register_operand")
	(ior:VF (match_dup 4) (match_dup 5)))]
  "TARGET_SSE"
{
  operands[3] = ix86_build_signbit_mask (<MODE>mode, 1, 0);

  operands[4] = gen_reg_rtx (<MODE>mode);
  operands[5] = gen_reg_rtx (<MODE>mode);
})

;; Also define scalar versions.  These are used for abs, neg, and
;; conditional move.  Using subregs into vector modes causes register
;; allocation lossage.  These patterns do not allow memory operands
;; because the native instructions read the full 128-bits.

(define_insn "*andnot<mode>3"
  [(set (match_operand:MODEF 0 "register_operand" "=x,x,v,v")
	(and:MODEF
	  (not:MODEF
	    (match_operand:MODEF 1 "register_operand" "0,x,v,v"))
	    (match_operand:MODEF 2 "register_operand" "x,x,v,v")))]
  "SSE_FLOAT_MODE_P (<MODE>mode)"
{
  static char buf[128];
  const char *ops;
  const char *suffix
    = (get_attr_mode (insn) == MODE_V4SF) ? "ps" : "<ssevecmodesuffix>";

  switch (which_alternative)
    {
    case 0:
      ops = "andn%s\t{%%2, %%0|%%0, %%2}";
      break;
    case 1:
      ops = "vandn%s\t{%%2, %%1, %%0|%%0, %%1, %%2}";
      break;
    case 2:
      if (TARGET_AVX512DQ)
	ops = "vandn%s\t{%%2, %%1, %%0|%%0, %%1, %%2}";
      else
	{
	  suffix = <MODE>mode == DFmode ? "q" : "d";
	  ops = "vpandn%s\t{%%2, %%1, %%0|%%0, %%1, %%2}";
	}
      break;
    case 3:
      if (TARGET_AVX512DQ)
	ops = "vandn%s\t{%%g2, %%g1, %%g0|%%g0, %%g1, %%g2}";
      else
	{
	  suffix = <MODE>mode == DFmode ? "q" : "d";
	  ops = "vpandn%s\t{%%g2, %%g1, %%g0|%%g0, %%g1, %%g2}";
	}
      break;
    default:
      gcc_unreachable ();
    }

  snprintf (buf, sizeof (buf), ops, suffix);
  return buf;
}
  [(set_attr "isa" "noavx,avx,avx512vl,avx512f")
   (set_attr "type" "sselog")
   (set_attr "prefix" "orig,vex,evex,evex")
   (set (attr "mode")
	(cond [(eq_attr "alternative" "2")
		 (if_then_else (match_test "TARGET_AVX512DQ")
			       (const_string "<ssevecmode>")
			       (const_string "TI"))
	       (eq_attr "alternative" "3")
		 (if_then_else (match_test "TARGET_AVX512DQ")
			       (const_string "<avx512fvecmode>")
			       (const_string "XI"))
	       (and (match_test "<MODE_SIZE> == 16")
		    (match_test "TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL"))
		 (const_string "V4SF")
	       (match_test "TARGET_AVX")
		 (const_string "<ssevecmode>")
	       (match_test "optimize_function_for_size_p (cfun)")
		 (const_string "V4SF")
	       ]
	       (const_string "<ssevecmode>")))])

(define_insn "*andnottf3"
  [(set (match_operand:TF 0 "register_operand" "=x,x,v,v")
	(and:TF
	  (not:TF (match_operand:TF 1 "register_operand" "0,x,v,v"))
	  (match_operand:TF 2 "vector_operand" "xBm,xm,vm,v")))]
  "TARGET_SSE"
{
  static char buf[128];
  const char *ops;
  const char *tmp
    = (which_alternative >= 2 ? "pandnq"
       : get_attr_mode (insn) == MODE_V4SF ? "andnps" : "pandn");

  switch (which_alternative)
    {
    case 0:
      ops = "%s\t{%%2, %%0|%%0, %%2}";
      break;
    case 1:
    case 2:
      ops = "v%s\t{%%2, %%1, %%0|%%0, %%1, %%2}";
      break;
    case 3:
      ops = "v%s\t{%%g2, %%g1, %%g0|%%g0, %%g1, %%g2}";
      break;
    default:
      gcc_unreachable ();
    }

  snprintf (buf, sizeof (buf), ops, tmp);
  return buf;
}
  [(set_attr "isa" "noavx,avx,avx512vl,avx512f")
   (set_attr "type" "sselog")
   (set (attr "prefix_data16")
     (if_then_else
       (and (eq_attr "alternative" "0")
	    (eq_attr "mode" "TI"))
       (const_string "1")
       (const_string "*")))
   (set_attr "prefix" "orig,vex,evex,evex")
   (set (attr "mode")
	(cond [(eq_attr "alternative" "2")
		 (const_string "TI")
	       (eq_attr "alternative" "3")
		 (const_string "XI")
	       (match_test "TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL")
		 (const_string "V4SF")
	       (match_test "TARGET_AVX")
		 (const_string "TI")
	       (ior (not (match_test "TARGET_SSE2"))
		    (match_test "optimize_function_for_size_p (cfun)"))
		 (const_string "V4SF")
	       ]
	       (const_string "TI")))])

(define_insn "*<code><mode>3"
  [(set (match_operand:MODEF 0 "register_operand" "=x,x,v,v")
	(any_logic:MODEF
	  (match_operand:MODEF 1 "register_operand" "%0,x,v,v")
	  (match_operand:MODEF 2 "register_operand" "x,x,v,v")))]
  "SSE_FLOAT_MODE_P (<MODE>mode)"
{
  static char buf[128];
  const char *ops;
  const char *suffix
    = (get_attr_mode (insn) == MODE_V4SF) ? "ps" : "<ssevecmodesuffix>";

  switch (which_alternative)
    {
    case 0:
      ops = "<logic>%s\t{%%2, %%0|%%0, %%2}";
      break;
    case 2:
      if (!TARGET_AVX512DQ)
	{
	  suffix = <MODE>mode == DFmode ? "q" : "d";
	  ops = "vp<logic>%s\t{%%2, %%1, %%0|%%0, %%1, %%2}";
	  break;
	}
      /* FALLTHRU */
    case 1:
      ops = "v<logic>%s\t{%%2, %%1, %%0|%%0, %%1, %%2}";
      break;
    case 3:
      if (TARGET_AVX512DQ)
	ops = "v<logic>%s\t{%%g2, %%g1, %%g0|%%g0, %%g1, %%g2}";
      else
	{
	  suffix = <MODE>mode == DFmode ? "q" : "d";
	  ops = "vp<logic>%s\t{%%g2, %%g1, %%g0|%%g0, %%g1, %%g2}";
	}
      break;
    default:
      gcc_unreachable ();
    }

  snprintf (buf, sizeof (buf), ops, suffix);
  return buf;
}
  [(set_attr "isa" "noavx,avx,avx512vl,avx512f")
   (set_attr "type" "sselog")
   (set_attr "prefix" "orig,vex,evex,evex")
   (set (attr "mode")
	(cond [(eq_attr "alternative" "2")
		 (if_then_else (match_test "TARGET_AVX512DQ")
			       (const_string "<ssevecmode>")
			       (const_string "TI"))
	       (eq_attr "alternative" "3")
		 (if_then_else (match_test "TARGET_AVX512DQ")
			       (const_string "<avx512fvecmode>")
			       (const_string "XI"))
	       (and (match_test "<MODE_SIZE> == 16")
		    (match_test "TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL"))
		 (const_string "V4SF")
	       (match_test "TARGET_AVX")
		 (const_string "<ssevecmode>")
	       (match_test "optimize_function_for_size_p (cfun)")
		 (const_string "V4SF")
	       ]
	       (const_string "<ssevecmode>")))])

(define_expand "<code>tf3"
  [(set (match_operand:TF 0 "register_operand")
	(any_logic:TF
	  (match_operand:TF 1 "vector_operand")
	  (match_operand:TF 2 "vector_operand")))]
  "TARGET_SSE"
  "ix86_fixup_binary_operands_no_copy (<CODE>, TFmode, operands);")

(define_insn "*<code>tf3"
  [(set (match_operand:TF 0 "register_operand" "=x,x,v,v")
	(any_logic:TF
	  (match_operand:TF 1 "vector_operand" "%0,x,v,v")
	  (match_operand:TF 2 "vector_operand" "xBm,xm,vm,v")))]
  "TARGET_SSE && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
{
  static char buf[128];
  const char *ops;
  const char *tmp
    = (which_alternative >= 2 ? "p<logic>q"
       : get_attr_mode (insn) == MODE_V4SF ? "<logic>ps" : "p<logic>");

  switch (which_alternative)
    {
    case 0:
      ops = "%s\t{%%2, %%0|%%0, %%2}";
      break;
    case 1:
    case 2:
      ops = "v%s\t{%%2, %%1, %%0|%%0, %%1, %%2}";
      break;
    case 3:
      ops = "v%s\t{%%g2, %%g1, %%g0|%%g0, %%g1, %%g2}";
      break;
    default:
      gcc_unreachable ();
    }

  snprintf (buf, sizeof (buf), ops, tmp);
  return buf;
}
  [(set_attr "isa" "noavx,avx,avx512vl,avx512f")
   (set_attr "type" "sselog")
   (set (attr "prefix_data16")
     (if_then_else
       (and (eq_attr "alternative" "0")
	    (eq_attr "mode" "TI"))
       (const_string "1")
       (const_string "*")))
   (set_attr "prefix" "orig,vex,evex,evex")
   (set (attr "mode")
	(cond [(eq_attr "alternative" "2")
		 (const_string "TI")
	       (eq_attr "alternative" "3")
		 (const_string "QI")
	       (match_test "TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL")
		 (const_string "V4SF")
	       (match_test "TARGET_AVX")
		 (const_string "TI")
	       (ior (not (match_test "TARGET_SSE2"))
		    (match_test "optimize_function_for_size_p (cfun)"))
		 (const_string "V4SF")
	       ]
	       (const_string "TI")))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FMA floating point multiply/accumulate instructions.  These include
;; scalar versions of the instructions as well as vector versions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The standard names for scalar FMA are only available with SSE math enabled.
;; CPUID bit AVX512F enables evex encoded scalar and 512-bit fma.  It doesn't
;; care about FMA bit, so we enable fma for TARGET_AVX512F even when TARGET_FMA
;; and TARGET_FMA4 are both false.
;; TODO: In theory AVX512F does not automatically imply FMA, and without FMA
;; one must force the EVEX encoding of the fma insns.  Ideally we'd improve
;; GAS to allow proper prefix selection.  However, for the moment all hardware
;; that supports AVX512F also supports FMA so we can ignore this for now.
(define_mode_iterator FMAMODEM
  [(SF "TARGET_SSE_MATH && (TARGET_FMA || TARGET_FMA4 || TARGET_AVX512F)")
   (DF "TARGET_SSE_MATH && (TARGET_FMA || TARGET_FMA4 || TARGET_AVX512F)")
   (V4SF "TARGET_FMA || TARGET_FMA4 || TARGET_AVX512VL")
   (V2DF "TARGET_FMA || TARGET_FMA4 || TARGET_AVX512VL")
   (V8SF "TARGET_FMA || TARGET_FMA4 || TARGET_AVX512VL")
   (V4DF "TARGET_FMA || TARGET_FMA4 || TARGET_AVX512VL")
   (V16SF "TARGET_AVX512F")
   (V8DF "TARGET_AVX512F")])

(define_expand "fma<mode>4"
  [(set (match_operand:FMAMODEM 0 "register_operand")
	(fma:FMAMODEM
	  (match_operand:FMAMODEM 1 "nonimmediate_operand")
	  (match_operand:FMAMODEM 2 "nonimmediate_operand")
	  (match_operand:FMAMODEM 3 "nonimmediate_operand")))])

(define_expand "fms<mode>4"
  [(set (match_operand:FMAMODEM 0 "register_operand")
	(fma:FMAMODEM
	  (match_operand:FMAMODEM 1 "nonimmediate_operand")
	  (match_operand:FMAMODEM 2 "nonimmediate_operand")
	  (neg:FMAMODEM (match_operand:FMAMODEM 3 "nonimmediate_operand"))))])

(define_expand "fnma<mode>4"
  [(set (match_operand:FMAMODEM 0 "register_operand")
	(fma:FMAMODEM
	  (neg:FMAMODEM (match_operand:FMAMODEM 1 "nonimmediate_operand"))
	  (match_operand:FMAMODEM 2 "nonimmediate_operand")
	  (match_operand:FMAMODEM 3 "nonimmediate_operand")))])

(define_expand "fnms<mode>4"
  [(set (match_operand:FMAMODEM 0 "register_operand")
	(fma:FMAMODEM
	  (neg:FMAMODEM (match_operand:FMAMODEM 1 "nonimmediate_operand"))
	  (match_operand:FMAMODEM 2 "nonimmediate_operand")
	  (neg:FMAMODEM (match_operand:FMAMODEM 3 "nonimmediate_operand"))))])

;; The builtins for intrinsics are not constrained by SSE math enabled.
(define_mode_iterator FMAMODE_AVX512
 [(SF "TARGET_FMA || TARGET_FMA4 || TARGET_AVX512F")
  (DF "TARGET_FMA || TARGET_FMA4 || TARGET_AVX512F")
  (V4SF "TARGET_FMA || TARGET_FMA4 || TARGET_AVX512VL")
  (V2DF "TARGET_FMA || TARGET_FMA4 || TARGET_AVX512VL")
  (V8SF "TARGET_FMA || TARGET_FMA4 || TARGET_AVX512VL")
  (V4DF "TARGET_FMA || TARGET_FMA4 || TARGET_AVX512VL")
  (V16SF "TARGET_AVX512F")
  (V8DF "TARGET_AVX512F")])

(define_mode_iterator FMAMODE
  [SF DF V4SF V2DF V8SF V4DF])

(define_expand "fma4i_fmadd_<mode>"
  [(set (match_operand:FMAMODE_AVX512 0 "register_operand")
	(fma:FMAMODE_AVX512
	  (match_operand:FMAMODE_AVX512 1 "nonimmediate_operand")
	  (match_operand:FMAMODE_AVX512 2 "nonimmediate_operand")
	  (match_operand:FMAMODE_AVX512 3 "nonimmediate_operand")))])

(define_expand "<avx512>_fmadd_<mode>_maskz<round_expand_name>"
  [(match_operand:VF_AVX512VL 0 "register_operand")
   (match_operand:VF_AVX512VL 1 "<round_expand_nimm_predicate>")
   (match_operand:VF_AVX512VL 2 "<round_expand_nimm_predicate>")
   (match_operand:VF_AVX512VL 3 "<round_expand_nimm_predicate>")
   (match_operand:<avx512fmaskmode> 4 "register_operand")]
  "TARGET_AVX512F && <round_mode512bit_condition>"
{
  emit_insn (gen_fma_fmadd_<mode>_maskz_1<round_expand_name> (
    operands[0], operands[1], operands[2], operands[3],
    CONST0_RTX (<MODE>mode), operands[4]<round_expand_operand>));
  DONE;
})

(define_insn "*fma_fmadd_<mode>"
  [(set (match_operand:FMAMODE 0 "register_operand" "=v,v,v,x,x")
	(fma:FMAMODE
	  (match_operand:FMAMODE 1 "nonimmediate_operand" "%0,0,v,x,x")
	  (match_operand:FMAMODE 2 "nonimmediate_operand" "vm,v,vm,x,m")
	  (match_operand:FMAMODE 3 "nonimmediate_operand" "v,vm,0,xm,x")))]
  "TARGET_FMA || TARGET_FMA4"
  "@
   vfmadd132<ssemodesuffix>\t{%2, %3, %0|%0, %3, %2}
   vfmadd213<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   vfmadd231<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}
   vfmadd<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}
   vfmadd<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "isa" "fma,fma,fma,fma4,fma4")
   (set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

;; Suppose AVX-512F as baseline
(define_mode_iterator VF_SF_AVX512VL
  [SF V16SF (V8SF "TARGET_AVX512VL") (V4SF "TARGET_AVX512VL")
   DF V8DF (V4DF "TARGET_AVX512VL") (V2DF "TARGET_AVX512VL")])

(define_insn "<sd_mask_codefor>fma_fmadd_<mode><sd_maskz_name><round_name>"
  [(set (match_operand:VF_SF_AVX512VL 0 "register_operand" "=v,v,v")
	(fma:VF_SF_AVX512VL
	  (match_operand:VF_SF_AVX512VL 1 "<round_nimm_predicate>" "%0,0,v")
	  (match_operand:VF_SF_AVX512VL 2 "<round_nimm_predicate>" "<round_constraint>,v,<round_constraint>")
	  (match_operand:VF_SF_AVX512VL 3 "<round_nimm_predicate>" "v,<round_constraint>,0")))]
  "TARGET_AVX512F && <sd_mask_mode512bit_condition> && <round_mode512bit_condition>"
  "@
   vfmadd132<ssemodesuffix>\t{<round_sd_mask_op4>%2, %3, %0<sd_mask_op4>|%0<sd_mask_op4>, %3, %2<round_sd_mask_op4>}
   vfmadd213<ssemodesuffix>\t{<round_sd_mask_op4>%3, %2, %0<sd_mask_op4>|%0<sd_mask_op4>, %2, %3<round_sd_mask_op4>}
   vfmadd231<ssemodesuffix>\t{<round_sd_mask_op4>%2, %1, %0<sd_mask_op4>|%0<sd_mask_op4>, %1, %2<round_sd_mask_op4>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<avx512>_fmadd_<mode>_mask<round_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v,v")
	(vec_merge:VF_AVX512VL
	  (fma:VF_AVX512VL
	    (match_operand:VF_AVX512VL 1 "register_operand" "0,0")
	    (match_operand:VF_AVX512VL 2 "nonimmediate_operand" "<round_constraint>,v")
	    (match_operand:VF_AVX512VL 3 "nonimmediate_operand" "v,<round_constraint>"))
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk,Yk")))]
  "TARGET_AVX512F && <round_mode512bit_condition>"
  "@
   vfmadd132<ssemodesuffix>\t{<round_op5>%2, %3, %0%{%4%}|%0%{%4%}, %3, %2<round_op5>}
   vfmadd213<ssemodesuffix>\t{<round_op5>%3, %2, %0%{%4%}|%0%{%4%}, %2, %3<round_op5>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<avx512>_fmadd_<mode>_mask3<round_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VF_AVX512VL
	  (fma:VF_AVX512VL
	    (match_operand:VF_AVX512VL 1 "register_operand" "v")
	    (match_operand:VF_AVX512VL 2 "nonimmediate_operand" "<round_constraint>")
	    (match_operand:VF_AVX512VL 3 "register_operand" "0"))
	  (match_dup 3)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512F"
  "vfmadd231<ssemodesuffix>\t{<round_op5>%2, %1, %0%{%4%}|%0%{%4%}, %1, %2<round_op5>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma_fmsub_<mode>"
  [(set (match_operand:FMAMODE 0 "register_operand" "=v,v,v,x,x")
	(fma:FMAMODE
	  (match_operand:FMAMODE   1 "nonimmediate_operand" "%0,0,v,x,x")
	  (match_operand:FMAMODE   2 "nonimmediate_operand" "vm,v,vm,x,m")
	  (neg:FMAMODE
	    (match_operand:FMAMODE 3 "nonimmediate_operand" "v,vm,0,xm,x"))))]
  "TARGET_FMA || TARGET_FMA4"
  "@
   vfmsub132<ssemodesuffix>\t{%2, %3, %0|%0, %3, %2}
   vfmsub213<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   vfmsub231<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}
   vfmsub<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}
   vfmsub<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "isa" "fma,fma,fma,fma4,fma4")
   (set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<sd_mask_codefor>fma_fmsub_<mode><sd_maskz_name><round_name>"
  [(set (match_operand:VF_SF_AVX512VL 0 "register_operand" "=v,v,v")
	(fma:VF_SF_AVX512VL
	  (match_operand:VF_SF_AVX512VL   1 "<round_nimm_predicate>" "%0,0,v")
	  (match_operand:VF_SF_AVX512VL   2 "<round_nimm_predicate>" "<round_constraint>,v,<round_constraint>")
	  (neg:VF_SF_AVX512VL
	    (match_operand:VF_SF_AVX512VL 3 "<round_nimm_predicate>" "v,<round_constraint>,0"))))]
  "TARGET_AVX512F && <sd_mask_mode512bit_condition> && <round_mode512bit_condition>"
  "@
   vfmsub132<ssemodesuffix>\t{<round_sd_mask_op4>%2, %3, %0<sd_mask_op4>|%0<sd_mask_op4>, %3, %2<round_sd_mask_op4>}
   vfmsub213<ssemodesuffix>\t{<round_sd_mask_op4>%3, %2, %0<sd_mask_op4>|%0<sd_mask_op4>, %2, %3<round_sd_mask_op4>}
   vfmsub231<ssemodesuffix>\t{<round_sd_mask_op4>%2, %1, %0<sd_mask_op4>|%0<sd_mask_op4>, %1, %2<round_sd_mask_op4>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<avx512>_fmsub_<mode>_mask<round_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v,v")
	(vec_merge:VF_AVX512VL
	  (fma:VF_AVX512VL
	    (match_operand:VF_AVX512VL 1 "register_operand" "0,0")
	    (match_operand:VF_AVX512VL 2 "nonimmediate_operand" "<round_constraint>,v")
	    (neg:VF_AVX512VL
	      (match_operand:VF_AVX512VL 3 "nonimmediate_operand" "v,<round_constraint>")))
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk,Yk")))]
  "TARGET_AVX512F"
  "@
   vfmsub132<ssemodesuffix>\t{<round_op5>%2, %3, %0%{%4%}|%0%{%4%}, %3, %2<round_op5>}
   vfmsub213<ssemodesuffix>\t{<round_op5>%3, %2, %0%{%4%}|%0%{%4%}, %2, %3<round_op5>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<avx512>_fmsub_<mode>_mask3<round_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VF_AVX512VL
	  (fma:VF_AVX512VL
	    (match_operand:VF_AVX512VL 1 "register_operand" "v")
	    (match_operand:VF_AVX512VL 2 "nonimmediate_operand" "<round_constraint>")
	    (neg:VF_AVX512VL
	      (match_operand:VF_AVX512VL 3 "register_operand" "0")))
	  (match_dup 3)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512F && <round_mode512bit_condition>"
  "vfmsub231<ssemodesuffix>\t{<round_op5>%2, %1, %0%{%4%}|%0%{%4%}, %1, %2<round_op5>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma_fnmadd_<mode>"
  [(set (match_operand:FMAMODE 0 "register_operand" "=v,v,v,x,x")
	(fma:FMAMODE
	  (neg:FMAMODE
	    (match_operand:FMAMODE 1 "nonimmediate_operand" "%0,0,v,x,x"))
	  (match_operand:FMAMODE   2 "nonimmediate_operand" "vm,v,vm,x,m")
	  (match_operand:FMAMODE   3 "nonimmediate_operand" "v,vm,0,xm,x")))]
  "TARGET_FMA || TARGET_FMA4"
  "@
   vfnmadd132<ssemodesuffix>\t{%2, %3, %0|%0, %3, %2}
   vfnmadd213<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   vfnmadd231<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}
   vfnmadd<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}
   vfnmadd<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "isa" "fma,fma,fma,fma4,fma4")
   (set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<sd_mask_codefor>fma_fnmadd_<mode><sd_maskz_name><round_name>"
  [(set (match_operand:VF_SF_AVX512VL 0 "register_operand" "=v,v,v")
	(fma:VF_SF_AVX512VL
	  (neg:VF_SF_AVX512VL
	    (match_operand:VF_SF_AVX512VL 1 "<round_nimm_predicate>" "%0,0,v"))
	  (match_operand:VF_SF_AVX512VL   2 "<round_nimm_predicate>" "<round_constraint>,v,<round_constraint>")
	  (match_operand:VF_SF_AVX512VL   3 "<round_nimm_predicate>" "v,<round_constraint>,0")))]
  "TARGET_AVX512F && <sd_mask_mode512bit_condition> && <round_mode512bit_condition>"
  "@
   vfnmadd132<ssemodesuffix>\t{<round_sd_mask_op4>%2, %3, %0<sd_mask_op4>|%0<sd_mask_op4>, %3, %2<round_sd_mask_op4>}
   vfnmadd213<ssemodesuffix>\t{<round_sd_mask_op4>%3, %2, %0<sd_mask_op4>|%0<sd_mask_op4>, %2, %3<round_sd_mask_op4>}
   vfnmadd231<ssemodesuffix>\t{<round_sd_mask_op4>%2, %1, %0<sd_mask_op4>|%0<sd_mask_op4>, %1, %2<round_sd_mask_op4>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<avx512>_fnmadd_<mode>_mask<round_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v,v")
	(vec_merge:VF_AVX512VL
	  (fma:VF_AVX512VL
	    (neg:VF_AVX512VL
	      (match_operand:VF_AVX512VL 1 "register_operand" "0,0"))
	    (match_operand:VF_AVX512VL 2 "nonimmediate_operand" "<round_constraint>,v")
	    (match_operand:VF_AVX512VL 3 "nonimmediate_operand" "v,<round_constraint>"))
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk,Yk")))]
  "TARGET_AVX512F && <round_mode512bit_condition>"
  "@
   vfnmadd132<ssemodesuffix>\t{<round_op5>%2, %3, %0%{%4%}|%0%{%4%}, %3, %2<round_op5>}
   vfnmadd213<ssemodesuffix>\t{<round_op5>%3, %2, %0%{%4%}|%0%{%4%}, %2, %3<round_op5>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<avx512>_fnmadd_<mode>_mask3<round_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VF_AVX512VL
	  (fma:VF_AVX512VL
	    (neg:VF_AVX512VL
	      (match_operand:VF_AVX512VL 1 "register_operand" "v"))
	    (match_operand:VF_AVX512VL 2 "nonimmediate_operand" "<round_constraint>")
	    (match_operand:VF_AVX512VL 3 "register_operand" "0"))
	  (match_dup 3)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512F && <round_mode512bit_condition>"
  "vfnmadd231<ssemodesuffix>\t{<round_op5>%2, %1, %0%{%4%}|%0%{%4%}, %1, %2<round_op5>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma_fnmsub_<mode>"
  [(set (match_operand:FMAMODE 0 "register_operand" "=v,v,v,x,x")
	(fma:FMAMODE
	  (neg:FMAMODE
	    (match_operand:FMAMODE 1 "nonimmediate_operand" "%0,0,v,x,x"))
	  (match_operand:FMAMODE   2 "nonimmediate_operand" "vm,v,vm,x,m")
	  (neg:FMAMODE
	    (match_operand:FMAMODE 3 "nonimmediate_operand" "v,vm,0,xm,x"))))]
  "TARGET_FMA || TARGET_FMA4"
  "@
   vfnmsub132<ssemodesuffix>\t{<round_sd_mask_op4>%2, %3, %0<sd_mask_op4>|%0<sd_mask_op4>, %3, %2<round_sd_mask_op4>}
   vfnmsub213<ssemodesuffix>\t{<round_sd_mask_op4>%3, %2, %0<sd_mask_op4>|%0<sd_mask_op4>, %2, %3<round_sd_mask_op4>}
   vfnmsub231<ssemodesuffix>\t{<round_sd_mask_op4>%2, %1, %0<sd_mask_op4>|%0<sd_mask_op4>, %1, %2<round_sd_mask_op4>}
   vfnmsub<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}
   vfnmsub<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "isa" "fma,fma,fma,fma4,fma4")
   (set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<sd_mask_codefor>fma_fnmsub_<mode><sd_maskz_name><round_name>"
  [(set (match_operand:VF_SF_AVX512VL 0 "register_operand" "=v,v,v")
	(fma:VF_SF_AVX512VL
	  (neg:VF_SF_AVX512VL
	    (match_operand:VF_SF_AVX512VL 1 "<round_nimm_predicate>" "%0,0,v"))
	  (match_operand:VF_SF_AVX512VL 2 "<round_nimm_predicate>" "<round_constraint>,v,<round_constraint>")
	  (neg:VF_SF_AVX512VL
	    (match_operand:VF_SF_AVX512VL 3 "<round_nimm_predicate>" "v,<round_constraint>,0"))))]
  "TARGET_AVX512F && <sd_mask_mode512bit_condition> && <round_mode512bit_condition>"
  "@
   vfnmsub132<ssemodesuffix>\t{<round_sd_mask_op4>%2, %3, %0<sd_mask_op4>|%0<sd_mask_op4>, %3, %2<round_sd_mask_op4>}
   vfnmsub213<ssemodesuffix>\t{<round_sd_mask_op4>%3, %2, %0<sd_mask_op4>|%0<sd_mask_op4>, %2, %3<round_sd_mask_op4>}
   vfnmsub231<ssemodesuffix>\t{<round_sd_mask_op4>%2, %1, %0<sd_mask_op4>|%0<sd_mask_op4>, %1, %2<round_sd_mask_op4>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<avx512>_fnmsub_<mode>_mask<round_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v,v")
	(vec_merge:VF_AVX512VL
	  (fma:VF_AVX512VL
	    (neg:VF_AVX512VL
	      (match_operand:VF_AVX512VL 1 "register_operand" "0,0"))
	    (match_operand:VF_AVX512VL 2 "nonimmediate_operand" "<round_constraint>,v")
	    (neg:VF_AVX512VL
	      (match_operand:VF_AVX512VL 3 "nonimmediate_operand" "v,<round_constraint>")))
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk,Yk")))]
  "TARGET_AVX512F && <round_mode512bit_condition>"
  "@
   vfnmsub132<ssemodesuffix>\t{<round_op5>%2, %3, %0%{%4%}|%0%{%4%}, %3, %2<round_op5>}
   vfnmsub213<ssemodesuffix>\t{<round_op5>%3, %2, %0%{%4%}|%0%{%4%}, %2, %3<round_op5>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<avx512>_fnmsub_<mode>_mask3<round_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VF_AVX512VL
	  (fma:VF_AVX512VL
	    (neg:VF_AVX512VL
	      (match_operand:VF_AVX512VL 1 "register_operand" "v"))
	    (match_operand:VF_AVX512VL 2 "nonimmediate_operand" "<round_constraint>")
	    (neg:VF_AVX512VL
	      (match_operand:VF_AVX512VL 3 "register_operand" "0")))
	  (match_dup 3)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512F"
  "vfnmsub231<ssemodesuffix>\t{<round_op5>%2, %1, %0%{%4%}|%0%{%4%}, %1, %2<round_op5>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

;; FMA parallel floating point multiply addsub and subadd operations.

;; It would be possible to represent these without the UNSPEC as
;;
;; (vec_merge
;;   (fma op1 op2 op3)
;;   (fma op1 op2 (neg op3))
;;   (merge-const))
;;
;; But this doesn't seem useful in practice.

(define_expand "fmaddsub_<mode>"
  [(set (match_operand:VF 0 "register_operand")
	(unspec:VF
	  [(match_operand:VF 1 "nonimmediate_operand")
	   (match_operand:VF 2 "nonimmediate_operand")
	   (match_operand:VF 3 "nonimmediate_operand")]
	  UNSPEC_FMADDSUB))]
  "TARGET_FMA || TARGET_FMA4 || TARGET_AVX512F")

(define_expand "<avx512>_fmaddsub_<mode>_maskz<round_expand_name>"
  [(match_operand:VF_AVX512VL 0 "register_operand")
   (match_operand:VF_AVX512VL 1 "<round_expand_nimm_predicate>")
   (match_operand:VF_AVX512VL 2 "<round_expand_nimm_predicate>")
   (match_operand:VF_AVX512VL 3 "<round_expand_nimm_predicate>")
   (match_operand:<avx512fmaskmode> 4 "register_operand")]
  "TARGET_AVX512F"
{
  emit_insn (gen_fma_fmaddsub_<mode>_maskz_1<round_expand_name> (
    operands[0], operands[1], operands[2], operands[3],
    CONST0_RTX (<MODE>mode), operands[4]<round_expand_operand>));
  DONE;
})

(define_insn "*fma_fmaddsub_<mode>"
  [(set (match_operand:VF_128_256 0 "register_operand" "=v,v,v,x,x")
	(unspec:VF_128_256
	  [(match_operand:VF_128_256 1 "nonimmediate_operand" "%0,0,v,x,x")
	   (match_operand:VF_128_256 2 "nonimmediate_operand" "vm,v,vm,x,m")
	   (match_operand:VF_128_256 3 "nonimmediate_operand" "v,vm,0,xm,x")]
	  UNSPEC_FMADDSUB))]
  "TARGET_FMA || TARGET_FMA4"
  "@
   vfmaddsub132<ssemodesuffix>\t{%2, %3, %0|%0, %3, %2}
   vfmaddsub213<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   vfmaddsub231<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}
   vfmaddsub<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}
   vfmaddsub<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "isa" "fma,fma,fma,fma4,fma4")
   (set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<sd_mask_codefor>fma_fmaddsub_<mode><sd_maskz_name><round_name>"
  [(set (match_operand:VF_SF_AVX512VL 0 "register_operand" "=v,v,v")
	(unspec:VF_SF_AVX512VL
	  [(match_operand:VF_SF_AVX512VL 1 "<round_nimm_predicate>" "%0,0,v")
	   (match_operand:VF_SF_AVX512VL 2 "<round_nimm_predicate>" "<round_constraint>,v,<round_constraint>")
	   (match_operand:VF_SF_AVX512VL 3 "<round_nimm_predicate>" "v,<round_constraint>,0")]
	  UNSPEC_FMADDSUB))]
  "TARGET_AVX512F && <sd_mask_mode512bit_condition> && <round_mode512bit_condition>"
  "@
   vfmaddsub132<ssemodesuffix>\t{<round_sd_mask_op4>%2, %3, %0<sd_mask_op4>|%0<sd_mask_op4>, %3, %2<round_sd_mask_op4>}
   vfmaddsub213<ssemodesuffix>\t{<round_sd_mask_op4>%3, %2, %0<sd_mask_op4>|%0<sd_mask_op4>, %2, %3<round_sd_mask_op4>}
   vfmaddsub231<ssemodesuffix>\t{<round_sd_mask_op4>%2, %1, %0<sd_mask_op4>|%0<sd_mask_op4>, %1, %2<round_sd_mask_op4>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<avx512>_fmaddsub_<mode>_mask<round_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v,v")
	(vec_merge:VF_AVX512VL
	  (unspec:VF_AVX512VL
	    [(match_operand:VF_AVX512VL 1 "register_operand" "0,0")
	     (match_operand:VF_AVX512VL 2 "nonimmediate_operand" "<round_constraint>,v")
	     (match_operand:VF_AVX512VL 3 "nonimmediate_operand" "v,<round_constraint>")]
	    UNSPEC_FMADDSUB)
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk,Yk")))]
  "TARGET_AVX512F"
  "@
   vfmaddsub132<ssemodesuffix>\t{<round_op5>%2, %3, %0%{%4%}|%0%{%4%}, %3, %2<round_op5>}
   vfmaddsub213<ssemodesuffix>\t{<round_op5>%3, %2, %0%{%4%}|%0%{%4%}, %2, %3<round_op5>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<avx512>_fmaddsub_<mode>_mask3<round_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VF_AVX512VL
	  (unspec:VF_AVX512VL
	    [(match_operand:VF_AVX512VL 1 "register_operand" "v")
	     (match_operand:VF_AVX512VL 2 "nonimmediate_operand" "<round_constraint>")
	     (match_operand:VF_AVX512VL 3 "register_operand" "0")]
	    UNSPEC_FMADDSUB)
	  (match_dup 3)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512F"
  "vfmaddsub231<ssemodesuffix>\t{<round_op5>%2, %1, %0%{%4%}|%0%{%4%}, %1, %2<round_op5>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma_fmsubadd_<mode>"
  [(set (match_operand:VF_128_256 0 "register_operand" "=v,v,v,x,x")
	(unspec:VF_128_256
	  [(match_operand:VF_128_256   1 "nonimmediate_operand" "%0,0,v,x,x")
	   (match_operand:VF_128_256   2 "nonimmediate_operand" "vm,v,vm,x,m")
	   (neg:VF_128_256
	     (match_operand:VF_128_256 3 "nonimmediate_operand" "v,vm,0,xm,x"))]
	  UNSPEC_FMADDSUB))]
  "TARGET_FMA || TARGET_FMA4"
  "@
   vfmsubadd132<ssemodesuffix>\t{%2, %3, %0|%0, %3, %2}
   vfmsubadd213<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   vfmsubadd231<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}
   vfmsubadd<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}
   vfmsubadd<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "isa" "fma,fma,fma,fma4,fma4")
   (set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<sd_mask_codefor>fma_fmsubadd_<mode><sd_maskz_name><round_name>"
  [(set (match_operand:VF_SF_AVX512VL 0 "register_operand" "=v,v,v")
	(unspec:VF_SF_AVX512VL
	  [(match_operand:VF_SF_AVX512VL   1 "<round_nimm_predicate>" "%0,0,v")
	   (match_operand:VF_SF_AVX512VL   2 "<round_nimm_predicate>" "<round_constraint>,v,<round_constraint>")
	   (neg:VF_SF_AVX512VL
	     (match_operand:VF_SF_AVX512VL 3 "<round_nimm_predicate>" "v,<round_constraint>,0"))]
	  UNSPEC_FMADDSUB))]
  "TARGET_AVX512F && <sd_mask_mode512bit_condition> && <round_mode512bit_condition>"
  "@
   vfmsubadd132<ssemodesuffix>\t{<round_sd_mask_op4>%2, %3, %0<sd_mask_op4>|%0<sd_mask_op4>, %3, %2<round_sd_mask_op4>}
   vfmsubadd213<ssemodesuffix>\t{<round_sd_mask_op4>%3, %2, %0<sd_mask_op4>|%0<sd_mask_op4>, %2, %3<round_sd_mask_op4>}
   vfmsubadd231<ssemodesuffix>\t{<round_sd_mask_op4>%2, %1, %0<sd_mask_op4>|%0<sd_mask_op4>, %1, %2<round_sd_mask_op4>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<avx512>_fmsubadd_<mode>_mask<round_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v,v")
	(vec_merge:VF_AVX512VL
	  (unspec:VF_AVX512VL
	    [(match_operand:VF_AVX512VL 1 "register_operand" "0,0")
	     (match_operand:VF_AVX512VL 2 "nonimmediate_operand" "<round_constraint>,v")
	     (neg:VF_AVX512VL
	       (match_operand:VF_AVX512VL 3 "nonimmediate_operand" "v,<round_constraint>"))]
	    UNSPEC_FMADDSUB)
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk,Yk")))]
  "TARGET_AVX512F"
  "@
   vfmsubadd132<ssemodesuffix>\t{<round_op5>%2, %3, %0%{%4%}|%0%{%4%}, %3, %2<round_op5>}
   vfmsubadd213<ssemodesuffix>\t{<round_op5>%3, %2, %0%{%4%}|%0%{%4%}, %2, %3<round_op5>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "<avx512>_fmsubadd_<mode>_mask3<round_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VF_AVX512VL
	  (unspec:VF_AVX512VL
	    [(match_operand:VF_AVX512VL 1 "register_operand" "v")
	     (match_operand:VF_AVX512VL 2 "nonimmediate_operand" "<round_constraint>")
	     (neg:VF_AVX512VL
	       (match_operand:VF_AVX512VL 3 "register_operand" "0"))]
	    UNSPEC_FMADDSUB)
	  (match_dup 3)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512F"
  "vfmsubadd231<ssemodesuffix>\t{<round_op5>%2, %1, %0%{%4%}|%0%{%4%}, %1, %2<round_op5>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

;; FMA3 floating point scalar intrinsics. These merge result with
;; high-order elements from the destination register.

(define_expand "fmai_vmfmadd_<mode><round_name>"
  [(set (match_operand:VF_128 0 "register_operand")
	(vec_merge:VF_128
	  (fma:VF_128
	    (match_operand:VF_128 1 "<round_nimm_predicate>")
	    (match_operand:VF_128 2 "<round_nimm_predicate>")
	    (match_operand:VF_128 3 "<round_nimm_predicate>"))
	  (match_dup 1)
	  (const_int 1)))]
  "TARGET_FMA")

(define_insn "*fmai_fmadd_<mode>"
  [(set (match_operand:VF_128 0 "register_operand" "=v,v")
        (vec_merge:VF_128
	  (fma:VF_128
	    (match_operand:VF_128 1 "<round_nimm_predicate>" " 0, 0")
	    (match_operand:VF_128 2 "<round_nimm_predicate>" "<round_constraint>, v")
	    (match_operand:VF_128 3 "<round_nimm_predicate>" " v,<round_constraint>"))
	  (match_dup 1)
	  (const_int 1)))]
  "TARGET_FMA || TARGET_AVX512F"
  "@
   vfmadd132<ssescalarmodesuffix>\t{<round_op4>%2, %3, %0|%0, %<iptr>3, %<iptr>2<round_op4>}
   vfmadd213<ssescalarmodesuffix>\t{<round_op4>%3, %2, %0|%0, %<iptr>2, %<iptr>3<round_op4>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fmai_fmsub_<mode>"
  [(set (match_operand:VF_128 0 "register_operand" "=v,v")
        (vec_merge:VF_128
	  (fma:VF_128
	    (match_operand:VF_128   1 "<round_nimm_predicate>" "0,0")
	    (match_operand:VF_128   2 "<round_nimm_predicate>" "<round_constraint>,v")
	    (neg:VF_128
	      (match_operand:VF_128 3 "<round_nimm_predicate>" " v,<round_constraint>")))
	  (match_dup 1)
	  (const_int 1)))]
  "TARGET_FMA || TARGET_AVX512F"
  "@
   vfmsub132<ssescalarmodesuffix>\t{<round_op4>%2, %3, %0|%0, %<iptr>3, %<iptr>2<round_op4>}
   vfmsub213<ssescalarmodesuffix>\t{<round_op4>%3, %2, %0|%0, %<iptr>2, %<iptr>3<round_op4>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fmai_fnmadd_<mode><round_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=v,v")
        (vec_merge:VF_128
	  (fma:VF_128
	    (neg:VF_128
	      (match_operand:VF_128 2 "<round_nimm_predicate>" "<round_constraint>,v"))
	    (match_operand:VF_128   1 "<round_nimm_predicate>" "0,0")
	    (match_operand:VF_128   3 "<round_nimm_predicate>" "v,<round_constraint>"))
	  (match_dup 1)
	  (const_int 1)))]
  "TARGET_FMA || TARGET_AVX512F"
  "@
   vfnmadd132<ssescalarmodesuffix>\t{<round_op4>%2, %3, %0|%0, %<iptr>3, %<iptr>2<round_op4>}
   vfnmadd213<ssescalarmodesuffix>\t{<round_op4>%3, %2, %0|%0, %<iptr>2, %<iptr>3<round_op4>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fmai_fnmsub_<mode><round_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=v,v")
        (vec_merge:VF_128
	  (fma:VF_128
	    (neg:VF_128
	      (match_operand:VF_128 2 "<round_nimm_predicate>" "<round_constraint>, v"))
	    (match_operand:VF_128   1 "<round_nimm_predicate>" " 0, 0")
	    (neg:VF_128
	      (match_operand:VF_128 3 "<round_nimm_predicate>" " v,<round_constraint>")))
	  (match_dup 1)
	  (const_int 1)))]
  "TARGET_FMA || TARGET_AVX512F"
  "@
   vfnmsub132<ssescalarmodesuffix>\t{<round_op4>%2, %3, %0|%0, %<iptr>3, %<iptr>2<round_op4>}
   vfnmsub213<ssescalarmodesuffix>\t{<round_op4>%3, %2, %0|%0, %<iptr>2, %<iptr>3<round_op4>}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

;; FMA4 floating point scalar intrinsics.  These write the
;; entire destination register, with the high-order elements zeroed.

(define_expand "fma4i_vmfmadd_<mode>"
  [(set (match_operand:VF_128 0 "register_operand")
	(vec_merge:VF_128
	  (fma:VF_128
	    (match_operand:VF_128 1 "nonimmediate_operand")
	    (match_operand:VF_128 2 "nonimmediate_operand")
	    (match_operand:VF_128 3 "nonimmediate_operand"))
	  (match_dup 4)
	  (const_int 1)))]
  "TARGET_FMA4"
  "operands[4] = CONST0_RTX (<MODE>mode);")

(define_insn "*fma4i_vmfmadd_<mode>"
  [(set (match_operand:VF_128 0 "register_operand" "=x,x")
	(vec_merge:VF_128
	  (fma:VF_128
	    (match_operand:VF_128 1 "nonimmediate_operand" "%x,x")
	    (match_operand:VF_128 2 "nonimmediate_operand" " x,m")
	    (match_operand:VF_128 3 "nonimmediate_operand" "xm,x"))
	  (match_operand:VF_128 4 "const0_operand")
	  (const_int 1)))]
  "TARGET_FMA4"
  "vfmadd<ssescalarmodesuffix>\t{%3, %2, %1, %0|%0, %1, %<iptr>2, %<iptr>3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma4i_vmfmsub_<mode>"
  [(set (match_operand:VF_128 0 "register_operand" "=x,x")
	(vec_merge:VF_128
	  (fma:VF_128
	    (match_operand:VF_128 1 "nonimmediate_operand" "%x,x")
	    (match_operand:VF_128 2 "nonimmediate_operand" " x,m")
	    (neg:VF_128
	      (match_operand:VF_128 3 "nonimmediate_operand" "xm,x")))
	  (match_operand:VF_128 4 "const0_operand")
	  (const_int 1)))]
  "TARGET_FMA4"
  "vfmsub<ssescalarmodesuffix>\t{%3, %2, %1, %0|%0, %1, %<iptr>2, %<iptr>3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma4i_vmfnmadd_<mode>"
  [(set (match_operand:VF_128 0 "register_operand" "=x,x")
	(vec_merge:VF_128
	  (fma:VF_128
	    (neg:VF_128
	      (match_operand:VF_128 1 "nonimmediate_operand" "%x,x"))
	    (match_operand:VF_128   2 "nonimmediate_operand" " x,m")
	    (match_operand:VF_128   3 "nonimmediate_operand" "xm,x"))
	  (match_operand:VF_128 4 "const0_operand")
	  (const_int 1)))]
  "TARGET_FMA4"
  "vfnmadd<ssescalarmodesuffix>\t{%3, %2, %1, %0|%0, %1, %<iptr>2, %<iptr>3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma4i_vmfnmsub_<mode>"
  [(set (match_operand:VF_128 0 "register_operand" "=x,x")
	(vec_merge:VF_128
	  (fma:VF_128
	    (neg:VF_128
	      (match_operand:VF_128 1 "nonimmediate_operand" "%x,x"))
	    (match_operand:VF_128   2 "nonimmediate_operand" " x,m")
	    (neg:VF_128
	      (match_operand:VF_128   3 "nonimmediate_operand" "xm,x")))
	  (match_operand:VF_128 4 "const0_operand")
	  (const_int 1)))]
  "TARGET_FMA4"
  "vfnmsub<ssescalarmodesuffix>\t{%3, %2, %1, %0|%0, %1, %<iptr>2, %<iptr>3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel single-precision floating point conversion operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "sse_cvtpi2ps"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_merge:V4SF
	  (vec_duplicate:V4SF
	    (float:V2SF (match_operand:V2SI 2 "nonimmediate_operand" "ym")))
	  (match_operand:V4SF 1 "register_operand" "0")
	  (const_int 3)))]
  "TARGET_SSE"
  "cvtpi2ps\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssecvt")
   (set_attr "mode" "V4SF")])

(define_insn "sse_cvtps2pi"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(vec_select:V2SI
	  (unspec:V4SI [(match_operand:V4SF 1 "nonimmediate_operand" "xm")]
		       UNSPEC_FIX_NOTRUNC)
	  (parallel [(const_int 0) (const_int 1)])))]
  "TARGET_SSE"
  "cvtps2pi\t{%1, %0|%0, %q1}"
  [(set_attr "type" "ssecvt")
   (set_attr "unit" "mmx")
   (set_attr "mode" "DI")])

(define_insn "sse_cvttps2pi"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(vec_select:V2SI
	  (fix:V4SI (match_operand:V4SF 1 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0) (const_int 1)])))]
  "TARGET_SSE"
  "cvttps2pi\t{%1, %0|%0, %q1}"
  [(set_attr "type" "ssecvt")
   (set_attr "unit" "mmx")
   (set_attr "prefix_rep" "0")
   (set_attr "mode" "SF")])

(define_insn "sse_cvtsi2ss<rex64namesuffix><round_name>"
  [(set (match_operand:V4SF 0 "register_operand" "=x,x,v")
	(vec_merge:V4SF
	  (vec_duplicate:V4SF
	    (float:SF (match_operand:SWI48 2 "<round_nimm_scalar_predicate>" "r,m,<round_constraint3>")))
	  (match_operand:V4SF 1 "register_operand" "0,0,v")
	  (const_int 1)))]
  "TARGET_SSE"
  "@
   cvtsi2ss<rex64suffix>\t{%2, %0|%0, %2}
   cvtsi2ss<rex64suffix>\t{%2, %0|%0, %2}
   vcvtsi2ss<rex64suffix>\t{%2, <round_op3>%1, %0|%0, %1<round_op3>, %2}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "vector,double,*")
   (set_attr "amdfam10_decode" "vector,double,*")
   (set_attr "bdver1_decode" "double,direct,*")
   (set_attr "btver2_decode" "double,double,double")
   (set_attr "znver1_decode" "double,double,double")
   (set (attr "length_vex")
	(if_then_else
	  (and (match_test "<MODE>mode == DImode")
	       (eq_attr "alternative" "2"))
	  (const_string "4")
	  (const_string "*")))
   (set (attr "prefix_rex")
	(if_then_else
	  (and (match_test "<MODE>mode == DImode")
	       (eq_attr "alternative" "0,1"))
	  (const_string "1")
	  (const_string "*")))
   (set_attr "prefix" "orig,orig,maybe_evex")
   (set_attr "mode" "SF")])

(define_insn "sse_cvtss2si<rex64namesuffix><round_name>"
  [(set (match_operand:SWI48 0 "register_operand" "=r,r")
	(unspec:SWI48
	  [(vec_select:SF
	     (match_operand:V4SF 1 "<round_nimm_scalar_predicate>" "v,<round_constraint2>")
	     (parallel [(const_int 0)]))]
	  UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE"
  "%vcvtss2si<rex64suffix>\t{<round_op2>%1, %0|%0, %k1<round_op2>}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

(define_insn "sse_cvtss2si<rex64namesuffix>_2"
  [(set (match_operand:SWI48 0 "register_operand" "=r,r")
	(unspec:SWI48 [(match_operand:SF 1 "nonimmediate_operand" "v,m")]
		      UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE"
  "%vcvtss2si<rex64suffix>\t{%1, %0|%0, %k1}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "amdfam10_decode" "double,double")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

(define_insn "sse_cvttss2si<rex64namesuffix><round_saeonly_name>"
  [(set (match_operand:SWI48 0 "register_operand" "=r,r")
	(fix:SWI48
	  (vec_select:SF
	    (match_operand:V4SF 1 "<round_saeonly_nimm_scalar_predicate>" "v,<round_saeonly_constraint>")
	    (parallel [(const_int 0)]))))]
  "TARGET_SSE"
  "%vcvttss2si<rex64suffix>\t{<round_saeonly_op2>%1, %0|%0, %k1<round_saeonly_op2>}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "amdfam10_decode" "double,double")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

(define_insn "cvtusi2<ssescalarmodesuffix>32<round_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
	  (vec_duplicate:VF_128
	    (unsigned_float:<ssescalarmode>
	      (match_operand:SI 2 "<round_nimm_predicate>" "<round_constraint3>")))
	  (match_operand:VF_128 1 "register_operand" "v")
	  (const_int 1)))]
  "TARGET_AVX512F && <round_modev4sf_condition>"
  "vcvtusi2<ssescalarmodesuffix>\t{%2, <round_op3>%1, %0|%0, %1<round_op3>, %2}"
  [(set_attr "type" "sseicvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "cvtusi2<ssescalarmodesuffix>64<round_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
	  (vec_duplicate:VF_128
	    (unsigned_float:<ssescalarmode>
	      (match_operand:DI 2 "<round_nimm_predicate>" "<round_constraint3>")))
	  (match_operand:VF_128 1 "register_operand" "v")
	  (const_int 1)))]
  "TARGET_AVX512F && TARGET_64BIT"
  "vcvtusi2<ssescalarmodesuffix>{q}\t{%2, <round_op3>%1, %0|%0, %1<round_op3>, %2}"
  [(set_attr "type" "sseicvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "float<sseintvecmodelower><mode>2<mask_name><round_name>"
  [(set (match_operand:VF1 0 "register_operand" "=x,v")
	(float:VF1
	  (match_operand:<sseintvecmode> 1 "<round_nimm_predicate>" "xBm,<round_constraint>")))]
  "TARGET_SSE2 && <mask_mode512bit_condition> && <round_mode512bit_condition>"
  "@
   cvtdq2ps\t{%1, %0|%0, %1}
   vcvtdq2ps\t{<round_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_mask_op2>}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "ssecvt")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "ufloat<sseintvecmodelower><mode>2<mask_name><round_name>"
  [(set (match_operand:VF1_AVX512VL 0 "register_operand" "=v")
	(unsigned_float:VF1_AVX512VL
	  (match_operand:<sseintvecmode> 1 "nonimmediate_operand" "<round_constraint>")))]
  "TARGET_AVX512F"
  "vcvtudq2ps\t{<round_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_expand "floatuns<sseintvecmodelower><mode>2"
  [(match_operand:VF1 0 "register_operand")
   (match_operand:<sseintvecmode> 1 "register_operand")]
  "TARGET_SSE2 && (<MODE>mode == V4SFmode || TARGET_AVX2)"
{
  if (<MODE>mode == V16SFmode)
    emit_insn (gen_ufloatv16siv16sf2 (operands[0], operands[1]));
  else
    if (TARGET_AVX512VL)
      {
	if (<MODE>mode == V4SFmode)
	  emit_insn (gen_ufloatv4siv4sf2 (operands[0], operands[1]));
	else
	  emit_insn (gen_ufloatv8siv8sf2 (operands[0], operands[1]));
      }
  else
    ix86_expand_vector_convert_uns_vsivsf (operands[0], operands[1]);

  DONE;
})


;; For <sse2_avx_avx512f>_fix_notrunc<sf2simodelower><mode> insn pattern
(define_mode_attr sf2simodelower
  [(V16SI "v16sf") (V8SI "v8sf") (V4SI "v4sf")])

(define_insn "<sse2_avx_avx512f>_fix_notrunc<sf2simodelower><mode><mask_name>"
  [(set (match_operand:VI4_AVX 0 "register_operand" "=v")
	(unspec:VI4_AVX
	  [(match_operand:<ssePSmode> 1 "vector_operand" "vBm")]
	  UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE2 && <mask_mode512bit_condition>"
  "%vcvtps2dq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssecvt")
   (set (attr "prefix_data16")
     (if_then_else
       (match_test "TARGET_AVX")
     (const_string "*")
     (const_string "1")))
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "avx512f_fix_notruncv16sfv16si<mask_name><round_name>"
  [(set (match_operand:V16SI 0 "register_operand" "=v")
	(unspec:V16SI
	  [(match_operand:V16SF 1 "<round_nimm_predicate>" "<round_constraint>")]
	  UNSPEC_FIX_NOTRUNC))]
  "TARGET_AVX512F"
  "vcvtps2dq\t{<round_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "<mask_codefor><avx512>_ufix_notrunc<sf2simodelower><mode><mask_name><round_name>"
  [(set (match_operand:VI4_AVX512VL 0 "register_operand" "=v")
	(unspec:VI4_AVX512VL
	  [(match_operand:<ssePSmode> 1 "nonimmediate_operand" "<round_constraint>")]
	  UNSPEC_UNSIGNED_FIX_NOTRUNC))]
  "TARGET_AVX512F"
  "vcvtps2udq\t{<round_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<mask_codefor>avx512dq_cvtps2qq<mode><mask_name><round_name>"
  [(set (match_operand:VI8_256_512 0 "register_operand" "=v")
	(unspec:VI8_256_512 [(match_operand:<ssePSmode2> 1 "nonimmediate_operand" "<round_constraint>")]
		     UNSPEC_FIX_NOTRUNC))]
  "TARGET_AVX512DQ && <round_mode512bit_condition>"
  "vcvtps2qq\t{<round_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<mask_codefor>avx512dq_cvtps2qqv2di<mask_name>"
  [(set (match_operand:V2DI 0 "register_operand" "=v")
	(unspec:V2DI
	  [(vec_select:V2SF
	     (match_operand:V4SF 1 "nonimmediate_operand" "vm")
	     (parallel [(const_int 0) (const_int 1)]))]
	  UNSPEC_FIX_NOTRUNC))]
  "TARGET_AVX512DQ && TARGET_AVX512VL"
  "vcvtps2qq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %q1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "<mask_codefor>avx512dq_cvtps2uqq<mode><mask_name><round_name>"
  [(set (match_operand:VI8_256_512 0 "register_operand" "=v")
	(unspec:VI8_256_512 [(match_operand:<ssePSmode2> 1 "nonimmediate_operand" "<round_constraint>")]
		     UNSPEC_UNSIGNED_FIX_NOTRUNC))]
  "TARGET_AVX512DQ && <round_mode512bit_condition>"
  "vcvtps2uqq\t{<round_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<mask_codefor>avx512dq_cvtps2uqqv2di<mask_name>"
  [(set (match_operand:V2DI 0 "register_operand" "=v")
	(unspec:V2DI
	  [(vec_select:V2SF
	     (match_operand:V4SF 1 "nonimmediate_operand" "vm")
	     (parallel [(const_int 0) (const_int 1)]))]
	  UNSPEC_UNSIGNED_FIX_NOTRUNC))]
  "TARGET_AVX512DQ && TARGET_AVX512VL"
  "vcvtps2uqq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %q1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "<fixsuffix>fix_truncv16sfv16si2<mask_name><round_saeonly_name>"
  [(set (match_operand:V16SI 0 "register_operand" "=v")
	(any_fix:V16SI
	  (match_operand:V16SF 1 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")))]
  "TARGET_AVX512F"
  "vcvttps2<fixsuffix>dq\t{<round_saeonly_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_saeonly_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "fix_truncv8sfv8si2<mask_name>"
  [(set (match_operand:V8SI 0 "register_operand" "=v")
	(fix:V8SI (match_operand:V8SF 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX && <mask_avx512vl_condition>"
  "vcvttps2dq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "<mask_prefix>")
   (set_attr "mode" "OI")])

(define_insn "fix_truncv4sfv4si2<mask_name>"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(fix:V4SI (match_operand:V4SF 1 "vector_operand" "vBm")))]
  "TARGET_SSE2 && <mask_avx512vl_condition>"
  "%vcvttps2dq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssecvt")
   (set (attr "prefix_rep")
     (if_then_else
       (match_test "TARGET_AVX")
     (const_string "*")
     (const_string "1")))
   (set (attr "prefix_data16")
     (if_then_else
       (match_test "TARGET_AVX")
     (const_string "*")
     (const_string "0")))
   (set_attr "prefix_data16" "0")
   (set_attr "prefix" "<mask_prefix2>")
   (set_attr "mode" "TI")])

(define_expand "fixuns_trunc<mode><sseintvecmodelower>2"
  [(match_operand:<sseintvecmode> 0 "register_operand")
   (match_operand:VF1 1 "register_operand")]
  "TARGET_SSE2"
{
  if (<MODE>mode == V16SFmode)
    emit_insn (gen_ufix_truncv16sfv16si2 (operands[0],
					  operands[1]));
  else
    {
      rtx tmp[3];
      tmp[0] = ix86_expand_adjust_ufix_to_sfix_si (operands[1], &tmp[2]);
      tmp[1] = gen_reg_rtx (<sseintvecmode>mode);
      emit_insn (gen_fix_trunc<mode><sseintvecmodelower>2 (tmp[1], tmp[0]));
      emit_insn (gen_xor<sseintvecmodelower>3 (operands[0], tmp[1], tmp[2]));
    }
  DONE;
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel double-precision floating point conversion operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "sse2_cvtpi2pd"
  [(set (match_operand:V2DF 0 "register_operand" "=x,x")
	(float:V2DF (match_operand:V2SI 1 "nonimmediate_operand" "y,m")))]
  "TARGET_SSE2"
  "cvtpi2pd\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "unit" "mmx,*")
   (set_attr "prefix_data16" "1,*")
   (set_attr "mode" "V2DF")])

(define_insn "sse2_cvtpd2pi"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(unspec:V2SI [(match_operand:V2DF 1 "nonimmediate_operand" "xm")]
		     UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE2"
  "cvtpd2pi\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "unit" "mmx")
   (set_attr "bdver1_decode" "double")
   (set_attr "btver2_decode" "direct")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "DI")])

(define_insn "sse2_cvttpd2pi"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(fix:V2SI (match_operand:V2DF 1 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2"
  "cvttpd2pi\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "unit" "mmx")
   (set_attr "bdver1_decode" "double")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "sse2_cvtsi2sd"
  [(set (match_operand:V2DF 0 "register_operand" "=x,x,v")
	(vec_merge:V2DF
	  (vec_duplicate:V2DF
	    (float:DF (match_operand:SI 2 "nonimmediate_operand" "r,m,rm")))
	  (match_operand:V2DF 1 "register_operand" "0,0,v")
	  (const_int 1)))]
  "TARGET_SSE2"
  "@
   cvtsi2sd\t{%2, %0|%0, %2}
   cvtsi2sd\t{%2, %0|%0, %2}
   vcvtsi2sd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,direct,*")
   (set_attr "amdfam10_decode" "vector,double,*")
   (set_attr "bdver1_decode" "double,direct,*")
   (set_attr "btver2_decode" "double,double,double")
   (set_attr "znver1_decode" "double,double,double")
   (set_attr "prefix" "orig,orig,maybe_evex")
   (set_attr "mode" "DF")])

(define_insn "sse2_cvtsi2sdq<round_name>"
  [(set (match_operand:V2DF 0 "register_operand" "=x,x,v")
	(vec_merge:V2DF
	  (vec_duplicate:V2DF
	    (float:DF (match_operand:DI 2 "<round_nimm_scalar_predicate>" "r,m,<round_constraint3>")))
	  (match_operand:V2DF 1 "register_operand" "0,0,v")
	  (const_int 1)))]
  "TARGET_SSE2 && TARGET_64BIT"
  "@
   cvtsi2sdq\t{%2, %0|%0, %2}
   cvtsi2sdq\t{%2, %0|%0, %2}
   vcvtsi2sdq\t{%2, <round_op3>%1, %0|%0, %1<round_op3>, %2}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,direct,*")
   (set_attr "amdfam10_decode" "vector,double,*")
   (set_attr "bdver1_decode" "double,direct,*")
   (set_attr "length_vex" "*,*,4")
   (set_attr "prefix_rex" "1,1,*")
   (set_attr "prefix" "orig,orig,maybe_evex")
   (set_attr "mode" "DF")])

(define_insn "avx512f_vcvtss2usi<rex64namesuffix><round_name>"
  [(set (match_operand:SWI48 0 "register_operand" "=r")
	(unspec:SWI48
	  [(vec_select:SF
	     (match_operand:V4SF 1 "<round_nimm_predicate>" "<round_constraint>")
	     (parallel [(const_int 0)]))]
	  UNSPEC_UNSIGNED_FIX_NOTRUNC))]
  "TARGET_AVX512F"
  "vcvtss2usi\t{<round_op2>%1, %0|%0, %k1<round_op2>}"
  [(set_attr "type" "sseicvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "avx512f_vcvttss2usi<rex64namesuffix><round_saeonly_name>"
  [(set (match_operand:SWI48 0 "register_operand" "=r")
	(unsigned_fix:SWI48
	  (vec_select:SF
	    (match_operand:V4SF 1 "<round_saeonly_nimm_scalar_predicate>" "<round_saeonly_constraint>")
	    (parallel [(const_int 0)]))))]
  "TARGET_AVX512F"
  "vcvttss2usi\t{<round_saeonly_op2>%1, %0|%0, %k1<round_saeonly_op2>}"
  [(set_attr "type" "sseicvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "avx512f_vcvtsd2usi<rex64namesuffix><round_name>"
  [(set (match_operand:SWI48 0 "register_operand" "=r")
	(unspec:SWI48
	  [(vec_select:DF
	     (match_operand:V2DF 1 "<round_nimm_predicate>" "<round_constraint>")
	     (parallel [(const_int 0)]))]
	  UNSPEC_UNSIGNED_FIX_NOTRUNC))]
  "TARGET_AVX512F"
  "vcvtsd2usi\t{<round_op2>%1, %0|%0, %q1<round_op2>}"
  [(set_attr "type" "sseicvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "avx512f_vcvttsd2usi<rex64namesuffix><round_saeonly_name>"
  [(set (match_operand:SWI48 0 "register_operand" "=r")
	(unsigned_fix:SWI48
	  (vec_select:DF
	    (match_operand:V2DF 1 "<round_saeonly_nimm_scalar_predicate>" "<round_saeonly_constraint>")
	    (parallel [(const_int 0)]))))]
  "TARGET_AVX512F"
  "vcvttsd2usi\t{<round_saeonly_op2>%1, %0|%0, %q1<round_saeonly_op2>}"
  [(set_attr "type" "sseicvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "sse2_cvtsd2si<rex64namesuffix><round_name>"
  [(set (match_operand:SWI48 0 "register_operand" "=r,r")
	(unspec:SWI48
	  [(vec_select:DF
	     (match_operand:V2DF 1 "<round_nimm_scalar_predicate>" "v,<round_constraint2>")
	     (parallel [(const_int 0)]))]
	  UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE2"
  "%vcvtsd2si<rex64suffix>\t{<round_op2>%1, %0|%0, %q1<round_op2>}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "btver2_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

(define_insn "sse2_cvtsd2si<rex64namesuffix>_2"
  [(set (match_operand:SWI48 0 "register_operand" "=r,r")
	(unspec:SWI48 [(match_operand:DF 1 "nonimmediate_operand" "v,m")]
		      UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE2"
  "%vcvtsd2si<rex64suffix>\t{%1, %0|%0, %q1}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "amdfam10_decode" "double,double")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

(define_insn "sse2_cvttsd2si<rex64namesuffix><round_saeonly_name>"
  [(set (match_operand:SWI48 0 "register_operand" "=r,r")
	(fix:SWI48
	  (vec_select:DF
	    (match_operand:V2DF 1 "<round_saeonly_nimm_scalar_predicate>" "v,<round_saeonly_constraint2>")
	    (parallel [(const_int 0)]))))]
  "TARGET_SSE2"
  "%vcvttsd2si<rex64suffix>\t{<round_saeonly_op2>%1, %0|%0, %q1<round_saeonly_op2>}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "amdfam10_decode" "double,double")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "btver2_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

;; For float<si2dfmode><mode>2 insn pattern
(define_mode_attr si2dfmode
  [(V8DF "V8SI") (V4DF "V4SI")])
(define_mode_attr si2dfmodelower
  [(V8DF "v8si") (V4DF "v4si")])

(define_insn "float<si2dfmodelower><mode>2<mask_name>"
  [(set (match_operand:VF2_512_256 0 "register_operand" "=v")
	(float:VF2_512_256 (match_operand:<si2dfmode> 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX && <mask_mode512bit_condition>"
  "vcvtdq2pd\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

(define_insn "float<floatunssuffix><sseintvecmodelower><mode>2<mask_name><round_name>"
  [(set (match_operand:VF2_AVX512VL 0 "register_operand" "=v")
	(any_float:VF2_AVX512VL
	  (match_operand:<sseintvecmode> 1 "nonimmediate_operand" "<round_constraint>")))]
  "TARGET_AVX512DQ"
  "vcvt<floatsuffix>qq2pd\t{<round_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

;; For float<floatunssuffix><sselondveclower><mode> insn patterns
(define_mode_attr qq2pssuff
  [(V8SF "") (V4SF "{y}")])

(define_mode_attr sselongvecmode
  [(V8SF "V8DI") (V4SF  "V4DI")])

(define_mode_attr sselongvecmodelower
  [(V8SF "v8di") (V4SF  "v4di")])

(define_mode_attr sseintvecmode3
  [(V8SF "XI") (V4SF "OI")
   (V8DF "OI") (V4DF "TI")])

(define_insn "float<floatunssuffix><sselongvecmodelower><mode>2<mask_name><round_name>"
  [(set (match_operand:VF1_128_256VL 0 "register_operand" "=v")
	 (any_float:VF1_128_256VL
	   (match_operand:<sselongvecmode> 1 "nonimmediate_operand" "<round_constraint>")))]
  "TARGET_AVX512DQ && <round_modev8sf_condition>"
  "vcvt<floatsuffix>qq2ps<qq2pssuff>\t{<round_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "float<floatunssuffix>v2div2sf2"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(vec_concat:V4SF
	    (any_float:V2SF (match_operand:V2DI 1 "nonimmediate_operand" "vm"))
	    (const_vector:V2SF [(const_int 0) (const_int 0)])))]
  "TARGET_AVX512DQ && TARGET_AVX512VL"
  "vcvt<floatsuffix>qq2ps{x}\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V4SF")])

(define_mode_attr vpckfloat_concat_mode
  [(V8DI "v16sf") (V4DI "v8sf") (V2DI "v8sf")])
(define_mode_attr vpckfloat_temp_mode
  [(V8DI "V8SF") (V4DI "V4SF") (V2DI "V4SF")])
(define_mode_attr vpckfloat_op_mode
  [(V8DI "v8sf") (V4DI "v4sf") (V2DI "v2sf")])

(define_expand "vec_pack<floatprefix>_float_<mode>"
  [(match_operand:<ssePSmode> 0 "register_operand")
   (any_float:<ssePSmode>
     (match_operand:VI8_AVX512VL 1 "register_operand"))
   (match_operand:VI8_AVX512VL 2 "register_operand")]
  "TARGET_AVX512DQ"
{
  rtx r1 = gen_reg_rtx (<vpckfloat_temp_mode>mode);
  rtx r2 = gen_reg_rtx (<vpckfloat_temp_mode>mode);
  rtx (*gen) (rtx, rtx) = gen_float<floatunssuffix><mode><vpckfloat_op_mode>2;
  emit_insn (gen (r1, operands[1]));
  emit_insn (gen (r2, operands[2]));
  if (<MODE>mode == V2DImode)
    emit_insn (gen_sse_movlhps (operands[0], r1, r2));
  else
    emit_insn (gen_avx_vec_concat<vpckfloat_concat_mode> (operands[0],
							  r1, r2));
  DONE;
})

(define_insn "float<floatunssuffix>v2div2sf2_mask"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
    (vec_concat:V4SF
        (vec_merge:V2SF
	        (any_float:V2SF (match_operand:V2DI 1 "nonimmediate_operand" "vm"))
            (vec_select:V2SF
                (match_operand:V4SF 2 "nonimm_or_0_operand" "0C")
                (parallel [(const_int 0) (const_int 1)]))
            (match_operand:QI 3 "register_operand" "Yk"))
	    (const_vector:V2SF [(const_int 0) (const_int 0)])))]
  "TARGET_AVX512DQ && TARGET_AVX512VL"
  "vcvt<floatsuffix>qq2ps{x}\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V4SF")])

(define_insn "*float<floatunssuffix>v2div2sf2_mask_1"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
    (vec_concat:V4SF
	(vec_merge:V2SF
		(any_float:V2SF (match_operand:V2DI 1
				  "nonimmediate_operand" "vm"))
	    (const_vector:V2SF [(const_int 0) (const_int 0)])
	    (match_operand:QI 2 "register_operand" "Yk"))
	    (const_vector:V2SF [(const_int 0) (const_int 0)])))]
  "TARGET_AVX512DQ && TARGET_AVX512VL"
  "vcvt<floatsuffix>qq2ps{x}\t{%1, %0%{%2%}%{z%}|%0%{%2%}%{z%}, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V4SF")])

(define_insn "ufloat<si2dfmodelower><mode>2<mask_name>"
  [(set (match_operand:VF2_512_256VL 0 "register_operand" "=v")
	(unsigned_float:VF2_512_256VL
	  (match_operand:<si2dfmode> 1 "nonimmediate_operand" "vm")))]
   "TARGET_AVX512F"
   "vcvtudq2pd\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
   [(set_attr "type" "ssecvt")
    (set_attr "prefix" "evex")
    (set_attr "mode" "<MODE>")])

(define_insn "ufloatv2siv2df2<mask_name>"
  [(set (match_operand:V2DF 0 "register_operand" "=v")
	(unsigned_float:V2DF
	  (vec_select:V2SI
	    (match_operand:V4SI 1 "nonimmediate_operand" "vm")
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_AVX512VL"
  "vcvtudq2pd\t{%1, %0<mask_operand2>|%0<mask_operand2>, %q1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V2DF")])

(define_insn "avx512f_cvtdq2pd512_2"
  [(set (match_operand:V8DF 0 "register_operand" "=v")
	(float:V8DF
	  (vec_select:V8SI
	    (match_operand:V16SI 1 "nonimmediate_operand" "vm")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)
		       (const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))))]
  "TARGET_AVX512F"
  "vcvtdq2pd\t{%t1, %0|%0, %t1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V8DF")])

(define_insn "avx_cvtdq2pd256_2"
  [(set (match_operand:V4DF 0 "register_operand" "=v")
	(float:V4DF
	  (vec_select:V4SI
	    (match_operand:V8SI 1 "nonimmediate_operand" "vm")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)]))))]
  "TARGET_AVX"
  "vcvtdq2pd\t{%x1, %0|%0, %x1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "V4DF")])

(define_insn "sse2_cvtdq2pd<mask_name>"
  [(set (match_operand:V2DF 0 "register_operand" "=v")
	(float:V2DF
	  (vec_select:V2SI
	    (match_operand:V4SI 1 "nonimmediate_operand" "vm")
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SSE2 && <mask_avx512vl_condition>"
  "%vcvtdq2pd\t{%1, %0<mask_operand2>|%0<mask_operand2>, %q1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V2DF")])

(define_insn "avx512f_cvtpd2dq512<mask_name><round_name>"
  [(set (match_operand:V8SI 0 "register_operand" "=v")
	(unspec:V8SI
	  [(match_operand:V8DF 1 "<round_nimm_predicate>" "<round_constraint>")]
	  UNSPEC_FIX_NOTRUNC))]
  "TARGET_AVX512F"
  "vcvtpd2dq\t{<round_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "OI")])

(define_insn "avx_cvtpd2dq256<mask_name>"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(unspec:V4SI [(match_operand:V4DF 1 "nonimmediate_operand" "vm")]
		     UNSPEC_FIX_NOTRUNC))]
  "TARGET_AVX && <mask_avx512vl_condition>"
  "vcvtpd2dq{y}\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "<mask_prefix>")
   (set_attr "mode" "OI")])

(define_expand "avx_cvtpd2dq256_2"
  [(set (match_operand:V8SI 0 "register_operand")
	(vec_concat:V8SI
	  (unspec:V4SI [(match_operand:V4DF 1 "nonimmediate_operand")]
		       UNSPEC_FIX_NOTRUNC)
	  (match_dup 2)))]
  "TARGET_AVX"
  "operands[2] = CONST0_RTX (V4SImode);")

(define_insn "*avx_cvtpd2dq256_2"
  [(set (match_operand:V8SI 0 "register_operand" "=v")
	(vec_concat:V8SI
	  (unspec:V4SI [(match_operand:V4DF 1 "nonimmediate_operand" "vm")]
		       UNSPEC_FIX_NOTRUNC)
	  (match_operand:V4SI 2 "const0_operand")))]
  "TARGET_AVX"
  "vcvtpd2dq{y}\t{%1, %x0|%x0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "btver2_decode" "vector")
   (set_attr "mode" "OI")])

(define_insn "sse2_cvtpd2dq<mask_name>"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(vec_concat:V4SI
	  (unspec:V2SI [(match_operand:V2DF 1 "vector_operand" "vBm")]
		       UNSPEC_FIX_NOTRUNC)
	  (const_vector:V2SI [(const_int 0) (const_int 0)])))]
  "TARGET_SSE2 && <mask_avx512vl_condition>"
{
  if (TARGET_AVX)
    return "vcvtpd2dq{x}\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}";
  else
    return "cvtpd2dq\t{%1, %0|%0, %1}";
}
  [(set_attr "type" "ssecvt")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")
   (set_attr "amdfam10_decode" "double")
   (set_attr "athlon_decode" "vector")
   (set_attr "bdver1_decode" "double")])

;; For ufix_notrunc* insn patterns
(define_mode_attr pd2udqsuff
  [(V8DF "") (V4DF "{y}")])

(define_insn "ufix_notrunc<mode><si2dfmodelower>2<mask_name><round_name>"
  [(set (match_operand:<si2dfmode> 0 "register_operand" "=v")
	(unspec:<si2dfmode>
	  [(match_operand:VF2_512_256VL 1 "nonimmediate_operand" "<round_constraint>")]
	  UNSPEC_UNSIGNED_FIX_NOTRUNC))]
  "TARGET_AVX512F"
  "vcvtpd2udq<pd2udqsuff>\t{<round_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "ufix_notruncv2dfv2si2<mask_name>"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(vec_concat:V4SI
	  (unspec:V2SI
	    [(match_operand:V2DF 1 "nonimmediate_operand" "vm")]
	    UNSPEC_UNSIGNED_FIX_NOTRUNC)
	  (const_vector:V2SI [(const_int 0) (const_int 0)])))]
  "TARGET_AVX512VL"
  "vcvtpd2udq{x}\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "fix<fixunssuffix>_truncv8dfv8si2<mask_name><round_saeonly_name>"
  [(set (match_operand:V8SI 0 "register_operand" "=v")
	(any_fix:V8SI
	  (match_operand:V8DF 1 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")))]
  "TARGET_AVX512F"
  "vcvttpd2<fixsuffix>dq\t{<round_saeonly_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_saeonly_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "OI")])

(define_insn "ufix_truncv2dfv2si2<mask_name>"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(vec_concat:V4SI
	  (unsigned_fix:V2SI (match_operand:V2DF 1 "nonimmediate_operand" "vm"))
	  (const_vector:V2SI [(const_int 0) (const_int 0)])))]
  "TARGET_AVX512VL"
  "vcvttpd2udq{x}\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "fix_truncv4dfv4si2<mask_name>"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(fix:V4SI (match_operand:V4DF 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX || (TARGET_AVX512VL && TARGET_AVX512F)"
  "vcvttpd2dq{y}\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "OI")])

(define_insn "ufix_truncv4dfv4si2<mask_name>"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(unsigned_fix:V4SI (match_operand:V4DF 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512VL && TARGET_AVX512F"
  "vcvttpd2udq{y}\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "OI")])

(define_insn "fix<fixunssuffix>_trunc<mode><sseintvecmodelower>2<mask_name><round_saeonly_name>"
  [(set (match_operand:<sseintvecmode> 0 "register_operand" "=v")
	(any_fix:<sseintvecmode>
	  (match_operand:VF2_AVX512VL 1 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")))]
  "TARGET_AVX512DQ && <round_saeonly_mode512bit_condition>"
  "vcvttpd2<fixsuffix>qq\t{<round_saeonly_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_saeonly_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseintvecmode2>")])

(define_insn "fix_notrunc<mode><sseintvecmodelower>2<mask_name><round_name>"
  [(set (match_operand:<sseintvecmode> 0 "register_operand" "=v")
	(unspec:<sseintvecmode>
	  [(match_operand:VF2_AVX512VL 1 "<round_nimm_predicate>" "<round_constraint>")]
	  UNSPEC_FIX_NOTRUNC))]
  "TARGET_AVX512DQ && <round_mode512bit_condition>"
  "vcvtpd2qq\t{<round_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseintvecmode2>")])

(define_insn "ufix_notrunc<mode><sseintvecmodelower>2<mask_name><round_name>"
  [(set (match_operand:<sseintvecmode> 0 "register_operand" "=v")
	(unspec:<sseintvecmode>
	  [(match_operand:VF2_AVX512VL 1 "nonimmediate_operand" "<round_constraint>")]
	  UNSPEC_UNSIGNED_FIX_NOTRUNC))]
  "TARGET_AVX512DQ && <round_mode512bit_condition>"
  "vcvtpd2uqq\t{<round_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseintvecmode2>")])

(define_insn "fix<fixunssuffix>_trunc<mode><sselongvecmodelower>2<mask_name><round_saeonly_name>"
  [(set (match_operand:<sselongvecmode> 0 "register_operand" "=v")
	(any_fix:<sselongvecmode>
	  (match_operand:VF1_128_256VL 1 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")))]
  "TARGET_AVX512DQ && <round_saeonly_modev8sf_condition>"
  "vcvttps2<fixsuffix>qq\t{<round_saeonly_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_saeonly_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseintvecmode3>")])

(define_insn "fix<fixunssuffix>_truncv2sfv2di2<mask_name>"
  [(set (match_operand:V2DI 0 "register_operand" "=v")
	(any_fix:V2DI
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "nonimmediate_operand" "vm")
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_AVX512DQ && TARGET_AVX512VL"
  "vcvttps2<fixsuffix>qq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %q1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_mode_attr vunpckfixt_mode
  [(V16SF "V8DI") (V8SF "V4DI") (V4SF "V2DI")])
(define_mode_attr vunpckfixt_model
  [(V16SF "v8di") (V8SF "v4di") (V4SF "v2di")])
(define_mode_attr vunpckfixt_extract_mode
  [(V16SF "v16sf") (V8SF "v8sf") (V4SF "v8sf")])

(define_expand "vec_unpack_<fixprefix>fix_trunc_lo_<mode>"
  [(match_operand:<vunpckfixt_mode> 0 "register_operand")
   (any_fix:<vunpckfixt_mode>
     (match_operand:VF1_AVX512VL 1 "register_operand"))]
  "TARGET_AVX512DQ"
{
  rtx tem = operands[1];
  if (<MODE>mode != V4SFmode)
    {
      tem = gen_reg_rtx (<ssehalfvecmode>mode);
      emit_insn (gen_vec_extract_lo_<vunpckfixt_extract_mode> (tem,
							       operands[1]));
    }
  rtx (*gen) (rtx, rtx)
    = gen_fix<fixunssuffix>_trunc<ssehalfvecmodelower><vunpckfixt_model>2;
  emit_insn (gen (operands[0], tem));
  DONE;
})

(define_expand "vec_unpack_<fixprefix>fix_trunc_hi_<mode>"
  [(match_operand:<vunpckfixt_mode> 0 "register_operand")
   (any_fix:<vunpckfixt_mode>
     (match_operand:VF1_AVX512VL 1 "register_operand"))]
  "TARGET_AVX512DQ"
{
  rtx tem;
  if (<MODE>mode != V4SFmode)
    {
      tem = gen_reg_rtx (<ssehalfvecmode>mode);
      emit_insn (gen_vec_extract_hi_<vunpckfixt_extract_mode> (tem,
							       operands[1]));
    }
  else
    {
      tem = gen_reg_rtx (V4SFmode);
      emit_insn (gen_avx_vpermilv4sf (tem, operands[1], GEN_INT (0x4e)));
    }
  rtx (*gen) (rtx, rtx)
    = gen_fix<fixunssuffix>_trunc<ssehalfvecmodelower><vunpckfixt_model>2;
  emit_insn (gen (operands[0], tem));
  DONE;
})

(define_insn "ufix_trunc<mode><sseintvecmodelower>2<mask_name>"
  [(set (match_operand:<sseintvecmode> 0 "register_operand" "=v")
	(unsigned_fix:<sseintvecmode>
	  (match_operand:VF1_128_256VL 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512VL"
  "vcvttps2udq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseintvecmode2>")])

(define_expand "avx_cvttpd2dq256_2"
  [(set (match_operand:V8SI 0 "register_operand")
	(vec_concat:V8SI
	  (fix:V4SI (match_operand:V4DF 1 "nonimmediate_operand"))
	  (match_dup 2)))]
  "TARGET_AVX"
  "operands[2] = CONST0_RTX (V4SImode);")

(define_insn "sse2_cvttpd2dq<mask_name>"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(vec_concat:V4SI
	  (fix:V2SI (match_operand:V2DF 1 "vector_operand" "vBm"))
	  (const_vector:V2SI [(const_int 0) (const_int 0)])))]
  "TARGET_SSE2 && <mask_avx512vl_condition>"
{
  if (TARGET_AVX)
    return "vcvttpd2dq{x}\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}";
  else
    return "cvttpd2dq\t{%1, %0|%0, %1}";
}
  [(set_attr "type" "ssecvt")
   (set_attr "amdfam10_decode" "double")
   (set_attr "athlon_decode" "vector")
   (set_attr "bdver1_decode" "double")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "sse2_cvtsd2ss<round_name>"
  [(set (match_operand:V4SF 0 "register_operand" "=x,x,v")
	(vec_merge:V4SF
	  (vec_duplicate:V4SF
	    (float_truncate:V2SF
	      (match_operand:V2DF 2 "nonimmediate_operand" "x,m,<round_constraint>")))
	  (match_operand:V4SF 1 "register_operand" "0,0,v")
	  (const_int 1)))]
  "TARGET_SSE2"
  "@
   cvtsd2ss\t{%2, %0|%0, %2}
   cvtsd2ss\t{%2, %0|%0, %q2}
   vcvtsd2ss\t{<round_op3>%2, %1, %0|%0, %1, %q2<round_op3>}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssecvt")
   (set_attr "athlon_decode" "vector,double,*")
   (set_attr "amdfam10_decode" "vector,double,*")
   (set_attr "bdver1_decode" "direct,direct,*")
   (set_attr "btver2_decode" "double,double,double")
   (set_attr "prefix" "orig,orig,<round_prefix>")
   (set_attr "mode" "SF")])

(define_insn "*sse2_vd_cvtsd2ss"
  [(set (match_operand:V4SF 0 "register_operand" "=x,x,v")
	(vec_merge:V4SF
	  (vec_duplicate:V4SF
	    (float_truncate:SF (match_operand:DF 2 "nonimmediate_operand" "x,m,vm")))
	  (match_operand:V4SF 1 "register_operand" "0,0,v")
	  (const_int 1)))]
  "TARGET_SSE2"
  "@
   cvtsd2ss\t{%2, %0|%0, %2}
   cvtsd2ss\t{%2, %0|%0, %2}
   vcvtsd2ss\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssecvt")
   (set_attr "athlon_decode" "vector,double,*")
   (set_attr "amdfam10_decode" "vector,double,*")
   (set_attr "bdver1_decode" "direct,direct,*")
   (set_attr "btver2_decode" "double,double,double")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "mode" "SF")])

(define_insn "sse2_cvtss2sd<round_saeonly_name>"
  [(set (match_operand:V2DF 0 "register_operand" "=x,x,v")
	(vec_merge:V2DF
	  (float_extend:V2DF
	    (vec_select:V2SF
	      (match_operand:V4SF 2 "<round_saeonly_nimm_scalar_predicate>" "x,m,<round_saeonly_constraint>")
	      (parallel [(const_int 0) (const_int 1)])))
	  (match_operand:V2DF 1 "register_operand" "0,0,v")
	  (const_int 1)))]
  "TARGET_SSE2"
  "@
   cvtss2sd\t{%2, %0|%0, %2}
   cvtss2sd\t{%2, %0|%0, %k2}
   vcvtss2sd\t{<round_saeonly_op3>%2, %1, %0|%0, %1, %k2<round_saeonly_op3>}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssecvt")
   (set_attr "amdfam10_decode" "vector,double,*")
   (set_attr "athlon_decode" "direct,direct,*")
   (set_attr "bdver1_decode" "direct,direct,*")
   (set_attr "btver2_decode" "double,double,double")
   (set_attr "prefix" "orig,orig,<round_saeonly_prefix>")
   (set_attr "mode" "DF")])

(define_insn "*sse2_vd_cvtss2sd"
  [(set (match_operand:V2DF 0 "register_operand" "=x,x,v")
	(vec_merge:V2DF
	  (vec_duplicate:V2DF
	    (float_extend:DF (match_operand:SF 2 "nonimmediate_operand" "x,m,vm")))
	  (match_operand:V2DF 1 "register_operand" "0,0,v")
	  (const_int 1)))]
  "TARGET_SSE2"
  "@
   cvtss2sd\t{%2, %0|%0, %2}
   cvtss2sd\t{%2, %0|%0, %2}
   vcvtss2sd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssecvt")
   (set_attr "amdfam10_decode" "vector,double,*")
   (set_attr "athlon_decode" "direct,direct,*")
   (set_attr "bdver1_decode" "direct,direct,*")
   (set_attr "btver2_decode" "double,double,double")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "mode" "DF")])

(define_insn "<mask_codefor>avx512f_cvtpd2ps512<mask_name><round_name>"
  [(set (match_operand:V8SF 0 "register_operand" "=v")
	(float_truncate:V8SF
	  (match_operand:V8DF 1 "<round_nimm_predicate>" "<round_constraint>")))]
  "TARGET_AVX512F"
  "vcvtpd2ps\t{<round_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V8SF")])

(define_insn "avx_cvtpd2ps256<mask_name>"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(float_truncate:V4SF
	  (match_operand:V4DF 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX && <mask_avx512vl_condition>"
  "vcvtpd2ps{y}\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "maybe_evex")
   (set_attr "btver2_decode" "vector")
   (set_attr "mode" "V4SF")])

(define_expand "sse2_cvtpd2ps"
  [(set (match_operand:V4SF 0 "register_operand")
	(vec_concat:V4SF
	  (float_truncate:V2SF
	    (match_operand:V2DF 1 "vector_operand"))
	  (match_dup 2)))]
  "TARGET_SSE2"
  "operands[2] = CONST0_RTX (V2SFmode);")

(define_expand "sse2_cvtpd2ps_mask"
  [(set (match_operand:V4SF 0 "register_operand")
	(vec_merge:V4SF
	  (vec_concat:V4SF
	    (float_truncate:V2SF
	      (match_operand:V2DF 1 "vector_operand"))
	    (match_dup 4))
	  (match_operand:V4SF 2 "register_operand")
	  (match_operand:QI 3 "register_operand")))]
  "TARGET_SSE2"
  "operands[4] = CONST0_RTX (V2SFmode);")

(define_insn "*sse2_cvtpd2ps<mask_name>"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(vec_concat:V4SF
	  (float_truncate:V2SF
	    (match_operand:V2DF 1 "vector_operand" "vBm"))
	  (match_operand:V2SF 2 "const0_operand")))]
  "TARGET_SSE2 && <mask_avx512vl_condition>"
{
  if (TARGET_AVX)
    return "vcvtpd2ps{x}\t{%1, %0<mask_operand3>|%0<mask_operand3>, %1}";
  else
    return "cvtpd2ps\t{%1, %0|%0, %1}";
}
  [(set_attr "type" "ssecvt")
   (set_attr "amdfam10_decode" "double")
   (set_attr "athlon_decode" "vector")
   (set_attr "bdver1_decode" "double")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V4SF")])

;; For <sse2_avx_avx512f>_cvtps2pd<avxsizesuffix> insn pattern
(define_mode_attr sf2dfmode
  [(V8DF "V8SF") (V4DF "V4SF")])

(define_insn "<sse2_avx_avx512f>_cvtps2pd<avxsizesuffix><mask_name><round_saeonly_name>"
  [(set (match_operand:VF2_512_256 0 "register_operand" "=v")
	(float_extend:VF2_512_256
	  (match_operand:<sf2dfmode> 1 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")))]
  "TARGET_AVX && <mask_mode512bit_condition> && <round_saeonly_mode512bit_condition>"
  "vcvtps2pd\t{<round_saeonly_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_saeonly_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

(define_insn "*avx_cvtps2pd256_2"
  [(set (match_operand:V4DF 0 "register_operand" "=v")
	(float_extend:V4DF
	  (vec_select:V4SF
	    (match_operand:V8SF 1 "nonimmediate_operand" "vm")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)]))))]
  "TARGET_AVX"
  "vcvtps2pd\t{%x1, %0|%0, %x1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_insn "vec_unpacks_lo_v16sf"
  [(set (match_operand:V8DF 0 "register_operand" "=v")
	(float_extend:V8DF
	  (vec_select:V8SF
	    (match_operand:V16SF 1 "nonimmediate_operand" "vm")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)
		       (const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))))]
  "TARGET_AVX512F"
  "vcvtps2pd\t{%t1, %0|%0, %t1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V8DF")])

(define_insn "<avx512>_cvt<ssemodesuffix>2mask<mode>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(unspec:<avx512fmaskmode>
	 [(match_operand:VI12_AVX512VL 1 "register_operand" "v")]
	 UNSPEC_CVTINT2MASK))]
  "TARGET_AVX512BW"
  "vpmov<ssemodesuffix>2m\t{%1, %0|%0, %1}"
  [(set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_cvt<ssemodesuffix>2mask<mode>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(unspec:<avx512fmaskmode>
	 [(match_operand:VI48_AVX512VL 1 "register_operand" "v")]
	 UNSPEC_CVTINT2MASK))]
  "TARGET_AVX512DQ"
  "vpmov<ssemodesuffix>2m\t{%1, %0|%0, %1}"
  [(set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "<avx512>_cvtmask2<ssemodesuffix><mode>"
  [(set (match_operand:VI12_AVX512VL 0 "register_operand")
	(vec_merge:VI12_AVX512VL
	  (match_dup 2)
	  (match_dup 3)
	  (match_operand:<avx512fmaskmode> 1 "register_operand")))]
  "TARGET_AVX512BW"
  {
    operands[2] = CONSTM1_RTX (<MODE>mode);
    operands[3] = CONST0_RTX (<MODE>mode);
  })

(define_insn "*<avx512>_cvtmask2<ssemodesuffix><mode>"
  [(set (match_operand:VI12_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI12_AVX512VL
	  (match_operand:VI12_AVX512VL 2 "vector_all_ones_operand")
	  (match_operand:VI12_AVX512VL 3 "const0_operand")
	  (match_operand:<avx512fmaskmode> 1 "register_operand" "Yk")))]
  "TARGET_AVX512BW"
  "vpmovm2<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "<avx512>_cvtmask2<ssemodesuffix><mode>"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand")
	(vec_merge:VI48_AVX512VL
	  (match_dup 2)
	  (match_dup 3)
	  (match_operand:<avx512fmaskmode> 1 "register_operand")))]
  "TARGET_AVX512DQ"
  "{
    operands[2] = CONSTM1_RTX (<MODE>mode);
    operands[3] = CONST0_RTX (<MODE>mode);
  }")

(define_insn "*<avx512>_cvtmask2<ssemodesuffix><mode>"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI48_AVX512VL
	  (match_operand:VI48_AVX512VL 2 "vector_all_ones_operand")
	  (match_operand:VI48_AVX512VL 3 "const0_operand")
	  (match_operand:<avx512fmaskmode> 1 "register_operand" "Yk")))]
  "TARGET_AVX512DQ"
  "vpmovm2<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "sse2_cvtps2pd<mask_name>"
  [(set (match_operand:V2DF 0 "register_operand" "=v")
	(float_extend:V2DF
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "vector_operand" "vm")
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SSE2 && <mask_avx512vl_condition>"
  "%vcvtps2pd\t{%1, %0<mask_operand2>|%0<mask_operand2>, %q1}"
  [(set_attr "type" "ssecvt")
   (set_attr "amdfam10_decode" "direct")
   (set_attr "athlon_decode" "double")
   (set_attr "bdver1_decode" "double")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V2DF")])

(define_expand "vec_unpacks_hi_v4sf"
  [(set (match_dup 2)
   (vec_select:V4SF
     (vec_concat:V8SF
       (match_dup 2)
       (match_operand:V4SF 1 "vector_operand"))
     (parallel [(const_int 6) (const_int 7)
		(const_int 2) (const_int 3)])))
  (set (match_operand:V2DF 0 "register_operand")
   (float_extend:V2DF
     (vec_select:V2SF
       (match_dup 2)
       (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SSE2"
  "operands[2] = gen_reg_rtx (V4SFmode);")

(define_expand "vec_unpacks_hi_v8sf"
  [(set (match_dup 2)
	(vec_select:V4SF
	  (match_operand:V8SF 1 "register_operand")
	  (parallel [(const_int 4) (const_int 5)
		     (const_int 6) (const_int 7)])))
   (set (match_operand:V4DF 0 "register_operand")
	(float_extend:V4DF
	  (match_dup 2)))]
  "TARGET_AVX"
  "operands[2] = gen_reg_rtx (V4SFmode);")

(define_expand "vec_unpacks_hi_v16sf"
  [(set (match_dup 2)
	(vec_select:V8SF
	  (match_operand:V16SF 1 "register_operand")
	  (parallel [(const_int 8) (const_int 9)
		     (const_int 10) (const_int 11)
		     (const_int 12) (const_int 13)
		     (const_int 14) (const_int 15)])))
   (set (match_operand:V8DF 0 "register_operand")
	(float_extend:V8DF
	  (match_dup 2)))]
"TARGET_AVX512F"
"operands[2] = gen_reg_rtx (V8SFmode);")

(define_expand "vec_unpacks_lo_v4sf"
  [(set (match_operand:V2DF 0 "register_operand")
	(float_extend:V2DF
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "vector_operand")
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SSE2")

(define_expand "vec_unpacks_lo_v8sf"
  [(set (match_operand:V4DF 0 "register_operand")
	(float_extend:V4DF
	  (vec_select:V4SF
	    (match_operand:V8SF 1 "nonimmediate_operand")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)]))))]
  "TARGET_AVX")

(define_mode_attr sseunpackfltmode
  [(V8HI "V4SF") (V4SI "V2DF") (V16HI "V8SF")
  (V8SI "V4DF") (V32HI "V16SF") (V16SI "V8DF")])

(define_expand "vec_unpacks_float_hi_<mode>"
  [(match_operand:<sseunpackfltmode> 0 "register_operand")
   (match_operand:VI2_AVX512F 1 "register_operand")]
  "TARGET_SSE2"
{
  rtx tmp = gen_reg_rtx (<sseunpackmode>mode);

  emit_insn (gen_vec_unpacks_hi_<mode> (tmp, operands[1]));
  emit_insn (gen_rtx_SET (operands[0],
			  gen_rtx_FLOAT (<sseunpackfltmode>mode, tmp)));
  DONE;
})

(define_expand "vec_unpacks_float_lo_<mode>"
  [(match_operand:<sseunpackfltmode> 0 "register_operand")
   (match_operand:VI2_AVX512F 1 "register_operand")]
  "TARGET_SSE2"
{
  rtx tmp = gen_reg_rtx (<sseunpackmode>mode);

  emit_insn (gen_vec_unpacks_lo_<mode> (tmp, operands[1]));
  emit_insn (gen_rtx_SET (operands[0],
			  gen_rtx_FLOAT (<sseunpackfltmode>mode, tmp)));
  DONE;
})

(define_expand "vec_unpacku_float_hi_<mode>"
  [(match_operand:<sseunpackfltmode> 0 "register_operand")
   (match_operand:VI2_AVX512F 1 "register_operand")]
  "TARGET_SSE2"
{
  rtx tmp = gen_reg_rtx (<sseunpackmode>mode);

  emit_insn (gen_vec_unpacku_hi_<mode> (tmp, operands[1]));
  emit_insn (gen_rtx_SET (operands[0],
			  gen_rtx_FLOAT (<sseunpackfltmode>mode, tmp)));
  DONE;
})

(define_expand "vec_unpacku_float_lo_<mode>"
  [(match_operand:<sseunpackfltmode> 0 "register_operand")
   (match_operand:VI2_AVX512F 1 "register_operand")]
  "TARGET_SSE2"
{
  rtx tmp = gen_reg_rtx (<sseunpackmode>mode);

  emit_insn (gen_vec_unpacku_lo_<mode> (tmp, operands[1]));
  emit_insn (gen_rtx_SET (operands[0],
			  gen_rtx_FLOAT (<sseunpackfltmode>mode, tmp)));
  DONE;
})

(define_expand "vec_unpacks_float_hi_v4si"
  [(set (match_dup 2)
	(vec_select:V4SI
	  (match_operand:V4SI 1 "vector_operand")
	  (parallel [(const_int 2) (const_int 3)
		     (const_int 2) (const_int 3)])))
   (set (match_operand:V2DF 0 "register_operand")
	(float:V2DF
	  (vec_select:V2SI
	  (match_dup 2)
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SSE2"
  "operands[2] = gen_reg_rtx (V4SImode);")

(define_expand "vec_unpacks_float_lo_v4si"
  [(set (match_operand:V2DF 0 "register_operand")
	(float:V2DF
	  (vec_select:V2SI
	    (match_operand:V4SI 1 "vector_operand")
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SSE2")

(define_expand "vec_unpacks_float_hi_v8si"
  [(set (match_dup 2)
	(vec_select:V4SI
	  (match_operand:V8SI 1 "vector_operand")
	  (parallel [(const_int 4) (const_int 5)
		     (const_int 6) (const_int 7)])))
   (set (match_operand:V4DF 0 "register_operand")
	(float:V4DF
	  (match_dup 2)))]
  "TARGET_AVX"
  "operands[2] = gen_reg_rtx (V4SImode);")

(define_expand "vec_unpacks_float_lo_v8si"
  [(set (match_operand:V4DF 0 "register_operand")
	(float:V4DF
	  (vec_select:V4SI
	    (match_operand:V8SI 1 "nonimmediate_operand")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)]))))]
  "TARGET_AVX")

(define_expand "vec_unpacks_float_hi_v16si"
  [(set (match_dup 2)
	(vec_select:V8SI
	  (match_operand:V16SI 1 "nonimmediate_operand")
	  (parallel [(const_int 8) (const_int 9)
		     (const_int 10) (const_int 11)
		     (const_int 12) (const_int 13)
		     (const_int 14) (const_int 15)])))
   (set (match_operand:V8DF 0 "register_operand")
	(float:V8DF
	  (match_dup 2)))]
  "TARGET_AVX512F"
  "operands[2] = gen_reg_rtx (V8SImode);")

(define_expand "vec_unpacks_float_lo_v16si"
  [(set (match_operand:V8DF 0 "register_operand")
	(float:V8DF
	  (vec_select:V8SI
	    (match_operand:V16SI 1 "nonimmediate_operand")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)
		       (const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))))]
  "TARGET_AVX512F")

(define_expand "vec_unpacku_float_hi_v4si"
  [(set (match_dup 5)
	(vec_select:V4SI
	  (match_operand:V4SI 1 "vector_operand")
	  (parallel [(const_int 2) (const_int 3)
		     (const_int 2) (const_int 3)])))
   (set (match_dup 6)
	(float:V2DF
	  (vec_select:V2SI
	  (match_dup 5)
	    (parallel [(const_int 0) (const_int 1)]))))
   (set (match_dup 7)
	(lt:V2DF (match_dup 6) (match_dup 3)))
   (set (match_dup 8)
	(and:V2DF (match_dup 7) (match_dup 4)))
   (set (match_operand:V2DF 0 "register_operand")
	(plus:V2DF (match_dup 6) (match_dup 8)))]
  "TARGET_SSE2"
{
  REAL_VALUE_TYPE TWO32r;
  rtx x;
  int i;

  real_ldexp (&TWO32r, &dconst1, 32);
  x = const_double_from_real_value (TWO32r, DFmode);

  operands[3] = force_reg (V2DFmode, CONST0_RTX (V2DFmode));
  operands[4] = force_reg (V2DFmode,
			   ix86_build_const_vector (V2DFmode, 1, x));

  operands[5] = gen_reg_rtx (V4SImode);

  for (i = 6; i < 9; i++)
    operands[i] = gen_reg_rtx (V2DFmode);
})

(define_expand "vec_unpacku_float_lo_v4si"
  [(set (match_dup 5)
	(float:V2DF
	  (vec_select:V2SI
	    (match_operand:V4SI 1 "vector_operand")
	    (parallel [(const_int 0) (const_int 1)]))))
   (set (match_dup 6)
	(lt:V2DF (match_dup 5) (match_dup 3)))
   (set (match_dup 7)
	(and:V2DF (match_dup 6) (match_dup 4)))
   (set (match_operand:V2DF 0 "register_operand")
	(plus:V2DF (match_dup 5) (match_dup 7)))]
  "TARGET_SSE2"
{
  REAL_VALUE_TYPE TWO32r;
  rtx x;
  int i;

  real_ldexp (&TWO32r, &dconst1, 32);
  x = const_double_from_real_value (TWO32r, DFmode);

  operands[3] = force_reg (V2DFmode, CONST0_RTX (V2DFmode));
  operands[4] = force_reg (V2DFmode,
			   ix86_build_const_vector (V2DFmode, 1, x));

  for (i = 5; i < 8; i++)
    operands[i] = gen_reg_rtx (V2DFmode);
})

(define_expand "vec_unpacku_float_hi_v8si"
  [(match_operand:V4DF 0 "register_operand")
   (match_operand:V8SI 1 "register_operand")]
  "TARGET_AVX"
{
  REAL_VALUE_TYPE TWO32r;
  rtx x, tmp[6];
  int i;

  real_ldexp (&TWO32r, &dconst1, 32);
  x = const_double_from_real_value (TWO32r, DFmode);

  tmp[0] = force_reg (V4DFmode, CONST0_RTX (V4DFmode));
  tmp[1] = force_reg (V4DFmode, ix86_build_const_vector (V4DFmode, 1, x));
  tmp[5] = gen_reg_rtx (V4SImode);

  for (i = 2; i < 5; i++)
    tmp[i] = gen_reg_rtx (V4DFmode);
  emit_insn (gen_vec_extract_hi_v8si (tmp[5], operands[1]));
  emit_insn (gen_floatv4siv4df2 (tmp[2], tmp[5]));
  emit_insn (gen_rtx_SET (tmp[3], gen_rtx_LT (V4DFmode, tmp[2], tmp[0])));
  emit_insn (gen_andv4df3 (tmp[4], tmp[3], tmp[1]));
  emit_insn (gen_addv4df3 (operands[0], tmp[2], tmp[4]));
  DONE;
})

(define_expand "vec_unpacku_float_hi_v16si"
  [(match_operand:V8DF 0 "register_operand")
   (match_operand:V16SI 1 "register_operand")]
  "TARGET_AVX512F"
{
  REAL_VALUE_TYPE TWO32r;
  rtx k, x, tmp[4];

  real_ldexp (&TWO32r, &dconst1, 32);
  x = const_double_from_real_value (TWO32r, DFmode);

  tmp[0] = force_reg (V8DFmode, CONST0_RTX (V8DFmode));
  tmp[1] = force_reg (V8DFmode, ix86_build_const_vector (V8DFmode, 1, x));
  tmp[2] = gen_reg_rtx (V8DFmode);
  tmp[3] = gen_reg_rtx (V8SImode);
  k = gen_reg_rtx (QImode);

  emit_insn (gen_vec_extract_hi_v16si (tmp[3], operands[1]));
  emit_insn (gen_floatv8siv8df2 (tmp[2], tmp[3]));
  emit_insn (gen_rtx_SET (k, gen_rtx_LT (QImode, tmp[2], tmp[0])));
  emit_insn (gen_addv8df3_mask (tmp[2], tmp[2], tmp[1], tmp[2], k));
  emit_move_insn (operands[0], tmp[2]);
  DONE;
})

(define_expand "vec_unpacku_float_lo_v8si"
  [(match_operand:V4DF 0 "register_operand")
   (match_operand:V8SI 1 "nonimmediate_operand")]
  "TARGET_AVX"
{
  REAL_VALUE_TYPE TWO32r;
  rtx x, tmp[5];
  int i;

  real_ldexp (&TWO32r, &dconst1, 32);
  x = const_double_from_real_value (TWO32r, DFmode);

  tmp[0] = force_reg (V4DFmode, CONST0_RTX (V4DFmode));
  tmp[1] = force_reg (V4DFmode, ix86_build_const_vector (V4DFmode, 1, x));

  for (i = 2; i < 5; i++)
    tmp[i] = gen_reg_rtx (V4DFmode);
  emit_insn (gen_avx_cvtdq2pd256_2 (tmp[2], operands[1]));
  emit_insn (gen_rtx_SET (tmp[3], gen_rtx_LT (V4DFmode, tmp[2], tmp[0])));
  emit_insn (gen_andv4df3 (tmp[4], tmp[3], tmp[1]));
  emit_insn (gen_addv4df3 (operands[0], tmp[2], tmp[4]));
  DONE;
})

(define_expand "vec_unpacku_float_lo_v16si"
  [(match_operand:V8DF 0 "register_operand")
   (match_operand:V16SI 1 "nonimmediate_operand")]
  "TARGET_AVX512F"
{
  REAL_VALUE_TYPE TWO32r;
  rtx k, x, tmp[3];

  real_ldexp (&TWO32r, &dconst1, 32);
  x = const_double_from_real_value (TWO32r, DFmode);

  tmp[0] = force_reg (V8DFmode, CONST0_RTX (V8DFmode));
  tmp[1] = force_reg (V8DFmode, ix86_build_const_vector (V8DFmode, 1, x));
  tmp[2] = gen_reg_rtx (V8DFmode);
  k = gen_reg_rtx (QImode);

  emit_insn (gen_avx512f_cvtdq2pd512_2 (tmp[2], operands[1]));
  emit_insn (gen_rtx_SET (k, gen_rtx_LT (QImode, tmp[2], tmp[0])));
  emit_insn (gen_addv8df3_mask (tmp[2], tmp[2], tmp[1], tmp[2], k));
  emit_move_insn (operands[0], tmp[2]);
  DONE;
})

(define_expand "vec_pack_trunc_<mode>"
  [(set (match_dup 3)
	(float_truncate:<sf2dfmode>
	  (match_operand:VF2_512_256 1 "nonimmediate_operand")))
   (set (match_dup 4)
	(float_truncate:<sf2dfmode>
	  (match_operand:VF2_512_256 2 "nonimmediate_operand")))
   (set (match_operand:<ssePSmode> 0 "register_operand")
	(vec_concat:<ssePSmode>
	  (match_dup 3)
	  (match_dup 4)))]
  "TARGET_AVX"
{
  operands[3] = gen_reg_rtx (<sf2dfmode>mode);
  operands[4] = gen_reg_rtx (<sf2dfmode>mode);
})

(define_expand "vec_pack_trunc_v2df"
  [(match_operand:V4SF 0 "register_operand")
   (match_operand:V2DF 1 "vector_operand")
   (match_operand:V2DF 2 "vector_operand")]
  "TARGET_SSE2"
{
  rtx tmp0, tmp1;

  if (TARGET_AVX && !TARGET_PREFER_AVX128 && optimize_insn_for_speed_p ())
    {
      tmp0 = gen_reg_rtx (V4DFmode);
      tmp1 = force_reg (V2DFmode, operands[1]);

      emit_insn (gen_avx_vec_concatv4df (tmp0, tmp1, operands[2]));
      emit_insn (gen_avx_cvtpd2ps256 (operands[0], tmp0));
    }
  else
    {
      tmp0 = gen_reg_rtx (V4SFmode);
      tmp1 = gen_reg_rtx (V4SFmode);

      emit_insn (gen_sse2_cvtpd2ps (tmp0, operands[1]));
      emit_insn (gen_sse2_cvtpd2ps (tmp1, operands[2]));
      emit_insn (gen_sse_movlhps (operands[0], tmp0, tmp1));
    }
  DONE;
})

(define_expand "vec_pack_sfix_trunc_v8df"
  [(match_operand:V16SI 0 "register_operand")
   (match_operand:V8DF 1 "nonimmediate_operand")
   (match_operand:V8DF 2 "nonimmediate_operand")]
  "TARGET_AVX512F"
{
  rtx r1, r2;

  r1 = gen_reg_rtx (V8SImode);
  r2 = gen_reg_rtx (V8SImode);

  emit_insn (gen_fix_truncv8dfv8si2 (r1, operands[1]));
  emit_insn (gen_fix_truncv8dfv8si2 (r2, operands[2]));
  emit_insn (gen_avx_vec_concatv16si (operands[0], r1, r2));
  DONE;
})

(define_expand "vec_pack_sfix_trunc_v4df"
  [(match_operand:V8SI 0 "register_operand")
   (match_operand:V4DF 1 "nonimmediate_operand")
   (match_operand:V4DF 2 "nonimmediate_operand")]
  "TARGET_AVX"
{
  rtx r1, r2;

  r1 = gen_reg_rtx (V4SImode);
  r2 = gen_reg_rtx (V4SImode);

  emit_insn (gen_fix_truncv4dfv4si2 (r1, operands[1]));
  emit_insn (gen_fix_truncv4dfv4si2 (r2, operands[2]));
  emit_insn (gen_avx_vec_concatv8si (operands[0], r1, r2));
  DONE;
})

(define_expand "vec_pack_sfix_trunc_v2df"
  [(match_operand:V4SI 0 "register_operand")
   (match_operand:V2DF 1 "vector_operand")
   (match_operand:V2DF 2 "vector_operand")]
  "TARGET_SSE2"
{
  rtx tmp0, tmp1, tmp2;

  if (TARGET_AVX && !TARGET_PREFER_AVX128 && optimize_insn_for_speed_p ())
    {
      tmp0 = gen_reg_rtx (V4DFmode);
      tmp1 = force_reg (V2DFmode, operands[1]);

      emit_insn (gen_avx_vec_concatv4df (tmp0, tmp1, operands[2]));
      emit_insn (gen_fix_truncv4dfv4si2 (operands[0], tmp0));
    }
  else
    {
      tmp0 = gen_reg_rtx (V4SImode);
      tmp1 = gen_reg_rtx (V4SImode);
      tmp2 = gen_reg_rtx (V2DImode);

      emit_insn (gen_sse2_cvttpd2dq (tmp0, operands[1]));
      emit_insn (gen_sse2_cvttpd2dq (tmp1, operands[2]));
      emit_insn (gen_vec_interleave_lowv2di (tmp2,
					     gen_lowpart (V2DImode, tmp0),
					     gen_lowpart (V2DImode, tmp1)));
      emit_move_insn (operands[0], gen_lowpart (V4SImode, tmp2));
    }
  DONE;
})

(define_mode_attr ssepackfltmode
  [(V8DF "V16SI") (V4DF "V8SI") (V2DF "V4SI")])

(define_expand "vec_pack_ufix_trunc_<mode>"
  [(match_operand:<ssepackfltmode> 0 "register_operand")
   (match_operand:VF2 1 "register_operand")
   (match_operand:VF2 2 "register_operand")]
  "TARGET_SSE2"
{
  if (<MODE>mode == V8DFmode)
    {
      rtx r1, r2;

      r1 = gen_reg_rtx (V8SImode);
      r2 = gen_reg_rtx (V8SImode);

      emit_insn (gen_fixuns_truncv8dfv8si2 (r1, operands[1]));
      emit_insn (gen_fixuns_truncv8dfv8si2 (r2, operands[2]));
      emit_insn (gen_avx_vec_concatv16si (operands[0], r1, r2));
    }
  else
    {
      rtx tmp[7];
      tmp[0] = ix86_expand_adjust_ufix_to_sfix_si (operands[1], &tmp[2]);
      tmp[1] = ix86_expand_adjust_ufix_to_sfix_si (operands[2], &tmp[3]);
      tmp[4] = gen_reg_rtx (<ssepackfltmode>mode);
      emit_insn (gen_vec_pack_sfix_trunc_<mode> (tmp[4], tmp[0], tmp[1]));
      if (<ssepackfltmode>mode == V4SImode || TARGET_AVX2)
	{
	  tmp[5] = gen_reg_rtx (<ssepackfltmode>mode);
	  ix86_expand_vec_extract_even_odd (tmp[5], tmp[2], tmp[3], 0);
	}
      else
	{
	  tmp[5] = gen_reg_rtx (V8SFmode);
	  ix86_expand_vec_extract_even_odd (tmp[5],
					    gen_lowpart (V8SFmode, tmp[2]),
					    gen_lowpart (V8SFmode, tmp[3]), 0);
	  tmp[5] = gen_lowpart (V8SImode, tmp[5]);
	}
      tmp[6] = expand_simple_binop (<ssepackfltmode>mode, XOR, tmp[4], tmp[5],
				    operands[0], 0, OPTAB_DIRECT);
      if (tmp[6] != operands[0])
	emit_move_insn (operands[0], tmp[6]);
    }

  DONE;
})

(define_expand "avx512f_vec_pack_sfix_v8df"
  [(match_operand:V16SI 0 "register_operand")
   (match_operand:V8DF 1 "nonimmediate_operand")
   (match_operand:V8DF 2 "nonimmediate_operand")]
  "TARGET_AVX512F"
{
  rtx r1, r2;

  r1 = gen_reg_rtx (V8SImode);
  r2 = gen_reg_rtx (V8SImode);

  emit_insn (gen_avx512f_cvtpd2dq512 (r1, operands[1]));
  emit_insn (gen_avx512f_cvtpd2dq512 (r2, operands[2]));
  emit_insn (gen_avx_vec_concatv16si (operands[0], r1, r2));
  DONE;
})

(define_expand "vec_pack_sfix_v4df"
  [(match_operand:V8SI 0 "register_operand")
   (match_operand:V4DF 1 "nonimmediate_operand")
   (match_operand:V4DF 2 "nonimmediate_operand")]
  "TARGET_AVX"
{
  rtx r1, r2;

  r1 = gen_reg_rtx (V4SImode);
  r2 = gen_reg_rtx (V4SImode);

  emit_insn (gen_avx_cvtpd2dq256 (r1, operands[1]));
  emit_insn (gen_avx_cvtpd2dq256 (r2, operands[2]));
  emit_insn (gen_avx_vec_concatv8si (operands[0], r1, r2));
  DONE;
})

(define_expand "vec_pack_sfix_v2df"
  [(match_operand:V4SI 0 "register_operand")
   (match_operand:V2DF 1 "vector_operand")
   (match_operand:V2DF 2 "vector_operand")]
  "TARGET_SSE2"
{
  rtx tmp0, tmp1, tmp2;

  if (TARGET_AVX && !TARGET_PREFER_AVX128 && optimize_insn_for_speed_p ())
    {
      tmp0 = gen_reg_rtx (V4DFmode);
      tmp1 = force_reg (V2DFmode, operands[1]);

      emit_insn (gen_avx_vec_concatv4df (tmp0, tmp1, operands[2]));
      emit_insn (gen_avx_cvtpd2dq256 (operands[0], tmp0));
    }
  else
    {
      tmp0 = gen_reg_rtx (V4SImode);
      tmp1 = gen_reg_rtx (V4SImode);
      tmp2 = gen_reg_rtx (V2DImode);

      emit_insn (gen_sse2_cvtpd2dq (tmp0, operands[1]));
      emit_insn (gen_sse2_cvtpd2dq (tmp1, operands[2]));
      emit_insn (gen_vec_interleave_lowv2di (tmp2,
					     gen_lowpart (V2DImode, tmp0),
					     gen_lowpart (V2DImode, tmp1)));
      emit_move_insn (operands[0], gen_lowpart (V4SImode, tmp2));
    }
  DONE;
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel single-precision floating point element swizzling
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "sse_movhlps_exp"
  [(set (match_operand:V4SF 0 "nonimmediate_operand")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "nonimmediate_operand")
	    (match_operand:V4SF 2 "nonimmediate_operand"))
	  (parallel [(const_int 6)
		     (const_int 7)
		     (const_int 2)
		     (const_int 3)])))]
  "TARGET_SSE"
{
  rtx dst = ix86_fixup_binary_operands (UNKNOWN, V4SFmode, operands);

  emit_insn (gen_sse_movhlps (dst, operands[1], operands[2]));

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);

  DONE;
})

(define_insn "sse_movhlps"
  [(set (match_operand:V4SF 0 "nonimmediate_operand"     "=x,v,x,v,m")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "nonimmediate_operand" " 0,v,0,v,0")
	    (match_operand:V4SF 2 "nonimmediate_operand" " x,v,o,o,v"))
	  (parallel [(const_int 6)
		     (const_int 7)
		     (const_int 2)
		     (const_int 3)])))]
  "TARGET_SSE && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   movhlps\t{%2, %0|%0, %2}
   vmovhlps\t{%2, %1, %0|%0, %1, %2}
   movlps\t{%H2, %0|%0, %H2}
   vmovlps\t{%H2, %1, %0|%0, %1, %H2}
   %vmovhps\t{%2, %0|%q0, %2}"
  [(set_attr "isa" "noavx,avx,noavx,avx,*")
   (set_attr "type" "ssemov")
   (set_attr "prefix" "orig,maybe_evex,orig,maybe_evex,maybe_vex")
   (set_attr "mode" "V4SF,V4SF,V2SF,V2SF,V2SF")])

(define_expand "sse_movlhps_exp"
  [(set (match_operand:V4SF 0 "nonimmediate_operand")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "nonimmediate_operand")
	    (match_operand:V4SF 2 "nonimmediate_operand"))
	  (parallel [(const_int 0)
		     (const_int 1)
		     (const_int 4)
		     (const_int 5)])))]
  "TARGET_SSE"
{
  rtx dst = ix86_fixup_binary_operands (UNKNOWN, V4SFmode, operands);

  emit_insn (gen_sse_movlhps (dst, operands[1], operands[2]));

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);

  DONE;
})

(define_insn "sse_movlhps"
  [(set (match_operand:V4SF 0 "nonimmediate_operand"     "=x,v,x,v,o")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "nonimmediate_operand" " 0,v,0,v,0")
	    (match_operand:V4SF 2 "nonimmediate_operand" " x,v,m,v,v"))
	  (parallel [(const_int 0)
		     (const_int 1)
		     (const_int 4)
		     (const_int 5)])))]
  "TARGET_SSE && ix86_binary_operator_ok (UNKNOWN, V4SFmode, operands)"
  "@
   movlhps\t{%2, %0|%0, %2}
   vmovlhps\t{%2, %1, %0|%0, %1, %2}
   movhps\t{%2, %0|%0, %q2}
   vmovhps\t{%2, %1, %0|%0, %1, %q2}
   %vmovlps\t{%2, %H0|%H0, %2}"
  [(set_attr "isa" "noavx,avx,noavx,avx,*")
   (set_attr "type" "ssemov")
   (set_attr "prefix" "orig,maybe_evex,orig,maybe_evex,maybe_vex")
   (set_attr "mode" "V4SF,V4SF,V2SF,V2SF,V2SF")])

(define_insn "<mask_codefor>avx512f_unpckhps512<mask_name>"
  [(set (match_operand:V16SF 0 "register_operand" "=v")
	(vec_select:V16SF
	  (vec_concat:V32SF
	    (match_operand:V16SF 1 "register_operand" "v")
	    (match_operand:V16SF 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 2) (const_int 18)
		     (const_int 3) (const_int 19)
		     (const_int 6) (const_int 22)
		     (const_int 7) (const_int 23)
		     (const_int 10) (const_int 26)
		     (const_int 11) (const_int 27)
		     (const_int 14) (const_int 30)
		     (const_int 15) (const_int 31)])))]
  "TARGET_AVX512F"
  "vunpckhps\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V16SF")])

;; Recall that the 256-bit unpck insns only shuffle within their lanes.
(define_insn "avx_unpckhps256<mask_name>"
  [(set (match_operand:V8SF 0 "register_operand" "=v")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "register_operand" "v")
	    (match_operand:V8SF 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))]
  "TARGET_AVX && <mask_avx512vl_condition>"
  "vunpckhps\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_expand "vec_interleave_highv8sf"
  [(set (match_dup 3)
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "register_operand")
	    (match_operand:V8SF 2 "nonimmediate_operand"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)])))
   (set (match_dup 4)
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_dup 1)
	    (match_dup 2))
	  (parallel [(const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))
   (set (match_operand:V8SF 0 "register_operand")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_dup 3)
	    (match_dup 4))
	  (parallel [(const_int 4) (const_int 5)
		     (const_int 6) (const_int 7)
		     (const_int 12) (const_int 13)
		     (const_int 14) (const_int 15)])))]
 "TARGET_AVX"
{
  operands[3] = gen_reg_rtx (V8SFmode);
  operands[4] = gen_reg_rtx (V8SFmode);
})

(define_insn "vec_interleave_highv4sf<mask_name>"
  [(set (match_operand:V4SF 0 "register_operand" "=x,v")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "register_operand" "0,v")
	    (match_operand:V4SF 2 "vector_operand" "xBm,vm"))
	  (parallel [(const_int 2) (const_int 6)
		     (const_int 3) (const_int 7)])))]
  "TARGET_SSE && <mask_avx512vl_condition>"
  "@
   unpckhps\t{%2, %0|%0, %2}
   vunpckhps\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "V4SF")])

(define_insn "<mask_codefor>avx512f_unpcklps512<mask_name>"
  [(set (match_operand:V16SF 0 "register_operand" "=v")
	(vec_select:V16SF
	  (vec_concat:V32SF
	    (match_operand:V16SF 1 "register_operand" "v")
	    (match_operand:V16SF 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 0) (const_int 16)
		     (const_int 1) (const_int 17)
		     (const_int 4) (const_int 20)
		     (const_int 5) (const_int 21)
		     (const_int 8) (const_int 24)
		     (const_int 9) (const_int 25)
		     (const_int 12) (const_int 28)
		     (const_int 13) (const_int 29)])))]
  "TARGET_AVX512F"
  "vunpcklps\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V16SF")])

;; Recall that the 256-bit unpck insns only shuffle within their lanes.
(define_insn "avx_unpcklps256<mask_name>"
  [(set (match_operand:V8SF 0 "register_operand" "=v")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "register_operand" "v")
	    (match_operand:V8SF 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)])))]
  "TARGET_AVX && <mask_avx512vl_condition>"
  "vunpcklps\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "unpcklps128_mask"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(vec_merge:V4SF
	  (vec_select:V4SF
	    (vec_concat:V8SF
	      (match_operand:V4SF 1 "register_operand" "v")
	      (match_operand:V4SF 2 "nonimmediate_operand" "vm"))
	    (parallel [(const_int 0) (const_int 4)
		      (const_int 1) (const_int 5)]))
	  (match_operand:V4SF 3 "nonimm_or_0_operand" "0C")
	  (match_operand:QI 4 "register_operand" "Yk")))]
  "TARGET_AVX512VL"
  "vunpcklps\t{%2, %1, %0%{%4%}%N3|%0%{%4%}%N3, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V4SF")])

(define_expand "vec_interleave_lowv8sf"
  [(set (match_dup 3)
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "register_operand")
	    (match_operand:V8SF 2 "nonimmediate_operand"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)])))
   (set (match_dup 4)
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_dup 1)
	    (match_dup 2))
	  (parallel [(const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))
   (set (match_operand:V8SF 0 "register_operand")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_dup 3)
	    (match_dup 4))
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)
		     (const_int 8) (const_int 9)
		     (const_int 10) (const_int 11)])))]
 "TARGET_AVX"
{
  operands[3] = gen_reg_rtx (V8SFmode);
  operands[4] = gen_reg_rtx (V8SFmode);
})

(define_insn "vec_interleave_lowv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=x,v")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "register_operand" "0,v")
	    (match_operand:V4SF 2 "vector_operand" "xBm,vm"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)])))]
  "TARGET_SSE"
  "@
   unpcklps\t{%2, %0|%0, %2}
   vunpcklps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog")
   (set_attr "prefix" "orig,maybe_evex")
   (set_attr "mode" "V4SF")])

;; These are modeled with the same vec_concat as the others so that we
;; capture users of shufps that can use the new instructions
(define_insn "avx_movshdup256<mask_name>"
  [(set (match_operand:V8SF 0 "register_operand" "=v")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "nonimmediate_operand" "vm")
	    (match_dup 1))
	  (parallel [(const_int 1) (const_int 1)
		     (const_int 3) (const_int 3)
		     (const_int 5) (const_int 5)
		     (const_int 7) (const_int 7)])))]
  "TARGET_AVX && <mask_avx512vl_condition>"
  "vmovshdup\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "sse3_movshdup<mask_name>"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "vector_operand" "vBm")
	    (match_dup 1))
	  (parallel [(const_int 1)
		     (const_int 1)
		     (const_int 7)
		     (const_int 7)])))]
  "TARGET_SSE3 && <mask_avx512vl_condition>"
  "%vmovshdup\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V4SF")])

(define_insn "<mask_codefor>avx512f_movshdup512<mask_name>"
  [(set (match_operand:V16SF 0 "register_operand" "=v")
	(vec_select:V16SF
	  (vec_concat:V32SF
	    (match_operand:V16SF 1 "nonimmediate_operand" "vm")
	    (match_dup 1))
	  (parallel [(const_int 1) (const_int 1)
		     (const_int 3) (const_int 3)
		     (const_int 5) (const_int 5)
		     (const_int 7) (const_int 7)
		     (const_int 9) (const_int 9)
		     (const_int 11) (const_int 11)
		     (const_int 13) (const_int 13)
		     (const_int 15) (const_int 15)])))]
  "TARGET_AVX512F"
  "vmovshdup\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V16SF")])

(define_insn "avx_movsldup256<mask_name>"
  [(set (match_operand:V8SF 0 "register_operand" "=v")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "nonimmediate_operand" "vm")
	    (match_dup 1))
	  (parallel [(const_int 0) (const_int 0)
		     (const_int 2) (const_int 2)
		     (const_int 4) (const_int 4)
		     (const_int 6) (const_int 6)])))]
  "TARGET_AVX && <mask_avx512vl_condition>"
  "vmovsldup\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "sse3_movsldup<mask_name>"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "vector_operand" "vBm")
	    (match_dup 1))
	  (parallel [(const_int 0)
		     (const_int 0)
		     (const_int 6)
		     (const_int 6)])))]
  "TARGET_SSE3 && <mask_avx512vl_condition>"
  "%vmovsldup\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V4SF")])

(define_insn "<mask_codefor>avx512f_movsldup512<mask_name>"
  [(set (match_operand:V16SF 0 "register_operand" "=v")
	(vec_select:V16SF
	  (vec_concat:V32SF
	    (match_operand:V16SF 1 "nonimmediate_operand" "vm")
	    (match_dup 1))
	  (parallel [(const_int 0) (const_int 0)
		     (const_int 2) (const_int 2)
		     (const_int 4) (const_int 4)
		     (const_int 6) (const_int 6)
		     (const_int 8) (const_int 8)
		     (const_int 10) (const_int 10)
		     (const_int 12) (const_int 12)
		     (const_int 14) (const_int 14)])))]
  "TARGET_AVX512F"
  "vmovsldup\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V16SF")])

(define_expand "avx_shufps256<mask_expand4_name>"
  [(match_operand:V8SF 0 "register_operand")
   (match_operand:V8SF 1 "register_operand")
   (match_operand:V8SF 2 "nonimmediate_operand")
   (match_operand:SI 3 "const_int_operand")]
  "TARGET_AVX"
{
  int mask = INTVAL (operands[3]);
  emit_insn (gen_avx_shufps256_1<mask_expand4_name> (operands[0],
						     operands[1],
						     operands[2],
						     GEN_INT ((mask >> 0) & 3),
						     GEN_INT ((mask >> 2) & 3),
						     GEN_INT (((mask >> 4) & 3) + 8),
						     GEN_INT (((mask >> 6) & 3) + 8),
						     GEN_INT (((mask >> 0) & 3) + 4),
						     GEN_INT (((mask >> 2) & 3) + 4),
						     GEN_INT (((mask >> 4) & 3) + 12),
						     GEN_INT (((mask >> 6) & 3) + 12)
						     <mask_expand4_args>));
  DONE;
})

;; One bit in mask selects 2 elements.
(define_insn "avx_shufps256_1<mask_name>"
  [(set (match_operand:V8SF 0 "register_operand" "=v")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "register_operand" "v")
	    (match_operand:V8SF 2 "nonimmediate_operand" "vm"))
	  (parallel [(match_operand 3  "const_0_to_3_operand"  )
		     (match_operand 4  "const_0_to_3_operand"  )
		     (match_operand 5  "const_8_to_11_operand" )
		     (match_operand 6  "const_8_to_11_operand" )
		     (match_operand 7  "const_4_to_7_operand"  )
		     (match_operand 8  "const_4_to_7_operand"  )
		     (match_operand 9  "const_12_to_15_operand")
		     (match_operand 10 "const_12_to_15_operand")])))]
  "TARGET_AVX
   && <mask_avx512vl_condition>
   && (INTVAL (operands[3]) == (INTVAL (operands[7]) - 4)
       && INTVAL (operands[4]) == (INTVAL (operands[8]) - 4)
       && INTVAL (operands[5]) == (INTVAL (operands[9]) - 4)
       && INTVAL (operands[6]) == (INTVAL (operands[10]) - 4))"
{
  int mask;
  mask = INTVAL (operands[3]);
  mask |= INTVAL (operands[4]) << 2;
  mask |= (INTVAL (operands[5]) - 8) << 4;
  mask |= (INTVAL (operands[6]) - 8) << 6;
  operands[3] = GEN_INT (mask);

  return "vshufps\t{%3, %2, %1, %0<mask_operand11>|%0<mask_operand11>, %1, %2, %3}";
}
  [(set_attr "type" "sseshuf")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "<mask_prefix>")
   (set_attr "mode" "V8SF")])

(define_expand "sse_shufps<mask_expand4_name>"
  [(match_operand:V4SF 0 "register_operand")
   (match_operand:V4SF 1 "register_operand")
   (match_operand:V4SF 2 "vector_operand")
   (match_operand:SI 3 "const_int_operand")]
  "TARGET_SSE"
{
  int mask = INTVAL (operands[3]);
  emit_insn (gen_sse_shufps_v4sf<mask_expand4_name> (operands[0],
						     operands[1],
						     operands[2],
						     GEN_INT ((mask >> 0) & 3),
						     GEN_INT ((mask >> 2) & 3),
						     GEN_INT (((mask >> 4) & 3) + 4),
						     GEN_INT (((mask >> 6) & 3) + 4)
						     <mask_expand4_args>));
  DONE;
})

(define_insn "sse_shufps_v4sf_mask"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
    (vec_merge:V4SF
	  (vec_select:V4SF
	    (vec_concat:V8SF
	      (match_operand:V4SF 1 "register_operand" "v")
	      (match_operand:V4SF 2 "nonimmediate_operand" "vm"))
	    (parallel [(match_operand 3 "const_0_to_3_operand")
	               (match_operand 4 "const_0_to_3_operand")
	               (match_operand 5 "const_4_to_7_operand")
	               (match_operand 6 "const_4_to_7_operand")]))
      (match_operand:V4SF 7 "nonimm_or_0_operand" "0C")
      (match_operand:QI 8 "register_operand" "Yk")))]
  "TARGET_AVX512VL"
{
  int mask = 0;
  mask |= INTVAL (operands[3]) << 0;
  mask |= INTVAL (operands[4]) << 2;
  mask |= (INTVAL (operands[5]) - 4) << 4;
  mask |= (INTVAL (operands[6]) - 4) << 6;
  operands[3] = GEN_INT (mask);

  return "vshufps\t{%3, %2, %1, %0%{%8%}%N7|%0%{%8%}%N7, %1, %2, %3}";
}
  [(set_attr "type" "sseshuf")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V4SF")])

(define_insn "sse_shufps_<mode>"
  [(set (match_operand:VI4F_128 0 "register_operand" "=x,v")
	(vec_select:VI4F_128
	  (vec_concat:<ssedoublevecmode>
	    (match_operand:VI4F_128 1 "register_operand" "0,v")
	    (match_operand:VI4F_128 2 "vector_operand" "xBm,vm"))
	  (parallel [(match_operand 3 "const_0_to_3_operand")
		     (match_operand 4 "const_0_to_3_operand")
		     (match_operand 5 "const_4_to_7_operand")
		     (match_operand 6 "const_4_to_7_operand")])))]
  "TARGET_SSE"
{
  int mask = 0;
  mask |= INTVAL (operands[3]) << 0;
  mask |= INTVAL (operands[4]) << 2;
  mask |= (INTVAL (operands[5]) - 4) << 4;
  mask |= (INTVAL (operands[6]) - 4) << 6;
  operands[3] = GEN_INT (mask);

  switch (which_alternative)
    {
    case 0:
      return "shufps\t{%3, %2, %0|%0, %2, %3}";
    case 1:
      return "vshufps\t{%3, %2, %1, %0|%0, %1, %2, %3}";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseshuf")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "orig,maybe_evex")
   (set_attr "mode" "V4SF")])

(define_insn "sse_storehps"
  [(set (match_operand:V2SF 0 "nonimmediate_operand" "=m,v,v")
	(vec_select:V2SF
	  (match_operand:V4SF 1 "nonimmediate_operand" "v,v,o")
	  (parallel [(const_int 2) (const_int 3)])))]
  "TARGET_SSE && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   %vmovhps\t{%1, %0|%q0, %1}
   %vmovhlps\t{%1, %d0|%d0, %1}
   %vmovlps\t{%H1, %d0|%d0, %H1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V2SF,V4SF,V2SF")])

(define_expand "sse_loadhps_exp"
  [(set (match_operand:V4SF 0 "nonimmediate_operand")
	(vec_concat:V4SF
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "nonimmediate_operand")
	    (parallel [(const_int 0) (const_int 1)]))
	  (match_operand:V2SF 2 "nonimmediate_operand")))]
  "TARGET_SSE"
{
  rtx dst = ix86_fixup_binary_operands (UNKNOWN, V4SFmode, operands);

  emit_insn (gen_sse_loadhps (dst, operands[1], operands[2]));

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);

  DONE;
})

(define_insn "sse_loadhps"
  [(set (match_operand:V4SF 0 "nonimmediate_operand"     "=x,v,x,v,o")
	(vec_concat:V4SF
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "nonimmediate_operand" " 0,v,0,v,0")
	    (parallel [(const_int 0) (const_int 1)]))
	  (match_operand:V2SF 2 "nonimmediate_operand"   " m,m,x,v,v")))]
  "TARGET_SSE"
  "@
   movhps\t{%2, %0|%0, %q2}
   vmovhps\t{%2, %1, %0|%0, %1, %q2}
   movlhps\t{%2, %0|%0, %2}
   vmovlhps\t{%2, %1, %0|%0, %1, %2}
   %vmovlps\t{%2, %H0|%H0, %2}"
  [(set_attr "isa" "noavx,avx,noavx,avx,*")
   (set_attr "type" "ssemov")
   (set_attr "prefix" "orig,maybe_evex,orig,maybe_evex,maybe_vex")
   (set_attr "mode" "V2SF,V2SF,V4SF,V4SF,V2SF")])

(define_insn "sse_storelps"
  [(set (match_operand:V2SF 0 "nonimmediate_operand"   "=m,v,v")
	(vec_select:V2SF
	  (match_operand:V4SF 1 "nonimmediate_operand" " v,v,m")
	  (parallel [(const_int 0) (const_int 1)])))]
  "TARGET_SSE && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   %vmovlps\t{%1, %0|%q0, %1}
   %vmovaps\t{%1, %0|%0, %1}
   %vmovlps\t{%1, %d0|%d0, %q1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V2SF,V4SF,V2SF")])

(define_expand "sse_loadlps_exp"
  [(set (match_operand:V4SF 0 "nonimmediate_operand")
	(vec_concat:V4SF
	  (match_operand:V2SF 2 "nonimmediate_operand")
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "nonimmediate_operand")
	    (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_SSE"
{
  rtx dst = ix86_fixup_binary_operands (UNKNOWN, V4SFmode, operands);

  emit_insn (gen_sse_loadlps (dst, operands[1], operands[2]));

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);

  DONE;
})

(define_insn "sse_loadlps"
  [(set (match_operand:V4SF 0 "nonimmediate_operand"     "=x,v,x,v,m")
	(vec_concat:V4SF
	  (match_operand:V2SF 2 "nonimmediate_operand"   " 0,v,m,m,v")
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "nonimmediate_operand" " x,v,0,v,0")
	    (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_SSE"
  "@
   shufps\t{$0xe4, %1, %0|%0, %1, 0xe4}
   vshufps\t{$0xe4, %1, %2, %0|%0, %2, %1, 0xe4}
   movlps\t{%2, %0|%0, %q2}
   vmovlps\t{%2, %1, %0|%0, %1, %q2}
   %vmovlps\t{%2, %0|%q0, %2}"
  [(set_attr "isa" "noavx,avx,noavx,avx,*")
   (set_attr "type" "sseshuf,sseshuf,ssemov,ssemov,ssemov")
   (set (attr "length_immediate")
     (if_then_else (eq_attr "alternative" "0,1")
		   (const_string "1")
		   (const_string "*")))
   (set_attr "prefix" "orig,maybe_evex,orig,maybe_evex,maybe_vex")
   (set_attr "mode" "V4SF,V4SF,V2SF,V2SF,V2SF")])

(define_insn "sse_movss"
  [(set (match_operand:V4SF 0 "register_operand"   "=x,v")
	(vec_merge:V4SF
	  (match_operand:V4SF 2 "register_operand" " x,v")
	  (match_operand:V4SF 1 "register_operand" " 0,v")
	  (const_int 1)))]
  "TARGET_SSE"
  "@
   movss\t{%2, %0|%0, %2}
   vmovss\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "ssemov")
   (set_attr "prefix" "orig,maybe_evex")
   (set_attr "mode" "SF")])

(define_insn "avx2_vec_dup<mode>"
  [(set (match_operand:VF1_128_256 0 "register_operand" "=v")
	(vec_duplicate:VF1_128_256
	  (vec_select:SF
	    (match_operand:V4SF 1 "register_operand" "v")
	    (parallel [(const_int 0)]))))]
  "TARGET_AVX2"
  "vbroadcastss\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog1")
    (set_attr "prefix" "maybe_evex")
    (set_attr "mode" "<MODE>")])

(define_insn "avx2_vec_dupv8sf_1"
  [(set (match_operand:V8SF 0 "register_operand" "=v")
	(vec_duplicate:V8SF
	  (vec_select:SF
	    (match_operand:V8SF 1 "register_operand" "v")
	    (parallel [(const_int 0)]))))]
  "TARGET_AVX2"
  "vbroadcastss\t{%x1, %0|%0, %x1}"
  [(set_attr "type" "sselog1")
    (set_attr "prefix" "maybe_evex")
    (set_attr "mode" "V8SF")])

(define_insn "avx512f_vec_dup<mode>_1"
  [(set (match_operand:VF_512 0 "register_operand" "=v")
	(vec_duplicate:VF_512
	  (vec_select:<ssescalarmode>
	    (match_operand:VF_512 1 "register_operand" "v")
	    (parallel [(const_int 0)]))))]
  "TARGET_AVX512F"
  "vbroadcast<bcstscalarsuff>\t{%x1, %0|%0, %x1}"
  [(set_attr "type" "sselog1")
    (set_attr "prefix" "evex")
    (set_attr "mode" "<MODE>")])

;; Although insertps takes register source, we prefer
;; unpcklps with register source since it is shorter.
(define_insn "*vec_concatv2sf_sse4_1"
  [(set (match_operand:V2SF 0 "register_operand"
	  "=Yr,*x, v,Yr,*x,v,v,*y ,*y")
	(vec_concat:V2SF
	  (match_operand:SF 1 "nonimmediate_operand"
	  "  0, 0,Yv, 0,0, v,m, 0 , m")
	  (match_operand:SF 2 "nonimm_or_0_operand"
	  " Yr,*x,Yv, m,m, m,C,*ym, C")))]
  "TARGET_SSE4_1 && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   unpcklps\t{%2, %0|%0, %2}
   unpcklps\t{%2, %0|%0, %2}
   vunpcklps\t{%2, %1, %0|%0, %1, %2}
   insertps\t{$0x10, %2, %0|%0, %2, 0x10}
   insertps\t{$0x10, %2, %0|%0, %2, 0x10}
   vinsertps\t{$0x10, %2, %1, %0|%0, %1, %2, 0x10}
   %vmovss\t{%1, %0|%0, %1}
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}"
  [(set (attr "isa")
     (cond [(eq_attr "alternative" "0,1,3,4")
	      (const_string "noavx")
	    (eq_attr "alternative" "2,5")
	      (const_string "avx")
	   ]
	   (const_string "*")))
   (set (attr "type")
     (cond [(eq_attr "alternative" "6")
	      (const_string "ssemov")
	    (eq_attr "alternative" "7")
	      (const_string "mmxcvt")
	    (eq_attr "alternative" "8")
	      (const_string "mmxmov")
	   ]
	   (const_string "sselog")))
   (set (attr "prefix_data16")
     (if_then_else (eq_attr "alternative" "3,4")
		   (const_string "1")
		   (const_string "*")))
   (set (attr "prefix_extra")
     (if_then_else (eq_attr "alternative" "3,4,5")
		   (const_string "1")
		   (const_string "*")))
   (set (attr "length_immediate")
     (if_then_else (eq_attr "alternative" "3,4,5")
		   (const_string "1")
		   (const_string "*")))
   (set (attr "prefix")
     (cond [(eq_attr "alternative" "2,5")
	      (const_string "maybe_evex")
	    (eq_attr "alternative" "6")
	      (const_string "maybe_vex")
	   ]
	   (const_string "orig")))
   (set_attr "mode" "V4SF,V4SF,V4SF,V4SF,V4SF,V4SF,SF,DI,DI")])

;; ??? In theory we can match memory for the MMX alternative, but allowing
;; vector_operand for operand 2 and *not* allowing memory for the SSE
;; alternatives pretty much forces the MMX alternative to be chosen.
(define_insn "*vec_concatv2sf_sse"
  [(set (match_operand:V2SF 0 "register_operand"     "=x,x,*y,*y")
	(vec_concat:V2SF
	  (match_operand:SF 1 "nonimmediate_operand" " 0,m, 0, m")
	  (match_operand:SF 2 "reg_or_0_operand"     " x,C,*y, C")))]
  "TARGET_SSE"
  "@
   unpcklps\t{%2, %0|%0, %2}
   movss\t{%1, %0|%0, %1}
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog,ssemov,mmxcvt,mmxmov")
   (set_attr "mode" "V4SF,SF,DI,DI")])

(define_insn "*vec_concatv4sf"
  [(set (match_operand:V4SF 0 "register_operand"       "=x,v,x,v")
	(vec_concat:V4SF
	  (match_operand:V2SF 1 "register_operand"     " 0,v,0,v")
	  (match_operand:V2SF 2 "nonimmediate_operand" " x,v,m,m")))]
  "TARGET_SSE"
  "@
   movlhps\t{%2, %0|%0, %2}
   vmovlhps\t{%2, %1, %0|%0, %1, %2}
   movhps\t{%2, %0|%0, %q2}
   vmovhps\t{%2, %1, %0|%0, %1, %q2}"
  [(set_attr "isa" "noavx,avx,noavx,avx")
   (set_attr "type" "ssemov")
   (set_attr "prefix" "orig,maybe_evex,orig,maybe_evex")
   (set_attr "mode" "V4SF,V4SF,V2SF,V2SF")])

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "vec_set<mode>_0"
  [(set (match_operand:VI4F_128 0 "nonimmediate_operand"
	  "=Yr,*x,v,v,v,x,x,v,Yr ,*x ,x  ,m ,m   ,m")
	(vec_merge:VI4F_128
	  (vec_duplicate:VI4F_128
	    (match_operand:<ssescalarmode> 2 "general_operand"
	  " Yr,*x,v,m,r ,m,x,v,*rm,*rm,*rm,!x,!*re,!*fF"))
	  (match_operand:VI4F_128 1 "nonimm_or_0_operand"
	  " C , C,C,C,C ,C,0,v,0  ,0  ,x  ,0 ,0   ,0")
	  (const_int 1)))]
  "TARGET_SSE"
  "@
   insertps\t{$0xe, %2, %0|%0, %2, 0xe}
   insertps\t{$0xe, %2, %0|%0, %2, 0xe}
   vinsertps\t{$0xe, %2, %2, %0|%0, %2, %2, 0xe}
   %vmov<ssescalarmodesuffix>\t{%2, %0|%0, %2}
   %vmovd\t{%2, %0|%0, %2}
   movss\t{%2, %0|%0, %2}
   movss\t{%2, %0|%0, %2}
   vmovss\t{%2, %1, %0|%0, %1, %2}
   pinsrd\t{$0, %2, %0|%0, %2, 0}
   pinsrd\t{$0, %2, %0|%0, %2, 0}
   vpinsrd\t{$0, %2, %1, %0|%0, %1, %2, 0}
   #
   #
   #"
  [(set (attr "isa")
     (cond [(eq_attr "alternative" "0,1,8,9")
	      (const_string "sse4_noavx")
	    (eq_attr "alternative" "2,7,10")
	      (const_string "avx")
	    (eq_attr "alternative" "3,4")
	      (const_string "sse2")
	    (eq_attr "alternative" "5,6")
	      (const_string "noavx")
	   ]
	   (const_string "*")))
   (set (attr "type")
     (cond [(eq_attr "alternative" "0,1,2,8,9,10")
	      (const_string "sselog")
	    (eq_attr "alternative" "12")
	      (const_string "imov")
	    (eq_attr "alternative" "13")
	      (const_string "fmov")
	   ]
	   (const_string "ssemov")))
   (set (attr "prefix_extra")
     (if_then_else (eq_attr "alternative" "8,9,10")
		   (const_string "1")
		   (const_string "*")))
   (set (attr "length_immediate")
     (if_then_else (eq_attr "alternative" "8,9,10")
		   (const_string "1")
		   (const_string "*")))
   (set (attr "prefix")
     (cond [(eq_attr "alternative" "0,1,5,6,8,9")
	      (const_string "orig")
	    (eq_attr "alternative" "2")
	      (const_string "maybe_evex")
	    (eq_attr "alternative" "3,4")
	      (const_string "maybe_vex")
	    (eq_attr "alternative" "7,10")
	      (const_string "vex")
	   ]
	   (const_string "*")))
   (set_attr "mode" "SF,SF,SF,<ssescalarmode>,SI,SF,SF,SF,TI,TI,TI,*,*,*")
   (set (attr "preferred_for_speed")
     (cond [(eq_attr "alternative" "4")
	      (symbol_ref "TARGET_INTER_UNIT_MOVES_TO_VEC")
	   ]
	   (symbol_ref "true")))])

;; A subset is vec_setv4sf.
(define_insn "*vec_setv4sf_sse4_1"
  [(set (match_operand:V4SF 0 "register_operand" "=Yr,*x,v")
	(vec_merge:V4SF
	  (vec_duplicate:V4SF
	    (match_operand:SF 2 "nonimmediate_operand" "Yrm,*xm,vm"))
	  (match_operand:V4SF 1 "register_operand" "0,0,v")
	  (match_operand:SI 3 "const_int_operand")))]
  "TARGET_SSE4_1
   && ((unsigned) exact_log2 (INTVAL (operands[3]))
       < GET_MODE_NUNITS (V4SFmode))"
{
  operands[3] = GEN_INT (exact_log2 (INTVAL (operands[3])) << 4);
  switch (which_alternative)
    {
    case 0:
    case 1:
      return "insertps\t{%3, %2, %0|%0, %2, %3}";
    case 2:
      return "vinsertps\t{%3, %2, %1, %0|%0, %1, %2, %3}";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "sselog")
   (set_attr "prefix_data16" "1,1,*")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "orig,orig,maybe_evex")
   (set_attr "mode" "V4SF")])

;; All of vinsertps, vmovss, vmovd clear also the higher bits.
(define_insn "vec_set<mode>_0"
  [(set (match_operand:VI4F_256_512 0 "register_operand" "=v,v,v")
	(vec_merge:VI4F_256_512
	  (vec_duplicate:VI4F_256_512
	    (match_operand:<ssescalarmode> 2 "general_operand" "v,m,r"))
	  (match_operand:VI4F_256_512 1 "const0_operand" "C,C,C")
	  (const_int 1)))]
  "TARGET_AVX"
  "@
   vinsertps\t{$0xe, %2, %2, %x0|%x0, %2, %2, 0xe}
   vmov<ssescalarmodesuffix>\t{%x2, %x0|%x0, %2}
   vmovd\t{%2, %x0|%x0, %2}"
  [(set (attr "type")
     (if_then_else (eq_attr "alternative" "0")
		   (const_string "sselog")
		   (const_string "ssemov")))
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "SF,<ssescalarmode>,SI")
   (set (attr "preferred_for_speed")
     (cond [(eq_attr "alternative" "2")
	      (symbol_ref "TARGET_INTER_UNIT_MOVES_TO_VEC")
	   ]
	   (symbol_ref "true")))])

(define_insn "sse4_1_insertps"
  [(set (match_operand:V4SF 0 "register_operand" "=Yr,*x,v")
	(unspec:V4SF [(match_operand:V4SF 2 "nonimmediate_operand" "Yrm,*xm,vm")
		      (match_operand:V4SF 1 "register_operand" "0,0,v")
		      (match_operand:SI 3 "const_0_to_255_operand" "n,n,n")]
		     UNSPEC_INSERTPS))]
  "TARGET_SSE4_1"
{
  if (MEM_P (operands[2]))
    {
      unsigned count_s = INTVAL (operands[3]) >> 6;
      if (count_s)
	operands[3] = GEN_INT (INTVAL (operands[3]) & 0x3f);
      operands[2] = adjust_address_nv (operands[2], SFmode, count_s * 4);
    }
  switch (which_alternative)
    {
    case 0:
    case 1:
      return "insertps\t{%3, %2, %0|%0, %2, %3}";
    case 2:
      return "vinsertps\t{%3, %2, %1, %0|%0, %1, %2, %3}";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "sselog")
   (set_attr "prefix_data16" "1,1,*")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "orig,orig,maybe_evex")
   (set_attr "mode" "V4SF")])

(define_split
  [(set (match_operand:VI4F_128 0 "memory_operand")
	(vec_merge:VI4F_128
	  (vec_duplicate:VI4F_128
	    (match_operand:<ssescalarmode> 1 "nonmemory_operand"))
	  (match_dup 0)
	  (const_int 1)))]
  "TARGET_SSE && reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  "operands[0] = adjust_address (operands[0], <ssescalarmode>mode, 0);")

(define_expand "vec_set<mode>"
  [(match_operand:V 0 "register_operand")
   (match_operand:<ssescalarmode> 1 "register_operand")
   (match_operand 2 "const_int_operand")]
  "TARGET_SSE"
{
  ix86_expand_vector_set (false, operands[0], operands[1],
			  INTVAL (operands[2]));
  DONE;
})

(define_insn_and_split "*vec_extractv4sf_0"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=v,m,f,r")
	(vec_select:SF
	  (match_operand:V4SF 1 "nonimmediate_operand" "vm,v,m,m")
	  (parallel [(const_int 0)])))]
  "TARGET_SSE && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  "operands[1] = gen_lowpart (SFmode, operands[1]);")

(define_insn_and_split "*sse4_1_extractps"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=rm,rm,rm,Yv,Yv")
	(vec_select:SF
	  (match_operand:V4SF 1 "register_operand" "Yr,*x,v,0,v")
	  (parallel [(match_operand:SI 2 "const_0_to_3_operand" "n,n,n,n,n")])))]
  "TARGET_SSE4_1"
  "@
   extractps\t{%2, %1, %0|%0, %1, %2}
   extractps\t{%2, %1, %0|%0, %1, %2}
   vextractps\t{%2, %1, %0|%0, %1, %2}
   #
   #"
  "&& reload_completed && SSE_REG_P (operands[0])"
  [(const_int 0)]
{
  rtx dest = lowpart_subreg (V4SFmode, operands[0], SFmode);
  switch (INTVAL (operands[2]))
    {
    case 1:
    case 3:
      emit_insn (gen_sse_shufps_v4sf (dest, operands[1], operands[1],
				      operands[2], operands[2],
				      GEN_INT (INTVAL (operands[2]) + 4),
				      GEN_INT (INTVAL (operands[2]) + 4)));
      break;
    case 2:
      emit_insn (gen_vec_interleave_highv4sf (dest, operands[1], operands[1]));
      break;
    default:
      /* 0 should be handled by the *vec_extractv4sf_0 pattern above.  */
      gcc_unreachable ();
    }
  DONE;
}
  [(set_attr "isa" "noavx,noavx,avx,noavx,avx")
   (set_attr "type" "sselog,sselog,sselog,*,*")
   (set_attr "prefix_data16" "1,1,1,*,*")
   (set_attr "prefix_extra" "1,1,1,*,*")
   (set_attr "length_immediate" "1,1,1,*,*")
   (set_attr "prefix" "orig,orig,maybe_evex,*,*")
   (set_attr "mode" "V4SF,V4SF,V4SF,*,*")])

(define_insn_and_split "*vec_extractv4sf_mem"
  [(set (match_operand:SF 0 "register_operand" "=v,*r,f")
	(vec_select:SF
	  (match_operand:V4SF 1 "memory_operand" "o,o,o")
	  (parallel [(match_operand 2 "const_0_to_3_operand" "n,n,n")])))]
  "TARGET_SSE"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 1))]
{
  operands[1] = adjust_address (operands[1], SFmode, INTVAL (operands[2]) * 4);
})

(define_mode_attr extract_type
  [(V16SF "avx512f") (V16SI "avx512f") (V8DF "avx512dq") (V8DI "avx512dq")])

(define_mode_attr extract_suf
  [(V16SF "32x4") (V16SI "32x4") (V8DF "64x2") (V8DI "64x2")])

(define_mode_iterator AVX512_VEC
  [(V8DF "TARGET_AVX512DQ") (V8DI "TARGET_AVX512DQ") V16SF V16SI])

(define_expand "<extract_type>_vextract<shuffletype><extract_suf>_mask"
  [(match_operand:<ssequartermode> 0 "nonimmediate_operand")
   (match_operand:AVX512_VEC 1 "register_operand")
   (match_operand:SI 2 "const_0_to_3_operand")
   (match_operand:<ssequartermode> 3 "nonimmediate_operand")
   (match_operand:QI 4 "register_operand")]
  "TARGET_AVX512F"
{
  int mask;
  mask = INTVAL (operands[2]);
  rtx dest = operands[0];

  if (MEM_P (operands[0]) && !rtx_equal_p (operands[0], operands[3]))
    dest = gen_reg_rtx (<ssequartermode>mode);

  if (<MODE>mode == V16SImode || <MODE>mode == V16SFmode)
    emit_insn (gen_avx512f_vextract<shuffletype>32x4_1_mask (dest,
        operands[1], GEN_INT (mask * 4), GEN_INT (mask * 4 + 1),
	GEN_INT (mask * 4 + 2), GEN_INT (mask * 4 + 3), operands[3],
	operands[4]));
  else
    emit_insn (gen_avx512dq_vextract<shuffletype>64x2_1_mask (dest,
        operands[1], GEN_INT (mask * 2), GEN_INT (mask * 2 + 1), operands[3],
	operands[4]));
  if (dest != operands[0])
    emit_move_insn (operands[0], dest);
  DONE;
})

(define_insn "avx512dq_vextract<shuffletype>64x2_1_maskm"
  [(set (match_operand:<ssequartermode> 0 "memory_operand" "=m")
	(vec_merge:<ssequartermode>
	  (vec_select:<ssequartermode>
	    (match_operand:V8FI 1 "register_operand" "v")
	    (parallel [(match_operand 2  "const_0_to_7_operand")
	      (match_operand 3  "const_0_to_7_operand")]))
	  (match_operand:<ssequartermode> 4 "memory_operand" "0")
	  (match_operand:QI 5 "register_operand" "Yk")))]
  "TARGET_AVX512DQ
   && INTVAL (operands[2]) % 2 == 0
   && INTVAL (operands[2]) == INTVAL (operands[3]) - 1
   && rtx_equal_p (operands[4], operands[0])"
{
  operands[2] = GEN_INT ((INTVAL (operands[2])) >> 1);
  return "vextract<shuffletype>64x2\t{%2, %1, %0%{%5%}|%0%{%5%}, %1, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "avx512f_vextract<shuffletype>32x4_1_maskm"
  [(set (match_operand:<ssequartermode> 0 "memory_operand" "=m")
	(vec_merge:<ssequartermode>
	  (vec_select:<ssequartermode>
	    (match_operand:V16FI 1 "register_operand" "v")
	    (parallel [(match_operand 2  "const_0_to_15_operand")
	      (match_operand 3  "const_0_to_15_operand")
	      (match_operand 4  "const_0_to_15_operand")
	      (match_operand 5  "const_0_to_15_operand")]))
	  (match_operand:<ssequartermode> 6 "memory_operand" "0")
	  (match_operand:QI 7 "register_operand" "Yk")))]
  "TARGET_AVX512F
   && INTVAL (operands[2]) % 4 == 0
   && INTVAL (operands[2]) == INTVAL (operands[3]) - 1
   && INTVAL (operands[3]) == INTVAL (operands[4]) - 1
   && INTVAL (operands[4]) == INTVAL (operands[5]) - 1
   && rtx_equal_p (operands[6], operands[0])"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) >> 2);
  return "vextract<shuffletype>32x4\t{%2, %1, %0%{%7%}|%0%{%7%}, %1, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<mask_codefor>avx512dq_vextract<shuffletype>64x2_1<mask_name>"
  [(set (match_operand:<ssequartermode> 0 "<store_mask_predicate>" "=<store_mask_constraint>")
	(vec_select:<ssequartermode>
	  (match_operand:V8FI 1 "register_operand" "v")
	  (parallel [(match_operand 2  "const_0_to_7_operand")
            (match_operand 3  "const_0_to_7_operand")])))]
  "TARGET_AVX512DQ
   && INTVAL (operands[2]) % 2 == 0
   && INTVAL (operands[2]) == INTVAL (operands[3]) - 1"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) >> 1);
  return "vextract<shuffletype>64x2\t{%2, %1, %0<mask_operand4>|%0<mask_operand4>, %1, %2}";
}
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_split
  [(set (match_operand:<ssequartermode> 0 "nonimmediate_operand")
	(vec_select:<ssequartermode>
	  (match_operand:V8FI 1 "register_operand")
	  (parallel [(const_int 0) (const_int 1)])))]
  "TARGET_AVX512DQ
   && reload_completed
   && (TARGET_AVX512VL
       || REG_P (operands[0])
       || !EXT_REX_SSE_REG_P (operands[1]))"
  [(set (match_dup 0) (match_dup 1))]
{
  if (!TARGET_AVX512VL
      && REG_P (operands[0])
      && EXT_REX_SSE_REG_P (operands[1]))
    operands[0]
      = lowpart_subreg (<MODE>mode, operands[0], <ssequartermode>mode);
  else
    operands[1] = gen_lowpart (<ssequartermode>mode, operands[1]);
})

(define_insn "<mask_codefor>avx512f_vextract<shuffletype>32x4_1<mask_name>"
  [(set (match_operand:<ssequartermode> 0 "<store_mask_predicate>" "=<store_mask_constraint>")
	(vec_select:<ssequartermode>
	  (match_operand:V16FI 1 "register_operand" "v")
	  (parallel [(match_operand 2  "const_0_to_15_operand")
            (match_operand 3  "const_0_to_15_operand")
            (match_operand 4  "const_0_to_15_operand")
            (match_operand 5  "const_0_to_15_operand")])))]
  "TARGET_AVX512F
   && INTVAL (operands[2]) % 4 == 0
   && INTVAL (operands[2]) == INTVAL (operands[3]) - 1
   && INTVAL (operands[3]) == INTVAL (operands[4]) - 1
   && INTVAL (operands[4]) == INTVAL (operands[5]) - 1"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) >> 2);
  return "vextract<shuffletype>32x4\t{%2, %1, %0<mask_operand6>|%0<mask_operand6>, %1, %2}";
}
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_split
  [(set (match_operand:<ssequartermode> 0 "nonimmediate_operand")
	(vec_select:<ssequartermode>
	  (match_operand:V16FI 1 "register_operand")
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)])))]
  "TARGET_AVX512F
   && reload_completed
   && (TARGET_AVX512VL
       || REG_P (operands[0])
       || !EXT_REX_SSE_REG_P (operands[1]))"
  [(set (match_dup 0) (match_dup 1))]
{
  if (!TARGET_AVX512VL
      && REG_P (operands[0])
      && EXT_REX_SSE_REG_P (operands[1]))
    operands[0]
      = lowpart_subreg (<MODE>mode, operands[0], <ssequartermode>mode);
  else
    operands[1] = gen_lowpart (<ssequartermode>mode, operands[1]);
})

(define_mode_attr extract_type_2
  [(V16SF "avx512dq") (V16SI "avx512dq") (V8DF "avx512f") (V8DI "avx512f")])

(define_mode_attr extract_suf_2
  [(V16SF "32x8") (V16SI "32x8") (V8DF "64x4") (V8DI "64x4")])

(define_mode_iterator AVX512_VEC_2
  [(V16SF "TARGET_AVX512DQ") (V16SI "TARGET_AVX512DQ") V8DF V8DI])

(define_expand "<extract_type_2>_vextract<shuffletype><extract_suf_2>_mask"
  [(match_operand:<ssehalfvecmode> 0 "nonimmediate_operand")
   (match_operand:AVX512_VEC_2 1 "register_operand")
   (match_operand:SI 2 "const_0_to_1_operand")
   (match_operand:<ssehalfvecmode> 3 "nonimmediate_operand")
   (match_operand:QI 4 "register_operand")]
  "TARGET_AVX512F"
{
  rtx (*insn)(rtx, rtx, rtx, rtx);
  rtx dest = operands[0];

  if (MEM_P (dest) && !rtx_equal_p (dest, operands[3]))
    dest = gen_reg_rtx (<ssehalfvecmode>mode);

  switch (INTVAL (operands[2]))
    {
    case 0:
      insn = gen_vec_extract_lo_<mode>_mask;
      break;
    case 1:
      insn = gen_vec_extract_hi_<mode>_mask;
      break;
    default:
      gcc_unreachable ();
    }

  emit_insn (insn (dest, operands[1], operands[3], operands[4]));
  if (dest != operands[0])
    emit_move_insn (operands[0], dest);
  DONE;
})

(define_split
  [(set (match_operand:<ssehalfvecmode> 0 "nonimmediate_operand")
	(vec_select:<ssehalfvecmode>
	  (match_operand:V8FI 1 "nonimmediate_operand")
	  (parallel [(const_int 0) (const_int 1)
            (const_int 2) (const_int 3)])))]
  "TARGET_AVX512F && !(MEM_P (operands[0]) && MEM_P (operands[1]))
   && reload_completed
   && (TARGET_AVX512VL
       || (REG_P (operands[0]) && !EXT_REX_SSE_REG_P (operands[1])))"
  [(set (match_dup 0) (match_dup 1))]
  "operands[1] = gen_lowpart (<ssehalfvecmode>mode, operands[1]);")

(define_insn "vec_extract_lo_<mode>_maskm"
  [(set (match_operand:<ssehalfvecmode> 0 "memory_operand" "=m")
	(vec_merge:<ssehalfvecmode>
	  (vec_select:<ssehalfvecmode>
	    (match_operand:V8FI 1 "register_operand" "v")
	    (parallel [(const_int 0) (const_int 1)
	      (const_int 2) (const_int 3)]))
	  (match_operand:<ssehalfvecmode> 2 "memory_operand" "0")
	  (match_operand:QI 3 "register_operand" "Yk")))]
  "TARGET_AVX512F
   && rtx_equal_p (operands[2], operands[0])"
  "vextract<shuffletype>64x4\t{$0x0, %1, %0%{%3%}|%0%{%3%}, %1, 0x0}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vec_extract_lo_<mode><mask_name>"
  [(set (match_operand:<ssehalfvecmode> 0 "<store_mask_predicate>" "=v,<store_mask_constraint>,v")
	(vec_select:<ssehalfvecmode>
	  (match_operand:V8FI 1 "<store_mask_predicate>" "v,v,<store_mask_constraint>")
	  (parallel [(const_int 0) (const_int 1)
            (const_int 2) (const_int 3)])))]
  "TARGET_AVX512F
   && (<mask_applied> || !(MEM_P (operands[0]) && MEM_P (operands[1])))"
{
  if (<mask_applied> || (!TARGET_AVX512VL && !MEM_P (operands[1])))
    return "vextract<shuffletype>64x4\t{$0x0, %1, %0<mask_operand2>|%0<mask_operand2>, %1, 0x0}";
  else
    return "#";
}
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,store,load")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vec_extract_hi_<mode>_maskm"
  [(set (match_operand:<ssehalfvecmode> 0 "memory_operand" "=m")
	(vec_merge:<ssehalfvecmode>
	  (vec_select:<ssehalfvecmode>
	    (match_operand:V8FI 1 "register_operand" "v")
	    (parallel [(const_int 4) (const_int 5)
	      (const_int 6) (const_int 7)]))
	  (match_operand:<ssehalfvecmode> 2 "memory_operand" "0")
	  (match_operand:QI 3 "register_operand" "Yk")))]
  "TARGET_AVX512F
   && rtx_equal_p (operands[2], operands[0])"
  "vextract<shuffletype>64x4\t{$0x1, %1, %0%{%3%}|%0%{%3%}, %1, 0x1}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vec_extract_hi_<mode><mask_name>"
  [(set (match_operand:<ssehalfvecmode> 0 "<store_mask_predicate>" "=<store_mask_constraint>")
	(vec_select:<ssehalfvecmode>
	  (match_operand:V8FI 1 "register_operand" "v")
	  (parallel [(const_int 4) (const_int 5)
            (const_int 6) (const_int 7)])))]
  "TARGET_AVX512F"
  "vextract<shuffletype>64x4\t{$0x1, %1, %0<mask_operand2>|%0<mask_operand2>, %1, 0x1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vec_extract_hi_<mode>_maskm"
   [(set (match_operand:<ssehalfvecmode> 0 "memory_operand" "=m")
	(vec_merge:<ssehalfvecmode>
	  (vec_select:<ssehalfvecmode>
	    (match_operand:V16FI 1 "register_operand" "v")
	    (parallel [(const_int 8) (const_int 9)
	      (const_int 10) (const_int 11)
	      (const_int 12) (const_int 13)
	      (const_int 14) (const_int 15)]))
	  (match_operand:<ssehalfvecmode> 2 "memory_operand" "0")
	  (match_operand:QI 3 "register_operand" "Yk")))]
  "TARGET_AVX512DQ
   && rtx_equal_p (operands[2], operands[0])"
  "vextract<shuffletype>32x8\t{$0x1, %1, %0%{%3%}|%0%{%3%}, %1, 0x1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vec_extract_hi_<mode><mask_name>"
  [(set (match_operand:<ssehalfvecmode> 0 "<store_mask_predicate>" "=<store_mask_constraint>,vm")
	(vec_select:<ssehalfvecmode>
	  (match_operand:V16FI 1 "register_operand" "v,v")
	  (parallel [(const_int 8) (const_int 9)
            (const_int 10) (const_int 11)
	    (const_int 12) (const_int 13)
	    (const_int 14) (const_int 15)])))]
  "TARGET_AVX512F && <mask_avx512dq_condition>"
  "@
   vextract<shuffletype>32x8\t{$0x1, %1, %0<mask_operand2>|%0<mask_operand2>, %1, 0x1}
   vextracti64x4\t{$0x1, %1, %0|%0, %1, 0x1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "isa" "avx512dq,noavx512dq")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "avx512vl_vextractf128<mode>"
  [(match_operand:<ssehalfvecmode> 0 "nonimmediate_operand")
   (match_operand:VI48F_256 1 "register_operand")
   (match_operand:SI 2 "const_0_to_1_operand")
   (match_operand:<ssehalfvecmode> 3 "nonimm_or_0_operand")
   (match_operand:QI 4 "register_operand")]
  "TARGET_AVX512DQ && TARGET_AVX512VL"
{
  rtx (*insn)(rtx, rtx, rtx, rtx);
  rtx dest = operands[0];

  if (MEM_P (dest)
      && (GET_MODE_SIZE (GET_MODE_INNER (<MODE>mode)) == 4
	  /* For V8S[IF]mode there are maskm insns with =m and 0
	     constraints.  */
	  ? !rtx_equal_p (dest, operands[3])
	  /* For V4D[IF]mode, hi insns don't allow memory, and
	     lo insns have =m and 0C constraints.  */
	  : (operands[2] != const0_rtx
	     || (!rtx_equal_p (dest, operands[3])
		 && GET_CODE (operands[3]) != CONST_VECTOR))))
    dest = gen_reg_rtx (<ssehalfvecmode>mode);
  switch (INTVAL (operands[2]))
    {
    case 0:
      insn = gen_vec_extract_lo_<mode>_mask;
      break;
    case 1:
      insn = gen_vec_extract_hi_<mode>_mask;
      break;
    default:
      gcc_unreachable ();
    }

  emit_insn (insn (dest, operands[1], operands[3], operands[4]));
  if (dest != operands[0])
    emit_move_insn (operands[0], dest);
  DONE;
})

(define_expand "avx_vextractf128<mode>"
  [(match_operand:<ssehalfvecmode> 0 "nonimmediate_operand")
   (match_operand:V_256 1 "register_operand")
   (match_operand:SI 2 "const_0_to_1_operand")]
  "TARGET_AVX"
{
  rtx (*insn)(rtx, rtx);

  switch (INTVAL (operands[2]))
    {
    case 0:
      insn = gen_vec_extract_lo_<mode>;
      break;
    case 1:
      insn = gen_vec_extract_hi_<mode>;
      break;
    default:
      gcc_unreachable ();
    }

  emit_insn (insn (operands[0], operands[1]));
  DONE;
})

(define_insn "vec_extract_lo_<mode><mask_name>"
  [(set (match_operand:<ssehalfvecmode> 0 "nonimmediate_operand" "=v,v,m")
	(vec_select:<ssehalfvecmode>
	  (match_operand:V16FI 1 "<store_mask_predicate>"
				 "v,<store_mask_constraint>,v")
	  (parallel [(const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)
                     (const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)])))]
  "TARGET_AVX512F
   && <mask_mode512bit_condition>
   && (<mask_applied> || !(MEM_P (operands[0]) && MEM_P (operands[1])))"
{
  if (<mask_applied>
      || (!TARGET_AVX512VL
	  && !REG_P (operands[0])
	  && EXT_REX_SSE_REG_P (operands[1])))
    return "vextract<shuffletype>32x8\t{$0x0, %1, %0<mask_operand2>|%0<mask_operand2>, %1, 0x0}";
  else
    return "#";
}
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,load,store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_split
  [(set (match_operand:<ssehalfvecmode> 0 "nonimmediate_operand")
	(vec_select:<ssehalfvecmode>
	  (match_operand:V16FI 1 "nonimmediate_operand")
	  (parallel [(const_int 0) (const_int 1)
            (const_int 2) (const_int 3)
	    (const_int 4) (const_int 5)
	    (const_int 6) (const_int 7)])))]
  "TARGET_AVX512F && !(MEM_P (operands[0]) && MEM_P (operands[1]))
   && reload_completed
   && (TARGET_AVX512VL
       || REG_P (operands[0])
       || !EXT_REX_SSE_REG_P (operands[1]))"
  [(set (match_dup 0) (match_dup 1))]
{
  if (!TARGET_AVX512VL
      && REG_P (operands[0])
      && EXT_REX_SSE_REG_P (operands[1]))
    operands[0]
      = lowpart_subreg (<MODE>mode, operands[0], <ssehalfvecmode>mode);
  else
    operands[1] = gen_lowpart (<ssehalfvecmode>mode, operands[1]);
})

(define_insn "vec_extract_lo_<mode><mask_name>"
  [(set (match_operand:<ssehalfvecmode> 0 "<store_mask_predicate>" "=v,v,m")
	(vec_select:<ssehalfvecmode>
	  (match_operand:VI8F_256 1 "<store_mask_predicate>"
				    "v,<store_mask_constraint>,v")
	  (parallel [(const_int 0) (const_int 1)])))]
  "TARGET_AVX
   && <mask_avx512vl_condition> && <mask_avx512dq_condition>
   && (<mask_applied> || !(MEM_P (operands[0]) && MEM_P (operands[1])))"
{
  if (<mask_applied>)
    return "vextract<shuffletype>64x2\t{$0x0, %1, %0%{%3%}|%0%{%3%}, %1, 0x0}";
  else
    return "#";
}
   [(set_attr "type" "sselog1")
    (set_attr "prefix_extra" "1")
    (set_attr "length_immediate" "1")
    (set_attr "memory" "none,load,store")
    (set_attr "prefix" "evex")
    (set_attr "mode" "XI")])

(define_split
  [(set (match_operand:<ssehalfvecmode> 0 "nonimmediate_operand")
	(vec_select:<ssehalfvecmode>
	  (match_operand:VI8F_256 1 "nonimmediate_operand")
	  (parallel [(const_int 0) (const_int 1)])))]
  "TARGET_AVX && !(MEM_P (operands[0]) && MEM_P (operands[1]))
   && reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  "operands[1] = gen_lowpart (<ssehalfvecmode>mode, operands[1]);")

(define_insn "vec_extract_hi_<mode><mask_name>"
  [(set (match_operand:<ssehalfvecmode> 0 "<store_mask_predicate>" "=v,<store_mask_constraint>")
	(vec_select:<ssehalfvecmode>
	  (match_operand:VI8F_256 1 "register_operand" "v,v")
	  (parallel [(const_int 2) (const_int 3)])))]
  "TARGET_AVX && <mask_avx512vl_condition> && <mask_avx512dq_condition>"
{
  if (TARGET_AVX512VL)
  {
    if (TARGET_AVX512DQ)
      return "vextract<shuffletype>64x2\t{$0x1, %1, %0<mask_operand2>|%0<mask_operand2>, %1, 0x1}";
    else
      return "vextract<shuffletype>32x4\t{$0x1, %1, %0|%0, %1, 0x1}";
  }
  else
    return "vextract<i128>\t{$0x1, %1, %0|%0, %1, 0x1}";
}
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_split
  [(set (match_operand:<ssehalfvecmode> 0 "nonimmediate_operand")
	(vec_select:<ssehalfvecmode>
	  (match_operand:VI4F_256 1 "nonimmediate_operand")
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)])))]
  "TARGET_AVX && !(MEM_P (operands[0]) && MEM_P (operands[1]))
   && reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  "operands[1] = gen_lowpart (<ssehalfvecmode>mode, operands[1]);")

(define_insn "vec_extract_lo_<mode><mask_name>"
  [(set (match_operand:<ssehalfvecmode> 0 "<store_mask_predicate>"
					  "=<store_mask_constraint>,v")
	(vec_select:<ssehalfvecmode>
	  (match_operand:VI4F_256 1 "<store_mask_predicate>"
				    "v,<store_mask_constraint>")
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)])))]
  "TARGET_AVX
   && <mask_avx512vl_condition> && <mask_avx512dq_condition>
   && (<mask_applied> || !(MEM_P (operands[0]) && MEM_P (operands[1])))"
{
  if (<mask_applied>)
    return "vextract<shuffletype>32x4\t{$0x0, %1, %0<mask_operand2>|%0<mask_operand2>, %1, 0x0}";
  else
    return "#";
}
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vec_extract_lo_<mode>_maskm"
  [(set (match_operand:<ssehalfvecmode> 0 "memory_operand" "=m")
	(vec_merge:<ssehalfvecmode>
	  (vec_select:<ssehalfvecmode>
	    (match_operand:VI4F_256 1 "register_operand" "v")
	    (parallel [(const_int 0) (const_int 1)
		      (const_int 2) (const_int 3)]))
	  (match_operand:<ssehalfvecmode> 2 "memory_operand" "0")
	  (match_operand:QI 3 "register_operand" "Yk")))]
  "TARGET_AVX512VL && TARGET_AVX512F
   && rtx_equal_p (operands[2], operands[0])"
  "vextract<shuffletype>32x4\t{$0x0, %1, %0%{%3%}|%0%{%3%}, %1, 0x0}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vec_extract_hi_<mode>_maskm"
  [(set (match_operand:<ssehalfvecmode> 0 "memory_operand" "=m")
	(vec_merge:<ssehalfvecmode>
	  (vec_select:<ssehalfvecmode>
	    (match_operand:VI4F_256 1 "register_operand" "v")
	    (parallel [(const_int 4) (const_int 5)
		      (const_int 6) (const_int 7)]))
	  (match_operand:<ssehalfvecmode> 2 "memory_operand" "0")
	  (match_operand:<ssehalfvecmode> 3 "register_operand" "Yk")))]
  "TARGET_AVX512F && TARGET_AVX512VL
   && rtx_equal_p (operands[2], operands[0])"
  "vextract<shuffletype>32x4\t{$0x1, %1, %0%{%3%}|%0%{%3%}, %1, 0x1}"
  [(set_attr "type" "sselog1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vec_extract_hi_<mode>_mask"
  [(set (match_operand:<ssehalfvecmode> 0 "register_operand" "=v")
	(vec_merge:<ssehalfvecmode>
	  (vec_select:<ssehalfvecmode>
	    (match_operand:VI4F_256 1 "register_operand" "v")
	    (parallel [(const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))
	  (match_operand:<ssehalfvecmode> 2 "nonimm_or_0_operand" "0C")
	  (match_operand:<avx512fmaskmode> 3 "register_operand" "Yk")))]
  "TARGET_AVX512VL"
  "vextract<shuffletype>32x4\t{$0x1, %1, %0%{%3%}%N2|%0%{%3%}%N2, %1, 0x1}"
  [(set_attr "type" "sselog1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vec_extract_hi_<mode>"
  [(set (match_operand:<ssehalfvecmode> 0 "nonimmediate_operand" "=xm, vm")
	(vec_select:<ssehalfvecmode>
	  (match_operand:VI4F_256 1 "register_operand" "x, v")
	  (parallel [(const_int 4) (const_int 5)
		     (const_int 6) (const_int 7)])))]
  "TARGET_AVX"
  "@
    vextract<i128>\t{$0x1, %1, %0|%0, %1, 0x1}
    vextract<shuffletype>32x4\t{$0x1, %1, %0|%0, %1, 0x1}"
  [(set_attr "isa" "*, avx512vl")
   (set_attr "prefix" "vex, evex")
   (set_attr "type" "sselog1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn_and_split "vec_extract_lo_v32hi"
  [(set (match_operand:V16HI 0 "nonimmediate_operand" "=v,v,m")
	(vec_select:V16HI
	  (match_operand:V32HI 1 "nonimmediate_operand" "v,m,v")
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)
		     (const_int 4) (const_int 5)
		     (const_int 6) (const_int 7)
		     (const_int 8) (const_int 9)
		     (const_int 10) (const_int 11)
		     (const_int 12) (const_int 13)
		     (const_int 14) (const_int 15)])))]
  "TARGET_AVX512F && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
{
  if (TARGET_AVX512VL
      || REG_P (operands[0])
      || !EXT_REX_SSE_REG_P (operands[1]))
    return "#";
  else
    return "vextracti64x4\t{$0x0, %1, %0|%0, %1, 0x0}";
}
  "&& reload_completed
   && (TARGET_AVX512VL
       || REG_P (operands[0])
       || !EXT_REX_SSE_REG_P (operands[1]))"
  [(set (match_dup 0) (match_dup 1))]
{
  if (!TARGET_AVX512VL
      && REG_P (operands[0])
      && EXT_REX_SSE_REG_P (operands[1]))
    operands[0] = lowpart_subreg (V32HImode, operands[0], V16HImode);
  else
    operands[1] = gen_lowpart (V16HImode, operands[1]);
}
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,load,store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "vec_extract_hi_v32hi"
  [(set (match_operand:V16HI 0 "nonimmediate_operand" "=vm")
	(vec_select:V16HI
	  (match_operand:V32HI 1 "register_operand" "v")
	  (parallel [(const_int 16) (const_int 17)
		     (const_int 18) (const_int 19)
		     (const_int 20) (const_int 21)
		     (const_int 22) (const_int 23)
		     (const_int 24) (const_int 25)
		     (const_int 26) (const_int 27)
		     (const_int 28) (const_int 29)
		     (const_int 30) (const_int 31)])))]
  "TARGET_AVX512F"
  "vextracti64x4\t{$0x1, %1, %0|%0, %1, 0x1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn_and_split "vec_extract_lo_v16hi"
  [(set (match_operand:V8HI 0 "nonimmediate_operand" "=v,m")
	(vec_select:V8HI
	  (match_operand:V16HI 1 "nonimmediate_operand" "vm,v")
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)
		     (const_int 4) (const_int 5)
		     (const_int 6) (const_int 7)])))]
  "TARGET_AVX && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  "operands[1] = gen_lowpart (V8HImode, operands[1]);")

(define_insn "vec_extract_hi_v16hi"
  [(set (match_operand:V8HI 0 "nonimmediate_operand" "=xm,vm,vm")
	(vec_select:V8HI
	  (match_operand:V16HI 1 "register_operand" "x,v,v")
	  (parallel [(const_int 8) (const_int 9)
		     (const_int 10) (const_int 11)
		     (const_int 12) (const_int 13)
		     (const_int 14) (const_int 15)])))]
  "TARGET_AVX"
  "@
   vextract%~128\t{$0x1, %1, %0|%0, %1, 0x1}
   vextracti32x4\t{$0x1, %1, %0|%0, %1, 0x1}
   vextracti32x4\t{$0x1, %g1, %0|%0, %g1, 0x1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "isa" "*,avx512dq,avx512f")
   (set_attr "prefix" "vex,evex,evex")
   (set_attr "mode" "OI")])

(define_insn_and_split "vec_extract_lo_v64qi"
  [(set (match_operand:V32QI 0 "nonimmediate_operand" "=v,v,m")
	(vec_select:V32QI
	  (match_operand:V64QI 1 "nonimmediate_operand" "v,m,v")
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)
		     (const_int 4) (const_int 5)
		     (const_int 6) (const_int 7)
		     (const_int 8) (const_int 9)
		     (const_int 10) (const_int 11)
		     (const_int 12) (const_int 13)
		     (const_int 14) (const_int 15)
		     (const_int 16) (const_int 17)
		     (const_int 18) (const_int 19)
		     (const_int 20) (const_int 21)
		     (const_int 22) (const_int 23)
		     (const_int 24) (const_int 25)
		     (const_int 26) (const_int 27)
		     (const_int 28) (const_int 29)
		     (const_int 30) (const_int 31)])))]
  "TARGET_AVX512F && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
{
  if (TARGET_AVX512VL
      || REG_P (operands[0])
      || !EXT_REX_SSE_REG_P (operands[1]))
    return "#";
  else
    return "vextracti64x4\t{$0x0, %1, %0|%0, %1, 0x0}";
}
  "&& reload_completed
   && (TARGET_AVX512VL
       || REG_P (operands[0])
       || !EXT_REX_SSE_REG_P (operands[1]))"
  [(set (match_dup 0) (match_dup 1))]
{
  if (!TARGET_AVX512VL
      && REG_P (operands[0])
      && EXT_REX_SSE_REG_P (operands[1]))
    operands[0] = lowpart_subreg (V64QImode, operands[0], V32QImode);
  else
    operands[1] = gen_lowpart (V32QImode, operands[1]);
}
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,load,store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "vec_extract_hi_v64qi"
  [(set (match_operand:V32QI 0 "nonimmediate_operand" "=vm")
	(vec_select:V32QI
	  (match_operand:V64QI 1 "register_operand" "v")
	  (parallel [(const_int 32) (const_int 33)
		     (const_int 34) (const_int 35)
		     (const_int 36) (const_int 37)
		     (const_int 38) (const_int 39)
		     (const_int 40) (const_int 41)
		     (const_int 42) (const_int 43)
		     (const_int 44) (const_int 45)
		     (const_int 46) (const_int 47)
		     (const_int 48) (const_int 49)
		     (const_int 50) (const_int 51)
		     (const_int 52) (const_int 53)
		     (const_int 54) (const_int 55)
		     (const_int 56) (const_int 57)
		     (const_int 58) (const_int 59)
		     (const_int 60) (const_int 61)
		     (const_int 62) (const_int 63)])))]
  "TARGET_AVX512F"
  "vextracti64x4\t{$0x1, %1, %0|%0, %1, 0x1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn_and_split "vec_extract_lo_v32qi"
  [(set (match_operand:V16QI 0 "nonimmediate_operand" "=v,m")
	(vec_select:V16QI
	  (match_operand:V32QI 1 "nonimmediate_operand" "vm,v")
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)
		     (const_int 4) (const_int 5)
		     (const_int 6) (const_int 7)
		     (const_int 8) (const_int 9)
		     (const_int 10) (const_int 11)
		     (const_int 12) (const_int 13)
		     (const_int 14) (const_int 15)])))]
  "TARGET_AVX && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  "operands[1] = gen_lowpart (V16QImode, operands[1]);")

(define_insn "vec_extract_hi_v32qi"
  [(set (match_operand:V16QI 0 "nonimmediate_operand" "=xm,vm,vm")
	(vec_select:V16QI
	  (match_operand:V32QI 1 "register_operand" "x,v,v")
	  (parallel [(const_int 16) (const_int 17)
		     (const_int 18) (const_int 19)
		     (const_int 20) (const_int 21)
		     (const_int 22) (const_int 23)
		     (const_int 24) (const_int 25)
		     (const_int 26) (const_int 27)
		     (const_int 28) (const_int 29)
		     (const_int 30) (const_int 31)])))]
  "TARGET_AVX"
  "@
   vextract%~128\t{$0x1, %1, %0|%0, %1, 0x1}
   vextracti32x4\t{$0x1, %1, %0|%0, %1, 0x1}
   vextracti32x4\t{$0x1, %g1, %0|%0, %g1, 0x1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "isa" "*,avx512dq,avx512f")
   (set_attr "prefix" "vex,evex,evex")
   (set_attr "mode" "OI")])

;; Modes handled by vec_extract patterns.
(define_mode_iterator VEC_EXTRACT_MODE
  [(V64QI "TARGET_AVX512BW") (V32QI "TARGET_AVX") V16QI
   (V32HI "TARGET_AVX512BW") (V16HI "TARGET_AVX") V8HI
   (V16SI "TARGET_AVX512F") (V8SI "TARGET_AVX") V4SI
   (V8DI "TARGET_AVX512F") (V4DI "TARGET_AVX") V2DI
   (V16SF "TARGET_AVX512F") (V8SF "TARGET_AVX") V4SF
   (V8DF "TARGET_AVX512F") (V4DF "TARGET_AVX") V2DF
   (V4TI "TARGET_AVX512F") (V2TI "TARGET_AVX")])

(define_expand "vec_extract<mode><ssescalarmodelower>"
  [(match_operand:<ssescalarmode> 0 "register_operand")
   (match_operand:VEC_EXTRACT_MODE 1 "register_operand")
   (match_operand 2 "const_int_operand")]
  "TARGET_SSE"
{
  ix86_expand_vector_extract (false, operands[0], operands[1],
			      INTVAL (operands[2]));
  DONE;
})

(define_expand "vec_extract<mode><ssehalfvecmodelower>"
  [(match_operand:<ssehalfvecmode> 0 "nonimmediate_operand")
   (match_operand:V_512 1 "register_operand")
   (match_operand 2 "const_0_to_1_operand")]
  "TARGET_AVX512F"
{
  if (INTVAL (operands[2]))
    emit_insn (gen_vec_extract_hi_<mode> (operands[0], operands[1]));
  else
    emit_insn (gen_vec_extract_lo_<mode> (operands[0], operands[1]));
  DONE;
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel double-precision floating point element swizzling
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "<mask_codefor>avx512f_unpckhpd512<mask_name>"
  [(set (match_operand:V8DF 0 "register_operand" "=v")
	(vec_select:V8DF
	  (vec_concat:V16DF
	    (match_operand:V8DF 1 "register_operand" "v")
	    (match_operand:V8DF 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 1) (const_int 9)
		     (const_int 3) (const_int 11)
		     (const_int 5) (const_int 13)
		     (const_int 7) (const_int 15)])))]
  "TARGET_AVX512F"
  "vunpckhpd\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V8DF")])

;; Recall that the 256-bit unpck insns only shuffle within their lanes.
(define_insn "avx_unpckhpd256<mask_name>"
  [(set (match_operand:V4DF 0 "register_operand" "=v")
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_operand:V4DF 1 "register_operand" "v")
	    (match_operand:V4DF 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 1) (const_int 5)
		     (const_int 3) (const_int 7)])))]
  "TARGET_AVX && <mask_avx512vl_condition>"
  "vunpckhpd\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_expand "vec_interleave_highv4df"
  [(set (match_dup 3)
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_operand:V4DF 1 "register_operand")
	    (match_operand:V4DF 2 "nonimmediate_operand"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))
   (set (match_dup 4)
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_dup 1)
	    (match_dup 2))
	  (parallel [(const_int 1) (const_int 5)
		     (const_int 3) (const_int 7)])))
   (set (match_operand:V4DF 0 "register_operand")
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_dup 3)
	    (match_dup 4))
	  (parallel [(const_int 2) (const_int 3)
		     (const_int 6) (const_int 7)])))]
 "TARGET_AVX"
{
  operands[3] = gen_reg_rtx (V4DFmode);
  operands[4] = gen_reg_rtx (V4DFmode);
})


(define_insn "avx512vl_unpckhpd128_mask"
  [(set (match_operand:V2DF 0 "register_operand" "=v")
	(vec_merge:V2DF
	  (vec_select:V2DF
	    (vec_concat:V4DF
	      (match_operand:V2DF 1 "register_operand" "v")
	      (match_operand:V2DF 2 "nonimmediate_operand" "vm"))
	    (parallel [(const_int 1) (const_int 3)]))
	  (match_operand:V2DF 3 "nonimm_or_0_operand" "0C")
	  (match_operand:QI 4 "register_operand" "Yk")))]
  "TARGET_AVX512VL"
  "vunpckhpd\t{%2, %1, %0%{%4%}%N3|%0%{%4%}%N3, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V2DF")])

(define_expand "vec_interleave_highv2df"
  [(set (match_operand:V2DF 0 "register_operand")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "nonimmediate_operand")
	    (match_operand:V2DF 2 "nonimmediate_operand"))
	  (parallel [(const_int 1)
		     (const_int 3)])))]
  "TARGET_SSE2"
{
  if (!ix86_vec_interleave_v2df_operator_ok (operands, 1))
    operands[2] = force_reg (V2DFmode, operands[2]);
})

(define_insn "*vec_interleave_highv2df"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"     "=x,v,v,x,v,m")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "nonimmediate_operand" " 0,v,o,o,o,v")
	    (match_operand:V2DF 2 "nonimmediate_operand" " x,v,1,0,v,0"))
	  (parallel [(const_int 1)
		     (const_int 3)])))]
  "TARGET_SSE2 && ix86_vec_interleave_v2df_operator_ok (operands, 1)"
  "@
   unpckhpd\t{%2, %0|%0, %2}
   vunpckhpd\t{%2, %1, %0|%0, %1, %2}
   %vmovddup\t{%H1, %0|%0, %H1}
   movlpd\t{%H1, %0|%0, %H1}
   vmovlpd\t{%H1, %2, %0|%0, %2, %H1}
   %vmovhpd\t{%1, %0|%q0, %1}"
  [(set_attr "isa" "noavx,avx,sse3,noavx,avx,*")
   (set_attr "type" "sselog,sselog,sselog,ssemov,ssemov,ssemov")
   (set (attr "prefix_data16")
     (if_then_else (eq_attr "alternative" "3,5")
		   (const_string "1")
		   (const_string "*")))
   (set_attr "prefix" "orig,maybe_evex,maybe_vex,orig,maybe_evex,maybe_vex")
   (set_attr "mode" "V2DF,V2DF,DF,V1DF,V1DF,V1DF")])

(define_expand "avx512f_movddup512<mask_name>"
  [(set (match_operand:V8DF 0 "register_operand")
	(vec_select:V8DF
	  (vec_concat:V16DF
	    (match_operand:V8DF 1 "nonimmediate_operand")
	    (match_dup 1))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 2) (const_int 10)
		     (const_int 4) (const_int 12)
		     (const_int 6) (const_int 14)])))]
  "TARGET_AVX512F")

(define_expand "avx512f_unpcklpd512<mask_name>"
  [(set (match_operand:V8DF 0 "register_operand")
	(vec_select:V8DF
	  (vec_concat:V16DF
	    (match_operand:V8DF 1 "register_operand")
	    (match_operand:V8DF 2 "nonimmediate_operand"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 2) (const_int 10)
		     (const_int 4) (const_int 12)
		     (const_int 6) (const_int 14)])))]
  "TARGET_AVX512F")

(define_insn "*avx512f_unpcklpd512<mask_name>"
  [(set (match_operand:V8DF 0 "register_operand" "=v,v")
	(vec_select:V8DF
	  (vec_concat:V16DF
	    (match_operand:V8DF 1 "nonimmediate_operand" "vm, v")
	    (match_operand:V8DF 2 "nonimmediate_operand" "1 ,vm"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 2) (const_int 10)
		     (const_int 4) (const_int 12)
		     (const_int 6) (const_int 14)])))]
  "TARGET_AVX512F"
  "@
   vmovddup\t{%1, %0<mask_operand3>|%0<mask_operand3>, %1}
   vunpcklpd\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V8DF")])

;; Recall that the 256-bit unpck insns only shuffle within their lanes.
(define_expand "avx_movddup256<mask_name>"
  [(set (match_operand:V4DF 0 "register_operand")
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_operand:V4DF 1 "nonimmediate_operand")
	    (match_dup 1))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))]
  "TARGET_AVX && <mask_avx512vl_condition>")

(define_expand "avx_unpcklpd256<mask_name>"
  [(set (match_operand:V4DF 0 "register_operand")
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_operand:V4DF 1 "register_operand")
	    (match_operand:V4DF 2 "nonimmediate_operand"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))]
  "TARGET_AVX && <mask_avx512vl_condition>")

(define_insn "*avx_unpcklpd256<mask_name>"
  [(set (match_operand:V4DF 0 "register_operand"         "=v,v")
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_operand:V4DF 1 "nonimmediate_operand" " v,m")
	    (match_operand:V4DF 2 "nonimmediate_operand" "vm,1"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))]
  "TARGET_AVX && <mask_avx512vl_condition>"
  "@
   vunpcklpd\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}
   vmovddup\t{%1, %0<mask_operand3>|%0<mask_operand3>, %1}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_expand "vec_interleave_lowv4df"
  [(set (match_dup 3)
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_operand:V4DF 1 "register_operand")
	    (match_operand:V4DF 2 "nonimmediate_operand"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))
   (set (match_dup 4)
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_dup 1)
	    (match_dup 2))
	  (parallel [(const_int 1) (const_int 5)
		     (const_int 3) (const_int 7)])))
   (set (match_operand:V4DF 0 "register_operand")
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_dup 3)
	    (match_dup 4))
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 4) (const_int 5)])))]
 "TARGET_AVX"
{
  operands[3] = gen_reg_rtx (V4DFmode);
  operands[4] = gen_reg_rtx (V4DFmode);
})

(define_insn "avx512vl_unpcklpd128_mask"
  [(set (match_operand:V2DF 0 "register_operand" "=v")
	(vec_merge:V2DF
	  (vec_select:V2DF
	    (vec_concat:V4DF
	      (match_operand:V2DF 1 "register_operand" "v")
	      (match_operand:V2DF 2 "nonimmediate_operand" "vm"))
	    (parallel [(const_int 0) (const_int 2)]))
	  (match_operand:V2DF 3 "nonimm_or_0_operand" "0C")
	  (match_operand:QI 4 "register_operand" "Yk")))]
  "TARGET_AVX512VL"
  "vunpcklpd\t{%2, %1, %0%{%4%}%N3|%0%{%4%}%N3, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V2DF")])

(define_expand "vec_interleave_lowv2df"
  [(set (match_operand:V2DF 0 "register_operand")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "nonimmediate_operand")
	    (match_operand:V2DF 2 "nonimmediate_operand"))
	  (parallel [(const_int 0)
		     (const_int 2)])))]
  "TARGET_SSE2"
{
  if (!ix86_vec_interleave_v2df_operator_ok (operands, 0))
    operands[1] = force_reg (V2DFmode, operands[1]);
})

(define_insn "*vec_interleave_lowv2df"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"     "=x,v,v,x,v,o")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "nonimmediate_operand" " 0,v,m,0,v,0")
	    (match_operand:V2DF 2 "nonimmediate_operand" " x,v,1,m,m,v"))
	  (parallel [(const_int 0)
		     (const_int 2)])))]
  "TARGET_SSE2 && ix86_vec_interleave_v2df_operator_ok (operands, 0)"
  "@
   unpcklpd\t{%2, %0|%0, %2}
   vunpcklpd\t{%2, %1, %0|%0, %1, %2}
   %vmovddup\t{%1, %0|%0, %q1}
   movhpd\t{%2, %0|%0, %q2}
   vmovhpd\t{%2, %1, %0|%0, %1, %q2}
   %vmovlpd\t{%2, %H0|%H0, %2}"
  [(set_attr "isa" "noavx,avx,sse3,noavx,avx,*")
   (set_attr "type" "sselog,sselog,sselog,ssemov,ssemov,ssemov")
   (set (attr "prefix_data16")
     (if_then_else (eq_attr "alternative" "3,5")
		   (const_string "1")
		   (const_string "*")))
   (set_attr "prefix" "orig,maybe_evex,maybe_vex,orig,maybe_evex,maybe_vex")
   (set_attr "mode" "V2DF,V2DF,DF,V1DF,V1DF,V1DF")])

(define_split
  [(set (match_operand:V2DF 0 "memory_operand")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "register_operand")
	    (match_dup 1))
	  (parallel [(const_int 0)
		     (const_int 2)])))]
  "TARGET_SSE3 && reload_completed"
  [(const_int 0)]
{
  rtx low = gen_lowpart (DFmode, operands[1]);

  emit_move_insn (adjust_address (operands[0], DFmode, 0), low);
  emit_move_insn (adjust_address (operands[0], DFmode, 8), low);
  DONE;
})

(define_split
  [(set (match_operand:V2DF 0 "register_operand")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "memory_operand")
	    (match_dup 1))
	  (parallel [(match_operand:SI 2 "const_0_to_1_operand")
		     (match_operand:SI 3 "const_int_operand")])))]
  "TARGET_SSE3 && INTVAL (operands[2]) + 2 == INTVAL (operands[3])"
  [(set (match_dup 0) (vec_duplicate:V2DF (match_dup 1)))]
{
  operands[1] = adjust_address (operands[1], DFmode, INTVAL (operands[2]) * 8);
})

(define_insn "avx512f_vmscalef<mode><mask_scalar_name><round_scalar_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
	  (unspec:VF_128
	    [(match_operand:VF_128 1 "register_operand" "v")
	     (match_operand:VF_128 2 "<round_scalar_nimm_predicate>" "<round_scalar_constraint>")]
	    UNSPEC_SCALEF)
	  (match_dup 1)
	  (const_int 1)))]
  "TARGET_AVX512F"
  "vscalef<ssescalarmodesuffix>\t{<round_scalar_mask_op3>%2, %1, %0<mask_scalar_operand3>|%0<mask_scalar_operand3>, %1, %2<round_scalar_mask_op3>}"
  [(set_attr "prefix" "evex")
   (set_attr "mode"  "<ssescalarmode>")])

(define_insn "<avx512>_scalef<mode><mask_name><round_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
	(unspec:VF_AVX512VL
	  [(match_operand:VF_AVX512VL 1 "register_operand" "v")
	   (match_operand:VF_AVX512VL 2 "nonimmediate_operand" "<round_constraint>")]
	  UNSPEC_SCALEF))]
  "TARGET_AVX512F"
  "vscalef<ssemodesuffix>\t{<round_mask_op3>%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2<round_mask_op3>}"
  [(set_attr "prefix" "evex")
   (set_attr "mode"  "<MODE>")])

(define_expand "<avx512>_vternlog<mode>_maskz"
  [(match_operand:VI48_AVX512VL 0 "register_operand")
   (match_operand:VI48_AVX512VL 1 "register_operand")
   (match_operand:VI48_AVX512VL 2 "register_operand")
   (match_operand:VI48_AVX512VL 3 "nonimmediate_operand")
   (match_operand:SI 4 "const_0_to_255_operand")
   (match_operand:<avx512fmaskmode> 5 "register_operand")]
  "TARGET_AVX512F"
{
  emit_insn (gen_<avx512>_vternlog<mode>_maskz_1 (
    operands[0], operands[1], operands[2], operands[3],
    operands[4], CONST0_RTX (<MODE>mode), operands[5]));
  DONE;
})

(define_insn "<avx512>_vternlog<mode><sd_maskz_name>"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand" "=v")
	(unspec:VI48_AVX512VL
	  [(match_operand:VI48_AVX512VL 1 "register_operand" "0")
	   (match_operand:VI48_AVX512VL 2 "register_operand" "v")
	   (match_operand:VI48_AVX512VL 3 "nonimmediate_operand" "vm")
	   (match_operand:SI 4 "const_0_to_255_operand")]
	  UNSPEC_VTERNLOG))]
  "TARGET_AVX512F"
  "vpternlog<ssemodesuffix>\t{%4, %3, %2, %0<sd_mask_op5>|%0<sd_mask_op5>, %2, %3, %4}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_vternlog<mode>_mask"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI48_AVX512VL
	  (unspec:VI48_AVX512VL
	    [(match_operand:VI48_AVX512VL 1 "register_operand" "0")
	     (match_operand:VI48_AVX512VL 2 "register_operand" "v")
	     (match_operand:VI48_AVX512VL 3 "nonimmediate_operand" "vm")
	     (match_operand:SI 4 "const_0_to_255_operand")]
	    UNSPEC_VTERNLOG)
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 5 "register_operand" "Yk")))]
  "TARGET_AVX512F"
  "vpternlog<ssemodesuffix>\t{%4, %3, %2, %0%{%5%}|%0%{%5%}, %2, %3, %4}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_getexp<mode><mask_name><round_saeonly_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
        (unspec:VF_AVX512VL [(match_operand:VF_AVX512VL 1 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")]
                        UNSPEC_GETEXP))]
   "TARGET_AVX512F"
   "vgetexp<ssemodesuffix>\t{<round_saeonly_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_saeonly_mask_op2>}";
    [(set_attr "prefix" "evex")
     (set_attr "mode" "<MODE>")])

(define_insn "avx512f_sgetexp<mode><mask_scalar_name><round_saeonly_scalar_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
	  (unspec:VF_128
	    [(match_operand:VF_128 1 "register_operand" "v")
	     (match_operand:VF_128 2 "<round_saeonly_scalar_nimm_predicate>" "<round_saeonly_scalar_constraint>")]
	    UNSPEC_GETEXP)
	  (match_dup 1)
	  (const_int 1)))]
   "TARGET_AVX512F"
   "vgetexp<ssescalarmodesuffix>\t{<round_saeonly_scalar_mask_op3>%2, %1, %0<mask_scalar_operand3>|%0<mask_scalar_operand3>, %1, %<iptr>2<round_saeonly_scalar_mask_op3>}";
    [(set_attr "prefix" "evex")
     (set_attr "mode" "<ssescalarmode>")])

(define_insn "<mask_codefor><avx512>_align<mode><mask_name>"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand" "=v")
        (unspec:VI48_AVX512VL [(match_operand:VI48_AVX512VL 1 "register_operand" "v")
			       (match_operand:VI48_AVX512VL 2 "nonimmediate_operand" "vm")
			       (match_operand:SI 3 "const_0_to_255_operand")]
			      UNSPEC_ALIGN))]
  "TARGET_AVX512F"
  "valign<ssemodesuffix>\t{%3, %2, %1, %0<mask_operand4>|%0<mask_operand4>, %1, %2, %3}";
  [(set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "avx512f_shufps512_mask"
  [(match_operand:V16SF 0 "register_operand")
   (match_operand:V16SF 1 "register_operand")
   (match_operand:V16SF 2 "nonimmediate_operand")
   (match_operand:SI 3 "const_0_to_255_operand")
   (match_operand:V16SF 4 "register_operand")
   (match_operand:HI 5 "register_operand")]
  "TARGET_AVX512F"
{
  int mask = INTVAL (operands[3]);
  emit_insn (gen_avx512f_shufps512_1_mask (operands[0], operands[1], operands[2],
					  GEN_INT ((mask >> 0) & 3),
					  GEN_INT ((mask >> 2) & 3),
					  GEN_INT (((mask >> 4) & 3) + 16),
					  GEN_INT (((mask >> 6) & 3) + 16),
					  GEN_INT (((mask >> 0) & 3) + 4),
					  GEN_INT (((mask >> 2) & 3) + 4),
					  GEN_INT (((mask >> 4) & 3) + 20),
					  GEN_INT (((mask >> 6) & 3) + 20),
					  GEN_INT (((mask >> 0) & 3) + 8),
					  GEN_INT (((mask >> 2) & 3) + 8),
					  GEN_INT (((mask >> 4) & 3) + 24),
					  GEN_INT (((mask >> 6) & 3) + 24),
					  GEN_INT (((mask >> 0) & 3) + 12),
					  GEN_INT (((mask >> 2) & 3) + 12),
					  GEN_INT (((mask >> 4) & 3) + 28),
					  GEN_INT (((mask >> 6) & 3) + 28),
					  operands[4], operands[5]));
  DONE;
})


(define_expand "<avx512>_fixupimm<mode>_maskz<round_saeonly_expand_name>"
  [(match_operand:VF_AVX512VL 0 "register_operand")
   (match_operand:VF_AVX512VL 1 "register_operand")
   (match_operand:VF_AVX512VL 2 "register_operand")
   (match_operand:<sseintvecmode> 3 "<round_saeonly_expand_nimm_predicate>")
   (match_operand:SI 4 "const_0_to_255_operand")
   (match_operand:<avx512fmaskmode> 5 "register_operand")]
  "TARGET_AVX512F"
{
  emit_insn (gen_<avx512>_fixupimm<mode>_maskz_1<round_saeonly_expand_name> (
	operands[0], operands[1], operands[2], operands[3],
	operands[4], CONST0_RTX (<MODE>mode), operands[5]
	<round_saeonly_expand_operand6>));
  DONE;
})

(define_insn "<avx512>_fixupimm<mode><sd_maskz_name><round_saeonly_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
        (unspec:VF_AVX512VL
          [(match_operand:VF_AVX512VL 1 "register_operand" "0")
	   (match_operand:VF_AVX512VL 2 "register_operand" "v")
           (match_operand:<sseintvecmode> 3 "nonimmediate_operand" "<round_saeonly_constraint>")
           (match_operand:SI 4 "const_0_to_255_operand")]
           UNSPEC_FIXUPIMM))]
  "TARGET_AVX512F"
  "vfixupimm<ssemodesuffix>\t{%4, <round_saeonly_sd_mask_op5>%3, %2, %0<sd_mask_op5>|%0<sd_mask_op5>, %2, %3<round_saeonly_sd_mask_op5>, %4}";
  [(set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "<avx512>_fixupimm<mode>_mask<round_saeonly_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VF_AVX512VL
          (unspec:VF_AVX512VL
            [(match_operand:VF_AVX512VL 1 "register_operand" "0")
	     (match_operand:VF_AVX512VL 2 "register_operand" "v")
             (match_operand:<sseintvecmode> 3 "nonimmediate_operand" "<round_saeonly_constraint>")
             (match_operand:SI 4 "const_0_to_255_operand")]
             UNSPEC_FIXUPIMM)
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 5 "register_operand" "Yk")))]
  "TARGET_AVX512F"
  "vfixupimm<ssemodesuffix>\t{%4, <round_saeonly_op6>%3, %2, %0%{%5%}|%0%{%5%}, %2, %3<round_saeonly_op6>, %4}";
  [(set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_expand "avx512f_sfixupimm<mode>_maskz<round_saeonly_expand_name>"
  [(match_operand:VF_128 0 "register_operand")
   (match_operand:VF_128 1 "register_operand")
   (match_operand:VF_128 2 "register_operand")
   (match_operand:<sseintvecmode> 3 "<round_saeonly_expand_nimm_predicate>")
   (match_operand:SI 4 "const_0_to_255_operand")
   (match_operand:<avx512fmaskmode> 5 "register_operand")]
  "TARGET_AVX512F"
{
  emit_insn (gen_avx512f_sfixupimm<mode>_maskz_1<round_saeonly_expand_name> (
	operands[0], operands[1], operands[2], operands[3],
	operands[4], CONST0_RTX (<MODE>mode), operands[5]
	<round_saeonly_expand_operand6>));
  DONE;
})

(define_insn "avx512f_sfixupimm<mode><sd_maskz_name><round_saeonly_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
          (unspec:VF_128
            [(match_operand:VF_128 1 "register_operand" "0")
	     (match_operand:VF_128 2 "register_operand" "v")
	     (match_operand:<sseintvecmode> 3 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")
	     (match_operand:SI 4 "const_0_to_255_operand")]
	    UNSPEC_FIXUPIMM)
	  (match_dup 1)
	  (const_int 1)))]
   "TARGET_AVX512F"
   "vfixupimm<ssescalarmodesuffix>\t{%4, <round_saeonly_sd_mask_op5>%3, %2, %0<sd_mask_op5>|%0<sd_mask_op5>, %2, %<iptr>3<round_saeonly_sd_mask_op5>, %4}";
   [(set_attr "prefix" "evex")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "avx512f_sfixupimm<mode>_mask<round_saeonly_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
	  (vec_merge:VF_128
	    (unspec:VF_128
	       [(match_operand:VF_128 1 "register_operand" "0")
		(match_operand:VF_128 2 "register_operand" "v")
		(match_operand:<sseintvecmode> 3 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")
		(match_operand:SI 4 "const_0_to_255_operand")]
	       UNSPEC_FIXUPIMM)
	    (match_dup 1)
	    (const_int 1))
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 5 "register_operand" "Yk")))]
  "TARGET_AVX512F"
  "vfixupimm<ssescalarmodesuffix>\t{%4, <round_saeonly_op6>%3, %2, %0%{%5%}|%0%{%5%}, %2, %<iptr>3<round_saeonly_op6>, %4}";
  [(set_attr "prefix" "evex")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "<avx512>_rndscale<mode><mask_name><round_saeonly_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
	(unspec:VF_AVX512VL
	  [(match_operand:VF_AVX512VL 1 "nonimmediate_operand" "<round_saeonly_constraint>")
	   (match_operand:SI 2 "const_0_to_255_operand")]
	  UNSPEC_ROUND))]
  "TARGET_AVX512F"
  "vrndscale<ssemodesuffix>\t{%2, <round_saeonly_mask_op3>%1, %0<mask_operand3>|%0<mask_operand3>, %1<round_saeonly_mask_op3>, %2}"
  [(set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "avx512f_rndscale<mode><round_saeonly_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
	  (unspec:VF_128
	    [(match_operand:VF_128 1 "register_operand" "v")
	     (match_operand:VF_128 2 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")
	     (match_operand:SI 3 "const_0_to_255_operand")]
	    UNSPEC_ROUND)
	  (match_dup 1)
	  (const_int 1)))]
  "TARGET_AVX512F"
  "vrndscale<ssescalarmodesuffix>\t{%3, <round_saeonly_op4>%2, %1, %0|%0, %1, %<iptr>2<round_saeonly_op4>, %3}"
  [(set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

;; One bit in mask selects 2 elements.
(define_insn "avx512f_shufps512_1<mask_name>"
  [(set (match_operand:V16SF 0 "register_operand" "=v")
	(vec_select:V16SF
	  (vec_concat:V32SF
	    (match_operand:V16SF 1 "register_operand" "v")
	    (match_operand:V16SF 2 "nonimmediate_operand" "vm"))
	  (parallel [(match_operand 3  "const_0_to_3_operand")
		     (match_operand 4  "const_0_to_3_operand")
		     (match_operand 5  "const_16_to_19_operand")
		     (match_operand 6  "const_16_to_19_operand")
		     (match_operand 7  "const_4_to_7_operand")
		     (match_operand 8  "const_4_to_7_operand")
		     (match_operand 9  "const_20_to_23_operand")
		     (match_operand 10  "const_20_to_23_operand")
		     (match_operand 11  "const_8_to_11_operand")
		     (match_operand 12  "const_8_to_11_operand")
		     (match_operand 13  "const_24_to_27_operand")
		     (match_operand 14  "const_24_to_27_operand")
		     (match_operand 15  "const_12_to_15_operand")
		     (match_operand 16  "const_12_to_15_operand")
		     (match_operand 17  "const_28_to_31_operand")
		     (match_operand 18  "const_28_to_31_operand")])))]
  "TARGET_AVX512F
   && (INTVAL (operands[3]) == (INTVAL (operands[7]) - 4)
       && INTVAL (operands[4]) == (INTVAL (operands[8]) - 4)
       && INTVAL (operands[5]) == (INTVAL (operands[9]) - 4)
       && INTVAL (operands[6]) == (INTVAL (operands[10]) - 4)
       && INTVAL (operands[3]) == (INTVAL (operands[11]) - 8)
       && INTVAL (operands[4]) == (INTVAL (operands[12]) - 8)
       && INTVAL (operands[5]) == (INTVAL (operands[13]) - 8)
       && INTVAL (operands[6]) == (INTVAL (operands[14]) - 8)
       && INTVAL (operands[3]) == (INTVAL (operands[15]) - 12)
       && INTVAL (operands[4]) == (INTVAL (operands[16]) - 12)
       && INTVAL (operands[5]) == (INTVAL (operands[17]) - 12)
       && INTVAL (operands[6]) == (INTVAL (operands[18]) - 12))"
{
  int mask;
  mask = INTVAL (operands[3]);
  mask |= INTVAL (operands[4]) << 2;
  mask |= (INTVAL (operands[5]) - 16) << 4;
  mask |= (INTVAL (operands[6]) - 16) << 6;
  operands[3] = GEN_INT (mask);

  return "vshufps\t{%3, %2, %1, %0<mask_operand19>|%0<mask_operand19>, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V16SF")])

(define_expand "avx512f_shufpd512_mask"
  [(match_operand:V8DF 0 "register_operand")
   (match_operand:V8DF 1 "register_operand")
   (match_operand:V8DF 2 "nonimmediate_operand")
   (match_operand:SI 3 "const_0_to_255_operand")
   (match_operand:V8DF 4 "register_operand")
   (match_operand:QI 5 "register_operand")]
  "TARGET_AVX512F"
{
  int mask = INTVAL (operands[3]);
  emit_insn (gen_avx512f_shufpd512_1_mask (operands[0], operands[1], operands[2],
					GEN_INT (mask & 1),
					GEN_INT (mask & 2 ? 9 : 8),
					GEN_INT (mask & 4 ? 3 : 2),
					GEN_INT (mask & 8 ? 11 : 10),
					GEN_INT (mask & 16 ? 5 : 4),
					GEN_INT (mask & 32 ? 13 : 12),
					GEN_INT (mask & 64 ? 7 : 6),
					GEN_INT (mask & 128 ? 15 : 14),
					operands[4], operands[5]));
  DONE;
})

(define_insn "avx512f_shufpd512_1<mask_name>"
  [(set (match_operand:V8DF 0 "register_operand" "=v")
	(vec_select:V8DF
	  (vec_concat:V16DF
	    (match_operand:V8DF 1 "register_operand" "v")
	    (match_operand:V8DF 2 "nonimmediate_operand" "vm"))
	  (parallel [(match_operand 3 "const_0_to_1_operand")
		     (match_operand 4 "const_8_to_9_operand")
		     (match_operand 5 "const_2_to_3_operand")
		     (match_operand 6 "const_10_to_11_operand")
		     (match_operand 7 "const_4_to_5_operand")
		     (match_operand 8 "const_12_to_13_operand")
		     (match_operand 9 "const_6_to_7_operand")
		     (match_operand 10 "const_14_to_15_operand")])))]
  "TARGET_AVX512F"
{
  int mask;
  mask = INTVAL (operands[3]);
  mask |= (INTVAL (operands[4]) - 8) << 1;
  mask |= (INTVAL (operands[5]) - 2) << 2;
  mask |= (INTVAL (operands[6]) - 10) << 3;
  mask |= (INTVAL (operands[7]) - 4) << 4;
  mask |= (INTVAL (operands[8]) - 12) << 5;
  mask |= (INTVAL (operands[9]) - 6) << 6;
  mask |= (INTVAL (operands[10]) - 14) << 7;
  operands[3] = GEN_INT (mask);

  return "vshufpd\t{%3, %2, %1, %0<mask_operand11>|%0<mask_operand11>, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V8DF")])

(define_expand "avx_shufpd256<mask_expand4_name>"
  [(match_operand:V4DF 0 "register_operand")
   (match_operand:V4DF 1 "register_operand")
   (match_operand:V4DF 2 "nonimmediate_operand")
   (match_operand:SI 3 "const_int_operand")]
  "TARGET_AVX"
{
  int mask = INTVAL (operands[3]);
  emit_insn (gen_avx_shufpd256_1<mask_expand4_name> (operands[0],
						     operands[1],
						     operands[2],
						     GEN_INT (mask & 1),
						     GEN_INT (mask & 2 ? 5 : 4),
						     GEN_INT (mask & 4 ? 3 : 2),
						     GEN_INT (mask & 8 ? 7 : 6)
						     <mask_expand4_args>));
  DONE;
})

(define_insn "avx_shufpd256_1<mask_name>"
  [(set (match_operand:V4DF 0 "register_operand" "=v")
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_operand:V4DF 1 "register_operand" "v")
	    (match_operand:V4DF 2 "nonimmediate_operand" "vm"))
	  (parallel [(match_operand 3 "const_0_to_1_operand")
		     (match_operand 4 "const_4_to_5_operand")
		     (match_operand 5 "const_2_to_3_operand")
		     (match_operand 6 "const_6_to_7_operand")])))]
  "TARGET_AVX && <mask_avx512vl_condition>"
{
  int mask;
  mask = INTVAL (operands[3]);
  mask |= (INTVAL (operands[4]) - 4) << 1;
  mask |= (INTVAL (operands[5]) - 2) << 2;
  mask |= (INTVAL (operands[6]) - 6) << 3;
  operands[3] = GEN_INT (mask);

  return "vshufpd\t{%3, %2, %1, %0<mask_operand7>|%0<mask_operand7>, %1, %2, %3}";
}
  [(set_attr "type" "sseshuf")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_expand "sse2_shufpd<mask_expand4_name>"
  [(match_operand:V2DF 0 "register_operand")
   (match_operand:V2DF 1 "register_operand")
   (match_operand:V2DF 2 "vector_operand")
   (match_operand:SI 3 "const_int_operand")]
  "TARGET_SSE2"
{
  int mask = INTVAL (operands[3]);
  emit_insn (gen_sse2_shufpd_v2df<mask_expand4_name> (operands[0], operands[1],
						      operands[2], GEN_INT (mask & 1),
						      GEN_INT (mask & 2 ? 3 : 2)
						      <mask_expand4_args>));
  DONE;
})

(define_insn "sse2_shufpd_v2df_mask"
  [(set (match_operand:V2DF 0 "register_operand" "=v")
    (vec_merge:V2DF
	  (vec_select:V2DF
	    (vec_concat:V4DF
	      (match_operand:V2DF 1 "register_operand" "v")
	      (match_operand:V2DF 2 "nonimmediate_operand" "vm"))
	    (parallel [(match_operand 3 "const_0_to_1_operand")
		           (match_operand 4 "const_2_to_3_operand")]))
      (match_operand:V2DF 5 "nonimm_or_0_operand" "0C")
      (match_operand:QI 6 "register_operand" "Yk")))]
  "TARGET_AVX512VL"
{
  int mask;
  mask = INTVAL (operands[3]);
  mask |= (INTVAL (operands[4]) - 2) << 1;
  operands[3] = GEN_INT (mask);

  return "vshufpd\t{%3, %2, %1, %0%{%6%}%N5|%0%{%6%}%N5, %1, %2, %3}";
}
  [(set_attr "type" "sseshuf")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V2DF")])

;; punpcklqdq and punpckhqdq are shorter than shufpd.
(define_insn "avx2_interleave_highv4di<mask_name>"
  [(set (match_operand:V4DI 0 "register_operand" "=v")
	(vec_select:V4DI
	  (vec_concat:V8DI
	    (match_operand:V4DI 1 "register_operand" "v")
	    (match_operand:V4DI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 1)
		     (const_int 5)
		     (const_int 3)
		     (const_int 7)])))]
  "TARGET_AVX2 && <mask_avx512vl_condition>"
  "vpunpckhqdq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "OI")])

(define_insn "<mask_codefor>avx512f_interleave_highv8di<mask_name>"
  [(set (match_operand:V8DI 0 "register_operand" "=v")
	(vec_select:V8DI
	  (vec_concat:V16DI
	    (match_operand:V8DI 1 "register_operand" "v")
	    (match_operand:V8DI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 1) (const_int 9)
		     (const_int 3) (const_int 11)
		     (const_int 5) (const_int 13)
		     (const_int 7) (const_int 15)])))]
  "TARGET_AVX512F"
  "vpunpckhqdq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "vec_interleave_highv2di<mask_name>"
  [(set (match_operand:V2DI 0 "register_operand" "=x,v")
	(vec_select:V2DI
	  (vec_concat:V4DI
	    (match_operand:V2DI 1 "register_operand" "0,v")
	    (match_operand:V2DI 2 "vector_operand" "xBm,vm"))
	  (parallel [(const_int 1)
		     (const_int 3)])))]
  "TARGET_SSE2 && <mask_avx512vl_condition>"
  "@
   punpckhqdq\t{%2, %0|%0, %2}
   vpunpckhqdq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,<mask_prefix>")
   (set_attr "mode" "TI")])

(define_insn "avx2_interleave_lowv4di<mask_name>"
  [(set (match_operand:V4DI 0 "register_operand" "=v")
	(vec_select:V4DI
	  (vec_concat:V8DI
	    (match_operand:V4DI 1 "register_operand" "v")
	    (match_operand:V4DI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 0)
		     (const_int 4)
		     (const_int 2)
		     (const_int 6)])))]
  "TARGET_AVX2 && <mask_avx512vl_condition>"
  "vpunpcklqdq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "OI")])

(define_insn "<mask_codefor>avx512f_interleave_lowv8di<mask_name>"
  [(set (match_operand:V8DI 0 "register_operand" "=v")
	(vec_select:V8DI
	  (vec_concat:V16DI
	    (match_operand:V8DI 1 "register_operand" "v")
	    (match_operand:V8DI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 2) (const_int 10)
		     (const_int 4) (const_int 12)
		     (const_int 6) (const_int 14)])))]
  "TARGET_AVX512F"
  "vpunpcklqdq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "vec_interleave_lowv2di<mask_name>"
  [(set (match_operand:V2DI 0 "register_operand" "=x,v")
	(vec_select:V2DI
	  (vec_concat:V4DI
	    (match_operand:V2DI 1 "register_operand" "0,v")
	    (match_operand:V2DI 2 "vector_operand" "xBm,vm"))
	  (parallel [(const_int 0)
		     (const_int 2)])))]
  "TARGET_SSE2 && <mask_avx512vl_condition>"
  "@
   punpcklqdq\t{%2, %0|%0, %2}
   vpunpcklqdq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "TI")])

(define_insn "sse2_shufpd_<mode>"
  [(set (match_operand:VI8F_128 0 "register_operand" "=x,v")
	(vec_select:VI8F_128
	  (vec_concat:<ssedoublevecmode>
	    (match_operand:VI8F_128 1 "register_operand" "0,v")
	    (match_operand:VI8F_128 2 "vector_operand" "xBm,vm"))
	  (parallel [(match_operand 3 "const_0_to_1_operand")
		     (match_operand 4 "const_2_to_3_operand")])))]
  "TARGET_SSE2"
{
  int mask;
  mask = INTVAL (operands[3]);
  mask |= (INTVAL (operands[4]) - 2) << 1;
  operands[3] = GEN_INT (mask);

  switch (which_alternative)
    {
    case 0:
      return "shufpd\t{%3, %2, %0|%0, %2, %3}";
    case 1:
      return "vshufpd\t{%3, %2, %1, %0|%0, %1, %2, %3}";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseshuf")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "orig,maybe_evex")
   (set_attr "mode" "V2DF")])

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "sse2_storehpd"
  [(set (match_operand:DF 0 "nonimmediate_operand"     "=m,x,Yv,x,*f,r")
	(vec_select:DF
	  (match_operand:V2DF 1 "nonimmediate_operand" " v,0, v,o,o,o")
	  (parallel [(const_int 1)])))]
  "TARGET_SSE2 && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   %vmovhpd\t{%1, %0|%0, %1}
   unpckhpd\t%0, %0
   vunpckhpd\t{%d1, %0|%0, %d1}
   #
   #
   #"
  [(set_attr "isa" "*,noavx,avx,*,*,*")
   (set_attr "type" "ssemov,sselog1,sselog1,ssemov,fmov,imov")
   (set (attr "prefix_data16")
     (if_then_else
       (and (eq_attr "alternative" "0")
	    (not (match_test "TARGET_AVX")))
       (const_string "1")
       (const_string "*")))
   (set_attr "prefix" "maybe_vex,orig,maybe_evex,*,*,*")
   (set_attr "mode" "V1DF,V1DF,V2DF,DF,DF,DF")])

(define_split
  [(set (match_operand:DF 0 "register_operand")
	(vec_select:DF
	  (match_operand:V2DF 1 "memory_operand")
	  (parallel [(const_int 1)])))]
  "TARGET_SSE2 && reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  "operands[1] = adjust_address (operands[1], DFmode, 8);")

(define_insn "*vec_extractv2df_1_sse"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=m,x,x")
	(vec_select:DF
	  (match_operand:V2DF 1 "nonimmediate_operand" "x,x,o")
	  (parallel [(const_int 1)])))]
  "!TARGET_SSE2 && TARGET_SSE
   && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   movhps\t{%1, %0|%q0, %1}
   movhlps\t{%1, %0|%0, %1}
   movlps\t{%H1, %0|%0, %H1}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "V2SF,V4SF,V2SF")])

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "sse2_storelpd"
  [(set (match_operand:DF 0 "nonimmediate_operand"     "=m,x,x,*f,r")
	(vec_select:DF
	  (match_operand:V2DF 1 "nonimmediate_operand" " v,x,m,m,m")
	  (parallel [(const_int 0)])))]
  "TARGET_SSE2 && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   %vmovlpd\t{%1, %0|%0, %1}
   #
   #
   #
   #"
  [(set_attr "type" "ssemov,ssemov,ssemov,fmov,imov")
   (set (attr "prefix_data16")
     (if_then_else (eq_attr "alternative" "0")
		   (const_string "1")
		   (const_string "*")))
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V1DF,DF,DF,DF,DF")])

(define_split
  [(set (match_operand:DF 0 "register_operand")
	(vec_select:DF
	  (match_operand:V2DF 1 "nonimmediate_operand")
	  (parallel [(const_int 0)])))]
  "TARGET_SSE2 && reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  "operands[1] = gen_lowpart (DFmode, operands[1]);")

(define_insn "*vec_extractv2df_0_sse"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=m,x,x")
	(vec_select:DF
	  (match_operand:V2DF 1 "nonimmediate_operand" "x,x,m")
	  (parallel [(const_int 0)])))]
  "!TARGET_SSE2 && TARGET_SSE
   && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   movlps\t{%1, %0|%0, %1}
   movaps\t{%1, %0|%0, %1}
   movlps\t{%1, %0|%0, %q1}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "V2SF,V4SF,V2SF")])

(define_expand "sse2_loadhpd_exp"
  [(set (match_operand:V2DF 0 "nonimmediate_operand")
	(vec_concat:V2DF
	  (vec_select:DF
	    (match_operand:V2DF 1 "nonimmediate_operand")
	    (parallel [(const_int 0)]))
	  (match_operand:DF 2 "nonimmediate_operand")))]
  "TARGET_SSE2"
{
  rtx dst = ix86_fixup_binary_operands (UNKNOWN, V2DFmode, operands);

  emit_insn (gen_sse2_loadhpd (dst, operands[1], operands[2]));

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);

  DONE;
})

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "sse2_loadhpd"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"
	  "=x,v,x,v ,o,o ,o")
	(vec_concat:V2DF
	  (vec_select:DF
	    (match_operand:V2DF 1 "nonimmediate_operand"
	  " 0,v,0,v ,0,0 ,0")
	    (parallel [(const_int 0)]))
	  (match_operand:DF 2 "nonimmediate_operand"
	  " m,m,x,Yv,x,*f,r")))]
  "TARGET_SSE2 && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   movhpd\t{%2, %0|%0, %2}
   vmovhpd\t{%2, %1, %0|%0, %1, %2}
   unpcklpd\t{%2, %0|%0, %2}
   vunpcklpd\t{%2, %1, %0|%0, %1, %2}
   #
   #
   #"
  [(set_attr "isa" "noavx,avx,noavx,avx,*,*,*")
   (set_attr "type" "ssemov,ssemov,sselog,sselog,ssemov,fmov,imov")
   (set (attr "prefix_data16")
     (if_then_else (eq_attr "alternative" "0")
		   (const_string "1")
		   (const_string "*")))
   (set_attr "prefix" "orig,maybe_evex,orig,maybe_evex,*,*,*")
   (set_attr "mode" "V1DF,V1DF,V2DF,V2DF,DF,DF,DF")])

(define_split
  [(set (match_operand:V2DF 0 "memory_operand")
	(vec_concat:V2DF
	  (vec_select:DF (match_dup 0) (parallel [(const_int 0)]))
	  (match_operand:DF 1 "register_operand")))]
  "TARGET_SSE2 && reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  "operands[0] = adjust_address (operands[0], DFmode, 8);")

(define_expand "sse2_loadlpd_exp"
  [(set (match_operand:V2DF 0 "nonimmediate_operand")
	(vec_concat:V2DF
	  (match_operand:DF 2 "nonimmediate_operand")
	  (vec_select:DF
	    (match_operand:V2DF 1 "nonimmediate_operand")
	    (parallel [(const_int 1)]))))]
  "TARGET_SSE2"
{
  rtx dst = ix86_fixup_binary_operands (UNKNOWN, V2DFmode, operands);

  emit_insn (gen_sse2_loadlpd (dst, operands[1], operands[2]));

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);

  DONE;
})

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "sse2_loadlpd"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"
	  "=v,x,v,x,v,x,x,v,m,m ,m")
	(vec_concat:V2DF
	  (match_operand:DF 2 "nonimmediate_operand"
	  "vm,m,m,x,v,0,0,v,x,*f,r")
	  (vec_select:DF
	    (match_operand:V2DF 1 "nonimm_or_0_operand"
	  " C,0,v,0,v,x,o,o,0,0 ,0")
	    (parallel [(const_int 1)]))))]
  "TARGET_SSE2 && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   %vmovq\t{%2, %0|%0, %2}
   movlpd\t{%2, %0|%0, %2}
   vmovlpd\t{%2, %1, %0|%0, %1, %2}
   movsd\t{%2, %0|%0, %2}
   vmovsd\t{%2, %1, %0|%0, %1, %2}
   shufpd\t{$2, %1, %0|%0, %1, 2}
   movhpd\t{%H1, %0|%0, %H1}
   vmovhpd\t{%H1, %2, %0|%0, %2, %H1}
   #
   #
   #"
  [(set_attr "isa" "*,noavx,avx,noavx,avx,noavx,noavx,avx,*,*,*")
   (set (attr "type")
     (cond [(eq_attr "alternative" "5")
	      (const_string "sselog")
	    (eq_attr "alternative" "9")
	      (const_string "fmov")
	    (eq_attr "alternative" "10")
	      (const_string "imov")
	   ]
	   (const_string "ssemov")))
   (set (attr "prefix_data16")
     (if_then_else (eq_attr "alternative" "1,6")
		   (const_string "1")
		   (const_string "*")))
   (set (attr "length_immediate")
     (if_then_else (eq_attr "alternative" "5")
		   (const_string "1")
		   (const_string "*")))
   (set (attr "prefix")
     (cond [(eq_attr "alternative" "0")
	      (const_string "maybe_vex")
	    (eq_attr "alternative" "1,3,5,6")
	      (const_string "orig")
	    (eq_attr "alternative" "2,4,7")
	      (const_string "maybe_evex")
	   ]
	   (const_string "*")))
   (set_attr "mode" "DF,V1DF,V1DF,V1DF,V1DF,V2DF,V1DF,V1DF,DF,DF,DF")])

(define_split
  [(set (match_operand:V2DF 0 "memory_operand")
	(vec_concat:V2DF
	  (match_operand:DF 1 "register_operand")
	  (vec_select:DF (match_dup 0) (parallel [(const_int 1)]))))]
  "TARGET_SSE2 && reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  "operands[0] = adjust_address (operands[0], DFmode, 0);")

(define_insn "sse2_movsd"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"   "=x,v,x,v,m,x,x,v,o")
	(vec_merge:V2DF
	  (match_operand:V2DF 2 "nonimmediate_operand" " x,v,m,m,v,0,0,v,0")
	  (match_operand:V2DF 1 "nonimmediate_operand" " 0,v,0,v,0,x,o,o,v")
	  (const_int 1)))]
  "TARGET_SSE2"
  "@
   movsd\t{%2, %0|%0, %2}
   vmovsd\t{%2, %1, %0|%0, %1, %2}
   movlpd\t{%2, %0|%0, %q2}
   vmovlpd\t{%2, %1, %0|%0, %1, %q2}
   %vmovlpd\t{%2, %0|%q0, %2}
   shufpd\t{$2, %1, %0|%0, %1, 2}
   movhps\t{%H1, %0|%0, %H1}
   vmovhps\t{%H1, %2, %0|%0, %2, %H1}
   %vmovhps\t{%1, %H0|%H0, %1}"
  [(set_attr "isa" "noavx,avx,noavx,avx,*,noavx,noavx,avx,*")
   (set (attr "type")
     (if_then_else
       (eq_attr "alternative" "5")
       (const_string "sselog")
       (const_string "ssemov")))
   (set (attr "prefix_data16")
     (if_then_else
       (and (eq_attr "alternative" "2,4")
	    (not (match_test "TARGET_AVX")))
       (const_string "1")
       (const_string "*")))
   (set (attr "length_immediate")
     (if_then_else (eq_attr "alternative" "5")
		   (const_string "1")
		   (const_string "*")))
   (set (attr "prefix")
     (cond [(eq_attr "alternative" "1,3,7")
	      (const_string "maybe_evex")
	    (eq_attr "alternative" "4,8")
	      (const_string "maybe_vex")
	   ]
	   (const_string "orig")))
   (set_attr "mode" "DF,DF,V1DF,V1DF,V1DF,V2DF,V1DF,V1DF,V1DF")])

(define_insn "vec_dupv2df<mask_name>"
  [(set (match_operand:V2DF 0 "register_operand"     "=x,x,v")
	(vec_duplicate:V2DF
	  (match_operand:DF 1 "nonimmediate_operand" " 0,xm,vm")))]
  "TARGET_SSE2 && <mask_avx512vl_condition>"
  "@
   unpcklpd\t%0, %0
   %vmovddup\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}
   vmovddup\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "isa" "noavx,sse3,avx512vl")
   (set_attr "type" "sselog1")
   (set_attr "prefix" "orig,maybe_vex,evex")
   (set_attr "mode" "V2DF,DF,DF")])

(define_insn "vec_concatv2df"
  [(set (match_operand:V2DF 0 "register_operand"     "=x,x,v,x,v,x,x, v,x,x")
	(vec_concat:V2DF
	  (match_operand:DF 1 "nonimmediate_operand" " 0,x,v,m,m,0,x,xm,0,0")
	  (match_operand:DF 2 "nonimm_or_0_operand"  " x,x,v,1,1,m,m, C,x,m")))]
  "TARGET_SSE
   && (!(MEM_P (operands[1]) && MEM_P (operands[2]))
       || (TARGET_SSE3 && rtx_equal_p (operands[1], operands[2])))"
  "@
   unpcklpd\t{%2, %0|%0, %2}
   vunpcklpd\t{%2, %1, %0|%0, %1, %2}
   vunpcklpd\t{%2, %1, %0|%0, %1, %2}
   %vmovddup\t{%1, %0|%0, %1}
   vmovddup\t{%1, %0|%0, %1}
   movhpd\t{%2, %0|%0, %2}
   vmovhpd\t{%2, %1, %0|%0, %1, %2}
   %vmovq\t{%1, %0|%0, %1}
   movlhps\t{%2, %0|%0, %2}
   movhps\t{%2, %0|%0, %2}"
  [(set (attr "isa")
     (cond [(eq_attr "alternative" "0,5")
	      (const_string "sse2_noavx")
	    (eq_attr "alternative" "1,6")
	      (const_string "avx")
	    (eq_attr "alternative" "2,4")
	      (const_string "avx512vl")
	    (eq_attr "alternative" "3")
	      (const_string "sse3")
	    (eq_attr "alternative" "7")
	      (const_string "sse2")
	   ]
	   (const_string "noavx")))
   (set (attr "type")
     (if_then_else
       (eq_attr "alternative" "0,1,2,3,4")
       (const_string "sselog")
       (const_string "ssemov")))
   (set (attr "prefix_data16")
	(if_then_else (eq_attr "alternative" "5")
		      (const_string "1")
		      (const_string "*")))
   (set (attr "prefix")
     (cond [(eq_attr "alternative" "1,6")
	      (const_string "vex")
	    (eq_attr "alternative" "2,4")
	      (const_string "evex")
	    (eq_attr "alternative" "3,7")
	      (const_string "maybe_vex")
	   ]
	   (const_string "orig")))
   (set_attr "mode" "V2DF,V2DF,V2DF, DF, DF, V1DF,V1DF,DF,V4SF,V2SF")])

;; vmovq clears also the higher bits.
(define_insn "vec_set<mode>_0"
  [(set (match_operand:VF2_512_256 0 "register_operand" "=v")
	(vec_merge:VF2_512_256
	  (vec_duplicate:VF2_512_256
	    (match_operand:<ssescalarmode> 2 "general_operand" "xm"))
	  (match_operand:VF2_512_256 1 "const0_operand" "C")
	  (const_int 1)))]
  "TARGET_AVX"
  "vmovq\t{%2, %x0|%x0, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "DF")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel integer down-conversion operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_mode_iterator PMOV_DST_MODE_1 [V16QI V16HI V8SI V8HI])
(define_mode_attr pmov_src_mode
  [(V16QI "V16SI") (V16HI "V16SI") (V8SI "V8DI") (V8HI "V8DI")])
(define_mode_attr pmov_src_lower
  [(V16QI "v16si") (V16HI "v16si") (V8SI "v8di") (V8HI "v8di")])
(define_mode_attr pmov_suff_1
  [(V16QI "db") (V16HI "dw") (V8SI "qd") (V8HI "qw")])

(define_insn "*avx512f_<code><pmov_src_lower><mode>2"
  [(set (match_operand:PMOV_DST_MODE_1 0 "nonimmediate_operand" "=v,m")
	(any_truncate:PMOV_DST_MODE_1
	  (match_operand:<pmov_src_mode> 1 "register_operand" "v,v")))]
  "TARGET_AVX512F"
  "vpmov<trunsuffix><pmov_suff_1>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "none,store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "avx512f_<code><pmov_src_lower><mode>2_mask"
  [(set (match_operand:PMOV_DST_MODE_1 0 "nonimmediate_operand" "=v,m")
    (vec_merge:PMOV_DST_MODE_1
      (any_truncate:PMOV_DST_MODE_1
        (match_operand:<pmov_src_mode> 1 "register_operand" "v,v"))
      (match_operand:PMOV_DST_MODE_1 2 "nonimm_or_0_operand" "0C,0")
      (match_operand:<avx512fmaskmode> 3 "register_operand" "Yk,Yk")))]
  "TARGET_AVX512F"
  "vpmov<trunsuffix><pmov_suff_1>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "none,store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "avx512f_<code><pmov_src_lower><mode>2_mask_store"
  [(set (match_operand:PMOV_DST_MODE_1 0 "memory_operand")
    (vec_merge:PMOV_DST_MODE_1
      (any_truncate:PMOV_DST_MODE_1
        (match_operand:<pmov_src_mode> 1 "register_operand"))
      (match_dup 0)
      (match_operand:<avx512fmaskmode> 2 "register_operand")))]
  "TARGET_AVX512F")

(define_insn "avx512bw_<code>v32hiv32qi2"
  [(set (match_operand:V32QI 0 "nonimmediate_operand" "=v,m")
	(any_truncate:V32QI
	    (match_operand:V32HI 1 "register_operand" "v,v")))]
  "TARGET_AVX512BW"
  "vpmov<trunsuffix>wb\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "none,store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "avx512bw_<code>v32hiv32qi2_mask"
  [(set (match_operand:V32QI 0 "nonimmediate_operand" "=v,m")
    (vec_merge:V32QI
      (any_truncate:V32QI
        (match_operand:V32HI 1 "register_operand" "v,v"))
      (match_operand:V32QI 2 "nonimm_or_0_operand" "0C,0")
      (match_operand:SI 3 "register_operand" "Yk,Yk")))]
  "TARGET_AVX512BW"
  "vpmov<trunsuffix>wb\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "none,store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_expand "avx512bw_<code>v32hiv32qi2_mask_store"
  [(set (match_operand:V32QI 0 "nonimmediate_operand")
    (vec_merge:V32QI
      (any_truncate:V32QI
        (match_operand:V32HI 1 "register_operand"))
      (match_dup 0)
      (match_operand:SI 2 "register_operand")))]
  "TARGET_AVX512BW")

(define_mode_iterator PMOV_DST_MODE_2
  [V4SI V8HI (V16QI "TARGET_AVX512BW")])
(define_mode_attr pmov_suff_2
  [(V16QI "wb") (V8HI "dw") (V4SI "qd")])

(define_insn "*avx512vl_<code><ssedoublemodelower><mode>2"
  [(set (match_operand:PMOV_DST_MODE_2 0 "nonimmediate_operand" "=v,m")
	(any_truncate:PMOV_DST_MODE_2
	    (match_operand:<ssedoublemode> 1 "register_operand" "v,v")))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix><pmov_suff_2>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "none,store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_<code><ssedoublemodelower><mode>2_mask"
  [(set (match_operand:PMOV_DST_MODE_2 0 "nonimmediate_operand" "=v,m")
    (vec_merge:PMOV_DST_MODE_2
      (any_truncate:PMOV_DST_MODE_2
        (match_operand:<ssedoublemode> 1 "register_operand" "v,v"))
      (match_operand:PMOV_DST_MODE_2 2 "nonimm_or_0_operand" "0C,0")
      (match_operand:<avx512fmaskmode> 3 "register_operand" "Yk,Yk")))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix><pmov_suff_2>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "none,store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "<avx512>_<code><ssedoublemodelower><mode>2_mask_store"
  [(set (match_operand:PMOV_DST_MODE_2 0 "nonimmediate_operand")
    (vec_merge:PMOV_DST_MODE_2
      (any_truncate:PMOV_DST_MODE_2
        (match_operand:<ssedoublemode> 1 "register_operand"))
      (match_dup 0)
      (match_operand:<avx512fmaskmode> 2 "register_operand")))]
  "TARGET_AVX512VL")

(define_mode_iterator PMOV_SRC_MODE_3 [V4DI V2DI V8SI V4SI (V8HI "TARGET_AVX512BW")])
(define_mode_attr pmov_dst_3
  [(V4DI "V4QI") (V2DI "V2QI") (V8SI "V8QI") (V4SI "V4QI") (V8HI "V8QI")])
(define_mode_attr pmov_dst_zeroed_3
  [(V4DI "V12QI") (V2DI "V14QI") (V8SI "V8QI") (V4SI "V12QI") (V8HI "V8QI")])
(define_mode_attr pmov_suff_3
  [(V4DI "qb") (V2DI "qb") (V8SI "db") (V4SI "db") (V8HI "wb")])

(define_insn "*avx512vl_<code><mode>v<ssescalarnum>qi2"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
    (vec_concat:V16QI
      (any_truncate:<pmov_dst_3>
	      (match_operand:PMOV_SRC_MODE_3 1 "register_operand" "v"))
      (match_operand:<pmov_dst_zeroed_3> 2 "const0_operand")))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix><pmov_suff_3>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "*avx512vl_<code>v2div2qi2_store"
  [(set (match_operand:V16QI 0 "memory_operand" "=m")
    (vec_concat:V16QI
      (any_truncate:V2QI
	      (match_operand:V2DI 1 "register_operand" "v"))
      (vec_select:V14QI
        (match_dup 0)
        (parallel [(const_int 2) (const_int 3)
                   (const_int 4) (const_int 5)
                   (const_int 6) (const_int 7)
                   (const_int 8) (const_int 9)
                   (const_int 10) (const_int 11)
                   (const_int 12) (const_int 13)
                   (const_int 14) (const_int 15)]))))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix>qb\t{%1, %0|%w0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "avx512vl_<code>v2div2qi2_mask"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
    (vec_concat:V16QI
      (vec_merge:V2QI
        (any_truncate:V2QI
          (match_operand:V2DI 1 "register_operand" "v"))
        (vec_select:V2QI
          (match_operand:V16QI 2 "nonimm_or_0_operand" "0C")
          (parallel [(const_int 0) (const_int 1)]))
        (match_operand:QI 3 "register_operand" "Yk"))
      (const_vector:V14QI [(const_int 0) (const_int 0)
                           (const_int 0) (const_int 0)
                           (const_int 0) (const_int 0)
                           (const_int 0) (const_int 0)
                           (const_int 0) (const_int 0)
                           (const_int 0) (const_int 0)
                           (const_int 0) (const_int 0)])))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix>qb\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "*avx512vl_<code>v2div2qi2_mask_1"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
    (vec_concat:V16QI
      (vec_merge:V2QI
	(any_truncate:V2QI
	  (match_operand:V2DI 1 "register_operand" "v"))
	(const_vector:V2QI [(const_int 0) (const_int 0)])
	(match_operand:QI 2 "register_operand" "Yk"))
      (const_vector:V14QI [(const_int 0) (const_int 0)
			   (const_int 0) (const_int 0)
			   (const_int 0) (const_int 0)
			   (const_int 0) (const_int 0)
			   (const_int 0) (const_int 0)
			   (const_int 0) (const_int 0)
			   (const_int 0) (const_int 0)])))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix>qb\t{%1, %0%{%2%}%{z%}|%0%{%2%}%{z%}, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "avx512vl_<code>v2div2qi2_mask_store"
  [(set (match_operand:V16QI 0 "memory_operand" "=m")
    (vec_concat:V16QI
      (vec_merge:V2QI
        (any_truncate:V2QI
          (match_operand:V2DI 1 "register_operand" "v"))
        (vec_select:V2QI
          (match_dup 0)
          (parallel [(const_int 0) (const_int 1)]))
        (match_operand:QI 2 "register_operand" "Yk"))
      (vec_select:V14QI
        (match_dup 0)
        (parallel [(const_int 2) (const_int 3)
                   (const_int 4) (const_int 5)
                   (const_int 6) (const_int 7)
                   (const_int 8) (const_int 9)
                   (const_int 10) (const_int 11)
                   (const_int 12) (const_int 13)
                   (const_int 14) (const_int 15)]))))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix>qb\t{%1, %0%{%2%}|%w0%{%2%}, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "*avx512vl_<code><mode>v4qi2_store"
  [(set (match_operand:V16QI 0 "memory_operand" "=m")
    (vec_concat:V16QI
      (any_truncate:V4QI
	      (match_operand:VI4_128_8_256 1 "register_operand" "v"))
      (vec_select:V12QI
        (match_dup 0)
        (parallel [(const_int 4) (const_int 5)
                   (const_int 6) (const_int 7)
                   (const_int 8) (const_int 9)
                   (const_int 10) (const_int 11)
                   (const_int 12) (const_int 13)
                   (const_int 14) (const_int 15)]))))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix><pmov_suff_3>\t{%1, %0|%k0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "avx512vl_<code><mode>v4qi2_mask"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
    (vec_concat:V16QI
      (vec_merge:V4QI
        (any_truncate:V4QI
          (match_operand:VI4_128_8_256 1 "register_operand" "v"))
        (vec_select:V4QI
          (match_operand:V16QI 2 "nonimm_or_0_operand" "0C")
          (parallel [(const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)]))
        (match_operand:QI 3 "register_operand" "Yk"))
      (const_vector:V12QI [(const_int 0) (const_int 0)
                           (const_int 0) (const_int 0)
                           (const_int 0) (const_int 0)
                           (const_int 0) (const_int 0)
                           (const_int 0) (const_int 0)
                           (const_int 0) (const_int 0)])))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix><pmov_suff_3>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "*avx512vl_<code><mode>v4qi2_mask_1"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
    (vec_concat:V16QI
      (vec_merge:V4QI
	(any_truncate:V4QI
	  (match_operand:VI4_128_8_256 1 "register_operand" "v"))
	(const_vector:V4QI [(const_int 0) (const_int 0)
			    (const_int 0) (const_int 0)])
	(match_operand:QI 2 "register_operand" "Yk"))
      (const_vector:V12QI [(const_int 0) (const_int 0)
			   (const_int 0) (const_int 0)
			   (const_int 0) (const_int 0)
			   (const_int 0) (const_int 0)
			   (const_int 0) (const_int 0)
			   (const_int 0) (const_int 0)])))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix><pmov_suff_3>\t{%1, %0%{%2%}%{z%}|%0%{%2%}%{z%}, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "avx512vl_<code><mode>v4qi2_mask_store"
  [(set (match_operand:V16QI 0 "memory_operand" "=m")
    (vec_concat:V16QI
      (vec_merge:V4QI
        (any_truncate:V4QI
          (match_operand:VI4_128_8_256 1 "register_operand" "v"))
        (vec_select:V4QI
          (match_dup 0)
          (parallel [(const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)]))
        (match_operand:QI 2 "register_operand" "Yk"))
      (vec_select:V12QI
        (match_dup 0)
        (parallel [(const_int 4) (const_int 5)
                   (const_int 6) (const_int 7)
                   (const_int 8) (const_int 9)
                   (const_int 10) (const_int 11)
                   (const_int 12) (const_int 13)
                   (const_int 14) (const_int 15)]))))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix><pmov_suff_3>\t{%1, %0%{%2%}|%k0%{%2%}, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_mode_iterator VI2_128_BW_4_256
  [(V8HI "TARGET_AVX512BW") V8SI])

(define_insn "*avx512vl_<code><mode>v8qi2_store"
  [(set (match_operand:V16QI 0 "memory_operand" "=m")
    (vec_concat:V16QI
      (any_truncate:V8QI
	      (match_operand:VI2_128_BW_4_256 1 "register_operand" "v"))
      (vec_select:V8QI
        (match_dup 0)
        (parallel [(const_int 8) (const_int 9)
                   (const_int 10) (const_int 11)
                   (const_int 12) (const_int 13)
                   (const_int 14) (const_int 15)]))))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix><pmov_suff_3>\t{%1, %0|%q0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "avx512vl_<code><mode>v8qi2_mask"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
    (vec_concat:V16QI
      (vec_merge:V8QI
        (any_truncate:V8QI
          (match_operand:VI2_128_BW_4_256 1 "register_operand" "v"))
        (vec_select:V8QI
          (match_operand:V16QI 2 "nonimm_or_0_operand" "0C")
          (parallel [(const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)
                     (const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)]))
        (match_operand:QI 3 "register_operand" "Yk"))
      (const_vector:V8QI [(const_int 0) (const_int 0)
                          (const_int 0) (const_int 0)
                          (const_int 0) (const_int 0)
                          (const_int 0) (const_int 0)])))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix><pmov_suff_3>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "*avx512vl_<code><mode>v8qi2_mask_1"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
    (vec_concat:V16QI
      (vec_merge:V8QI
	(any_truncate:V8QI
	  (match_operand:VI2_128_BW_4_256 1 "register_operand" "v"))
	(const_vector:V8QI [(const_int 0) (const_int 0)
			    (const_int 0) (const_int 0)
			    (const_int 0) (const_int 0)
			    (const_int 0) (const_int 0)])
	(match_operand:QI 2 "register_operand" "Yk"))
      (const_vector:V8QI [(const_int 0) (const_int 0)
			  (const_int 0) (const_int 0)
			  (const_int 0) (const_int 0)
			  (const_int 0) (const_int 0)])))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix><pmov_suff_3>\t{%1, %0%{%2%}%{z%}|%0%{%2%}%{z%}, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "avx512vl_<code><mode>v8qi2_mask_store"
  [(set (match_operand:V16QI 0 "memory_operand" "=m")
    (vec_concat:V16QI
      (vec_merge:V8QI
        (any_truncate:V8QI
          (match_operand:VI2_128_BW_4_256 1 "register_operand" "v"))
        (vec_select:V8QI
          (match_dup 0)
          (parallel [(const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)
                     (const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)]))
        (match_operand:QI 2 "register_operand" "Yk"))
      (vec_select:V8QI
        (match_dup 0)
        (parallel [(const_int 8) (const_int 9)
                   (const_int 10) (const_int 11)
                   (const_int 12) (const_int 13)
                   (const_int 14) (const_int 15)]))))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix><pmov_suff_3>\t{%1, %0%{%2%}|%q0%{%2%}, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_mode_iterator PMOV_SRC_MODE_4 [V4DI V2DI V4SI])
(define_mode_attr pmov_dst_4
  [(V4DI "V4HI") (V2DI "V2HI") (V4SI "V4HI")])
(define_mode_attr pmov_dst_zeroed_4
  [(V4DI "V4HI") (V2DI "V6HI") (V4SI "V4HI")])
(define_mode_attr pmov_suff_4
  [(V4DI "qw") (V2DI "qw") (V4SI "dw")])

(define_insn "*avx512vl_<code><mode>v<ssescalarnum>hi2"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
    (vec_concat:V8HI
      (any_truncate:<pmov_dst_4>
	      (match_operand:PMOV_SRC_MODE_4 1 "register_operand" "v"))
      (match_operand:<pmov_dst_zeroed_4> 2 "const0_operand")))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix><pmov_suff_4>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "*avx512vl_<code><mode>v4hi2_store"
  [(set (match_operand:V8HI 0 "memory_operand" "=m")
    (vec_concat:V8HI
      (any_truncate:V4HI
	      (match_operand:VI4_128_8_256 1 "register_operand" "v"))
      (vec_select:V4HI
        (match_dup 0)
        (parallel [(const_int 4) (const_int 5)
                   (const_int 6) (const_int 7)]))))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix><pmov_suff_4>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "avx512vl_<code><mode>v4hi2_mask"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
    (vec_concat:V8HI
      (vec_merge:V4HI
        (any_truncate:V4HI
          (match_operand:VI4_128_8_256 1 "register_operand" "v"))
        (vec_select:V4HI
          (match_operand:V8HI 2 "nonimm_or_0_operand" "0C")
          (parallel [(const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)]))
        (match_operand:QI 3 "register_operand" "Yk"))
      (const_vector:V4HI [(const_int 0) (const_int 0)
                          (const_int 0) (const_int 0)])))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix><pmov_suff_4>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "*avx512vl_<code><mode>v4hi2_mask_1"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
    (vec_concat:V8HI
      (vec_merge:V4HI
	(any_truncate:V4HI
	  (match_operand:VI4_128_8_256 1 "register_operand" "v"))
	(const_vector:V4HI [(const_int 0) (const_int 0)
			    (const_int 0) (const_int 0)])
	(match_operand:QI 2 "register_operand" "Yk"))
      (const_vector:V4HI [(const_int 0) (const_int 0)
			  (const_int 0) (const_int 0)])))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix><pmov_suff_4>\t{%1, %0%{%2%}%{z%}|%0%{%2%}%{z%}, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "avx512vl_<code><mode>v4hi2_mask_store"
  [(set (match_operand:V8HI 0 "memory_operand" "=m")
    (vec_concat:V8HI
      (vec_merge:V4HI
        (any_truncate:V4HI
          (match_operand:VI4_128_8_256 1 "register_operand" "v"))
        (vec_select:V4HI
          (match_dup 0)
          (parallel [(const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)]))
        (match_operand:QI 2 "register_operand" "Yk"))
      (vec_select:V4HI
        (match_dup 0)
        (parallel [(const_int 4) (const_int 5)
                   (const_int 6) (const_int 7)]))))]
  "TARGET_AVX512VL"
{
  if (GET_MODE_SIZE (GET_MODE_INNER (<MODE>mode)) == 4)
    return "vpmov<trunsuffix><pmov_suff_4>\t{%1, %0%{%2%}|%0%{%2%}, %t1}";
  return "vpmov<trunsuffix><pmov_suff_4>\t{%1, %0%{%2%}|%0%{%2%}, %g1}";
}
  [(set_attr "type" "ssemov")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "*avx512vl_<code>v2div2hi2_store"
  [(set (match_operand:V8HI 0 "memory_operand" "=m")
    (vec_concat:V8HI
      (any_truncate:V2HI
	      (match_operand:V2DI 1 "register_operand" "v"))
      (vec_select:V6HI
        (match_dup 0)
        (parallel [(const_int 2) (const_int 3)
                   (const_int 4) (const_int 5)
                   (const_int 6) (const_int 7)]))))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix>qw\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "avx512vl_<code>v2div2hi2_mask"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
    (vec_concat:V8HI
      (vec_merge:V2HI
        (any_truncate:V2HI
          (match_operand:V2DI 1 "register_operand" "v"))
        (vec_select:V2HI
          (match_operand:V8HI 2 "nonimm_or_0_operand" "0C")
          (parallel [(const_int 0) (const_int 1)]))
        (match_operand:QI 3 "register_operand" "Yk"))
      (const_vector:V6HI [(const_int 0) (const_int 0)
                          (const_int 0) (const_int 0)
                          (const_int 0) (const_int 0)])))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix>qw\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "*avx512vl_<code>v2div2hi2_mask_1"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
    (vec_concat:V8HI
      (vec_merge:V2HI
	(any_truncate:V2HI
	  (match_operand:V2DI 1 "register_operand" "v"))
	(const_vector:V2HI [(const_int 0) (const_int 0)])
	(match_operand:QI 2 "register_operand" "Yk"))
      (const_vector:V6HI [(const_int 0) (const_int 0)
			  (const_int 0) (const_int 0)
			  (const_int 0) (const_int 0)])))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix>qw\t{%1, %0%{%2%}%{z%}|%0%{%2%}%{z%}, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "avx512vl_<code>v2div2hi2_mask_store"
  [(set (match_operand:V8HI 0 "memory_operand" "=m")
    (vec_concat:V8HI
      (vec_merge:V2HI
        (any_truncate:V2HI
          (match_operand:V2DI 1 "register_operand" "v"))
        (vec_select:V2HI
          (match_dup 0)
          (parallel [(const_int 0) (const_int 1)]))
        (match_operand:QI 2 "register_operand" "Yk"))
      (vec_select:V6HI
        (match_dup 0)
        (parallel [(const_int 2) (const_int 3)
                   (const_int 4) (const_int 5)
                   (const_int 6) (const_int 7)]))))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix>qw\t{%1, %0%{%2%}|%0%{%2%}, %g1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "*avx512vl_<code>v2div2si2"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
    (vec_concat:V4SI
      (any_truncate:V2SI
	      (match_operand:V2DI 1 "register_operand" "v"))
      (match_operand:V2SI 2 "const0_operand")))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix>qd\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "*avx512vl_<code>v2div2si2_store"
  [(set (match_operand:V4SI 0 "memory_operand" "=m")
    (vec_concat:V4SI
      (any_truncate:V2SI
	      (match_operand:V2DI 1 "register_operand" "v"))
      (vec_select:V2SI
        (match_dup 0)
        (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix>qd\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "avx512vl_<code>v2div2si2_mask"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
    (vec_concat:V4SI
      (vec_merge:V2SI
        (any_truncate:V2SI
          (match_operand:V2DI 1 "register_operand" "v"))
        (vec_select:V2SI
          (match_operand:V4SI 2 "nonimm_or_0_operand" "0C")
          (parallel [(const_int 0) (const_int 1)]))
        (match_operand:QI 3 "register_operand" "Yk"))
      (const_vector:V2SI [(const_int 0) (const_int 0)])))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix>qd\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "*avx512vl_<code>v2div2si2_mask_1"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
    (vec_concat:V4SI
      (vec_merge:V2SI
	(any_truncate:V2SI
	  (match_operand:V2DI 1 "register_operand" "v"))
	(const_vector:V2SI [(const_int 0) (const_int 0)])
	(match_operand:QI 2 "register_operand" "Yk"))
      (const_vector:V2SI [(const_int 0) (const_int 0)])))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix>qd\t{%1, %0%{%2%}%{z%}|%0%{%2%}%{z%}, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "avx512vl_<code>v2div2si2_mask_store"
  [(set (match_operand:V4SI 0 "memory_operand" "=m")
    (vec_concat:V4SI
      (vec_merge:V2SI
        (any_truncate:V2SI
          (match_operand:V2DI 1 "register_operand" "v"))
        (vec_select:V2SI
          (match_dup 0)
          (parallel [(const_int 0) (const_int 1)]))
        (match_operand:QI 2 "register_operand" "Yk"))
      (vec_select:V2SI
        (match_dup 0)
        (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_AVX512VL"
  "vpmov<trunsuffix>qd\t{%1, %0%{%2%}|%0%{%2%}, %t1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "*avx512f_<code>v8div16qi2"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
	(vec_concat:V16QI
	  (any_truncate:V8QI
	    (match_operand:V8DI 1 "register_operand" "v"))
	  (const_vector:V8QI [(const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)
			      (const_int 0) (const_int 0)])))]
  "TARGET_AVX512F"
  "vpmov<trunsuffix>qb\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "*avx512f_<code>v8div16qi2_store"
  [(set (match_operand:V16QI 0 "memory_operand" "=m")
	(vec_concat:V16QI
	  (any_truncate:V8QI
	    (match_operand:V8DI 1 "register_operand" "v"))
	  (vec_select:V8QI
	    (match_dup 0)
	    (parallel [(const_int 8) (const_int 9)
		       (const_int 10) (const_int 11)
		       (const_int 12) (const_int 13)
		       (const_int 14) (const_int 15)]))))]
  "TARGET_AVX512F"
  "vpmov<trunsuffix>qb\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "avx512f_<code>v8div16qi2_mask"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
    (vec_concat:V16QI
      (vec_merge:V8QI
        (any_truncate:V8QI
          (match_operand:V8DI 1 "register_operand" "v"))
        (vec_select:V8QI
          (match_operand:V16QI 2 "nonimm_or_0_operand" "0C")
          (parallel [(const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)
                     (const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)]))
        (match_operand:QI 3 "register_operand" "Yk"))
      (const_vector:V8QI [(const_int 0) (const_int 0)
                          (const_int 0) (const_int 0)
                          (const_int 0) (const_int 0)
                          (const_int 0) (const_int 0)])))]
  "TARGET_AVX512F"
  "vpmov<trunsuffix>qb\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "*avx512f_<code>v8div16qi2_mask_1"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
    (vec_concat:V16QI
      (vec_merge:V8QI
	(any_truncate:V8QI
	  (match_operand:V8DI 1 "register_operand" "v"))
	(const_vector:V8QI [(const_int 0) (const_int 0)
			    (const_int 0) (const_int 0)
			    (const_int 0) (const_int 0)
			    (const_int 0) (const_int 0)])
	(match_operand:QI 2 "register_operand" "Yk"))
      (const_vector:V8QI [(const_int 0) (const_int 0)
			  (const_int 0) (const_int 0)
			  (const_int 0) (const_int 0)
			  (const_int 0) (const_int 0)])))]
  "TARGET_AVX512F"
  "vpmov<trunsuffix>qb\t{%1, %0%{%2%}%{z%}|%0%{%2%}%{z%}, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

(define_insn "avx512f_<code>v8div16qi2_mask_store"
  [(set (match_operand:V16QI 0 "memory_operand" "=m")
    (vec_concat:V16QI
      (vec_merge:V8QI
        (any_truncate:V8QI
          (match_operand:V8DI 1 "register_operand" "v"))
        (vec_select:V8QI
          (match_dup 0)
          (parallel [(const_int 0) (const_int 1)
                     (const_int 2) (const_int 3)
                     (const_int 4) (const_int 5)
                     (const_int 6) (const_int 7)]))
        (match_operand:QI 2 "register_operand" "Yk"))
      (vec_select:V8QI
        (match_dup 0)
        (parallel [(const_int 8) (const_int 9)
                   (const_int 10) (const_int 11)
                   (const_int 12) (const_int 13)
                   (const_int 14) (const_int 15)]))))]
  "TARGET_AVX512F"
  "vpmov<trunsuffix>qb\t{%1, %0%{%2%}|%q0%{%2%}, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "memory" "store")
   (set_attr "prefix" "evex")
   (set_attr "mode" "TI")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel integral arithmetic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "neg<mode>2"
  [(set (match_operand:VI_AVX2 0 "register_operand")
	(minus:VI_AVX2
	  (match_dup 2)
	  (match_operand:VI_AVX2 1 "vector_operand")))]
  "TARGET_SSE2"
  "operands[2] = force_reg (<MODE>mode, CONST0_RTX (<MODE>mode));")

(define_expand "<plusminus_insn><mode>3"
  [(set (match_operand:VI_AVX2 0 "register_operand")
	(plusminus:VI_AVX2
	  (match_operand:VI_AVX2 1 "vector_operand")
	  (match_operand:VI_AVX2 2 "vector_operand")))]
  "TARGET_SSE2"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_expand "<plusminus_insn><mode>3_mask"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand")
	(vec_merge:VI48_AVX512VL
	  (plusminus:VI48_AVX512VL
	    (match_operand:VI48_AVX512VL 1 "nonimmediate_operand")
	    (match_operand:VI48_AVX512VL 2 "nonimmediate_operand"))
	  (match_operand:VI48_AVX512VL 3 "nonimm_or_0_operand")
	  (match_operand:<avx512fmaskmode> 4 "register_operand")))]
  "TARGET_AVX512F"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_expand "<plusminus_insn><mode>3_mask"
  [(set (match_operand:VI12_AVX512VL 0 "register_operand")
	(vec_merge:VI12_AVX512VL
	  (plusminus:VI12_AVX512VL
	    (match_operand:VI12_AVX512VL 1 "nonimmediate_operand")
	    (match_operand:VI12_AVX512VL 2 "nonimmediate_operand"))
	  (match_operand:VI12_AVX512VL 3 "nonimm_or_0_operand")
	  (match_operand:<avx512fmaskmode> 4 "register_operand")))]
  "TARGET_AVX512BW"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*<plusminus_insn><mode>3"
  [(set (match_operand:VI_AVX2 0 "register_operand" "=x,v")
	(plusminus:VI_AVX2
	  (match_operand:VI_AVX2 1 "vector_operand" "<comm>0,v")
	  (match_operand:VI_AVX2 2 "vector_operand" "xBm,vm")))]
  "TARGET_SSE2 && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "@
   p<plusminus_mnemonic><ssemodesuffix>\t{%2, %0|%0, %2}
   vp<plusminus_mnemonic><ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseiadd")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "*<plusminus_insn><mode>3_mask"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI48_AVX512VL
	  (plusminus:VI48_AVX512VL
	    (match_operand:VI48_AVX512VL 1 "nonimmediate_operand" "<comm>v")
	    (match_operand:VI48_AVX512VL 2 "nonimmediate_operand" "vm"))
	  (match_operand:VI48_AVX512VL 3 "nonimm_or_0_operand" "0C")
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512F && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "vp<plusminus_mnemonic><ssemodesuffix>\t{%2, %1, %0%{%4%}%N3|%0%{%4%}%N3, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "*<plusminus_insn><mode>3_mask"
  [(set (match_operand:VI12_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI12_AVX512VL
	  (plusminus:VI12_AVX512VL
	    (match_operand:VI12_AVX512VL 1 "nonimmediate_operand" "<comm>v")
	    (match_operand:VI12_AVX512VL 2 "nonimmediate_operand" "vm"))
	  (match_operand:VI12_AVX512VL 3 "nonimm_or_0_operand" "0C")
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512BW && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "vp<plusminus_mnemonic><ssemodesuffix>\t{%2, %1, %0%{%4%}%N3|%0%{%4%}%N3, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "<sse2_avx2>_<plusminus_insn><mode>3<mask_name>"
  [(set (match_operand:VI12_AVX2 0 "register_operand")
	(sat_plusminus:VI12_AVX2
	  (match_operand:VI12_AVX2 1 "vector_operand")
	  (match_operand:VI12_AVX2 2 "vector_operand")))]
  "TARGET_SSE2 && <mask_mode512bit_condition> && <mask_avx512bw_condition>"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*<sse2_avx2>_<plusminus_insn><mode>3<mask_name>"
  [(set (match_operand:VI12_AVX2 0 "register_operand" "=x,v")
	(sat_plusminus:VI12_AVX2
	  (match_operand:VI12_AVX2 1 "vector_operand" "<comm>0,v")
	  (match_operand:VI12_AVX2 2 "vector_operand" "xBm,vm")))]
  "TARGET_SSE2 && <mask_mode512bit_condition> && <mask_avx512bw_condition>
   && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "@
   p<plusminus_mnemonic><ssemodesuffix>\t{%2, %0|%0, %2}
   vp<plusminus_mnemonic><ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseiadd")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,maybe_evex")
   (set_attr "mode" "TI")])

(define_expand "mul<mode>3<mask_name>"
  [(set (match_operand:VI1_AVX512 0 "register_operand")
	(mult:VI1_AVX512 (match_operand:VI1_AVX512 1 "register_operand")
		       (match_operand:VI1_AVX512 2 "register_operand")))]
  "TARGET_SSE2 && <mask_mode512bit_condition> && <mask_avx512bw_condition>"
{
  ix86_expand_vecop_qihi (MULT, operands[0], operands[1], operands[2]);
  DONE;
})

(define_expand "mul<mode>3<mask_name>"
  [(set (match_operand:VI2_AVX2 0 "register_operand")
	(mult:VI2_AVX2 (match_operand:VI2_AVX2 1 "vector_operand")
		       (match_operand:VI2_AVX2 2 "vector_operand")))]
  "TARGET_SSE2 && <mask_mode512bit_condition> && <mask_avx512bw_condition>"
  "ix86_fixup_binary_operands_no_copy (MULT, <MODE>mode, operands);")

(define_insn "*mul<mode>3<mask_name>"
  [(set (match_operand:VI2_AVX2 0 "register_operand" "=x,v")
	(mult:VI2_AVX2 (match_operand:VI2_AVX2 1 "vector_operand" "%0,v")
		       (match_operand:VI2_AVX2 2 "vector_operand" "xBm,vm")))]
  "TARGET_SSE2 && !(MEM_P (operands[1]) && MEM_P (operands[2]))
   && <mask_mode512bit_condition> && <mask_avx512bw_condition>"
  "@
   pmullw\t{%2, %0|%0, %2}
   vpmullw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseimul")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "<s>mul<mode>3_highpart<mask_name>"
  [(set (match_operand:VI2_AVX2 0 "register_operand")
	(truncate:VI2_AVX2
	  (lshiftrt:<ssedoublemode>
	    (mult:<ssedoublemode>
	      (any_extend:<ssedoublemode>
		(match_operand:VI2_AVX2 1 "vector_operand"))
	      (any_extend:<ssedoublemode>
		(match_operand:VI2_AVX2 2 "vector_operand")))
	    (const_int 16))))]
  "TARGET_SSE2
   && <mask_mode512bit_condition> && <mask_avx512bw_condition>"
  "ix86_fixup_binary_operands_no_copy (MULT, <MODE>mode, operands);")

(define_insn "*<s>mul<mode>3_highpart<mask_name>"
  [(set (match_operand:VI2_AVX2 0 "register_operand" "=x,v")
	(truncate:VI2_AVX2
	  (lshiftrt:<ssedoublemode>
	    (mult:<ssedoublemode>
	      (any_extend:<ssedoublemode>
		(match_operand:VI2_AVX2 1 "vector_operand" "%0,v"))
	      (any_extend:<ssedoublemode>
		(match_operand:VI2_AVX2 2 "vector_operand" "xBm,vm")))
	    (const_int 16))))]
  "TARGET_SSE2 && !(MEM_P (operands[1]) && MEM_P (operands[2]))
   && <mask_mode512bit_condition> && <mask_avx512bw_condition>"
  "@
   pmulh<u>w\t{%2, %0|%0, %2}
   vpmulh<u>w\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseimul")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "vec_widen_umult_even_v16si<mask_name>"
  [(set (match_operand:V8DI 0 "register_operand")
        (mult:V8DI
          (zero_extend:V8DI
            (vec_select:V8SI
              (match_operand:V16SI 1 "nonimmediate_operand")
              (parallel [(const_int 0) (const_int 2)
                         (const_int 4) (const_int 6)
                         (const_int 8) (const_int 10)
                         (const_int 12) (const_int 14)])))
          (zero_extend:V8DI
            (vec_select:V8SI
              (match_operand:V16SI 2 "nonimmediate_operand")
              (parallel [(const_int 0) (const_int 2)
                         (const_int 4) (const_int 6)
                         (const_int 8) (const_int 10)
                         (const_int 12) (const_int 14)])))))]
  "TARGET_AVX512F"
  "ix86_fixup_binary_operands_no_copy (MULT, V16SImode, operands);")

(define_insn "*vec_widen_umult_even_v16si<mask_name>"
  [(set (match_operand:V8DI 0 "register_operand" "=v")
        (mult:V8DI
          (zero_extend:V8DI
            (vec_select:V8SI
              (match_operand:V16SI 1 "nonimmediate_operand" "%v")
              (parallel [(const_int 0) (const_int 2)
                         (const_int 4) (const_int 6)
                         (const_int 8) (const_int 10)
                         (const_int 12) (const_int 14)])))
          (zero_extend:V8DI
            (vec_select:V8SI
              (match_operand:V16SI 2 "nonimmediate_operand" "vm")
              (parallel [(const_int 0) (const_int 2)
                         (const_int 4) (const_int 6)
                         (const_int 8) (const_int 10)
                         (const_int 12) (const_int 14)])))))]
  "TARGET_AVX512F && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "vpmuludq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_expand "vec_widen_umult_even_v8si<mask_name>"
  [(set (match_operand:V4DI 0 "register_operand")
	(mult:V4DI
	  (zero_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 1 "nonimmediate_operand")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))
	  (zero_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 2 "nonimmediate_operand")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))))]
  "TARGET_AVX2 && <mask_avx512vl_condition>"
  "ix86_fixup_binary_operands_no_copy (MULT, V8SImode, operands);")

(define_insn "*vec_widen_umult_even_v8si<mask_name>"
  [(set (match_operand:V4DI 0 "register_operand" "=v")
	(mult:V4DI
	  (zero_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 1 "nonimmediate_operand" "%v")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))
	  (zero_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 2 "nonimmediate_operand" "vm")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))))]
  "TARGET_AVX2 && <mask_avx512vl_condition>
   && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "vpmuludq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "OI")])

(define_expand "vec_widen_umult_even_v4si<mask_name>"
  [(set (match_operand:V2DI 0 "register_operand")
	(mult:V2DI
	  (zero_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "vector_operand")
	      (parallel [(const_int 0) (const_int 2)])))
	  (zero_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "vector_operand")
	      (parallel [(const_int 0) (const_int 2)])))))]
  "TARGET_SSE2 && <mask_avx512vl_condition>"
  "ix86_fixup_binary_operands_no_copy (MULT, V4SImode, operands);")

(define_insn "*vec_widen_umult_even_v4si<mask_name>"
  [(set (match_operand:V2DI 0 "register_operand" "=x,v")
	(mult:V2DI
	  (zero_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "vector_operand" "%0,v")
	      (parallel [(const_int 0) (const_int 2)])))
	  (zero_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "vector_operand" "xBm,vm")
	      (parallel [(const_int 0) (const_int 2)])))))]
  "TARGET_SSE2 && <mask_avx512vl_condition>
   && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   pmuludq\t{%2, %0|%0, %2}
   vpmuludq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseimul")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,maybe_evex")
   (set_attr "mode" "TI")])

(define_expand "vec_widen_smult_even_v16si<mask_name>"
  [(set (match_operand:V8DI 0 "register_operand")
        (mult:V8DI
          (sign_extend:V8DI
            (vec_select:V8SI
              (match_operand:V16SI 1 "nonimmediate_operand")
              (parallel [(const_int 0) (const_int 2)
                         (const_int 4) (const_int 6)
                         (const_int 8) (const_int 10)
                         (const_int 12) (const_int 14)])))
          (sign_extend:V8DI
            (vec_select:V8SI
              (match_operand:V16SI 2 "nonimmediate_operand")
              (parallel [(const_int 0) (const_int 2)
                         (const_int 4) (const_int 6)
                         (const_int 8) (const_int 10)
                         (const_int 12) (const_int 14)])))))]
  "TARGET_AVX512F"
  "ix86_fixup_binary_operands_no_copy (MULT, V16SImode, operands);")

(define_insn "*vec_widen_smult_even_v16si<mask_name>"
  [(set (match_operand:V8DI 0 "register_operand" "=v")
        (mult:V8DI
          (sign_extend:V8DI
            (vec_select:V8SI
              (match_operand:V16SI 1 "nonimmediate_operand" "%v")
              (parallel [(const_int 0) (const_int 2)
                         (const_int 4) (const_int 6)
                         (const_int 8) (const_int 10)
                         (const_int 12) (const_int 14)])))
          (sign_extend:V8DI
            (vec_select:V8SI
              (match_operand:V16SI 2 "nonimmediate_operand" "vm")
              (parallel [(const_int 0) (const_int 2)
                         (const_int 4) (const_int 6)
                         (const_int 8) (const_int 10)
                         (const_int 12) (const_int 14)])))))]
  "TARGET_AVX512F && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "vpmuldq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_expand "vec_widen_smult_even_v8si<mask_name>"
  [(set (match_operand:V4DI 0 "register_operand")
	(mult:V4DI
	  (sign_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 1 "nonimmediate_operand")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))
	  (sign_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 2 "nonimmediate_operand")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))))]
  "TARGET_AVX2 && <mask_avx512vl_condition>"
  "ix86_fixup_binary_operands_no_copy (MULT, V8SImode, operands);")

(define_insn "*vec_widen_smult_even_v8si<mask_name>"
  [(set (match_operand:V4DI 0 "register_operand" "=v")
	(mult:V4DI
	  (sign_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 1 "nonimmediate_operand" "%v")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))
	  (sign_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 2 "nonimmediate_operand" "vm")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))))]
  "TARGET_AVX2 && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "vpmuldq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "OI")])

(define_expand "sse4_1_mulv2siv2di3<mask_name>"
  [(set (match_operand:V2DI 0 "register_operand")
	(mult:V2DI
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "vector_operand")
	      (parallel [(const_int 0) (const_int 2)])))
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "vector_operand")
	      (parallel [(const_int 0) (const_int 2)])))))]
  "TARGET_SSE4_1 && <mask_avx512vl_condition>"
  "ix86_fixup_binary_operands_no_copy (MULT, V4SImode, operands);")

(define_insn "*sse4_1_mulv2siv2di3<mask_name>"
  [(set (match_operand:V2DI 0 "register_operand" "=Yr,*x,v")
	(mult:V2DI
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "vector_operand" "%0,0,v")
	      (parallel [(const_int 0) (const_int 2)])))
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "vector_operand" "YrBm,*xBm,vm")
	      (parallel [(const_int 0) (const_int 2)])))))]
  "TARGET_SSE4_1 && <mask_avx512vl_condition>
   && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   pmuldq\t{%2, %0|%0, %2}
   pmuldq\t{%2, %0|%0, %2}
   vpmuldq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "sseimul")
   (set_attr "prefix_data16" "1,1,*")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "mode" "TI")])

(define_insn "avx512bw_pmaddwd512<mode><mask_name>"
  [(set (match_operand:<sseunpackmode> 0 "register_operand" "=v")
          (unspec:<sseunpackmode>
            [(match_operand:VI2_AVX2 1 "register_operand" "v")
             (match_operand:VI2_AVX2 2 "nonimmediate_operand" "vm")]
             UNSPEC_PMADDWD512))]
   "TARGET_AVX512BW && <mask_mode512bit_condition>"
   "vpmaddwd\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}";
  [(set_attr "type" "sseiadd")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_expand "avx2_pmaddwd"
  [(set (match_operand:V8SI 0 "register_operand")
	(plus:V8SI
	  (mult:V8SI
	    (sign_extend:V8SI
	      (vec_select:V8HI
		(match_operand:V16HI 1 "nonimmediate_operand")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)])))
	    (sign_extend:V8SI
	      (vec_select:V8HI
		(match_operand:V16HI 2 "nonimmediate_operand")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)]))))
	  (mult:V8SI
	    (sign_extend:V8SI
	      (vec_select:V8HI (match_dup 1)
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)])))
	    (sign_extend:V8SI
	      (vec_select:V8HI (match_dup 2)
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)]))))))]
  "TARGET_AVX2"
  "ix86_fixup_binary_operands_no_copy (MULT, V16HImode, operands);")

(define_insn "*avx2_pmaddwd"
  [(set (match_operand:V8SI 0 "register_operand" "=x,v")
	(plus:V8SI
	  (mult:V8SI
	    (sign_extend:V8SI
	      (vec_select:V8HI
		(match_operand:V16HI 1 "nonimmediate_operand" "%x,v")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)])))
	    (sign_extend:V8SI
	      (vec_select:V8HI
		(match_operand:V16HI 2 "nonimmediate_operand" "xm,vm")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)]))))
	  (mult:V8SI
	    (sign_extend:V8SI
	      (vec_select:V8HI (match_dup 1)
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)])))
	    (sign_extend:V8SI
	      (vec_select:V8HI (match_dup 2)
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)]))))))]
  "TARGET_AVX2 && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "vpmaddwd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "isa" "*,avx512bw")
   (set_attr "prefix" "vex,evex")
   (set_attr "mode" "OI")])

(define_expand "sse2_pmaddwd"
  [(set (match_operand:V4SI 0 "register_operand")
	(plus:V4SI
	  (mult:V4SI
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 1 "vector_operand")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 2 "vector_operand")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)]))))
	  (mult:V4SI
	    (sign_extend:V4SI
	      (vec_select:V4HI (match_dup 1)
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI (match_dup 2)
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)]))))))]
  "TARGET_SSE2"
  "ix86_fixup_binary_operands_no_copy (MULT, V8HImode, operands);")

(define_insn "*sse2_pmaddwd"
  [(set (match_operand:V4SI 0 "register_operand" "=x,x,v")
	(plus:V4SI
	  (mult:V4SI
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 1 "vector_operand" "%0,x,v")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 2 "vector_operand" "xBm,xm,vm")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)]))))
	  (mult:V4SI
	    (sign_extend:V4SI
	      (vec_select:V4HI (match_dup 1)
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI (match_dup 2)
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)]))))))]
  "TARGET_SSE2 && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   pmaddwd\t{%2, %0|%0, %2}
   vpmaddwd\t{%2, %1, %0|%0, %1, %2}
   vpmaddwd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx,avx512bw")
   (set_attr "type" "sseiadd")
   (set_attr "atom_unit" "simul")
   (set_attr "prefix_data16" "1,*,*")
   (set_attr "prefix" "orig,vex,evex")
   (set_attr "mode" "TI")])

(define_insn "avx512dq_mul<mode>3<mask_name>"
  [(set (match_operand:VI8 0 "register_operand" "=v")
	(mult:VI8
	  (match_operand:VI8 1 "register_operand" "v")
	  (match_operand:VI8 2 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512DQ && <mask_mode512bit_condition>"
  "vpmullq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "mul<mode>3<mask_name>"
  [(set (match_operand:VI4_AVX512F 0 "register_operand")
	(mult:VI4_AVX512F
	  (match_operand:VI4_AVX512F 1 "general_vector_operand")
	  (match_operand:VI4_AVX512F 2 "general_vector_operand")))]
  "TARGET_SSE2 && <mask_mode512bit_condition>"
{
  if (TARGET_SSE4_1)
    {
      if (!vector_operand (operands[1], <MODE>mode))
	operands[1] = force_reg (<MODE>mode, operands[1]);
      if (!vector_operand (operands[2], <MODE>mode))
	operands[2] = force_reg (<MODE>mode, operands[2]);
      ix86_fixup_binary_operands_no_copy (MULT, <MODE>mode, operands);
    }
  else
    {
      ix86_expand_sse2_mulv4si3 (operands[0], operands[1], operands[2]);
      DONE;
    }
})

(define_insn "*<sse4_1_avx2>_mul<mode>3<mask_name>"
  [(set (match_operand:VI4_AVX512F 0 "register_operand" "=Yr,*x,v")
	(mult:VI4_AVX512F
	  (match_operand:VI4_AVX512F 1 "vector_operand" "%0,0,v")
	  (match_operand:VI4_AVX512F 2 "vector_operand" "YrBm,*xBm,vm")))]
  "TARGET_SSE4_1 && !(MEM_P (operands[1]) && MEM_P (operands[2]))
   && <mask_mode512bit_condition>"
  "@
   pmulld\t{%2, %0|%0, %2}
   pmulld\t{%2, %0|%0, %2}
   vpmulld\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "sseimul")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "<mask_prefix4>")
   (set_attr "btver2_decode" "vector,vector,vector")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "mul<mode>3"
  [(set (match_operand:VI8_AVX2_AVX512F 0 "register_operand")
	(mult:VI8_AVX2_AVX512F
	  (match_operand:VI8_AVX2_AVX512F 1 "register_operand")
	  (match_operand:VI8_AVX2_AVX512F 2 "register_operand")))]
  "TARGET_SSE2"
{
  ix86_expand_sse2_mulvxdi3 (operands[0], operands[1], operands[2]);
  DONE;
})

(define_expand "vec_widen_<s>mult_hi_<mode>"
  [(match_operand:<sseunpackmode> 0 "register_operand")
   (any_extend:<sseunpackmode>
     (match_operand:VI124_AVX2 1 "register_operand"))
   (match_operand:VI124_AVX2 2 "register_operand")]
  "TARGET_SSE2"
{
  ix86_expand_mul_widen_hilo (operands[0], operands[1], operands[2],
			      <u_bool>, true);
  DONE;
})

(define_expand "vec_widen_<s>mult_lo_<mode>"
  [(match_operand:<sseunpackmode> 0 "register_operand")
   (any_extend:<sseunpackmode>
     (match_operand:VI124_AVX2 1 "register_operand"))
   (match_operand:VI124_AVX2 2 "register_operand")]
  "TARGET_SSE2"
{
  ix86_expand_mul_widen_hilo (operands[0], operands[1], operands[2],
			      <u_bool>, false);
  DONE;
})

;; Most widen_<s>mult_even_<mode> can be handled directly from other
;; named patterns, but signed V4SI needs special help for plain SSE2.
(define_expand "vec_widen_smult_even_v4si"
  [(match_operand:V2DI 0 "register_operand")
   (match_operand:V4SI 1 "vector_operand")
   (match_operand:V4SI 2 "vector_operand")]
  "TARGET_SSE2"
{
  ix86_expand_mul_widen_evenodd (operands[0], operands[1], operands[2],
				 false, false);
  DONE;
})

(define_expand "vec_widen_<s>mult_odd_<mode>"
  [(match_operand:<sseunpackmode> 0 "register_operand")
   (any_extend:<sseunpackmode>
     (match_operand:VI4_AVX512F 1 "general_vector_operand"))
   (match_operand:VI4_AVX512F 2 "general_vector_operand")]
  "TARGET_SSE2"
{
  ix86_expand_mul_widen_evenodd (operands[0], operands[1], operands[2],
				 <u_bool>, true);
  DONE;
})

(define_mode_attr SDOT_PMADD_SUF
  [(V32HI "512v32hi") (V16HI "") (V8HI "")])

(define_expand "sdot_prod<mode>"
  [(match_operand:<sseunpackmode> 0 "register_operand")
   (match_operand:VI2_AVX2 1 "register_operand")
   (match_operand:VI2_AVX2 2 "register_operand")
   (match_operand:<sseunpackmode> 3 "register_operand")]
  "TARGET_SSE2"
{
  rtx t = gen_reg_rtx (<sseunpackmode>mode);
  emit_insn (gen_<sse2_avx2>_pmaddwd<SDOT_PMADD_SUF> (t, operands[1], operands[2]));
  emit_insn (gen_rtx_SET (operands[0],
			  gen_rtx_PLUS (<sseunpackmode>mode,
					operands[3], t)));
  DONE;
})

;; Normally we use widen_mul_even/odd, but combine can't quite get it all
;; back together when madd is available.
(define_expand "sdot_prodv4si"
  [(match_operand:V2DI 0 "register_operand")
   (match_operand:V4SI 1 "register_operand")
   (match_operand:V4SI 2 "register_operand")
   (match_operand:V2DI 3 "register_operand")]
  "TARGET_XOP"
{
  rtx t = gen_reg_rtx (V2DImode);
  emit_insn (gen_xop_pmacsdqh (t, operands[1], operands[2], operands[3]));
  emit_insn (gen_xop_pmacsdql (operands[0], operands[1], operands[2], t));
  DONE;
})

(define_expand "uavg<mode>3_ceil"
  [(set (match_operand:VI12_AVX2 0 "register_operand")
	(truncate:VI12_AVX2
	  (lshiftrt:<ssedoublemode>
	    (plus:<ssedoublemode>
	      (plus:<ssedoublemode>
		(zero_extend:<ssedoublemode>
		  (match_operand:VI12_AVX2 1 "vector_operand"))
		(zero_extend:<ssedoublemode>
		  (match_operand:VI12_AVX2 2 "vector_operand")))
	      (match_dup 3))
	    (const_int 1))))]
  "TARGET_SSE2"
{
  operands[3] = CONST1_RTX(<MODE>mode);
  ix86_fixup_binary_operands_no_copy (PLUS, <MODE>mode, operands);
})

(define_expand "usadv16qi"
  [(match_operand:V4SI 0 "register_operand")
   (match_operand:V16QI 1 "register_operand")
   (match_operand:V16QI 2 "vector_operand")
   (match_operand:V4SI 3 "vector_operand")]
  "TARGET_SSE2"
{
  rtx t1 = gen_reg_rtx (V2DImode);
  rtx t2 = gen_reg_rtx (V4SImode);
  emit_insn (gen_sse2_psadbw (t1, operands[1], operands[2]));
  convert_move (t2, t1, 0);
  emit_insn (gen_addv4si3 (operands[0], t2, operands[3]));
  DONE;
})

(define_expand "usadv32qi"
  [(match_operand:V8SI 0 "register_operand")
   (match_operand:V32QI 1 "register_operand")
   (match_operand:V32QI 2 "nonimmediate_operand")
   (match_operand:V8SI 3 "nonimmediate_operand")]
  "TARGET_AVX2"
{
  rtx t1 = gen_reg_rtx (V4DImode);
  rtx t2 = gen_reg_rtx (V8SImode);
  emit_insn (gen_avx2_psadbw (t1, operands[1], operands[2]));
  convert_move (t2, t1, 0);
  emit_insn (gen_addv8si3 (operands[0], t2, operands[3]));
  DONE;
})

(define_expand "usadv64qi"
  [(match_operand:V16SI 0 "register_operand")
   (match_operand:V64QI 1 "register_operand")
   (match_operand:V64QI 2 "nonimmediate_operand")
   (match_operand:V16SI 3 "nonimmediate_operand")]
  "TARGET_AVX512BW"
{
  rtx t1 = gen_reg_rtx (V8DImode);
  rtx t2 = gen_reg_rtx (V16SImode);
  emit_insn (gen_avx512f_psadbw (t1, operands[1], operands[2]));
  convert_move (t2, t1, 0);
  emit_insn (gen_addv16si3 (operands[0], t2, operands[3]));
  DONE;
})

(define_insn "<mask_codefor>ashr<mode>3<mask_name>"
  [(set (match_operand:VI248_AVX512BW_1 0 "register_operand" "=v,v")
	(ashiftrt:VI248_AVX512BW_1
	  (match_operand:VI248_AVX512BW_1 1 "nonimmediate_operand" "v,vm")
	  (match_operand:DI 2 "nonmemory_operand" "v,N")))]
  "TARGET_AVX512VL"
  "vpsra<ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseishft")
   (set (attr "length_immediate")
     (if_then_else (match_operand 2 "const_int_operand")
       (const_string "1")
       (const_string "0")))
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "ashr<mode>3"
  [(set (match_operand:VI24_AVX2 0 "register_operand" "=x,x")
	(ashiftrt:VI24_AVX2
	  (match_operand:VI24_AVX2 1 "register_operand" "0,x")
	  (match_operand:DI 2 "nonmemory_operand" "xN,xN")))]
  "TARGET_SSE2"
  "@
   psra<ssemodesuffix>\t{%2, %0|%0, %2}
   vpsra<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseishft")
   (set (attr "length_immediate")
     (if_then_else (match_operand 2 "const_int_operand")
       (const_string "1")
       (const_string "0")))
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "ashr<mode>3<mask_name>"
  [(set (match_operand:VI248_AVX512BW_AVX512VL 0 "register_operand" "=v,v")
	(ashiftrt:VI248_AVX512BW_AVX512VL
	  (match_operand:VI248_AVX512BW_AVX512VL 1 "nonimmediate_operand" "v,vm")
	  (match_operand:DI 2 "nonmemory_operand" "v,N")))]
  "TARGET_AVX512F"
  "vpsra<ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseishft")
   (set (attr "length_immediate")
     (if_then_else (match_operand 2 "const_int_operand")
       (const_string "1")
       (const_string "0")))
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<mask_codefor><shift_insn><mode>3<mask_name>"
  [(set (match_operand:VI248_AVX512BW_2 0 "register_operand" "=v,v")
	(any_lshift:VI248_AVX512BW_2
	  (match_operand:VI248_AVX512BW_2 1 "nonimmediate_operand" "v,vm")
	  (match_operand:DI 2 "nonmemory_operand" "v,N")))]
  "TARGET_AVX512VL"
  "vp<vshift><ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseishft")
   (set (attr "length_immediate")
     (if_then_else (match_operand 2 "const_int_operand")
       (const_string "1")
       (const_string "0")))
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<shift_insn><mode>3"
  [(set (match_operand:VI248_AVX2 0 "register_operand" "=x,x")
	(any_lshift:VI248_AVX2
	  (match_operand:VI248_AVX2 1 "register_operand" "0,x")
	  (match_operand:DI 2 "nonmemory_operand" "xN,xN")))]
  "TARGET_SSE2"
  "@
   p<vshift><ssemodesuffix>\t{%2, %0|%0, %2}
   vp<vshift><ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseishft")
   (set (attr "length_immediate")
     (if_then_else (match_operand 2 "const_int_operand")
       (const_string "1")
       (const_string "0")))
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<shift_insn><mode>3<mask_name>"
  [(set (match_operand:VI248_AVX512BW 0 "register_operand" "=v,v")
	(any_lshift:VI248_AVX512BW
	  (match_operand:VI248_AVX512BW 1 "nonimmediate_operand" "v,m")
	  (match_operand:DI 2 "nonmemory_operand" "vN,N")))]
  "TARGET_AVX512F"
  "vp<vshift><ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseishft")
   (set (attr "length_immediate")
     (if_then_else (match_operand 2 "const_int_operand")
       (const_string "1")
       (const_string "0")))
   (set_attr "mode" "<sseinsnmode>")])


(define_expand "vec_shr_<mode>"
  [(set (match_dup 3)
	(lshiftrt:V1TI
	 (match_operand:VI_128 1 "register_operand")
	 (match_operand:SI 2 "const_0_to_255_mul_8_operand")))
   (set (match_operand:VI_128 0 "register_operand") (match_dup 4))]
  "TARGET_SSE2"
{
  operands[1] = gen_lowpart (V1TImode, operands[1]);
  operands[3] = gen_reg_rtx (V1TImode);
  operands[4] = gen_lowpart (<MODE>mode, operands[3]);
})

(define_insn "avx512bw_<shift_insn><mode>3"
  [(set (match_operand:VIMAX_AVX512VL 0 "register_operand" "=v")
	(any_lshift:VIMAX_AVX512VL
	 (match_operand:VIMAX_AVX512VL 1 "nonimmediate_operand" "vm")
	 (match_operand:SI 2 "const_0_to_255_mul_8_operand" "n")))]
  "TARGET_AVX512BW"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) / 8);
  return "vp<vshift>dq\t{%2, %1, %0|%0, %1, %2}";
}
  [(set_attr "type" "sseishft")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<sse2_avx2>_<shift_insn><mode>3"
  [(set (match_operand:VIMAX_AVX2 0 "register_operand" "=x,v")
	(any_lshift:VIMAX_AVX2
	 (match_operand:VIMAX_AVX2 1 "register_operand" "0,v")
	 (match_operand:SI 2 "const_0_to_255_mul_8_operand" "n,n")))]
  "TARGET_SSE2"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) / 8);

  switch (which_alternative)
    {
    case 0:
      return "p<vshift>dq\t{%2, %0|%0, %2}";
    case 1:
      return "vp<vshift>dq\t{%2, %1, %0|%0, %1, %2}";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseishft")
   (set_attr "length_immediate" "1")
   (set_attr "atom_unit" "sishuf")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_<rotate>v<mode><mask_name>"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand" "=v")
	(any_rotate:VI48_AVX512VL
	  (match_operand:VI48_AVX512VL 1 "register_operand" "v")
	  (match_operand:VI48_AVX512VL 2 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512F"
  "vp<rotate>v<ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_<rotate><mode><mask_name>"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand" "=v")
	(any_rotate:VI48_AVX512VL
	  (match_operand:VI48_AVX512VL 1 "nonimmediate_operand" "vm")
	  (match_operand:SI 2 "const_0_to_255_operand")))]
  "TARGET_AVX512F"
  "vp<rotate><ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "<code><mode>3"
  [(set (match_operand:VI124_256_AVX512F_AVX512BW 0 "register_operand")
	(maxmin:VI124_256_AVX512F_AVX512BW
	  (match_operand:VI124_256_AVX512F_AVX512BW 1 "nonimmediate_operand")
	  (match_operand:VI124_256_AVX512F_AVX512BW 2 "nonimmediate_operand")))]
  "TARGET_AVX2"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*avx2_<code><mode>3"
  [(set (match_operand:VI124_256 0 "register_operand" "=v")
	(maxmin:VI124_256
	  (match_operand:VI124_256 1 "nonimmediate_operand" "%v")
	  (match_operand:VI124_256 2 "nonimmediate_operand" "vm")))]
  "TARGET_AVX2 && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "vp<maxmin_int><ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "OI")])

(define_expand "<code><mode>3_mask"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand")
	(vec_merge:VI48_AVX512VL
	  (maxmin:VI48_AVX512VL
	    (match_operand:VI48_AVX512VL 1 "nonimmediate_operand")
	    (match_operand:VI48_AVX512VL 2 "nonimmediate_operand"))
	  (match_operand:VI48_AVX512VL 3 "nonimm_or_0_operand")
	  (match_operand:<avx512fmaskmode> 4 "register_operand")))]
  "TARGET_AVX512F"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*avx512f_<code><mode>3<mask_name>"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand" "=v")
	(maxmin:VI48_AVX512VL
	  (match_operand:VI48_AVX512VL 1 "nonimmediate_operand" "%v")
	  (match_operand:VI48_AVX512VL 2 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512F && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "vp<maxmin_int><ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<mask_codefor><code><mode>3<mask_name>"
  [(set (match_operand:VI12_AVX512VL 0 "register_operand" "=v")
        (maxmin:VI12_AVX512VL
          (match_operand:VI12_AVX512VL 1 "register_operand" "v")
          (match_operand:VI12_AVX512VL 2 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512BW"
  "vp<maxmin_int><ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "<code><mode>3"
  [(set (match_operand:VI8_AVX2_AVX512F 0 "register_operand")
	(maxmin:VI8_AVX2_AVX512F
	  (match_operand:VI8_AVX2_AVX512F 1 "register_operand")
	  (match_operand:VI8_AVX2_AVX512F 2 "register_operand")))]
  "TARGET_SSE4_2"
{
  if (TARGET_AVX512F
      && (<MODE>mode == V8DImode || TARGET_AVX512VL))
    ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);
  else 
    {
      enum rtx_code code;
      rtx xops[6];
      bool ok;


      xops[0] = operands[0];

      if (<CODE> == SMAX || <CODE> == UMAX)
	{
	  xops[1] = operands[1];
	  xops[2] = operands[2];
	}
      else
	{
	  xops[1] = operands[2];
	  xops[2] = operands[1];
	}

      code = (<CODE> == UMAX || <CODE> == UMIN) ? GTU : GT;

      xops[3] = gen_rtx_fmt_ee (code, VOIDmode, operands[1], operands[2]);
      xops[4] = operands[1];
      xops[5] = operands[2];

      ok = ix86_expand_int_vcond (xops);
      gcc_assert (ok);
      DONE;
    }
})

(define_expand "<code><mode>3"
  [(set (match_operand:VI124_128 0 "register_operand")
	(smaxmin:VI124_128
	  (match_operand:VI124_128 1 "vector_operand")
	  (match_operand:VI124_128 2 "vector_operand")))]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1 || <MODE>mode == V8HImode)
    ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);
  else
    {
      rtx xops[6];
      bool ok;

      xops[0] = operands[0];
      operands[1] = force_reg (<MODE>mode, operands[1]);
      operands[2] = force_reg (<MODE>mode, operands[2]);

      if (<CODE> == SMAX)
	{
	  xops[1] = operands[1];
	  xops[2] = operands[2];
	}
      else
	{
	  xops[1] = operands[2];
	  xops[2] = operands[1];
	}

      xops[3] = gen_rtx_GT (VOIDmode, operands[1], operands[2]);
      xops[4] = operands[1];
      xops[5] = operands[2];

      ok = ix86_expand_int_vcond (xops);
      gcc_assert (ok);
      DONE;
    }
})

(define_insn "*sse4_1_<code><mode>3<mask_name>"
  [(set (match_operand:VI14_128 0 "register_operand" "=Yr,*x,v")
	(smaxmin:VI14_128
	  (match_operand:VI14_128 1 "vector_operand" "%0,0,v")
	  (match_operand:VI14_128 2 "vector_operand" "YrBm,*xBm,vm")))]
  "TARGET_SSE4_1
   && <mask_mode512bit_condition>
   && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   p<maxmin_int><ssemodesuffix>\t{%2, %0|%0, %2}
   p<maxmin_int><ssemodesuffix>\t{%2, %0|%0, %2}
   vp<maxmin_int><ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1,1,*")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "mode" "TI")])

(define_insn "*<code>v8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x,x,v")
	(smaxmin:V8HI
	  (match_operand:V8HI 1 "vector_operand" "%0,x,v")
	  (match_operand:V8HI 2 "vector_operand" "xBm,xm,vm")))]
  "TARGET_SSE2 && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   p<maxmin_int>w\t{%2, %0|%0, %2}
   vp<maxmin_int>w\t{%2, %1, %0|%0, %1, %2}
   vp<maxmin_int>w\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx,avx512bw")
   (set_attr "type" "sseiadd")
   (set_attr "prefix_data16" "1,*,*")
   (set_attr "prefix_extra" "*,1,1")
   (set_attr "prefix" "orig,vex,evex")
   (set_attr "mode" "TI")])

(define_expand "<code><mode>3"
  [(set (match_operand:VI124_128 0 "register_operand")
	(umaxmin:VI124_128
	  (match_operand:VI124_128 1 "vector_operand")
	  (match_operand:VI124_128 2 "vector_operand")))]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1 || <MODE>mode == V16QImode)
    ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);
  else if (<CODE> == UMAX && <MODE>mode == V8HImode)
    {
      rtx op0 = operands[0], op2 = operands[2], op3 = op0;
      operands[1] = force_reg (<MODE>mode, operands[1]);
      if (rtx_equal_p (op3, op2))
	op3 = gen_reg_rtx (V8HImode);
      emit_insn (gen_sse2_ussubv8hi3 (op3, operands[1], op2));
      emit_insn (gen_addv8hi3 (op0, op3, op2));
      DONE;
    }
  else
    {
      rtx xops[6];
      bool ok;

      operands[1] = force_reg (<MODE>mode, operands[1]);
      operands[2] = force_reg (<MODE>mode, operands[2]);

      xops[0] = operands[0];

      if (<CODE> == UMAX)
	{
	  xops[1] = operands[1];
	  xops[2] = operands[2];
	}
      else
	{
	  xops[1] = operands[2];
	  xops[2] = operands[1];
	}

      xops[3] = gen_rtx_GTU (VOIDmode, operands[1], operands[2]);
      xops[4] = operands[1];
      xops[5] = operands[2];

      ok = ix86_expand_int_vcond (xops);
      gcc_assert (ok);
      DONE;
    }
})

(define_insn "*sse4_1_<code><mode>3<mask_name>"
  [(set (match_operand:VI24_128 0 "register_operand" "=Yr,*x,v")
	(umaxmin:VI24_128
	  (match_operand:VI24_128 1 "vector_operand" "%0,0,v")
	  (match_operand:VI24_128 2 "vector_operand" "YrBm,*xBm,vm")))]
  "TARGET_SSE4_1
   && <mask_mode512bit_condition>
   && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   p<maxmin_int><ssemodesuffix>\t{%2, %0|%0, %2}
   p<maxmin_int><ssemodesuffix>\t{%2, %0|%0, %2}
   vp<maxmin_int><ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1,1,*")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "mode" "TI")])

(define_insn "*<code>v16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "=x,x,v")
	(umaxmin:V16QI
	  (match_operand:V16QI 1 "vector_operand" "%0,x,v")
	  (match_operand:V16QI 2 "vector_operand" "xBm,xm,vm")))]
  "TARGET_SSE2 && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   p<maxmin_int>b\t{%2, %0|%0, %2}
   vp<maxmin_int>b\t{%2, %1, %0|%0, %1, %2}
   vp<maxmin_int>b\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx,avx512bw")
   (set_attr "type" "sseiadd")
   (set_attr "prefix_data16" "1,*,*")
   (set_attr "prefix_extra" "*,1,1")
   (set_attr "prefix" "orig,vex,evex")
   (set_attr "mode" "TI")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel integral comparisons
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "avx2_eq<mode>3"
  [(set (match_operand:VI_256 0 "register_operand")
	(eq:VI_256
	  (match_operand:VI_256 1 "nonimmediate_operand")
	  (match_operand:VI_256 2 "nonimmediate_operand")))]
  "TARGET_AVX2"
  "ix86_fixup_binary_operands_no_copy (EQ, <MODE>mode, operands);")

(define_insn "*avx2_eq<mode>3"
  [(set (match_operand:VI_256 0 "register_operand" "=x")
	(eq:VI_256
	  (match_operand:VI_256 1 "nonimmediate_operand" "%x")
	  (match_operand:VI_256 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX2 && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "vpcmpeq<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "OI")])

(define_expand "<avx512>_eq<mode>3<mask_scalar_merge_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand")
	(unspec:<avx512fmaskmode>
	  [(match_operand:VI12_AVX512VL 1 "nonimmediate_operand")
	   (match_operand:VI12_AVX512VL 2 "nonimmediate_operand")]
	  UNSPEC_MASKED_EQ))]
  "TARGET_AVX512BW"
  "ix86_fixup_binary_operands_no_copy (EQ, <MODE>mode, operands);")

(define_expand "<avx512>_eq<mode>3<mask_scalar_merge_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand")
	(unspec:<avx512fmaskmode>
	  [(match_operand:VI48_AVX512VL 1 "nonimmediate_operand")
	   (match_operand:VI48_AVX512VL 2 "nonimmediate_operand")]
	  UNSPEC_MASKED_EQ))]
  "TARGET_AVX512F"
  "ix86_fixup_binary_operands_no_copy (EQ, <MODE>mode, operands);")

(define_insn "<avx512>_eq<mode>3<mask_scalar_merge_name>_1"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk,Yk")
	(unspec:<avx512fmaskmode>
	  [(match_operand:VI12_AVX512VL 1 "nonimm_or_0_operand" "%v,v")
	   (match_operand:VI12_AVX512VL 2 "nonimm_or_0_operand" "vm,C")]
	  UNSPEC_MASKED_EQ))]
  "TARGET_AVX512BW && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   vpcmpeq<ssemodesuffix>\t{%2, %1, %0<mask_scalar_merge_operand3>|%0<mask_scalar_merge_operand3>, %1, %2}
   vptestnm<ssemodesuffix>\t{%1, %1, %0<mask_scalar_merge_operand3>|%0<mask_scalar_merge_operand3>, %1, %1}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_eq<mode>3<mask_scalar_merge_name>_1"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk,Yk")
	(unspec:<avx512fmaskmode>
	  [(match_operand:VI48_AVX512VL 1 "nonimm_or_0_operand" "%v,v")
	   (match_operand:VI48_AVX512VL 2 "nonimm_or_0_operand" "vm,C")]
	  UNSPEC_MASKED_EQ))]
  "TARGET_AVX512F && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   vpcmpeq<ssemodesuffix>\t{%2, %1, %0<mask_scalar_merge_operand3>|%0<mask_scalar_merge_operand3>, %1, %2}
   vptestnm<ssemodesuffix>\t{%1, %1, %0<mask_scalar_merge_operand3>|%0<mask_scalar_merge_operand3>, %1, %1}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "*sse4_1_eqv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "=Yr,*x,x")
	(eq:V2DI
	  (match_operand:V2DI 1 "vector_operand" "%0,0,x")
	  (match_operand:V2DI 2 "vector_operand" "YrBm,*xBm,xm")))]
  "TARGET_SSE4_1 && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   pcmpeqq\t{%2, %0|%0, %2}
   pcmpeqq\t{%2, %0|%0, %2}
   vpcmpeqq\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssecmp")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "mode" "TI")])

(define_insn "*sse2_eq<mode>3"
  [(set (match_operand:VI124_128 0 "register_operand" "=x,x")
	(eq:VI124_128
	  (match_operand:VI124_128 1 "vector_operand" "%0,x")
	  (match_operand:VI124_128 2 "vector_operand" "xBm,xm")))]
  "TARGET_SSE2 && !TARGET_XOP
   && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   pcmpeq<ssemodesuffix>\t{%2, %0|%0, %2}
   vpcmpeq<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "ssecmp")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "TI")])

(define_expand "sse2_eq<mode>3"
  [(set (match_operand:VI124_128 0 "register_operand")
	(eq:VI124_128
	  (match_operand:VI124_128 1 "vector_operand")
	  (match_operand:VI124_128 2 "vector_operand")))]
  "TARGET_SSE2 && !TARGET_XOP "
  "ix86_fixup_binary_operands_no_copy (EQ, <MODE>mode, operands);")

(define_expand "sse4_1_eqv2di3"
  [(set (match_operand:V2DI 0 "register_operand")
	(eq:V2DI
	  (match_operand:V2DI 1 "vector_operand")
	  (match_operand:V2DI 2 "vector_operand")))]
  "TARGET_SSE4_1"
  "ix86_fixup_binary_operands_no_copy (EQ, V2DImode, operands);")

(define_insn "sse4_2_gtv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "=Yr,*x,x")
	(gt:V2DI
	  (match_operand:V2DI 1 "register_operand" "0,0,x")
	  (match_operand:V2DI 2 "vector_operand" "YrBm,*xBm,xm")))]
  "TARGET_SSE4_2"
  "@
   pcmpgtq\t{%2, %0|%0, %2}
   pcmpgtq\t{%2, %0|%0, %2}
   vpcmpgtq\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssecmp")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "mode" "TI")])

(define_insn "avx2_gt<mode>3"
  [(set (match_operand:VI_256 0 "register_operand" "=x")
	(gt:VI_256
	  (match_operand:VI_256 1 "register_operand" "x")
	  (match_operand:VI_256 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX2"
  "vpcmpgt<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "OI")])

(define_insn "<avx512>_gt<mode>3<mask_scalar_merge_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(unspec:<avx512fmaskmode>
	  [(match_operand:VI48_AVX512VL 1 "register_operand" "v")
	   (match_operand:VI48_AVX512VL 2 "nonimmediate_operand" "vm")] UNSPEC_MASKED_GT))]
  "TARGET_AVX512F"
  "vpcmpgt<ssemodesuffix>\t{%2, %1, %0<mask_scalar_merge_operand3>|%0<mask_scalar_merge_operand3>, %1, %2}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_gt<mode>3<mask_scalar_merge_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(unspec:<avx512fmaskmode>
	  [(match_operand:VI12_AVX512VL 1 "register_operand" "v")
	   (match_operand:VI12_AVX512VL 2 "nonimmediate_operand" "vm")] UNSPEC_MASKED_GT))]
  "TARGET_AVX512BW"
  "vpcmpgt<ssemodesuffix>\t{%2, %1, %0<mask_scalar_merge_operand3>|%0<mask_scalar_merge_operand3>, %1, %2}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "sse2_gt<mode>3"
  [(set (match_operand:VI124_128 0 "register_operand" "=x,x")
	(gt:VI124_128
	  (match_operand:VI124_128 1 "register_operand" "0,x")
	  (match_operand:VI124_128 2 "vector_operand" "xBm,xm")))]
  "TARGET_SSE2 && !TARGET_XOP"
  "@
   pcmpgt<ssemodesuffix>\t{%2, %0|%0, %2}
   vpcmpgt<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "ssecmp")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "TI")])

(define_expand "vcond<V_512:mode><VI_AVX512BW:mode>"
  [(set (match_operand:V_512 0 "register_operand")
	(if_then_else:V_512
	  (match_operator 3 ""
	    [(match_operand:VI_AVX512BW 4 "nonimmediate_operand")
	     (match_operand:VI_AVX512BW 5 "general_operand")])
	  (match_operand:V_512 1)
	  (match_operand:V_512 2)))]
  "TARGET_AVX512F
   && (GET_MODE_NUNITS (<V_512:MODE>mode)
       == GET_MODE_NUNITS (<VI_AVX512BW:MODE>mode))"
{
  bool ok = ix86_expand_int_vcond (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vcond<V_256:mode><VI_256:mode>"
  [(set (match_operand:V_256 0 "register_operand")
	(if_then_else:V_256
	  (match_operator 3 ""
	    [(match_operand:VI_256 4 "nonimmediate_operand")
	     (match_operand:VI_256 5 "general_operand")])
	  (match_operand:V_256 1)
	  (match_operand:V_256 2)))]
  "TARGET_AVX2
   && (GET_MODE_NUNITS (<V_256:MODE>mode)
       == GET_MODE_NUNITS (<VI_256:MODE>mode))"
{
  bool ok = ix86_expand_int_vcond (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vcond<V_128:mode><VI124_128:mode>"
  [(set (match_operand:V_128 0 "register_operand")
	(if_then_else:V_128
	  (match_operator 3 ""
	    [(match_operand:VI124_128 4 "vector_operand")
	     (match_operand:VI124_128 5 "general_operand")])
	  (match_operand:V_128 1)
	  (match_operand:V_128 2)))]
  "TARGET_SSE2
   && (GET_MODE_NUNITS (<V_128:MODE>mode)
       == GET_MODE_NUNITS (<VI124_128:MODE>mode))"
{
  bool ok = ix86_expand_int_vcond (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vcond<VI8F_128:mode>v2di"
  [(set (match_operand:VI8F_128 0 "register_operand")
	(if_then_else:VI8F_128
	  (match_operator 3 ""
	    [(match_operand:V2DI 4 "vector_operand")
	     (match_operand:V2DI 5 "general_operand")])
	  (match_operand:VI8F_128 1)
	  (match_operand:VI8F_128 2)))]
  "TARGET_SSE4_2"
{
  bool ok = ix86_expand_int_vcond (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vcondu<V_512:mode><VI_AVX512BW:mode>"
  [(set (match_operand:V_512 0 "register_operand")
	(if_then_else:V_512
	  (match_operator 3 ""
	    [(match_operand:VI_AVX512BW 4 "nonimmediate_operand")
	     (match_operand:VI_AVX512BW 5 "nonimmediate_operand")])
	  (match_operand:V_512 1 "general_operand")
	  (match_operand:V_512 2 "general_operand")))]
  "TARGET_AVX512F
   && (GET_MODE_NUNITS (<V_512:MODE>mode)
       == GET_MODE_NUNITS (<VI_AVX512BW:MODE>mode))"
{
  bool ok = ix86_expand_int_vcond (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vcondu<V_256:mode><VI_256:mode>"
  [(set (match_operand:V_256 0 "register_operand")
	(if_then_else:V_256
	  (match_operator 3 ""
	    [(match_operand:VI_256 4 "nonimmediate_operand")
	     (match_operand:VI_256 5 "nonimmediate_operand")])
	  (match_operand:V_256 1 "general_operand")
	  (match_operand:V_256 2 "general_operand")))]
  "TARGET_AVX2
   && (GET_MODE_NUNITS (<V_256:MODE>mode)
       == GET_MODE_NUNITS (<VI_256:MODE>mode))"
{
  bool ok = ix86_expand_int_vcond (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vcondu<V_128:mode><VI124_128:mode>"
  [(set (match_operand:V_128 0 "register_operand")
	(if_then_else:V_128
	  (match_operator 3 ""
	    [(match_operand:VI124_128 4 "vector_operand")
	     (match_operand:VI124_128 5 "vector_operand")])
	  (match_operand:V_128 1 "general_operand")
	  (match_operand:V_128 2 "general_operand")))]
  "TARGET_SSE2
   && (GET_MODE_NUNITS (<V_128:MODE>mode)
       == GET_MODE_NUNITS (<VI124_128:MODE>mode))"
{
  bool ok = ix86_expand_int_vcond (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vcondu<VI8F_128:mode>v2di"
  [(set (match_operand:VI8F_128 0 "register_operand")
	(if_then_else:VI8F_128
	  (match_operator 3 ""
	    [(match_operand:V2DI 4 "vector_operand")
	     (match_operand:V2DI 5 "vector_operand")])
	  (match_operand:VI8F_128 1 "general_operand")
	  (match_operand:VI8F_128 2 "general_operand")))]
  "TARGET_SSE4_2"
{
  bool ok = ix86_expand_int_vcond (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vcondeq<VI8F_128:mode>v2di"
  [(set (match_operand:VI8F_128 0 "register_operand")
	(if_then_else:VI8F_128
	  (match_operator 3 ""
	    [(match_operand:V2DI 4 "vector_operand")
	     (match_operand:V2DI 5 "general_operand")])
	  (match_operand:VI8F_128 1)
	  (match_operand:VI8F_128 2)))]
  "TARGET_SSE4_1"
{
  bool ok = ix86_expand_int_vcond (operands);
  gcc_assert (ok);
  DONE;
})

(define_mode_iterator VEC_PERM_AVX2
  [V16QI V8HI V4SI V2DI V4SF V2DF
   (V32QI "TARGET_AVX2") (V16HI "TARGET_AVX2")
   (V8SI "TARGET_AVX2") (V4DI "TARGET_AVX2")
   (V8SF "TARGET_AVX2") (V4DF "TARGET_AVX2")
   (V16SF "TARGET_AVX512F") (V8DF "TARGET_AVX512F")
   (V16SI "TARGET_AVX512F") (V8DI "TARGET_AVX512F")
   (V32HI "TARGET_AVX512BW") (V64QI "TARGET_AVX512VBMI")])

(define_expand "vec_perm<mode>"
  [(match_operand:VEC_PERM_AVX2 0 "register_operand")
   (match_operand:VEC_PERM_AVX2 1 "register_operand")
   (match_operand:VEC_PERM_AVX2 2 "register_operand")
   (match_operand:<sseintvecmode> 3 "register_operand")]
  "TARGET_SSSE3 || TARGET_AVX || TARGET_XOP"
{
  ix86_expand_vec_perm (operands);
  DONE;
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel bitwise logical operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "one_cmpl<mode>2"
  [(set (match_operand:VI 0 "register_operand")
	(xor:VI (match_operand:VI 1 "vector_operand")
		(match_dup 2)))]
  "TARGET_SSE"
{
  operands[2] = force_reg (<MODE>mode, CONSTM1_RTX (<MODE>mode));
})

(define_expand "<sse2_avx2>_andnot<mode>3"
  [(set (match_operand:VI_AVX2 0 "register_operand")
	(and:VI_AVX2
	  (not:VI_AVX2 (match_operand:VI_AVX2 1 "register_operand"))
	  (match_operand:VI_AVX2 2 "vector_operand")))]
  "TARGET_SSE2")

(define_expand "<sse2_avx2>_andnot<mode>3_mask"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand")
	(vec_merge:VI48_AVX512VL
	  (and:VI48_AVX512VL
	    (not:VI48_AVX512VL
	      (match_operand:VI48_AVX512VL 1 "register_operand"))
	    (match_operand:VI48_AVX512VL 2 "nonimmediate_operand"))
	  (match_operand:VI48_AVX512VL 3 "nonimm_or_0_operand")
	  (match_operand:<avx512fmaskmode> 4 "register_operand")))]
  "TARGET_AVX512F")

(define_expand "<sse2_avx2>_andnot<mode>3_mask"
  [(set (match_operand:VI12_AVX512VL 0 "register_operand")
	(vec_merge:VI12_AVX512VL
	  (and:VI12_AVX512VL
	    (not:VI12_AVX512VL
	      (match_operand:VI12_AVX512VL 1 "register_operand"))
	    (match_operand:VI12_AVX512VL 2 "nonimmediate_operand"))
	  (match_operand:VI12_AVX512VL 3 "nonimm_or_0_operand")
	  (match_operand:<avx512fmaskmode> 4 "register_operand")))]
  "TARGET_AVX512BW")

(define_insn "*andnot<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=x,x,v")
	(and:VI
	  (not:VI (match_operand:VI 1 "register_operand" "0,x,v"))
	  (match_operand:VI 2 "vector_operand" "xBm,xm,vm")))]
  "TARGET_SSE"
{
  static char buf[64];
  const char *ops;
  const char *tmp;
  const char *ssesuffix;

  switch (get_attr_mode (insn))
    {
    case MODE_XI:
      gcc_assert (TARGET_AVX512F);
      /* FALLTHRU */
    case MODE_OI:
      gcc_assert (TARGET_AVX2);
      /* FALLTHRU */
    case MODE_TI:
      gcc_assert (TARGET_SSE2);
      tmp = "pandn";
      switch (<MODE>mode)
	{
	case E_V64QImode:
	case E_V32HImode:
	  /* There is no vpandnb or vpandnw instruction, nor vpandn for
	     512-bit vectors. Use vpandnq instead.  */
	  ssesuffix = "q";
	  break;
	case E_V16SImode:
	case E_V8DImode:
	  ssesuffix = "<ssemodesuffix>";
	  break;
	case E_V8SImode:
	case E_V4DImode:
	case E_V4SImode:
	case E_V2DImode:
	  ssesuffix = (TARGET_AVX512VL && which_alternative == 2
		       ? "<ssemodesuffix>" : "");
	  break;
	default:
	  ssesuffix = TARGET_AVX512VL && which_alternative == 2 ? "q" : "";
	}
      break;

    case MODE_V16SF:
      gcc_assert (TARGET_AVX512F);
      /* FALLTHRU */
    case MODE_V8SF:
      gcc_assert (TARGET_AVX);
      /* FALLTHRU */
    case MODE_V4SF:
      gcc_assert (TARGET_SSE);
      tmp = "andn";
      ssesuffix = "ps";
      break;

    default:
      gcc_unreachable ();
    }

  switch (which_alternative)
    {
    case 0:
      ops = "%s%s\t{%%2, %%0|%%0, %%2}";
      break;
    case 1:
    case 2:
      ops = "v%s%s\t{%%2, %%1, %%0|%%0, %%1, %%2}";
      break;
    default:
      gcc_unreachable ();
    }

  snprintf (buf, sizeof (buf), ops, tmp, ssesuffix);
  return buf;
}
  [(set_attr "isa" "noavx,avx,avx")
   (set_attr "type" "sselog")
   (set (attr "prefix_data16")
     (if_then_else
       (and (eq_attr "alternative" "0")
	    (eq_attr "mode" "TI"))
       (const_string "1")
       (const_string "*")))
   (set_attr "prefix" "orig,vex,evex")
   (set (attr "mode")
	(cond [(and (match_test "<MODE_SIZE> == 16")
		    (match_test "TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL"))
		 (const_string "<ssePSmode>")
	       (match_test "TARGET_AVX2")
		 (const_string "<sseinsnmode>")
	       (match_test "TARGET_AVX")
		 (if_then_else
		   (match_test "<MODE_SIZE> > 16")
		   (const_string "V8SF")
		   (const_string "<sseinsnmode>"))
	       (ior (not (match_test "TARGET_SSE2"))
		    (match_test "optimize_function_for_size_p (cfun)"))
		 (const_string "V4SF")
	      ]
	      (const_string "<sseinsnmode>")))])

(define_insn "*andnot<mode>3_mask"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI48_AVX512VL
	  (and:VI48_AVX512VL
	    (not:VI48_AVX512VL
	      (match_operand:VI48_AVX512VL 1 "register_operand" "v"))
	    (match_operand:VI48_AVX512VL 2 "nonimmediate_operand" "vm"))
	  (match_operand:VI48_AVX512VL 3 "nonimm_or_0_operand" "0C")
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512F"
  "vpandn<ssemodesuffix>\t{%2, %1, %0%{%4%}%N3|%0%{%4%}%N3, %1, %2}";
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "<code><mode>3"
  [(set (match_operand:VI 0 "register_operand")
	(any_logic:VI
	  (match_operand:VI 1 "nonimmediate_or_const_vector_operand")
	  (match_operand:VI 2 "nonimmediate_or_const_vector_operand")))]
  "TARGET_SSE"
{
  ix86_expand_vector_logical_operator (<CODE>, <MODE>mode, operands);
  DONE;
})

(define_insn "<mask_codefor><code><mode>3<mask_name>"
  [(set (match_operand:VI48_AVX_AVX512F 0 "register_operand" "=x,x,v")
	(any_logic:VI48_AVX_AVX512F
	  (match_operand:VI48_AVX_AVX512F 1 "vector_operand" "%0,x,v")
	  (match_operand:VI48_AVX_AVX512F 2 "vector_operand" "xBm,xm,vm")))]
  "TARGET_SSE && <mask_mode512bit_condition>
   && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
{
  static char buf[64];
  const char *ops;
  const char *tmp;
  const char *ssesuffix;

  switch (get_attr_mode (insn))
    {
    case MODE_XI:
      gcc_assert (TARGET_AVX512F);
      /* FALLTHRU */
    case MODE_OI:
      gcc_assert (TARGET_AVX2);
      /* FALLTHRU */
    case MODE_TI:
      gcc_assert (TARGET_SSE2);
      tmp = "p<logic>";
      switch (<MODE>mode)
	{
	case E_V16SImode:
	case E_V8DImode:
	  ssesuffix = "<ssemodesuffix>";
	  break;
	case E_V8SImode:
	case E_V4DImode:
	case E_V4SImode:
	case E_V2DImode:
	  ssesuffix = (TARGET_AVX512VL
		       && (<mask_applied> || which_alternative == 2)
		       ? "<ssemodesuffix>" : "");
	  break;
	default:
	  gcc_unreachable ();
	}
      break;

    case MODE_V8SF:
      gcc_assert (TARGET_AVX);
      /* FALLTHRU */
    case MODE_V4SF:
      gcc_assert (TARGET_SSE);
      tmp = "<logic>";
      ssesuffix = "ps";
      break;

    default:
      gcc_unreachable ();
    }

  switch (which_alternative)
    {
    case 0:
      if (<mask_applied>)
        ops = "v%s%s\t{%%2, %%0, %%0<mask_operand3_1>|%%0<mask_operand3_1>, %%0, %%2}";
      else
        ops = "%s%s\t{%%2, %%0|%%0, %%2}";
      break;
    case 1:
    case 2:
      ops = "v%s%s\t{%%2, %%1, %%0<mask_operand3_1>|%%0<mask_operand3_1>, %%1, %%2}";
      break;
    default:
      gcc_unreachable ();
    }

  snprintf (buf, sizeof (buf), ops, tmp, ssesuffix);
  return buf;
}
  [(set_attr "isa" "noavx,avx,avx")
   (set_attr "type" "sselog")
   (set (attr "prefix_data16")
     (if_then_else
       (and (eq_attr "alternative" "0")
	    (eq_attr "mode" "TI"))
       (const_string "1")
       (const_string "*")))
   (set_attr "prefix" "<mask_prefix3>,evex")
   (set (attr "mode")
	(cond [(and (match_test "<MODE_SIZE> == 16")
		    (match_test "TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL"))
		 (const_string "<ssePSmode>")
	       (match_test "TARGET_AVX2")
		 (const_string "<sseinsnmode>")
	       (match_test "TARGET_AVX")
		 (if_then_else
		   (match_test "<MODE_SIZE> > 16")
		   (const_string "V8SF")
		   (const_string "<sseinsnmode>"))
	       (ior (not (match_test "TARGET_SSE2"))
		    (match_test "optimize_function_for_size_p (cfun)"))
		 (const_string "V4SF")
	      ]
	      (const_string "<sseinsnmode>")))])

(define_insn "*<code><mode>3"
  [(set (match_operand:VI12_AVX_AVX512F 0 "register_operand" "=x,x,v")
	(any_logic:VI12_AVX_AVX512F
	  (match_operand:VI12_AVX_AVX512F 1 "vector_operand" "%0,x,v")
	  (match_operand:VI12_AVX_AVX512F 2 "vector_operand" "xBm,xm,vm")))]
  "TARGET_SSE && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
{
  static char buf[64];
  const char *ops;
  const char *tmp;
  const char *ssesuffix;

  switch (get_attr_mode (insn))
    {
    case MODE_XI:
      gcc_assert (TARGET_AVX512F);
      /* FALLTHRU */
    case MODE_OI:
      gcc_assert (TARGET_AVX2);
      /* FALLTHRU */
    case MODE_TI:
      gcc_assert (TARGET_SSE2);
      tmp = "p<logic>";
      switch (<MODE>mode)
	{
	case E_V64QImode:
	case E_V32HImode:
	  ssesuffix = "q";
	  break;
	case E_V32QImode:
	case E_V16HImode:
	case E_V16QImode:
	case E_V8HImode:
	  ssesuffix = TARGET_AVX512VL && which_alternative == 2 ? "q" : "";
	  break;
	default:
	  gcc_unreachable ();
	}
      break;

    case MODE_V8SF:
      gcc_assert (TARGET_AVX);
      /* FALLTHRU */
    case MODE_V4SF:
      gcc_assert (TARGET_SSE);
      tmp = "<logic>";
      ssesuffix = "ps";
      break;

    default:
      gcc_unreachable ();
    }

  switch (which_alternative)
    {
    case 0:
      ops = "%s%s\t{%%2, %%0|%%0, %%2}";
      break;
    case 1:
    case 2:
      ops = "v%s%s\t{%%2, %%1, %%0|%%0, %%1, %%2}";
      break;
    default:
      gcc_unreachable ();
    }

  snprintf (buf, sizeof (buf), ops, tmp, ssesuffix);
  return buf;
}
  [(set_attr "isa" "noavx,avx,avx")
   (set_attr "type" "sselog")
   (set (attr "prefix_data16")
     (if_then_else
       (and (eq_attr "alternative" "0")
	    (eq_attr "mode" "TI"))
       (const_string "1")
       (const_string "*")))
   (set_attr "prefix" "orig,vex,evex")
   (set (attr "mode")
	(cond [(and (match_test "<MODE_SIZE> == 16")
		    (match_test "TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL"))
		 (const_string "<ssePSmode>")
	       (match_test "TARGET_AVX2")
		 (const_string "<sseinsnmode>")
	       (match_test "TARGET_AVX")
		 (if_then_else
		   (match_test "<MODE_SIZE> > 16")
		   (const_string "V8SF")
		   (const_string "<sseinsnmode>"))
	       (ior (not (match_test "TARGET_SSE2"))
		    (match_test "optimize_function_for_size_p (cfun)"))
		 (const_string "V4SF")
	      ]
	      (const_string "<sseinsnmode>")))])

(define_insn "<avx512>_testm<mode>3<mask_scalar_merge_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(unspec:<avx512fmaskmode>
	 [(match_operand:VI12_AVX512VL 1 "register_operand" "v")
	  (match_operand:VI12_AVX512VL 2 "nonimmediate_operand" "vm")]
	 UNSPEC_TESTM))]
  "TARGET_AVX512BW"
  "vptestm<ssemodesuffix>\t{%2, %1, %0<mask_scalar_merge_operand3>|%0<mask_scalar_merge_operand3>, %1, %2}"
  [(set_attr "prefix" "evex")
   (set_attr "mode"  "<sseinsnmode>")])

(define_insn "<avx512>_testm<mode>3<mask_scalar_merge_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(unspec:<avx512fmaskmode>
	 [(match_operand:VI48_AVX512VL 1 "register_operand" "v")
	  (match_operand:VI48_AVX512VL 2 "nonimmediate_operand" "vm")]
	 UNSPEC_TESTM))]
  "TARGET_AVX512F"
  "vptestm<ssemodesuffix>\t{%2, %1, %0<mask_scalar_merge_operand3>|%0<mask_scalar_merge_operand3>, %1, %2}"
  [(set_attr "prefix" "evex")
   (set_attr "mode"  "<sseinsnmode>")])

(define_insn "<avx512>_testnm<mode>3<mask_scalar_merge_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(unspec:<avx512fmaskmode>
	 [(match_operand:VI12_AVX512VL 1 "register_operand" "v")
	  (match_operand:VI12_AVX512VL 2 "nonimmediate_operand" "vm")]
	 UNSPEC_TESTNM))]
  "TARGET_AVX512BW"
  "vptestnm<ssemodesuffix>\t{%2, %1, %0<mask_scalar_merge_operand3>|%0<mask_scalar_merge_operand3>, %1, %2}"
  [(set_attr "prefix" "evex")
   (set_attr "mode"  "<sseinsnmode>")])

(define_insn "<avx512>_testnm<mode>3<mask_scalar_merge_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(unspec:<avx512fmaskmode>
	 [(match_operand:VI48_AVX512VL 1 "register_operand" "v")
	  (match_operand:VI48_AVX512VL 2 "nonimmediate_operand" "vm")]
	 UNSPEC_TESTNM))]
  "TARGET_AVX512F"
  "vptestnm<ssemodesuffix>\t{%2, %1, %0<mask_scalar_merge_operand3>|%0<mask_scalar_merge_operand3>, %1, %2}"
  [(set_attr "prefix" "evex")
   (set_attr "mode"  "<sseinsnmode>")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel integral element swizzling
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "vec_pack_trunc_<mode>"
  [(match_operand:<ssepackmode> 0 "register_operand")
   (match_operand:VI248_AVX2_8_AVX512F_24_AVX512BW 1 "register_operand")
   (match_operand:VI248_AVX2_8_AVX512F_24_AVX512BW 2 "register_operand")]
  "TARGET_SSE2"
{
  rtx op1 = gen_lowpart (<ssepackmode>mode, operands[1]);
  rtx op2 = gen_lowpart (<ssepackmode>mode, operands[2]);
  ix86_expand_vec_extract_even_odd (operands[0], op1, op2, 0);
  DONE;
})

(define_expand "vec_pack_trunc_qi"
  [(set (match_operand:HI 0 ("register_operand"))
        (ior:HI (ashift:HI (zero_extend:HI (match_operand:QI 2 ("register_operand")))
                           (const_int 8))
                (zero_extend:HI (match_operand:QI 1 ("register_operand")))))]
  "TARGET_AVX512F")

(define_expand "vec_pack_trunc_<mode>"
  [(set (match_operand:<DOUBLEMASKMODE> 0 ("register_operand"))
        (ior:<DOUBLEMASKMODE> (ashift:<DOUBLEMASKMODE> (zero_extend:<DOUBLEMASKMODE> (match_operand:SWI24 2 ("register_operand")))
                           (match_dup 3))
                (zero_extend:<DOUBLEMASKMODE> (match_operand:SWI24 1 ("register_operand")))))]
  "TARGET_AVX512BW"
{
  operands[3] = GEN_INT (GET_MODE_BITSIZE (<MODE>mode));
})

(define_insn "<sse2_avx2>_packsswb<mask_name>"
  [(set (match_operand:VI1_AVX512 0 "register_operand" "=x,x,v")
	(vec_concat:VI1_AVX512
	  (ss_truncate:<ssehalfvecmode>
	    (match_operand:<sseunpackmode> 1 "register_operand" "0,x,v"))
	  (ss_truncate:<ssehalfvecmode>
	    (match_operand:<sseunpackmode> 2 "vector_operand" "xBm,xm,vm"))))]
  "TARGET_SSE2 && <mask_mode512bit_condition> && <mask_avx512bw_condition>"
  "@
   packsswb\t{%2, %0|%0, %2}
   vpacksswb\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}
   vpacksswb\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx,avx512bw")
   (set_attr "type" "sselog")
   (set_attr "prefix_data16" "1,*,*")
   (set_attr "prefix" "orig,<mask_prefix>,evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<sse2_avx2>_packssdw<mask_name>"
  [(set (match_operand:VI2_AVX2 0 "register_operand" "=x,x,v")
	(vec_concat:VI2_AVX2
	  (ss_truncate:<ssehalfvecmode>
	    (match_operand:<sseunpackmode> 1 "register_operand" "0,x,v"))
	  (ss_truncate:<ssehalfvecmode>
	    (match_operand:<sseunpackmode> 2 "vector_operand" "xBm,xm,vm"))))]
  "TARGET_SSE2 && <mask_mode512bit_condition> && <mask_avx512bw_condition>"
  "@
   packssdw\t{%2, %0|%0, %2}
   vpackssdw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}
   vpackssdw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx,avx512bw")
   (set_attr "type" "sselog")
   (set_attr "prefix_data16" "1,*,*")
   (set_attr "prefix" "orig,<mask_prefix>,evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<sse2_avx2>_packuswb<mask_name>"
  [(set (match_operand:VI1_AVX512 0 "register_operand" "=x,x,v")
	(vec_concat:VI1_AVX512
	  (us_truncate:<ssehalfvecmode>
	    (match_operand:<sseunpackmode> 1 "register_operand" "0,x,v"))
	  (us_truncate:<ssehalfvecmode>
	    (match_operand:<sseunpackmode> 2 "vector_operand" "xBm,xm,vm"))))]
  "TARGET_SSE2 && <mask_mode512bit_condition> && <mask_avx512bw_condition>"
  "@
   packuswb\t{%2, %0|%0, %2}
   vpackuswb\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}
   vpackuswb\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx,avx512bw")
   (set_attr "type" "sselog")
   (set_attr "prefix_data16" "1,*,*")
   (set_attr "prefix" "orig,<mask_prefix>,evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "avx512bw_interleave_highv64qi<mask_name>"
  [(set (match_operand:V64QI 0 "register_operand" "=v")
	(vec_select:V64QI
	  (vec_concat:V128QI
	    (match_operand:V64QI 1 "register_operand" "v")
	    (match_operand:V64QI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 8)  (const_int 72)
		     (const_int 9)  (const_int 73)
		     (const_int 10) (const_int 74)
		     (const_int 11) (const_int 75)
		     (const_int 12) (const_int 76)
		     (const_int 13) (const_int 77)
		     (const_int 14) (const_int 78)
		     (const_int 15) (const_int 79)
		     (const_int 24) (const_int 88)
		     (const_int 25) (const_int 89)
		     (const_int 26) (const_int 90)
		     (const_int 27) (const_int 91)
		     (const_int 28) (const_int 92)
		     (const_int 29) (const_int 93)
		     (const_int 30) (const_int 94)
		     (const_int 31) (const_int 95)
		     (const_int 40) (const_int 104)
		     (const_int 41) (const_int 105)
		     (const_int 42) (const_int 106)
		     (const_int 43) (const_int 107)
		     (const_int 44) (const_int 108)
		     (const_int 45) (const_int 109)
		     (const_int 46) (const_int 110)
		     (const_int 47) (const_int 111)
		     (const_int 56) (const_int 120)
		     (const_int 57) (const_int 121)
		     (const_int 58) (const_int 122)
		     (const_int 59) (const_int 123)
		     (const_int 60) (const_int 124)
		     (const_int 61) (const_int 125)
		     (const_int 62) (const_int 126)
		     (const_int 63) (const_int 127)])))]
  "TARGET_AVX512BW"
  "vpunpckhbw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "avx2_interleave_highv32qi<mask_name>"
  [(set (match_operand:V32QI 0 "register_operand" "=v")
	(vec_select:V32QI
	  (vec_concat:V64QI
	    (match_operand:V32QI 1 "register_operand" "v")
	    (match_operand:V32QI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 8)  (const_int 40)
		     (const_int 9)  (const_int 41)
		     (const_int 10) (const_int 42)
		     (const_int 11) (const_int 43)
		     (const_int 12) (const_int 44)
		     (const_int 13) (const_int 45)
		     (const_int 14) (const_int 46)
		     (const_int 15) (const_int 47)
		     (const_int 24) (const_int 56)
		     (const_int 25) (const_int 57)
		     (const_int 26) (const_int 58)
		     (const_int 27) (const_int 59)
		     (const_int 28) (const_int 60)
		     (const_int 29) (const_int 61)
		     (const_int 30) (const_int 62)
		     (const_int 31) (const_int 63)])))]
  "TARGET_AVX2 && <mask_avx512vl_condition>"
  "vpunpckhbw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "<mask_prefix>")
   (set_attr "mode" "OI")])

(define_insn "vec_interleave_highv16qi<mask_name>"
  [(set (match_operand:V16QI 0 "register_operand" "=x,v")
	(vec_select:V16QI
	  (vec_concat:V32QI
	    (match_operand:V16QI 1 "register_operand" "0,v")
	    (match_operand:V16QI 2 "vector_operand" "xBm,vm"))
	  (parallel [(const_int 8)  (const_int 24)
		     (const_int 9)  (const_int 25)
		     (const_int 10) (const_int 26)
		     (const_int 11) (const_int 27)
		     (const_int 12) (const_int 28)
		     (const_int 13) (const_int 29)
		     (const_int 14) (const_int 30)
		     (const_int 15) (const_int 31)])))]
  "TARGET_SSE2 && <mask_avx512vl_condition>"
  "@
   punpckhbw\t{%2, %0|%0, %2}
   vpunpckhbw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,<mask_prefix>")
   (set_attr "mode" "TI")])

(define_insn "avx512bw_interleave_lowv64qi<mask_name>"
  [(set (match_operand:V64QI 0 "register_operand" "=v")
	(vec_select:V64QI
	  (vec_concat:V128QI
	    (match_operand:V64QI 1 "register_operand" "v")
	    (match_operand:V64QI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 0) (const_int 64)
		     (const_int 1) (const_int 65)
		     (const_int 2) (const_int 66)
		     (const_int 3) (const_int 67)
		     (const_int 4) (const_int 68)
		     (const_int 5) (const_int 69)
		     (const_int 6) (const_int 70)
		     (const_int 7) (const_int 71)
		     (const_int 16) (const_int 80)
		     (const_int 17) (const_int 81)
		     (const_int 18) (const_int 82)
		     (const_int 19) (const_int 83)
		     (const_int 20) (const_int 84)
		     (const_int 21) (const_int 85)
		     (const_int 22) (const_int 86)
		     (const_int 23) (const_int 87)
		     (const_int 32) (const_int 96)
		     (const_int 33) (const_int 97)
		     (const_int 34) (const_int 98)
		     (const_int 35) (const_int 99)
		     (const_int 36) (const_int 100)
		     (const_int 37) (const_int 101)
		     (const_int 38) (const_int 102)
		     (const_int 39) (const_int 103)
		     (const_int 48) (const_int 112)
		     (const_int 49) (const_int 113)
		     (const_int 50) (const_int 114)
		     (const_int 51) (const_int 115)
		     (const_int 52) (const_int 116)
		     (const_int 53) (const_int 117)
		     (const_int 54) (const_int 118)
		     (const_int 55) (const_int 119)])))]
  "TARGET_AVX512BW"
  "vpunpcklbw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "avx2_interleave_lowv32qi<mask_name>"
  [(set (match_operand:V32QI 0 "register_operand" "=v")
	(vec_select:V32QI
	  (vec_concat:V64QI
	    (match_operand:V32QI 1 "register_operand" "v")
	    (match_operand:V32QI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 0) (const_int 32)
		     (const_int 1) (const_int 33)
		     (const_int 2) (const_int 34)
		     (const_int 3) (const_int 35)
		     (const_int 4) (const_int 36)
		     (const_int 5) (const_int 37)
		     (const_int 6) (const_int 38)
		     (const_int 7) (const_int 39)
		     (const_int 16) (const_int 48)
		     (const_int 17) (const_int 49)
		     (const_int 18) (const_int 50)
		     (const_int 19) (const_int 51)
		     (const_int 20) (const_int 52)
		     (const_int 21) (const_int 53)
		     (const_int 22) (const_int 54)
		     (const_int 23) (const_int 55)])))]
  "TARGET_AVX2 && <mask_avx512vl_condition> && <mask_avx512bw_condition>"
  "vpunpcklbw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "OI")])

(define_insn "vec_interleave_lowv16qi<mask_name>"
  [(set (match_operand:V16QI 0 "register_operand" "=x,v")
	(vec_select:V16QI
	  (vec_concat:V32QI
	    (match_operand:V16QI 1 "register_operand" "0,v")
	    (match_operand:V16QI 2 "vector_operand" "xBm,vm"))
	  (parallel [(const_int 0) (const_int 16)
		     (const_int 1) (const_int 17)
		     (const_int 2) (const_int 18)
		     (const_int 3) (const_int 19)
		     (const_int 4) (const_int 20)
		     (const_int 5) (const_int 21)
		     (const_int 6) (const_int 22)
		     (const_int 7) (const_int 23)])))]
  "TARGET_SSE2 && <mask_avx512vl_condition> && <mask_avx512bw_condition>"
  "@
   punpcklbw\t{%2, %0|%0, %2}
   vpunpcklbw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "TI")])

(define_insn "avx512bw_interleave_highv32hi<mask_name>"
  [(set (match_operand:V32HI 0 "register_operand" "=v")
	(vec_select:V32HI
	  (vec_concat:V64HI
	    (match_operand:V32HI 1 "register_operand" "v")
	    (match_operand:V32HI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 4) (const_int 36)
		     (const_int 5) (const_int 37)
		     (const_int 6) (const_int 38)
		     (const_int 7) (const_int 39)
		     (const_int 12) (const_int 44)
		     (const_int 13) (const_int 45)
		     (const_int 14) (const_int 46)
		     (const_int 15) (const_int 47)
		     (const_int 20) (const_int 52)
		     (const_int 21) (const_int 53)
		     (const_int 22) (const_int 54)
		     (const_int 23) (const_int 55)
		     (const_int 28) (const_int 60)
		     (const_int 29) (const_int 61)
		     (const_int 30) (const_int 62)
		     (const_int 31) (const_int 63)])))]
  "TARGET_AVX512BW"
  "vpunpckhwd\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "avx2_interleave_highv16hi<mask_name>"
  [(set (match_operand:V16HI 0 "register_operand" "=v")
	(vec_select:V16HI
	  (vec_concat:V32HI
	    (match_operand:V16HI 1 "register_operand" "v")
	    (match_operand:V16HI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 4) (const_int 20)
		     (const_int 5) (const_int 21)
		     (const_int 6) (const_int 22)
		     (const_int 7) (const_int 23)
		     (const_int 12) (const_int 28)
		     (const_int 13) (const_int 29)
		     (const_int 14) (const_int 30)
		     (const_int 15) (const_int 31)])))]
  "TARGET_AVX2 && <mask_avx512vl_condition> && <mask_avx512bw_condition>"
  "vpunpckhwd\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "OI")])

(define_insn "vec_interleave_highv8hi<mask_name>"
  [(set (match_operand:V8HI 0 "register_operand" "=x,v")
	(vec_select:V8HI
	  (vec_concat:V16HI
	    (match_operand:V8HI 1 "register_operand" "0,v")
	    (match_operand:V8HI 2 "vector_operand" "xBm,vm"))
	  (parallel [(const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))]
  "TARGET_SSE2 && <mask_avx512vl_condition> && <mask_avx512bw_condition>"
  "@
   punpckhwd\t{%2, %0|%0, %2}
   vpunpckhwd\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "<mask_codefor>avx512bw_interleave_lowv32hi<mask_name>"
  [(set (match_operand:V32HI 0 "register_operand" "=v")
	(vec_select:V32HI
	  (vec_concat:V64HI
	    (match_operand:V32HI 1 "register_operand" "v")
	    (match_operand:V32HI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 0) (const_int 32)
		     (const_int 1) (const_int 33)
		     (const_int 2) (const_int 34)
		     (const_int 3) (const_int 35)
		     (const_int 8) (const_int 40)
		     (const_int 9) (const_int 41)
		     (const_int 10) (const_int 42)
		     (const_int 11) (const_int 43)
		     (const_int 16) (const_int 48)
		     (const_int 17) (const_int 49)
		     (const_int 18) (const_int 50)
		     (const_int 19) (const_int 51)
		     (const_int 24) (const_int 56)
		     (const_int 25) (const_int 57)
		     (const_int 26) (const_int 58)
		     (const_int 27) (const_int 59)])))]
  "TARGET_AVX512BW"
  "vpunpcklwd\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "avx2_interleave_lowv16hi<mask_name>"
  [(set (match_operand:V16HI 0 "register_operand" "=v")
	(vec_select:V16HI
	  (vec_concat:V32HI
	    (match_operand:V16HI 1 "register_operand" "v")
	    (match_operand:V16HI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 0) (const_int 16)
		     (const_int 1) (const_int 17)
		     (const_int 2) (const_int 18)
		     (const_int 3) (const_int 19)
		     (const_int 8) (const_int 24)
		     (const_int 9) (const_int 25)
		     (const_int 10) (const_int 26)
		     (const_int 11) (const_int 27)])))]
  "TARGET_AVX2 && <mask_avx512vl_condition> && <mask_avx512bw_condition>"
  "vpunpcklwd\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "OI")])

(define_insn "vec_interleave_lowv8hi<mask_name>"
  [(set (match_operand:V8HI 0 "register_operand" "=x,v")
	(vec_select:V8HI
	  (vec_concat:V16HI
	    (match_operand:V8HI 1 "register_operand" "0,v")
	    (match_operand:V8HI 2 "vector_operand" "xBm,vm"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)])))]
  "TARGET_SSE2 && <mask_avx512vl_condition> && <mask_avx512bw_condition>"
  "@
   punpcklwd\t{%2, %0|%0, %2}
   vpunpcklwd\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,maybe_evex")
   (set_attr "mode" "TI")])

(define_insn "avx2_interleave_highv8si<mask_name>"
  [(set (match_operand:V8SI 0 "register_operand" "=v")
	(vec_select:V8SI
	  (vec_concat:V16SI
	    (match_operand:V8SI 1 "register_operand" "v")
	    (match_operand:V8SI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))]
  "TARGET_AVX2 && <mask_avx512vl_condition>"
  "vpunpckhdq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "OI")])

(define_insn "<mask_codefor>avx512f_interleave_highv16si<mask_name>"
  [(set (match_operand:V16SI 0 "register_operand" "=v")
	(vec_select:V16SI
	  (vec_concat:V32SI
	    (match_operand:V16SI 1 "register_operand" "v")
	    (match_operand:V16SI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 2) (const_int 18)
		     (const_int 3) (const_int 19)
		     (const_int 6) (const_int 22)
		     (const_int 7) (const_int 23)
		     (const_int 10) (const_int 26)
		     (const_int 11) (const_int 27)
		     (const_int 14) (const_int 30)
		     (const_int 15) (const_int 31)])))]
  "TARGET_AVX512F"
  "vpunpckhdq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])


(define_insn "vec_interleave_highv4si<mask_name>"
  [(set (match_operand:V4SI 0 "register_operand" "=x,v")
	(vec_select:V4SI
	  (vec_concat:V8SI
	    (match_operand:V4SI 1 "register_operand" "0,v")
	    (match_operand:V4SI 2 "vector_operand" "xBm,vm"))
	  (parallel [(const_int 2) (const_int 6)
		     (const_int 3) (const_int 7)])))]
  "TARGET_SSE2 && <mask_avx512vl_condition>"
  "@
   punpckhdq\t{%2, %0|%0, %2}
   vpunpckhdq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "avx2_interleave_lowv8si<mask_name>"
  [(set (match_operand:V8SI 0 "register_operand" "=v")
	(vec_select:V8SI
	  (vec_concat:V16SI
	    (match_operand:V8SI 1 "register_operand" "v")
	    (match_operand:V8SI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)])))]
  "TARGET_AVX2 && <mask_avx512vl_condition>"
  "vpunpckldq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "OI")])

(define_insn "<mask_codefor>avx512f_interleave_lowv16si<mask_name>"
  [(set (match_operand:V16SI 0 "register_operand" "=v")
	(vec_select:V16SI
	  (vec_concat:V32SI
	    (match_operand:V16SI 1 "register_operand" "v")
	    (match_operand:V16SI 2 "nonimmediate_operand" "vm"))
	  (parallel [(const_int 0) (const_int 16)
		     (const_int 1) (const_int 17)
		     (const_int 4) (const_int 20)
		     (const_int 5) (const_int 21)
		     (const_int 8) (const_int 24)
		     (const_int 9) (const_int 25)
		     (const_int 12) (const_int 28)
		     (const_int 13) (const_int 29)])))]
  "TARGET_AVX512F"
  "vpunpckldq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "vec_interleave_lowv4si<mask_name>"
  [(set (match_operand:V4SI 0 "register_operand" "=x,v")
	(vec_select:V4SI
	  (vec_concat:V8SI
	    (match_operand:V4SI 1 "register_operand" "0,v")
	    (match_operand:V4SI 2 "vector_operand" "xBm,vm"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)])))]
  "TARGET_SSE2 && <mask_avx512vl_condition>"
  "@
   punpckldq\t{%2, %0|%0, %2}
   vpunpckldq\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "TI")])

(define_expand "vec_interleave_high<mode>"
  [(match_operand:VI_256 0 "register_operand")
   (match_operand:VI_256 1 "register_operand")
   (match_operand:VI_256 2 "nonimmediate_operand")]
 "TARGET_AVX2"
{
  rtx t1 = gen_reg_rtx (<MODE>mode);
  rtx t2 = gen_reg_rtx (<MODE>mode);
  rtx t3 = gen_reg_rtx (V4DImode);
  emit_insn (gen_avx2_interleave_low<mode> (t1, operands[1], operands[2]));
  emit_insn (gen_avx2_interleave_high<mode> (t2,  operands[1], operands[2]));
  emit_insn (gen_avx2_permv2ti (t3, gen_lowpart (V4DImode, t1),
				gen_lowpart (V4DImode, t2),
				GEN_INT (1 + (3 << 4))));
  emit_move_insn (operands[0], gen_lowpart (<MODE>mode, t3));
  DONE;
})

(define_expand "vec_interleave_low<mode>"
  [(match_operand:VI_256 0 "register_operand")
   (match_operand:VI_256 1 "register_operand")
   (match_operand:VI_256 2 "nonimmediate_operand")]
 "TARGET_AVX2"
{
  rtx t1 = gen_reg_rtx (<MODE>mode);
  rtx t2 = gen_reg_rtx (<MODE>mode);
  rtx t3 = gen_reg_rtx (V4DImode);
  emit_insn (gen_avx2_interleave_low<mode> (t1, operands[1], operands[2]));
  emit_insn (gen_avx2_interleave_high<mode> (t2, operands[1], operands[2]));
  emit_insn (gen_avx2_permv2ti (t3, gen_lowpart (V4DImode, t1),
				gen_lowpart (V4DImode, t2),
				GEN_INT (0 + (2 << 4))));
  emit_move_insn (operands[0], gen_lowpart (<MODE>mode, t3));
  DONE;
})

;; Modes handled by pinsr patterns.
(define_mode_iterator PINSR_MODE
  [(V16QI "TARGET_SSE4_1") V8HI
   (V4SI "TARGET_SSE4_1")
   (V2DI "TARGET_SSE4_1 && TARGET_64BIT")])

(define_mode_attr sse2p4_1
  [(V16QI "sse4_1") (V8HI "sse2")
   (V4SI "sse4_1") (V2DI "sse4_1")])

(define_mode_attr pinsr_evex_isa
  [(V16QI "avx512bw") (V8HI "avx512bw")
   (V4SI "avx512dq") (V2DI "avx512dq")])

;; sse4_1_pinsrd must come before sse2_loadld since it is preferred.
(define_insn "<sse2p4_1>_pinsr<ssemodesuffix>"
  [(set (match_operand:PINSR_MODE 0 "register_operand" "=x,x,x,x,v,v")
	(vec_merge:PINSR_MODE
	  (vec_duplicate:PINSR_MODE
	    (match_operand:<ssescalarmode> 2 "nonimmediate_operand" "r,m,r,m,r,m"))
	  (match_operand:PINSR_MODE 1 "register_operand" "0,0,x,x,v,v")
	  (match_operand:SI 3 "const_int_operand")))]
  "TARGET_SSE2
   && ((unsigned) exact_log2 (INTVAL (operands[3]))
       < GET_MODE_NUNITS (<MODE>mode))"
{
  operands[3] = GEN_INT (exact_log2 (INTVAL (operands[3])));

  switch (which_alternative)
    {
    case 0:
      if (GET_MODE_SIZE (<ssescalarmode>mode) < GET_MODE_SIZE (SImode))
	return "pinsr<ssemodesuffix>\t{%3, %k2, %0|%0, %k2, %3}";
      /* FALLTHRU */
    case 1:
      return "pinsr<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}";
    case 2:
    case 4:
      if (GET_MODE_SIZE (<ssescalarmode>mode) < GET_MODE_SIZE (SImode))
	return "vpinsr<ssemodesuffix>\t{%3, %k2, %1, %0|%0, %1, %k2, %3}";
      /* FALLTHRU */
    case 3:
    case 5:
      return "vpinsr<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "isa" "noavx,noavx,avx,avx,<pinsr_evex_isa>,<pinsr_evex_isa>")
   (set_attr "type" "sselog")
   (set (attr "prefix_rex")
     (if_then_else
       (and (not (match_test "TARGET_AVX"))
	    (eq (const_string "<MODE>mode") (const_string "V2DImode")))
       (const_string "1")
       (const_string "*")))
   (set (attr "prefix_data16")
     (if_then_else
       (and (not (match_test "TARGET_AVX"))
	    (eq (const_string "<MODE>mode") (const_string "V8HImode")))
       (const_string "1")
       (const_string "*")))
   (set (attr "prefix_extra")
     (if_then_else
       (and (not (match_test "TARGET_AVX"))
	    (eq (const_string "<MODE>mode") (const_string "V8HImode")))
       (const_string "*")
       (const_string "1")))
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "orig,orig,vex,vex,evex,evex")
   (set_attr "mode" "TI")])

(define_expand "<extract_type>_vinsert<shuffletype><extract_suf>_mask"
  [(match_operand:AVX512_VEC 0 "register_operand")
   (match_operand:AVX512_VEC 1 "register_operand")
   (match_operand:<ssequartermode> 2 "nonimmediate_operand")
   (match_operand:SI 3 "const_0_to_3_operand")
   (match_operand:AVX512_VEC 4 "register_operand")
   (match_operand:<avx512fmaskmode> 5 "register_operand")]
  "TARGET_AVX512F"
{
  int mask, selector;
  mask = INTVAL (operands[3]);
  selector = (GET_MODE_UNIT_SIZE (<MODE>mode) == 4
  	      ? 0xFFFF ^ (0x000F << mask * 4)
	      : 0xFF ^ (0x03 << mask * 2));
  emit_insn (gen_<extract_type>_vinsert<shuffletype><extract_suf>_1_mask
    (operands[0], operands[1], operands[2], GEN_INT (selector),
     operands[4], operands[5]));
  DONE;
})

(define_insn "*<extract_type>_vinsert<shuffletype><extract_suf>_0"
  [(set (match_operand:AVX512_VEC 0 "register_operand" "=v,x,Yv")
	(vec_merge:AVX512_VEC
	  (match_operand:AVX512_VEC 1 "reg_or_0_operand" "v,C,C")
	  (vec_duplicate:AVX512_VEC
		(match_operand:<ssequartermode> 2 "nonimmediate_operand" "vm,xm,vm"))
	  (match_operand:SI 3 "const_int_operand" "n,n,n")))]
  "TARGET_AVX512F
   && (INTVAL (operands[3])
       == (GET_MODE_UNIT_SIZE (<MODE>mode) == 4 ? 0xFFF0 : 0xFC))"
{
  if (which_alternative == 0)
    return "vinsert<shuffletype><extract_suf>\t{$0, %2, %1, %0|%0, %1, %2, 0}";
  switch (<MODE>mode)
    {
    case E_V8DFmode:
      return "vmovapd\t{%2, %x0|%x0, %2}";
    case E_V16SFmode:
      return "vmovaps\t{%2, %x0|%x0, %2}";
    case E_V8DImode:
      return which_alternative == 2 ? "vmovdqa64\t{%2, %x0|%x0, %2}"
				    : "vmovdqa\t{%2, %x0|%x0, %2}";
    case E_V16SImode:
      return which_alternative == 2 ? "vmovdqa32\t{%2, %x0|%x0, %2}"
				    : "vmovdqa\t{%2, %x0|%x0, %2}";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "sselog,ssemov,ssemov")
   (set_attr "length_immediate" "1,0,0")
   (set_attr "prefix" "evex,vex,evex")
   (set_attr "mode" "<sseinsnmode>,<ssequarterinsnmode>,<ssequarterinsnmode>")])

(define_insn "<mask_codefor><extract_type>_vinsert<shuffletype><extract_suf>_1<mask_name>"
  [(set (match_operand:AVX512_VEC 0 "register_operand" "=v")
	(vec_merge:AVX512_VEC
	  (match_operand:AVX512_VEC 1 "register_operand" "v")
	  (vec_duplicate:AVX512_VEC
		(match_operand:<ssequartermode> 2 "nonimmediate_operand" "vm"))
	  (match_operand:SI 3 "const_int_operand" "n")))]
  "TARGET_AVX512F"
{
  int mask;
  int selector = INTVAL (operands[3]);

  if (selector == (GET_MODE_UNIT_SIZE (<MODE>mode) == 4 ? 0xFFF0 : 0xFC))
    mask = 0;
  else if (selector == (GET_MODE_UNIT_SIZE (<MODE>mode) == 4 ? 0xFF0F : 0xF3))
    mask = 1;
  else if (selector == (GET_MODE_UNIT_SIZE (<MODE>mode) == 4 ? 0xF0FF : 0xCF))
    mask = 2;
  else if (selector == (GET_MODE_UNIT_SIZE (<MODE>mode) == 4 ? 0x0FFF : 0x3F))
    mask = 3;
  else
    gcc_unreachable ();

  operands[3] = GEN_INT (mask);

  return "vinsert<shuffletype><extract_suf>\t{%3, %2, %1, %0<mask_operand4>|%0<mask_operand4>, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "<extract_type_2>_vinsert<shuffletype><extract_suf_2>_mask"
  [(match_operand:AVX512_VEC_2 0 "register_operand")
   (match_operand:AVX512_VEC_2 1 "register_operand")
   (match_operand:<ssehalfvecmode> 2 "nonimmediate_operand")
   (match_operand:SI 3 "const_0_to_1_operand")
   (match_operand:AVX512_VEC_2 4 "register_operand")
   (match_operand:<avx512fmaskmode> 5 "register_operand")]
  "TARGET_AVX512F"
{
  int mask = INTVAL (operands[3]);
  if (mask == 0)
    emit_insn (gen_vec_set_lo_<mode>_mask (operands[0], operands[1],
					   operands[2], operands[4],
					   operands[5]));
  else
    emit_insn (gen_vec_set_hi_<mode>_mask (operands[0], operands[1],
					   operands[2], operands[4],
					   operands[5]));
  DONE;
})

(define_insn "vec_set_lo_<mode><mask_name>"
  [(set (match_operand:V16FI 0 "register_operand" "=v")
	(vec_concat:V16FI
	  (match_operand:<ssehalfvecmode> 2 "nonimmediate_operand" "vm")
	  (vec_select:<ssehalfvecmode>
	    (match_operand:V16FI 1 "register_operand" "v")
	    (parallel [(const_int 8) (const_int 9)
		       (const_int 10) (const_int 11)
		       (const_int 12) (const_int 13)
		       (const_int 14) (const_int 15)]))))]
  "TARGET_AVX512DQ"
  "vinsert<shuffletype>32x8\t{$0x0, %2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2, 0x0}"
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vec_set_hi_<mode><mask_name>"
  [(set (match_operand:V16FI 0 "register_operand" "=v")
	(vec_concat:V16FI
	  (vec_select:<ssehalfvecmode>
	    (match_operand:V16FI 1 "register_operand" "v")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)
		       (const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))
	  (match_operand:<ssehalfvecmode> 2 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512DQ"
  "vinsert<shuffletype>32x8\t{$0x1, %2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2, 0x1}"
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vec_set_lo_<mode><mask_name>"
  [(set (match_operand:V8FI 0 "register_operand" "=v")
	(vec_concat:V8FI
	  (match_operand:<ssehalfvecmode> 2 "nonimmediate_operand" "vm")
	  (vec_select:<ssehalfvecmode>
	    (match_operand:V8FI 1 "register_operand" "v")
	    (parallel [(const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))))]
  "TARGET_AVX512F"
  "vinsert<shuffletype>64x4\t{$0x0, %2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2, 0x0}"
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "vec_set_hi_<mode><mask_name>"
  [(set (match_operand:V8FI 0 "register_operand" "=v")
	(vec_concat:V8FI
	  (vec_select:<ssehalfvecmode>
	    (match_operand:V8FI 1 "register_operand" "v")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)]))
	  (match_operand:<ssehalfvecmode> 2 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512F"
  "vinsert<shuffletype>64x4\t{$0x1, %2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2, 0x1}"
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_expand "avx512dq_shuf_<shuffletype>64x2_mask"
  [(match_operand:VI8F_256 0 "register_operand")
   (match_operand:VI8F_256 1 "register_operand")
   (match_operand:VI8F_256 2 "nonimmediate_operand")
   (match_operand:SI 3 "const_0_to_3_operand")
   (match_operand:VI8F_256 4 "register_operand")
   (match_operand:QI 5 "register_operand")]
  "TARGET_AVX512DQ"
{
  int mask = INTVAL (operands[3]);
  emit_insn (gen_avx512dq_shuf_<shuffletype>64x2_1_mask
      (operands[0], operands[1], operands[2],
       GEN_INT (((mask >> 0) & 1) * 2 + 0),
       GEN_INT (((mask >> 0) & 1) * 2 + 1),
       GEN_INT (((mask >> 1) & 1) * 2 + 4),
       GEN_INT (((mask >> 1) & 1) * 2 + 5),
       operands[4], operands[5]));
  DONE;
})

(define_insn "<mask_codefor>avx512dq_shuf_<shuffletype>64x2_1<mask_name>"
  [(set (match_operand:VI8F_256 0 "register_operand" "=v")
	(vec_select:VI8F_256
	  (vec_concat:<ssedoublemode>
	    (match_operand:VI8F_256 1 "register_operand" "v")
	    (match_operand:VI8F_256 2 "nonimmediate_operand" "vm"))
	  (parallel [(match_operand 3  "const_0_to_3_operand")
		     (match_operand 4  "const_0_to_3_operand")
		     (match_operand 5  "const_4_to_7_operand")
		     (match_operand 6  "const_4_to_7_operand")])))]
  "TARGET_AVX512VL
   && (INTVAL (operands[3]) == (INTVAL (operands[4]) - 1)
       && INTVAL (operands[5]) == (INTVAL (operands[6]) - 1))"
{
  int mask;
  mask = INTVAL (operands[3]) / 2;
  mask |= (INTVAL (operands[5]) - 4) / 2 << 1;
  operands[3] = GEN_INT (mask);
  return "vshuf<shuffletype>64x2\t{%3, %2, %1, %0<mask_operand7>|%0<mask_operand7>, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_expand "avx512f_shuf_<shuffletype>64x2_mask"
  [(match_operand:V8FI 0 "register_operand")
   (match_operand:V8FI 1 "register_operand")
   (match_operand:V8FI 2 "nonimmediate_operand")
   (match_operand:SI 3 "const_0_to_255_operand")
   (match_operand:V8FI 4 "register_operand")
   (match_operand:QI 5 "register_operand")]
  "TARGET_AVX512F"
{
  int mask = INTVAL (operands[3]);
  emit_insn (gen_avx512f_shuf_<shuffletype>64x2_1_mask
      (operands[0], operands[1], operands[2],
       GEN_INT (((mask >> 0) & 3) * 2),
       GEN_INT (((mask >> 0) & 3) * 2 + 1),
       GEN_INT (((mask >> 2) & 3) * 2),
       GEN_INT (((mask >> 2) & 3) * 2 + 1),
       GEN_INT (((mask >> 4) & 3) * 2 + 8),
       GEN_INT (((mask >> 4) & 3) * 2 + 9),
       GEN_INT (((mask >> 6) & 3) * 2 + 8),
       GEN_INT (((mask >> 6) & 3) * 2 + 9),
       operands[4], operands[5]));
  DONE;
})

(define_insn "avx512f_shuf_<shuffletype>64x2_1<mask_name>"
  [(set (match_operand:V8FI 0 "register_operand" "=v")
	(vec_select:V8FI
	  (vec_concat:<ssedoublemode>
	    (match_operand:V8FI 1 "register_operand" "v")
	    (match_operand:V8FI 2 "nonimmediate_operand" "vm"))
	  (parallel [(match_operand 3  "const_0_to_7_operand")
		     (match_operand 4  "const_0_to_7_operand")
		     (match_operand 5  "const_0_to_7_operand")
		     (match_operand 6  "const_0_to_7_operand")
		     (match_operand 7  "const_8_to_15_operand")
		     (match_operand 8  "const_8_to_15_operand")
		     (match_operand 9  "const_8_to_15_operand")
		     (match_operand 10  "const_8_to_15_operand")])))]
  "TARGET_AVX512F
   && (INTVAL (operands[3]) == (INTVAL (operands[4]) - 1)
       && INTVAL (operands[5]) == (INTVAL (operands[6]) - 1)
       && INTVAL (operands[7]) == (INTVAL (operands[8]) - 1)
       && INTVAL (operands[9]) == (INTVAL (operands[10]) - 1))"
{
  int mask;
  mask = INTVAL (operands[3]) / 2;
  mask |= INTVAL (operands[5]) / 2 << 2;
  mask |= (INTVAL (operands[7]) - 8) / 2 << 4;
  mask |= (INTVAL (operands[9]) - 8) / 2 << 6;
  operands[3] = GEN_INT (mask);

  return "vshuf<shuffletype>64x2\t{%3, %2, %1, %0<mask_operand11>|%0<mask_operand11>, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "avx512vl_shuf_<shuffletype>32x4_mask"
  [(match_operand:VI4F_256 0 "register_operand")
   (match_operand:VI4F_256 1 "register_operand")
   (match_operand:VI4F_256 2 "nonimmediate_operand")
   (match_operand:SI 3 "const_0_to_3_operand")
   (match_operand:VI4F_256 4 "register_operand")
   (match_operand:QI 5 "register_operand")]
  "TARGET_AVX512VL"
{
  int mask = INTVAL (operands[3]);
  emit_insn (gen_avx512vl_shuf_<shuffletype>32x4_1_mask
      (operands[0], operands[1], operands[2],
       GEN_INT (((mask >> 0) & 1) * 4 + 0),
       GEN_INT (((mask >> 0) & 1) * 4 + 1),
       GEN_INT (((mask >> 0) & 1) * 4 + 2),
       GEN_INT (((mask >> 0) & 1) * 4 + 3),
       GEN_INT (((mask >> 1) & 1) * 4 + 8),
       GEN_INT (((mask >> 1) & 1) * 4 + 9),
       GEN_INT (((mask >> 1) & 1) * 4 + 10),
       GEN_INT (((mask >> 1) & 1) * 4 + 11),
       operands[4], operands[5]));
  DONE;
})

(define_insn "avx512vl_shuf_<shuffletype>32x4_1<mask_name>"
  [(set (match_operand:VI4F_256 0 "register_operand" "=v")
	(vec_select:VI4F_256
	  (vec_concat:<ssedoublemode>
	    (match_operand:VI4F_256 1 "register_operand" "v")
	    (match_operand:VI4F_256 2 "nonimmediate_operand" "vm"))
	  (parallel [(match_operand 3  "const_0_to_7_operand")
		     (match_operand 4  "const_0_to_7_operand")
		     (match_operand 5  "const_0_to_7_operand")
		     (match_operand 6  "const_0_to_7_operand")
		     (match_operand 7  "const_8_to_15_operand")
		     (match_operand 8  "const_8_to_15_operand")
		     (match_operand 9  "const_8_to_15_operand")
		     (match_operand 10 "const_8_to_15_operand")])))]
  "TARGET_AVX512VL
   && (INTVAL (operands[3]) == (INTVAL (operands[4]) - 1)
       && INTVAL (operands[3]) == (INTVAL (operands[5]) - 2)
       && INTVAL (operands[3]) == (INTVAL (operands[6]) - 3)
       && INTVAL (operands[7]) == (INTVAL (operands[8]) - 1)
       && INTVAL (operands[7]) == (INTVAL (operands[9]) - 2)
       && INTVAL (operands[7]) == (INTVAL (operands[10]) - 3))"
{
  int mask;
  mask = INTVAL (operands[3]) / 4;
  mask |= (INTVAL (operands[7]) - 8) / 4 << 1;
  operands[3] = GEN_INT (mask);

  return "vshuf<shuffletype>32x4\t{%3, %2, %1, %0<mask_operand11>|%0<mask_operand11>, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "avx512f_shuf_<shuffletype>32x4_mask"
  [(match_operand:V16FI 0 "register_operand")
   (match_operand:V16FI 1 "register_operand")
   (match_operand:V16FI 2 "nonimmediate_operand")
   (match_operand:SI 3 "const_0_to_255_operand")
   (match_operand:V16FI 4 "register_operand")
   (match_operand:HI 5 "register_operand")]
  "TARGET_AVX512F"
{
  int mask = INTVAL (operands[3]);
  emit_insn (gen_avx512f_shuf_<shuffletype>32x4_1_mask
      (operands[0], operands[1], operands[2],
       GEN_INT (((mask >> 0) & 3) * 4),
       GEN_INT (((mask >> 0) & 3) * 4 + 1),
       GEN_INT (((mask >> 0) & 3) * 4 + 2),
       GEN_INT (((mask >> 0) & 3) * 4 + 3),
       GEN_INT (((mask >> 2) & 3) * 4),
       GEN_INT (((mask >> 2) & 3) * 4 + 1),
       GEN_INT (((mask >> 2) & 3) * 4 + 2),
       GEN_INT (((mask >> 2) & 3) * 4 + 3),
       GEN_INT (((mask >> 4) & 3) * 4 + 16),
       GEN_INT (((mask >> 4) & 3) * 4 + 17),
       GEN_INT (((mask >> 4) & 3) * 4 + 18),
       GEN_INT (((mask >> 4) & 3) * 4 + 19),
       GEN_INT (((mask >> 6) & 3) * 4 + 16),
       GEN_INT (((mask >> 6) & 3) * 4 + 17),
       GEN_INT (((mask >> 6) & 3) * 4 + 18),
       GEN_INT (((mask >> 6) & 3) * 4 + 19),
       operands[4], operands[5]));
  DONE;
})

(define_insn "avx512f_shuf_<shuffletype>32x4_1<mask_name>"
  [(set (match_operand:V16FI 0 "register_operand" "=v")
	(vec_select:V16FI
	  (vec_concat:<ssedoublemode>
	    (match_operand:V16FI 1 "register_operand" "v")
	    (match_operand:V16FI 2 "nonimmediate_operand" "vm"))
	  (parallel [(match_operand 3  "const_0_to_15_operand")
		     (match_operand 4  "const_0_to_15_operand")
		     (match_operand 5  "const_0_to_15_operand")
		     (match_operand 6  "const_0_to_15_operand")
		     (match_operand 7  "const_0_to_15_operand")
		     (match_operand 8  "const_0_to_15_operand")
		     (match_operand 9  "const_0_to_15_operand")
		     (match_operand 10  "const_0_to_15_operand")
		     (match_operand 11  "const_16_to_31_operand")
		     (match_operand 12  "const_16_to_31_operand")
		     (match_operand 13  "const_16_to_31_operand")
		     (match_operand 14  "const_16_to_31_operand")
		     (match_operand 15  "const_16_to_31_operand")
		     (match_operand 16  "const_16_to_31_operand")
		     (match_operand 17  "const_16_to_31_operand")
		     (match_operand 18  "const_16_to_31_operand")])))]
  "TARGET_AVX512F
   && (INTVAL (operands[3]) == (INTVAL (operands[4]) - 1)
       && INTVAL (operands[3]) == (INTVAL (operands[5]) - 2)
       && INTVAL (operands[3]) == (INTVAL (operands[6]) - 3)
       && INTVAL (operands[7]) == (INTVAL (operands[8]) - 1)
       && INTVAL (operands[7]) == (INTVAL (operands[9]) - 2)
       && INTVAL (operands[7]) == (INTVAL (operands[10]) - 3)
       && INTVAL (operands[11]) == (INTVAL (operands[12]) - 1)
       && INTVAL (operands[11]) == (INTVAL (operands[13]) - 2)
       && INTVAL (operands[11]) == (INTVAL (operands[14]) - 3)
       && INTVAL (operands[15]) == (INTVAL (operands[16]) - 1)
       && INTVAL (operands[15]) == (INTVAL (operands[17]) - 2)
       && INTVAL (operands[15]) == (INTVAL (operands[18]) - 3))"
{
  int mask;
  mask = INTVAL (operands[3]) / 4;
  mask |= INTVAL (operands[7]) / 4 << 2;
  mask |= (INTVAL (operands[11]) - 16) / 4 << 4;
  mask |= (INTVAL (operands[15]) - 16) / 4 << 6;
  operands[3] = GEN_INT (mask);

  return "vshuf<shuffletype>32x4\t{%3, %2, %1, %0<mask_operand19>|%0<mask_operand19>, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "avx512f_pshufdv3_mask"
  [(match_operand:V16SI 0 "register_operand")
   (match_operand:V16SI 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_255_operand")
   (match_operand:V16SI 3 "register_operand")
   (match_operand:HI 4 "register_operand")]
  "TARGET_AVX512F"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_avx512f_pshufd_1_mask (operands[0], operands[1],
				       GEN_INT ((mask >> 0) & 3),
				       GEN_INT ((mask >> 2) & 3),
				       GEN_INT ((mask >> 4) & 3),
				       GEN_INT ((mask >> 6) & 3),
				       GEN_INT (((mask >> 0) & 3) + 4),
				       GEN_INT (((mask >> 2) & 3) + 4),
				       GEN_INT (((mask >> 4) & 3) + 4),
				       GEN_INT (((mask >> 6) & 3) + 4),
				       GEN_INT (((mask >> 0) & 3) + 8),
				       GEN_INT (((mask >> 2) & 3) + 8),
				       GEN_INT (((mask >> 4) & 3) + 8),
				       GEN_INT (((mask >> 6) & 3) + 8),
				       GEN_INT (((mask >> 0) & 3) + 12),
				       GEN_INT (((mask >> 2) & 3) + 12),
				       GEN_INT (((mask >> 4) & 3) + 12),
				       GEN_INT (((mask >> 6) & 3) + 12),
				       operands[3], operands[4]));
  DONE;
})

(define_insn "avx512f_pshufd_1<mask_name>"
  [(set (match_operand:V16SI 0 "register_operand" "=v")
	(vec_select:V16SI
	  (match_operand:V16SI 1 "nonimmediate_operand" "vm")
	  (parallel [(match_operand 2 "const_0_to_3_operand")
		     (match_operand 3 "const_0_to_3_operand")
		     (match_operand 4 "const_0_to_3_operand")
		     (match_operand 5 "const_0_to_3_operand")
		     (match_operand 6 "const_4_to_7_operand")
		     (match_operand 7 "const_4_to_7_operand")
		     (match_operand 8 "const_4_to_7_operand")
		     (match_operand 9 "const_4_to_7_operand")
		     (match_operand 10 "const_8_to_11_operand")
		     (match_operand 11 "const_8_to_11_operand")
		     (match_operand 12 "const_8_to_11_operand")
		     (match_operand 13 "const_8_to_11_operand")
		     (match_operand 14 "const_12_to_15_operand")
		     (match_operand 15 "const_12_to_15_operand")
		     (match_operand 16 "const_12_to_15_operand")
		     (match_operand 17 "const_12_to_15_operand")])))]
  "TARGET_AVX512F
   && INTVAL (operands[2]) + 4 == INTVAL (operands[6])
   && INTVAL (operands[3]) + 4 == INTVAL (operands[7])
   && INTVAL (operands[4]) + 4 == INTVAL (operands[8])
   && INTVAL (operands[5]) + 4 == INTVAL (operands[9])
   && INTVAL (operands[2]) + 8 == INTVAL (operands[10])
   && INTVAL (operands[3]) + 8 == INTVAL (operands[11])
   && INTVAL (operands[4]) + 8 == INTVAL (operands[12])
   && INTVAL (operands[5]) + 8 == INTVAL (operands[13])
   && INTVAL (operands[2]) + 12 == INTVAL (operands[14])
   && INTVAL (operands[3]) + 12 == INTVAL (operands[15])
   && INTVAL (operands[4]) + 12 == INTVAL (operands[16])
   && INTVAL (operands[5]) + 12 == INTVAL (operands[17])"
{
  int mask = 0;
  mask |= INTVAL (operands[2]) << 0;
  mask |= INTVAL (operands[3]) << 2;
  mask |= INTVAL (operands[4]) << 4;
  mask |= INTVAL (operands[5]) << 6;
  operands[2] = GEN_INT (mask);

  return "vpshufd\t{%2, %1, %0<mask_operand18>|%0<mask_operand18>, %1, %2}";
}
  [(set_attr "type" "sselog1")
   (set_attr "prefix" "evex")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "XI")])

(define_expand "avx512vl_pshufdv3_mask"
  [(match_operand:V8SI 0 "register_operand")
   (match_operand:V8SI 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_255_operand")
   (match_operand:V8SI 3 "register_operand")
   (match_operand:QI 4 "register_operand")]
  "TARGET_AVX512VL"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_avx2_pshufd_1_mask (operands[0], operands[1],
				GEN_INT ((mask >> 0) & 3),
				GEN_INT ((mask >> 2) & 3),
				GEN_INT ((mask >> 4) & 3),
				GEN_INT ((mask >> 6) & 3),
				GEN_INT (((mask >> 0) & 3) + 4),
				GEN_INT (((mask >> 2) & 3) + 4),
				GEN_INT (((mask >> 4) & 3) + 4),
				GEN_INT (((mask >> 6) & 3) + 4),
                operands[3], operands[4]));
  DONE;
})

(define_expand "avx2_pshufdv3"
  [(match_operand:V8SI 0 "register_operand")
   (match_operand:V8SI 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_255_operand")]
  "TARGET_AVX2"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_avx2_pshufd_1 (operands[0], operands[1],
				GEN_INT ((mask >> 0) & 3),
				GEN_INT ((mask >> 2) & 3),
				GEN_INT ((mask >> 4) & 3),
				GEN_INT ((mask >> 6) & 3),
				GEN_INT (((mask >> 0) & 3) + 4),
				GEN_INT (((mask >> 2) & 3) + 4),
				GEN_INT (((mask >> 4) & 3) + 4),
				GEN_INT (((mask >> 6) & 3) + 4)));
  DONE;
})

(define_insn "avx2_pshufd_1<mask_name>"
  [(set (match_operand:V8SI 0 "register_operand" "=v")
	(vec_select:V8SI
	  (match_operand:V8SI 1 "nonimmediate_operand" "vm")
	  (parallel [(match_operand 2 "const_0_to_3_operand")
		     (match_operand 3 "const_0_to_3_operand")
		     (match_operand 4 "const_0_to_3_operand")
		     (match_operand 5 "const_0_to_3_operand")
		     (match_operand 6 "const_4_to_7_operand")
		     (match_operand 7 "const_4_to_7_operand")
		     (match_operand 8 "const_4_to_7_operand")
		     (match_operand 9 "const_4_to_7_operand")])))]
  "TARGET_AVX2
   && <mask_avx512vl_condition>
   && INTVAL (operands[2]) + 4 == INTVAL (operands[6])
   && INTVAL (operands[3]) + 4 == INTVAL (operands[7])
   && INTVAL (operands[4]) + 4 == INTVAL (operands[8])
   && INTVAL (operands[5]) + 4 == INTVAL (operands[9])"
{
  int mask = 0;
  mask |= INTVAL (operands[2]) << 0;
  mask |= INTVAL (operands[3]) << 2;
  mask |= INTVAL (operands[4]) << 4;
  mask |= INTVAL (operands[5]) << 6;
  operands[2] = GEN_INT (mask);

  return "vpshufd\t{%2, %1, %0<mask_operand10>|%0<mask_operand10>, %1, %2}";
}
  [(set_attr "type" "sselog1")
   (set_attr "prefix" "maybe_evex")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "OI")])

(define_expand "avx512vl_pshufd_mask"
  [(match_operand:V4SI 0 "register_operand")
   (match_operand:V4SI 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_255_operand")
   (match_operand:V4SI 3 "register_operand")
   (match_operand:QI 4 "register_operand")]
  "TARGET_AVX512VL"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_sse2_pshufd_1_mask (operands[0], operands[1],
				GEN_INT ((mask >> 0) & 3),
				GEN_INT ((mask >> 2) & 3),
				GEN_INT ((mask >> 4) & 3),
				GEN_INT ((mask >> 6) & 3),
                operands[3], operands[4]));
  DONE;
})

(define_expand "sse2_pshufd"
  [(match_operand:V4SI 0 "register_operand")
   (match_operand:V4SI 1 "vector_operand")
   (match_operand:SI 2 "const_int_operand")]
  "TARGET_SSE2"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_sse2_pshufd_1 (operands[0], operands[1],
				GEN_INT ((mask >> 0) & 3),
				GEN_INT ((mask >> 2) & 3),
				GEN_INT ((mask >> 4) & 3),
				GEN_INT ((mask >> 6) & 3)));
  DONE;
})

(define_insn "sse2_pshufd_1<mask_name>"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(vec_select:V4SI
	  (match_operand:V4SI 1 "vector_operand" "vBm")
	  (parallel [(match_operand 2 "const_0_to_3_operand")
		     (match_operand 3 "const_0_to_3_operand")
		     (match_operand 4 "const_0_to_3_operand")
		     (match_operand 5 "const_0_to_3_operand")])))]
  "TARGET_SSE2 && <mask_avx512vl_condition>"
{
  int mask = 0;
  mask |= INTVAL (operands[2]) << 0;
  mask |= INTVAL (operands[3]) << 2;
  mask |= INTVAL (operands[4]) << 4;
  mask |= INTVAL (operands[5]) << 6;
  operands[2] = GEN_INT (mask);

  return "%vpshufd\t{%2, %1, %0<mask_operand6>|%0<mask_operand6>, %1, %2}";
}
  [(set_attr "type" "sselog1")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix" "<mask_prefix2>")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "<mask_codefor>avx512bw_pshuflwv32hi<mask_name>"
  [(set (match_operand:V32HI 0 "register_operand" "=v")
	(unspec:V32HI
	  [(match_operand:V32HI 1 "nonimmediate_operand" "vm")
	   (match_operand:SI 2 "const_0_to_255_operand" "n")]
	  UNSPEC_PSHUFLW))]
  "TARGET_AVX512BW"
  "vpshuflw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_expand "avx512vl_pshuflwv3_mask"
  [(match_operand:V16HI 0 "register_operand")
   (match_operand:V16HI 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_255_operand")
   (match_operand:V16HI 3 "register_operand")
   (match_operand:HI 4 "register_operand")]
  "TARGET_AVX512VL && TARGET_AVX512BW"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_avx2_pshuflw_1_mask (operands[0], operands[1],
				 GEN_INT ((mask >> 0) & 3),
				 GEN_INT ((mask >> 2) & 3),
				 GEN_INT ((mask >> 4) & 3),
				 GEN_INT ((mask >> 6) & 3),
				 GEN_INT (((mask >> 0) & 3) + 8),
				 GEN_INT (((mask >> 2) & 3) + 8),
				 GEN_INT (((mask >> 4) & 3) + 8),
				 GEN_INT (((mask >> 6) & 3) + 8),
                 operands[3], operands[4]));
  DONE;
})

(define_expand "avx2_pshuflwv3"
  [(match_operand:V16HI 0 "register_operand")
   (match_operand:V16HI 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_255_operand")]
  "TARGET_AVX2"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_avx2_pshuflw_1 (operands[0], operands[1],
				 GEN_INT ((mask >> 0) & 3),
				 GEN_INT ((mask >> 2) & 3),
				 GEN_INT ((mask >> 4) & 3),
				 GEN_INT ((mask >> 6) & 3),
				 GEN_INT (((mask >> 0) & 3) + 8),
				 GEN_INT (((mask >> 2) & 3) + 8),
				 GEN_INT (((mask >> 4) & 3) + 8),
				 GEN_INT (((mask >> 6) & 3) + 8)));
  DONE;
})

(define_insn "avx2_pshuflw_1<mask_name>"
  [(set (match_operand:V16HI 0 "register_operand" "=v")
	(vec_select:V16HI
	  (match_operand:V16HI 1 "nonimmediate_operand" "vm")
	  (parallel [(match_operand 2 "const_0_to_3_operand")
		     (match_operand 3 "const_0_to_3_operand")
		     (match_operand 4 "const_0_to_3_operand")
		     (match_operand 5 "const_0_to_3_operand")
		     (const_int 4)
		     (const_int 5)
		     (const_int 6)
		     (const_int 7)
		     (match_operand 6 "const_8_to_11_operand")
		     (match_operand 7 "const_8_to_11_operand")
		     (match_operand 8 "const_8_to_11_operand")
		     (match_operand 9 "const_8_to_11_operand")
		     (const_int 12)
		     (const_int 13)
		     (const_int 14)
		     (const_int 15)])))]
  "TARGET_AVX2
   && <mask_avx512bw_condition> && <mask_avx512vl_condition>
   && INTVAL (operands[2]) + 8 == INTVAL (operands[6])
   && INTVAL (operands[3]) + 8 == INTVAL (operands[7])
   && INTVAL (operands[4]) + 8 == INTVAL (operands[8])
   && INTVAL (operands[5]) + 8 == INTVAL (operands[9])"
{
  int mask = 0;
  mask |= INTVAL (operands[2]) << 0;
  mask |= INTVAL (operands[3]) << 2;
  mask |= INTVAL (operands[4]) << 4;
  mask |= INTVAL (operands[5]) << 6;
  operands[2] = GEN_INT (mask);

  return "vpshuflw\t{%2, %1, %0<mask_operand10>|%0<mask_operand10>, %1, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix" "maybe_evex")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "OI")])

(define_expand "avx512vl_pshuflw_mask"
  [(match_operand:V8HI 0 "register_operand")
   (match_operand:V8HI 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_255_operand")
   (match_operand:V8HI 3 "register_operand")
   (match_operand:QI 4 "register_operand")]
  "TARGET_AVX512VL && TARGET_AVX512BW"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_sse2_pshuflw_1_mask (operands[0], operands[1],
				 GEN_INT ((mask >> 0) & 3),
				 GEN_INT ((mask >> 2) & 3),
				 GEN_INT ((mask >> 4) & 3),
				 GEN_INT ((mask >> 6) & 3),
                 operands[3], operands[4]));
  DONE;
})

(define_expand "sse2_pshuflw"
  [(match_operand:V8HI 0 "register_operand")
   (match_operand:V8HI 1 "vector_operand")
   (match_operand:SI 2 "const_int_operand")]
  "TARGET_SSE2"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_sse2_pshuflw_1 (operands[0], operands[1],
				 GEN_INT ((mask >> 0) & 3),
				 GEN_INT ((mask >> 2) & 3),
				 GEN_INT ((mask >> 4) & 3),
				 GEN_INT ((mask >> 6) & 3)));
  DONE;
})

(define_insn "sse2_pshuflw_1<mask_name>"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
	(vec_select:V8HI
	  (match_operand:V8HI 1 "vector_operand" "vBm")
	  (parallel [(match_operand 2 "const_0_to_3_operand")
		     (match_operand 3 "const_0_to_3_operand")
		     (match_operand 4 "const_0_to_3_operand")
		     (match_operand 5 "const_0_to_3_operand")
		     (const_int 4)
		     (const_int 5)
		     (const_int 6)
		     (const_int 7)])))]
  "TARGET_SSE2 && <mask_avx512bw_condition> && <mask_avx512vl_condition>"
{
  int mask = 0;
  mask |= INTVAL (operands[2]) << 0;
  mask |= INTVAL (operands[3]) << 2;
  mask |= INTVAL (operands[4]) << 4;
  mask |= INTVAL (operands[5]) << 6;
  operands[2] = GEN_INT (mask);

  return "%vpshuflw\t{%2, %1, %0<mask_operand6>|%0<mask_operand6>, %1, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_expand "avx2_pshufhwv3"
  [(match_operand:V16HI 0 "register_operand")
   (match_operand:V16HI 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_255_operand")]
  "TARGET_AVX2"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_avx2_pshufhw_1 (operands[0], operands[1],
				 GEN_INT (((mask >> 0) & 3) + 4),
				 GEN_INT (((mask >> 2) & 3) + 4),
				 GEN_INT (((mask >> 4) & 3) + 4),
				 GEN_INT (((mask >> 6) & 3) + 4),
				 GEN_INT (((mask >> 0) & 3) + 12),
				 GEN_INT (((mask >> 2) & 3) + 12),
				 GEN_INT (((mask >> 4) & 3) + 12),
				 GEN_INT (((mask >> 6) & 3) + 12)));
  DONE;
})

(define_insn "<mask_codefor>avx512bw_pshufhwv32hi<mask_name>"
  [(set (match_operand:V32HI 0 "register_operand" "=v")
	(unspec:V32HI
	  [(match_operand:V32HI 1 "nonimmediate_operand" "vm")
	   (match_operand:SI 2 "const_0_to_255_operand" "n")]
	  UNSPEC_PSHUFHW))]
  "TARGET_AVX512BW"
  "vpshufhw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_expand "avx512vl_pshufhwv3_mask"
  [(match_operand:V16HI 0 "register_operand")
   (match_operand:V16HI 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_255_operand")
   (match_operand:V16HI 3 "register_operand")
   (match_operand:HI 4 "register_operand")]
  "TARGET_AVX512VL && TARGET_AVX512BW"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_avx2_pshufhw_1_mask (operands[0], operands[1],
				 GEN_INT (((mask >> 0) & 3) + 4),
				 GEN_INT (((mask >> 2) & 3) + 4),
				 GEN_INT (((mask >> 4) & 3) + 4),
				 GEN_INT (((mask >> 6) & 3) + 4),
				 GEN_INT (((mask >> 0) & 3) + 12),
				 GEN_INT (((mask >> 2) & 3) + 12),
				 GEN_INT (((mask >> 4) & 3) + 12),
				 GEN_INT (((mask >> 6) & 3) + 12),
                 operands[3], operands[4]));
  DONE;
})

(define_insn "avx2_pshufhw_1<mask_name>"
  [(set (match_operand:V16HI 0 "register_operand" "=v")
	(vec_select:V16HI
	  (match_operand:V16HI 1 "nonimmediate_operand" "vm")
	  (parallel [(const_int 0)
		     (const_int 1)
		     (const_int 2)
		     (const_int 3)
		     (match_operand 2 "const_4_to_7_operand")
		     (match_operand 3 "const_4_to_7_operand")
		     (match_operand 4 "const_4_to_7_operand")
		     (match_operand 5 "const_4_to_7_operand")
		     (const_int 8)
		     (const_int 9)
		     (const_int 10)
		     (const_int 11)
		     (match_operand 6 "const_12_to_15_operand")
		     (match_operand 7 "const_12_to_15_operand")
		     (match_operand 8 "const_12_to_15_operand")
		     (match_operand 9 "const_12_to_15_operand")])))]
  "TARGET_AVX2
   && <mask_avx512bw_condition> && <mask_avx512vl_condition>
   && INTVAL (operands[2]) + 8 == INTVAL (operands[6])
   && INTVAL (operands[3]) + 8 == INTVAL (operands[7])
   && INTVAL (operands[4]) + 8 == INTVAL (operands[8])
   && INTVAL (operands[5]) + 8 == INTVAL (operands[9])"
{
  int mask = 0;
  mask |= (INTVAL (operands[2]) - 4) << 0;
  mask |= (INTVAL (operands[3]) - 4) << 2;
  mask |= (INTVAL (operands[4]) - 4) << 4;
  mask |= (INTVAL (operands[5]) - 4) << 6;
  operands[2] = GEN_INT (mask);

  return "vpshufhw\t{%2, %1, %0<mask_operand10>|%0<mask_operand10>, %1, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix" "maybe_evex")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "OI")])

(define_expand "avx512vl_pshufhw_mask"
  [(match_operand:V8HI 0 "register_operand")
   (match_operand:V8HI 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_255_operand")
   (match_operand:V8HI 3 "register_operand")
   (match_operand:QI 4 "register_operand")]
  "TARGET_AVX512VL && TARGET_AVX512BW"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_sse2_pshufhw_1_mask (operands[0], operands[1],
				 GEN_INT (((mask >> 0) & 3) + 4),
				 GEN_INT (((mask >> 2) & 3) + 4),
				 GEN_INT (((mask >> 4) & 3) + 4),
				 GEN_INT (((mask >> 6) & 3) + 4),
                 operands[3], operands[4]));
  DONE;
})

(define_expand "sse2_pshufhw"
  [(match_operand:V8HI 0 "register_operand")
   (match_operand:V8HI 1 "vector_operand")
   (match_operand:SI 2 "const_int_operand")]
  "TARGET_SSE2"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_sse2_pshufhw_1 (operands[0], operands[1],
				 GEN_INT (((mask >> 0) & 3) + 4),
				 GEN_INT (((mask >> 2) & 3) + 4),
				 GEN_INT (((mask >> 4) & 3) + 4),
				 GEN_INT (((mask >> 6) & 3) + 4)));
  DONE;
})

(define_insn "sse2_pshufhw_1<mask_name>"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
	(vec_select:V8HI
	  (match_operand:V8HI 1 "vector_operand" "vBm")
	  (parallel [(const_int 0)
		     (const_int 1)
		     (const_int 2)
		     (const_int 3)
		     (match_operand 2 "const_4_to_7_operand")
		     (match_operand 3 "const_4_to_7_operand")
		     (match_operand 4 "const_4_to_7_operand")
		     (match_operand 5 "const_4_to_7_operand")])))]
  "TARGET_SSE2 && <mask_avx512bw_condition> && <mask_avx512vl_condition>"
{
  int mask = 0;
  mask |= (INTVAL (operands[2]) - 4) << 0;
  mask |= (INTVAL (operands[3]) - 4) << 2;
  mask |= (INTVAL (operands[4]) - 4) << 4;
  mask |= (INTVAL (operands[5]) - 4) << 6;
  operands[2] = GEN_INT (mask);

  return "%vpshufhw\t{%2, %1, %0<mask_operand6>|%0<mask_operand6>, %1, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix" "maybe_vex")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_expand "sse2_loadd"
  [(set (match_operand:V4SI 0 "register_operand")
	(vec_merge:V4SI
	  (vec_duplicate:V4SI
	    (match_operand:SI 1 "nonimmediate_operand"))
	  (match_dup 2)
	  (const_int 1)))]
  "TARGET_SSE"
  "operands[2] = CONST0_RTX (V4SImode);")

(define_insn "sse2_loadld"
  [(set (match_operand:V4SI 0 "register_operand"       "=v,v,x,x,v")
	(vec_merge:V4SI
	  (vec_duplicate:V4SI
	    (match_operand:SI 2 "nonimmediate_operand" "m ,r ,m,x,v"))
	  (match_operand:V4SI 1 "reg_or_0_operand"     "C ,C ,C,0,v")
	  (const_int 1)))]
  "TARGET_SSE"
  "@
   %vmovd\t{%2, %0|%0, %2}
   %vmovd\t{%2, %0|%0, %2}
   movss\t{%2, %0|%0, %2}
   movss\t{%2, %0|%0, %2}
   vmovss\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "sse2,sse2,noavx,noavx,avx")
   (set_attr "type" "ssemov")
   (set_attr "prefix" "maybe_vex,maybe_vex,orig,orig,maybe_evex")
   (set_attr "mode" "TI,TI,V4SF,SF,SF")
   (set (attr "preferred_for_speed")
     (cond [(eq_attr "alternative" "1")
	      (symbol_ref "TARGET_INTER_UNIT_MOVES_TO_VEC")
	   ]
	   (symbol_ref "true")))])

;; QI and HI modes handled by pextr patterns.
(define_mode_iterator PEXTR_MODE12
  [(V16QI "TARGET_SSE4_1") V8HI])

(define_insn "*vec_extract<mode>"
  [(set (match_operand:<ssescalarmode> 0 "register_sse4nonimm_operand" "=r,m,r,m")
	(vec_select:<ssescalarmode>
	  (match_operand:PEXTR_MODE12 1 "register_operand" "x,x,v,v")
	  (parallel
	    [(match_operand:SI 2 "const_0_to_<ssescalarnummask>_operand")])))]
  "TARGET_SSE2"
  "@
   %vpextr<ssemodesuffix>\t{%2, %1, %k0|%k0, %1, %2}
   %vpextr<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}
   vpextr<ssemodesuffix>\t{%2, %1, %k0|%k0, %1, %2}
   vpextr<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "*,sse4,avx512bw,avx512bw")
   (set_attr "type" "sselog1")
   (set_attr "prefix_data16" "1")
   (set (attr "prefix_extra")
     (if_then_else
       (and (eq_attr "alternative" "0,2")
	    (eq (const_string "<MODE>mode") (const_string "V8HImode")))
       (const_string "*")
       (const_string "1")))
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex,maybe_vex,evex,evex")
   (set_attr "mode" "TI")])

(define_insn "*vec_extract<PEXTR_MODE12:mode>_zext"
  [(set (match_operand:SWI48 0 "register_operand" "=r,r")
	(zero_extend:SWI48
	  (vec_select:<PEXTR_MODE12:ssescalarmode>
	    (match_operand:PEXTR_MODE12 1 "register_operand" "x,v")
	    (parallel
	      [(match_operand:SI 2
		"const_0_to_<PEXTR_MODE12:ssescalarnummask>_operand")]))))]
  "TARGET_SSE2"
  "@
   %vpextr<PEXTR_MODE12:ssemodesuffix>\t{%2, %1, %k0|%k0, %1, %2}
   vpextr<PEXTR_MODE12:ssemodesuffix>\t{%2, %1, %k0|%k0, %1, %2}"
  [(set_attr "isa" "*,avx512bw")
   (set_attr "type" "sselog1")
   (set_attr "prefix_data16" "1")
   (set (attr "prefix_extra")
     (if_then_else
       (eq (const_string "<PEXTR_MODE12:MODE>mode") (const_string "V8HImode"))
       (const_string "*")
       (const_string "1")))
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "*vec_extract<mode>_mem"
  [(set (match_operand:<ssescalarmode> 0 "register_operand" "=r")
	(vec_select:<ssescalarmode>
	  (match_operand:VI12_128 1 "memory_operand" "o")
	  (parallel
	    [(match_operand 2 "const_0_to_<ssescalarnummask>_operand")])))]
  "TARGET_SSE"
  "#")

(define_insn "*vec_extract<ssevecmodelower>_0"
  [(set (match_operand:SWI48 0 "nonimmediate_operand"	       "=r,r,v ,m")
	(vec_select:SWI48
	  (match_operand:<ssevecmode> 1 "nonimmediate_operand" "m ,v,vm,v")
	  (parallel [(const_int 0)])))]
  "TARGET_SSE && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "#"
  [(set_attr "isa" "*,sse2,*,*")
   (set (attr "preferred_for_speed")
     (cond [(eq_attr "alternative" "1")
	      (symbol_ref "TARGET_INTER_UNIT_MOVES_FROM_VEC")
	   ]
	   (symbol_ref "true")))])

(define_insn "*vec_extractv2di_0_sse"
  [(set (match_operand:DI 0 "nonimmediate_operand"     "=v,m")
	(vec_select:DI
	  (match_operand:V2DI 1 "nonimmediate_operand" "vm,v")
	  (parallel [(const_int 0)])))]
  "TARGET_SSE && !TARGET_64BIT
   && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "#")

(define_split
  [(set (match_operand:SWI48x 0 "nonimmediate_operand")
	(vec_select:SWI48x
	  (match_operand:<ssevecmode> 1 "register_operand")
	  (parallel [(const_int 0)])))]
  "TARGET_SSE && reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  "operands[1] = gen_lowpart (<MODE>mode, operands[1]);")

(define_insn "*vec_extractv4si_0_zext_sse4"
  [(set (match_operand:DI 0 "register_operand" "=r,x,v")
	(zero_extend:DI
	  (vec_select:SI
	    (match_operand:V4SI 1 "register_operand" "v,x,v")
	    (parallel [(const_int 0)]))))]
  "TARGET_SSE4_1"
  "#"
  [(set_attr "isa" "x64,*,avx512f")
   (set (attr "preferred_for_speed")
     (cond [(eq_attr "alternative" "0")
	      (symbol_ref "TARGET_INTER_UNIT_MOVES_FROM_VEC")
	   ]
	   (symbol_ref "true")))])

(define_insn "*vec_extractv4si_0_zext"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (vec_select:SI
	    (match_operand:V4SI 1 "register_operand" "x")
	    (parallel [(const_int 0)]))))]
  "TARGET_64BIT && TARGET_SSE2 && TARGET_INTER_UNIT_MOVES_FROM_VEC"
  "#")

(define_split
  [(set (match_operand:DI 0 "register_operand")
	(zero_extend:DI
	  (vec_select:SI
	    (match_operand:V4SI 1 "register_operand")
	    (parallel [(const_int 0)]))))]
  "TARGET_SSE2 && reload_completed"
  [(set (match_dup 0) (zero_extend:DI (match_dup 1)))]
  "operands[1] = gen_lowpart (SImode, operands[1]);")

(define_insn "*vec_extractv4si"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=rm,rm,Yr,*x,x,Yv")
	(vec_select:SI
	  (match_operand:V4SI 1 "register_operand" "x,v,0,0,x,v")
	  (parallel [(match_operand:SI 2 "const_0_to_3_operand")])))]
  "TARGET_SSE4_1"
{
  switch (which_alternative)
    {
    case 0:
    case 1:
      return "%vpextrd\t{%2, %1, %0|%0, %1, %2}";

    case 2:
    case 3:
      operands[2] = GEN_INT (INTVAL (operands[2]) * 4);
      return "psrldq\t{%2, %0|%0, %2}";

    case 4:
    case 5:
      operands[2] = GEN_INT (INTVAL (operands[2]) * 4);
      return "vpsrldq\t{%2, %1, %0|%0, %1, %2}";

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "isa" "*,avx512dq,noavx,noavx,avx,avx512bw")
   (set_attr "type" "sselog1,sselog1,sseishft1,sseishft1,sseishft1,sseishft1")
   (set (attr "prefix_extra")
     (if_then_else (eq_attr "alternative" "0,1")
		   (const_string "1")
		   (const_string "*")))
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex,evex,orig,orig,vex,evex")
   (set_attr "mode" "TI")])

(define_insn "*vec_extractv4si_zext"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI
	  (vec_select:SI
	    (match_operand:V4SI 1 "register_operand" "x,v")
	    (parallel [(match_operand:SI 2 "const_0_to_3_operand")]))))]
  "TARGET_64BIT && TARGET_SSE4_1"
  "%vpextrd\t{%2, %1, %k0|%k0, %1, %2}"
  [(set_attr "isa" "*,avx512dq")
   (set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "*vec_extractv4si_mem"
  [(set (match_operand:SI 0 "register_operand" "=x,r")
	(vec_select:SI
	  (match_operand:V4SI 1 "memory_operand" "o,o")
	  (parallel [(match_operand 2 "const_0_to_3_operand")])))]
  "TARGET_SSE"
  "#")

(define_insn_and_split "*vec_extractv4si_zext_mem"
  [(set (match_operand:DI 0 "register_operand" "=x,r")
	(zero_extend:DI
	  (vec_select:SI
	    (match_operand:V4SI 1 "memory_operand" "o,o")
	    (parallel [(match_operand:SI 2 "const_0_to_3_operand")]))))]
  "TARGET_64BIT && TARGET_SSE"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (zero_extend:DI (match_dup 1)))]
{
  operands[1] = adjust_address (operands[1], SImode, INTVAL (operands[2]) * 4);
})

(define_insn "*vec_extractv2di_1"
  [(set (match_operand:DI 0 "nonimmediate_operand"     "=rm,rm,m,x,x,Yv,x,v,r")
	(vec_select:DI
	  (match_operand:V2DI 1 "nonimmediate_operand"  "x ,v ,v,0,x, v,x,o,o")
	  (parallel [(const_int 1)])))]
  "TARGET_SSE && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   %vpextrq\t{$1, %1, %0|%0, %1, 1}
   vpextrq\t{$1, %1, %0|%0, %1, 1}
   %vmovhps\t{%1, %0|%0, %1}
   psrldq\t{$8, %0|%0, 8}
   vpsrldq\t{$8, %1, %0|%0, %1, 8}
   vpsrldq\t{$8, %1, %0|%0, %1, 8}
   movhlps\t{%1, %0|%0, %1}
   #
   #"
  [(set (attr "isa")
     (cond [(eq_attr "alternative" "0")
	      (const_string "x64_sse4")
	    (eq_attr "alternative" "1")
	      (const_string "x64_avx512dq")
	    (eq_attr "alternative" "3")
	      (const_string "sse2_noavx")
	    (eq_attr "alternative" "4")
	      (const_string "avx")
	    (eq_attr "alternative" "5")
	      (const_string "avx512bw")
	    (eq_attr "alternative" "6")
	      (const_string "noavx")
	    (eq_attr "alternative" "8")
	      (const_string "x64")
	   ]
	   (const_string "*")))
   (set (attr "type")
     (cond [(eq_attr "alternative" "2,6,7")
	      (const_string "ssemov")
	    (eq_attr "alternative" "3,4,5")
	      (const_string "sseishft1")
	    (eq_attr "alternative" "8")
	      (const_string "imov")
	   ]
	   (const_string "sselog1")))
   (set (attr "length_immediate")
     (if_then_else (eq_attr "alternative" "0,1,3,4,5")
		   (const_string "1")
		   (const_string "*")))
   (set (attr "prefix_rex")
     (if_then_else (eq_attr "alternative" "0,1")
		   (const_string "1")
		   (const_string "*")))
   (set (attr "prefix_extra")
     (if_then_else (eq_attr "alternative" "0,1")
		   (const_string "1")
		   (const_string "*")))
   (set_attr "prefix" "maybe_vex,evex,maybe_vex,orig,vex,evex,orig,*,*")
   (set_attr "mode" "TI,TI,V2SF,TI,TI,TI,V4SF,DI,DI")])

(define_split
  [(set (match_operand:<ssescalarmode> 0 "register_operand")
	(vec_select:<ssescalarmode>
	  (match_operand:VI_128 1 "memory_operand")
	  (parallel
	    [(match_operand 2 "const_0_to_<ssescalarnummask>_operand")])))]
  "TARGET_SSE && reload_completed"
  [(set (match_dup 0) (match_dup 1))]
{
  int offs = INTVAL (operands[2]) * GET_MODE_SIZE (<ssescalarmode>mode);

  operands[1] = adjust_address (operands[1], <ssescalarmode>mode, offs);
})

(define_insn "*vec_extractv2ti"
  [(set (match_operand:TI 0 "nonimmediate_operand" "=xm,vm")
	(vec_select:TI
	  (match_operand:V2TI 1 "register_operand" "x,v")
	  (parallel
	    [(match_operand:SI 2 "const_0_to_1_operand")])))]
  "TARGET_AVX"
  "@
   vextract%~128\t{%2, %1, %0|%0, %1, %2}
   vextracti32x4\t{%2, %g1, %0|%0, %g1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex,evex")
   (set_attr "mode" "OI")])

(define_insn "*vec_extractv4ti"
  [(set (match_operand:TI 0 "nonimmediate_operand" "=vm")
	(vec_select:TI
	  (match_operand:V4TI 1 "register_operand" "v")
	  (parallel
	    [(match_operand:SI 2 "const_0_to_3_operand")])))]
  "TARGET_AVX512F"
  "vextracti32x4\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_mode_iterator VEXTRACTI128_MODE
  [(V4TI "TARGET_AVX512F") V2TI])

(define_split
  [(set (match_operand:TI 0 "nonimmediate_operand")
	(vec_select:TI
	  (match_operand:VEXTRACTI128_MODE 1 "register_operand")
	  (parallel [(const_int 0)])))]
  "TARGET_AVX
   && reload_completed
   && (TARGET_AVX512VL || !EXT_REX_SSE_REG_P (operands[1]))"
  [(set (match_dup 0) (match_dup 1))]
  "operands[1] = gen_lowpart (TImode, operands[1]);")

;; Turn SImode or DImode extraction from arbitrary SSE/AVX/AVX512F
;; vector modes into vec_extract*.
(define_split
  [(set (match_operand:SWI48x 0 "nonimmediate_operand")
	(subreg:SWI48x (match_operand 1 "register_operand") 0))]
  "can_create_pseudo_p ()
   && REG_P (operands[1])
   && VECTOR_MODE_P (GET_MODE (operands[1]))
   && ((TARGET_SSE && GET_MODE_SIZE (GET_MODE (operands[1])) == 16)
       || (TARGET_AVX && GET_MODE_SIZE (GET_MODE (operands[1])) == 32)
       || (TARGET_AVX512F && GET_MODE_SIZE (GET_MODE (operands[1])) == 64))
   && (<MODE>mode == SImode || TARGET_64BIT || MEM_P (operands[0]))"
  [(set (match_dup 0) (vec_select:SWI48x (match_dup 1)
					 (parallel [(const_int 0)])))]
{
  rtx tmp;

  switch (GET_MODE_SIZE (GET_MODE (operands[1])))
    {
    case 64:
      if (<MODE>mode == SImode)
	{
	  tmp = gen_reg_rtx (V8SImode);
	  emit_insn (gen_vec_extract_lo_v16si (tmp,
					       gen_lowpart (V16SImode,
							    operands[1])));
	}
      else
	{
	  tmp = gen_reg_rtx (V4DImode);
	  emit_insn (gen_vec_extract_lo_v8di (tmp,
					      gen_lowpart (V8DImode,
							   operands[1])));
	}
      operands[1] = tmp;
      /* FALLTHRU */
    case 32:
      tmp = gen_reg_rtx (<ssevecmode>mode);
      if (<MODE>mode == SImode)
	emit_insn (gen_vec_extract_lo_v8si (tmp, gen_lowpart (V8SImode,
							      operands[1])));
      else
	emit_insn (gen_vec_extract_lo_v4di (tmp, gen_lowpart (V4DImode,
							      operands[1])));
      operands[1] = tmp;
      break;
    case 16:
      operands[1] = gen_lowpart (<ssevecmode>mode, operands[1]);
      break;
    }
})

(define_insn "*vec_concatv2si_sse4_1"
  [(set (match_operand:V2SI 0 "register_operand"
	  "=Yr,*x, x, v,Yr,*x, v, v, *y,*y")
	(vec_concat:V2SI
	  (match_operand:SI 1 "nonimmediate_operand"
	  "  0, 0, x,Yv, 0, 0,Yv,rm,  0,rm")
	  (match_operand:SI 2 "nonimm_or_0_operand"
	  " rm,rm,rm,rm,Yr,*x,Yv, C,*ym, C")))]
  "TARGET_SSE4_1 && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   pinsrd\t{$1, %2, %0|%0, %2, 1}
   pinsrd\t{$1, %2, %0|%0, %2, 1}
   vpinsrd\t{$1, %2, %1, %0|%0, %1, %2, 1}
   vpinsrd\t{$1, %2, %1, %0|%0, %1, %2, 1}
   punpckldq\t{%2, %0|%0, %2}
   punpckldq\t{%2, %0|%0, %2}
   vpunpckldq\t{%2, %1, %0|%0, %1, %2}
   %vmovd\t{%1, %0|%0, %1}
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}"
  [(set_attr "isa" "noavx,noavx,avx,avx512dq,noavx,noavx,avx,*,*,*")
   (set (attr "type")
     (cond [(eq_attr "alternative" "7")
	      (const_string "ssemov")
	    (eq_attr "alternative" "8")
	      (const_string "mmxcvt")
	    (eq_attr "alternative" "9")
	      (const_string "mmxmov")
	   ]
	   (const_string "sselog")))
   (set (attr "prefix_extra")
     (if_then_else (eq_attr "alternative" "0,1,2,3")
		   (const_string "1")
		   (const_string "*")))
   (set (attr "length_immediate")
     (if_then_else (eq_attr "alternative" "0,1,2,3")
		   (const_string "1")
		   (const_string "*")))
   (set_attr "prefix" "orig,orig,vex,evex,orig,orig,maybe_evex,maybe_vex,orig,orig")
   (set_attr "mode" "TI,TI,TI,TI,TI,TI,TI,TI,DI,DI")])

;; ??? In theory we can match memory for the MMX alternative, but allowing
;; nonimmediate_operand for operand 2 and *not* allowing memory for the SSE
;; alternatives pretty much forces the MMX alternative to be chosen.
(define_insn "*vec_concatv2si"
  [(set (match_operand:V2SI 0 "register_operand"     "=x,x ,*y,x,x,*y,*y")
	(vec_concat:V2SI
	  (match_operand:SI 1 "nonimmediate_operand" " 0,rm,rm,0,m, 0,*rm")
	  (match_operand:SI 2 "reg_or_0_operand"     " x,C ,C, x,C,*y,C")))]
  "TARGET_SSE && !TARGET_SSE4_1"
  "@
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}
   movd\t{%1, %0|%0, %1}
   unpcklps\t{%2, %0|%0, %2}
   movss\t{%1, %0|%0, %1}
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}"
  [(set_attr "isa" "sse2,sse2,sse2,*,*,*,*")
   (set_attr "type" "sselog,ssemov,mmxmov,sselog,ssemov,mmxcvt,mmxmov")
   (set_attr "mode" "TI,TI,DI,V4SF,SF,DI,DI")])

(define_insn "*vec_concatv4si"
  [(set (match_operand:V4SI 0 "register_operand"       "=x,v,x,x,v")
	(vec_concat:V4SI
	  (match_operand:V2SI 1 "register_operand"     " 0,v,0,0,v")
	  (match_operand:V2SI 2 "nonimmediate_operand" " x,v,x,m,m")))]
  "TARGET_SSE"
  "@
   punpcklqdq\t{%2, %0|%0, %2}
   vpunpcklqdq\t{%2, %1, %0|%0, %1, %2}
   movlhps\t{%2, %0|%0, %2}
   movhps\t{%2, %0|%0, %q2}
   vmovhps\t{%2, %1, %0|%0, %1, %q2}"
  [(set_attr "isa" "sse2_noavx,avx,noavx,noavx,avx")
   (set_attr "type" "sselog,sselog,ssemov,ssemov,ssemov")
   (set_attr "prefix" "orig,maybe_evex,orig,orig,maybe_evex")
   (set_attr "mode" "TI,TI,V4SF,V2SF,V2SF")])

;; movd instead of movq is required to handle broken assemblers.
(define_insn "vec_concatv2di"
  [(set (match_operand:V2DI 0 "register_operand"
	  "=Yr,*x,x ,v ,v,v ,x   ,x,v ,x,x,v")
	(vec_concat:V2DI
	  (match_operand:DI 1 "nonimmediate_operand"
	  "  0, 0,x ,Yv,r,vm,?!*y,0,Yv,0,0,v")
	  (match_operand:DI 2 "nonimm_or_0_operand"
	  " rm,rm,rm,rm,C ,C ,C ,x,Yv,x,m,m")))]
  "TARGET_SSE"
  "@
   pinsrq\t{$1, %2, %0|%0, %2, 1}
   pinsrq\t{$1, %2, %0|%0, %2, 1}
   vpinsrq\t{$1, %2, %1, %0|%0, %1, %2, 1}
   vpinsrq\t{$1, %2, %1, %0|%0, %1, %2, 1}
   * return HAVE_AS_IX86_INTERUNIT_MOVQ ? \"%vmovq\t{%1, %0|%0, %1}\" : \"%vmovd\t{%1, %0|%0, %1}\";
   %vmovq\t{%1, %0|%0, %1}
   movq2dq\t{%1, %0|%0, %1}
   punpcklqdq\t{%2, %0|%0, %2}
   vpunpcklqdq\t{%2, %1, %0|%0, %1, %2}
   movlhps\t{%2, %0|%0, %2}
   movhps\t{%2, %0|%0, %2}
   vmovhps\t{%2, %1, %0|%0, %1, %2}"
  [(set (attr "isa")
     (cond [(eq_attr "alternative" "0,1")
	      (const_string "x64_sse4_noavx")
	    (eq_attr "alternative" "2")
	      (const_string "x64_avx")
	    (eq_attr "alternative" "3")
	      (const_string "x64_avx512dq")
	    (eq_attr "alternative" "4")
	      (const_string "x64_sse2")
	    (eq_attr "alternative" "5,6")
	      (const_string "sse2")
	    (eq_attr "alternative" "7")
	      (const_string "sse2_noavx")
	    (eq_attr "alternative" "8,11")
	      (const_string "avx")
	   ]
	   (const_string "noavx")))
   (set (attr "type")
     (if_then_else
       (eq_attr "alternative" "0,1,2,3,7,8")
       (const_string "sselog")
       (const_string "ssemov")))
   (set (attr "prefix_rex")
     (if_then_else (eq_attr "alternative" "0,1,2,3,4")
		   (const_string "1")
		   (const_string "*")))
   (set (attr "prefix_extra")
     (if_then_else (eq_attr "alternative" "0,1,2,3")
		   (const_string "1")
		   (const_string "*")))
   (set (attr "length_immediate")
     (if_then_else (eq_attr "alternative" "0,1,2,3")
		   (const_string "1")
		   (const_string "*")))
   (set (attr "prefix")
     (cond [(eq_attr "alternative" "2")
	      (const_string "vex")
	    (eq_attr "alternative" "3")
	      (const_string "evex")
	    (eq_attr "alternative" "4,5")
	      (const_string "maybe_vex")
	    (eq_attr "alternative" "8,11")
	      (const_string "maybe_evex")
	   ]
	   (const_string "orig")))
   (set_attr "mode" "TI,TI,TI,TI,TI,TI,TI,TI,TI,V4SF,V2SF,V2SF")
   (set (attr "preferred_for_speed")
     (cond [(eq_attr "alternative" "4")
	      (symbol_ref "TARGET_INTER_UNIT_MOVES_TO_VEC")
	    (eq_attr "alternative" "6")
	      (symbol_ref "TARGET_INTER_UNIT_MOVES_FROM_VEC")
	   ]
	   (symbol_ref "true")))])

;; vmovq clears also the higher bits.
(define_insn "vec_set<mode>_0"
  [(set (match_operand:VI8_AVX_AVX512F 0 "register_operand" "=v,v")
	(vec_merge:VI8_AVX_AVX512F
	  (vec_duplicate:VI8_AVX_AVX512F
	    (match_operand:<ssescalarmode> 2 "general_operand" "r,vm"))
	  (match_operand:VI8_AVX_AVX512F 1 "const0_operand" "C,C")
	  (const_int 1)))]
  "TARGET_AVX"
  "vmovq\t{%2, %x0|%x0, %2}"
  [(set_attr "isa" "x64,*")
   (set_attr "type" "ssemov")
   (set_attr "prefix_rex" "1,*")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "TI")
   (set (attr "preferred_for_speed")
     (cond [(eq_attr "alternative" "0")
	      (symbol_ref "TARGET_INTER_UNIT_MOVES_TO_VEC")
	   ]
	   (symbol_ref "true")))])

(define_expand "vec_unpacks_lo_<mode>"
  [(match_operand:<sseunpackmode> 0 "register_operand")
   (match_operand:VI124_AVX2_24_AVX512F_1_AVX512BW 1 "register_operand")]
  "TARGET_SSE2"
  "ix86_expand_sse_unpack (operands[0], operands[1], false, false); DONE;")

(define_expand "vec_unpacks_hi_<mode>"
  [(match_operand:<sseunpackmode> 0 "register_operand")
   (match_operand:VI124_AVX2_24_AVX512F_1_AVX512BW 1 "register_operand")]
  "TARGET_SSE2"
  "ix86_expand_sse_unpack (operands[0], operands[1], false, true); DONE;")

(define_expand "vec_unpacku_lo_<mode>"
  [(match_operand:<sseunpackmode> 0 "register_operand")
   (match_operand:VI124_AVX2_24_AVX512F_1_AVX512BW 1 "register_operand")]
  "TARGET_SSE2"
  "ix86_expand_sse_unpack (operands[0], operands[1], true, false); DONE;")

(define_expand "vec_unpacks_lo_hi"
  [(set (subreg:HI (match_operand:QI 0 "register_operand") 0)
        (match_operand:HI 1 "register_operand"))]
  "TARGET_AVX512F")

(define_expand "vec_unpacks_lo_si"
  [(set (match_operand:HI 0 "register_operand")
        (subreg:HI (match_operand:SI 1 "register_operand") 0))]
  "TARGET_AVX512F")

(define_expand "vec_unpacks_lo_di"
  [(set (match_operand:SI 0 "register_operand")
        (subreg:SI (match_operand:DI 1 "register_operand") 0))]
  "TARGET_AVX512BW")

(define_expand "vec_unpacku_hi_<mode>"
  [(match_operand:<sseunpackmode> 0 "register_operand")
   (match_operand:VI124_AVX2_24_AVX512F_1_AVX512BW 1 "register_operand")]
  "TARGET_SSE2"
  "ix86_expand_sse_unpack (operands[0], operands[1], true, true); DONE;")

(define_expand "vec_unpacks_hi_hi"
  [(parallel
     [(set (subreg:HI (match_operand:QI 0 "register_operand") 0)
	   (lshiftrt:HI (match_operand:HI 1 "register_operand")
			(const_int 8)))
      (unspec [(const_int 0)] UNSPEC_MASKOP)])]
  "TARGET_AVX512F")

(define_expand "vec_unpacks_hi_<mode>"
  [(parallel
     [(set (subreg:SWI48x
	     (match_operand:<HALFMASKMODE> 0 "register_operand") 0)
	   (lshiftrt:SWI48x (match_operand:SWI48x 1 "register_operand")
			    (match_dup 2)))
      (unspec [(const_int 0)] UNSPEC_MASKOP)])]
  "TARGET_AVX512BW"
  "operands[2] = GEN_INT (GET_MODE_BITSIZE (<HALFMASKMODE>mode));")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Miscellaneous
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "<sse2_avx2>_uavg<mode>3<mask_name>"
  [(set (match_operand:VI12_AVX2 0 "register_operand")
	(truncate:VI12_AVX2
	  (lshiftrt:<ssedoublemode>
	    (plus:<ssedoublemode>
	      (plus:<ssedoublemode>
		(zero_extend:<ssedoublemode>
		  (match_operand:VI12_AVX2 1 "vector_operand"))
		(zero_extend:<ssedoublemode>
		  (match_operand:VI12_AVX2 2 "vector_operand")))
	      (match_dup <mask_expand_op3>))
	    (const_int 1))))]
  "TARGET_SSE2 && <mask_mode512bit_condition> && <mask_avx512bw_condition>"
{
  operands[<mask_expand_op3>] = CONST1_RTX(<MODE>mode);
  ix86_fixup_binary_operands_no_copy (PLUS, <MODE>mode, operands);
})

(define_insn "*<sse2_avx2>_uavg<mode>3<mask_name>"
  [(set (match_operand:VI12_AVX2 0 "register_operand" "=x,v")
	(truncate:VI12_AVX2
	  (lshiftrt:<ssedoublemode>
	    (plus:<ssedoublemode>
	      (plus:<ssedoublemode>
		(zero_extend:<ssedoublemode>
		  (match_operand:VI12_AVX2 1 "vector_operand" "%0,v"))
		(zero_extend:<ssedoublemode>
		  (match_operand:VI12_AVX2 2 "vector_operand" "xBm,vm")))
	      (match_operand:VI12_AVX2 <mask_expand_op3> "const1_operand"))
	    (const_int 1))))]
  "TARGET_SSE2 && <mask_mode512bit_condition> && <mask_avx512bw_condition>
   && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   pavg<ssemodesuffix>\t{%2, %0|%0, %2}
   vpavg<ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseiadd")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,<mask_prefix>")
   (set_attr "mode" "<sseinsnmode>")])

;; The correct representation for this is absolutely enormous, and
;; surely not generally useful.
(define_insn "<sse2_avx2>_psadbw"
  [(set (match_operand:VI8_AVX2_AVX512BW 0 "register_operand" "=x,v")
	(unspec:VI8_AVX2_AVX512BW
	  [(match_operand:<ssebytemode> 1 "register_operand" "0,v")
	   (match_operand:<ssebytemode> 2 "vector_operand" "xBm,vm")]
	  UNSPEC_PSADBW))]
  "TARGET_SSE2"
  "@
   psadbw\t{%2, %0|%0, %2}
   vpsadbw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseiadd")
   (set_attr "atom_unit" "simul")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix" "orig,maybe_evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<sse>_movmsk<ssemodesuffix><avxsizesuffix>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	  [(match_operand:VF_128_256 1 "register_operand" "x")]
	  UNSPEC_MOVMSK))]
  "TARGET_SSE"
  "%vmovmsk<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

(define_insn "*<sse>_movmsk<ssemodesuffix><avxsizesuffix>_zext"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (unspec:SI
	    [(match_operand:VF_128_256 1 "register_operand" "x")]
	    UNSPEC_MOVMSK)))]
  "TARGET_64BIT && TARGET_SSE"
  "%vmovmsk<ssemodesuffix>\t{%1, %k0|%k0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

(define_insn "<sse2_avx2>_pmovmskb"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	  [(match_operand:VI1_AVX2 1 "register_operand" "x")]
	  UNSPEC_MOVMSK))]
  "TARGET_SSE2"
  "%vpmovmskb\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set (attr "prefix_data16")
     (if_then_else
       (match_test "TARGET_AVX")
     (const_string "*")
     (const_string "1")))
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "SI")])

(define_insn "*<sse2_avx2>_pmovmskb_zext"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (unspec:SI
	    [(match_operand:VI1_AVX2 1 "register_operand" "x")]
	    UNSPEC_MOVMSK)))]
  "TARGET_64BIT && TARGET_SSE2"
  "%vpmovmskb\t{%1, %k0|%k0, %1}"
  [(set_attr "type" "ssemov")
   (set (attr "prefix_data16")
     (if_then_else
       (match_test "TARGET_AVX")
     (const_string "*")
     (const_string "1")))
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "SI")])

(define_expand "sse2_maskmovdqu"
  [(set (match_operand:V16QI 0 "memory_operand")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand")
		       (match_operand:V16QI 2 "register_operand")
		       (match_dup 0)]
		      UNSPEC_MASKMOV))]
  "TARGET_SSE2")

(define_insn "*sse2_maskmovdqu"
  [(set (mem:V16QI (match_operand:P 0 "register_operand" "D"))
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "x")
		       (match_operand:V16QI 2 "register_operand" "x")
		       (mem:V16QI (match_dup 0))]
		      UNSPEC_MASKMOV))]
  "TARGET_SSE2"
{
  /* We can't use %^ here due to ASM_OUTPUT_OPCODE processing
     that requires %v to be at the beginning of the opcode name.  */
  if (Pmode != word_mode)
    fputs ("\taddr32", asm_out_file);
  return "%vmaskmovdqu\t{%2, %1|%1, %2}";
}
  [(set_attr "type" "ssemov")
   (set_attr "prefix_data16" "1")
   (set (attr "length_address")
     (symbol_ref ("Pmode != word_mode")))
   ;; The implicit %rdi operand confuses default length_vex computation.
   (set (attr "length_vex")
     (symbol_ref ("3 + REX_SSE_REGNO_P (REGNO (operands[2]))")))
   (set_attr "prefix" "maybe_vex")
   (set_attr "znver1_decode" "vector")
   (set_attr "mode" "TI")])

(define_insn "sse_ldmxcsr"
  [(unspec_volatile [(match_operand:SI 0 "memory_operand" "m")]
		    UNSPECV_LDMXCSR)]
  "TARGET_SSE"
  "%vldmxcsr\t%0"
  [(set_attr "type" "sse")
   (set_attr "atom_sse_attr" "mxcsr")
   (set_attr "prefix" "maybe_vex")
   (set_attr "memory" "load")])

(define_insn "sse_stmxcsr"
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(unspec_volatile:SI [(const_int 0)] UNSPECV_STMXCSR))]
  "TARGET_SSE"
  "%vstmxcsr\t%0"
  [(set_attr "type" "sse")
   (set_attr "atom_sse_attr" "mxcsr")
   (set_attr "prefix" "maybe_vex")
   (set_attr "memory" "store")])

(define_insn "sse2_clflush"
  [(unspec_volatile [(match_operand 0 "address_operand" "p")]
		    UNSPECV_CLFLUSH)]
  "TARGET_SSE2"
  "clflush\t%a0"
  [(set_attr "type" "sse")
   (set_attr "atom_sse_attr" "fence")
   (set_attr "memory" "unknown")])

;; As per AMD and Intel ISA manuals, the first operand is extensions
;; and it goes to %ecx. The second operand received is hints and it goes
;; to %eax.
(define_insn "sse3_mwait"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "c")
		     (match_operand:SI 1 "register_operand" "a")]
		    UNSPECV_MWAIT)]
  "TARGET_SSE3"
;; 64bit version is "mwait %rax,%rcx". But only lower 32bits are used.
;; Since 32bit register operands are implicitly zero extended to 64bit,
;; we only need to set up 32bit registers.
  "mwait"
  [(set_attr "length" "3")])

(define_insn "sse3_monitor_<mode>"
  [(unspec_volatile [(match_operand:P 0 "register_operand" "a")
		     (match_operand:SI 1 "register_operand" "c")
		     (match_operand:SI 2 "register_operand" "d")]
		    UNSPECV_MONITOR)]
  "TARGET_SSE3"
;; 64bit version is "monitor %rax,%rcx,%rdx". But only lower 32bits in
;; RCX and RDX are used.  Since 32bit register operands are implicitly
;; zero extended to 64bit, we only need to set up 32bit registers.
  "%^monitor"
  [(set (attr "length")
     (symbol_ref ("(Pmode != word_mode) + 3")))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SSSE3 instructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_code_iterator ssse3_plusminus [plus ss_plus minus ss_minus])

(define_insn "avx2_ph<plusminus_mnemonic>wv16hi3"
  [(set (match_operand:V16HI 0 "register_operand" "=x")
	(vec_concat:V16HI
	  (vec_concat:V8HI
	    (vec_concat:V4HI
	      (vec_concat:V2HI
		(ssse3_plusminus:HI
		  (vec_select:HI
		    (match_operand:V16HI 1 "register_operand" "x")
		    (parallel [(const_int 0)]))
		  (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
		(ssse3_plusminus:HI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 2)]))
		  (vec_select:HI (match_dup 1) (parallel [(const_int 3)]))))
	      (vec_concat:V2HI
		(ssse3_plusminus:HI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 4)]))
		  (vec_select:HI (match_dup 1) (parallel [(const_int 5)])))
		(ssse3_plusminus:HI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 6)]))
		  (vec_select:HI (match_dup 1) (parallel [(const_int 7)])))))
	    (vec_concat:V4HI
	      (vec_concat:V2HI
		(ssse3_plusminus:HI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 8)]))
		  (vec_select:HI (match_dup 1) (parallel [(const_int 9)])))
		(ssse3_plusminus:HI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 10)]))
		  (vec_select:HI (match_dup 1) (parallel [(const_int 11)]))))
	      (vec_concat:V2HI
		(ssse3_plusminus:HI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 12)]))
		  (vec_select:HI (match_dup 1) (parallel [(const_int 13)])))
		(ssse3_plusminus:HI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 14)]))
		  (vec_select:HI (match_dup 1) (parallel [(const_int 15)]))))))
	  (vec_concat:V8HI
	    (vec_concat:V4HI
	      (vec_concat:V2HI
		(ssse3_plusminus:HI
		  (vec_select:HI
		    (match_operand:V16HI 2 "nonimmediate_operand" "xm")
		    (parallel [(const_int 0)]))
		  (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
		(ssse3_plusminus:HI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 2)]))
		  (vec_select:HI (match_dup 2) (parallel [(const_int 3)]))))
	      (vec_concat:V2HI
		(ssse3_plusminus:HI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 4)]))
		  (vec_select:HI (match_dup 2) (parallel [(const_int 5)])))
		(ssse3_plusminus:HI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 6)]))
		  (vec_select:HI (match_dup 2) (parallel [(const_int 7)])))))
	    (vec_concat:V4HI
	      (vec_concat:V2HI
		(ssse3_plusminus:HI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 8)]))
		  (vec_select:HI (match_dup 2) (parallel [(const_int 9)])))
		(ssse3_plusminus:HI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 10)]))
		  (vec_select:HI (match_dup 2) (parallel [(const_int 11)]))))
	      (vec_concat:V2HI
		(ssse3_plusminus:HI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 12)]))
		  (vec_select:HI (match_dup 2) (parallel [(const_int 13)])))
		(ssse3_plusminus:HI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 14)]))
		  (vec_select:HI (match_dup 2) (parallel [(const_int 15)]))))))))]
  "TARGET_AVX2"
  "vph<plusminus_mnemonic>w\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "OI")])

(define_insn "ssse3_ph<plusminus_mnemonic>wv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x,x")
	(vec_concat:V8HI
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (ssse3_plusminus:HI
		(vec_select:HI
		  (match_operand:V8HI 1 "register_operand" "0,x")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	      (ssse3_plusminus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (ssse3_plusminus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 5)])))
	      (ssse3_plusminus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 7)])))))
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (ssse3_plusminus:HI
		(vec_select:HI
		  (match_operand:V8HI 2 "vector_operand" "xBm,xm")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
	      (ssse3_plusminus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (ssse3_plusminus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 5)])))
	      (ssse3_plusminus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 7)])))))))]
  "TARGET_SSSE3"
  "@
   ph<plusminus_mnemonic>w\t{%2, %0|%0, %2}
   vph<plusminus_mnemonic>w\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "TI")])

(define_insn "ssse3_ph<plusminus_mnemonic>wv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(vec_concat:V4HI
	  (vec_concat:V2HI
	    (ssse3_plusminus:HI
	      (vec_select:HI
		(match_operand:V4HI 1 "register_operand" "0")
		(parallel [(const_int 0)]))
	      (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	    (ssse3_plusminus:HI
	      (vec_select:HI (match_dup 1) (parallel [(const_int 2)]))
	      (vec_select:HI (match_dup 1) (parallel [(const_int 3)]))))
	  (vec_concat:V2HI
	    (ssse3_plusminus:HI
	      (vec_select:HI
		(match_operand:V4HI 2 "nonimmediate_operand" "ym")
		(parallel [(const_int 0)]))
	      (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
	    (ssse3_plusminus:HI
	      (vec_select:HI (match_dup 2) (parallel [(const_int 2)]))
	      (vec_select:HI (match_dup 2) (parallel [(const_int 3)]))))))]
  "TARGET_SSSE3"
  "ph<plusminus_mnemonic>w\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_insn "avx2_ph<plusminus_mnemonic>dv8si3"
  [(set (match_operand:V8SI 0 "register_operand" "=x")
	(vec_concat:V8SI
	  (vec_concat:V4SI
	    (vec_concat:V2SI
	      (plusminus:SI
		(vec_select:SI
		  (match_operand:V8SI 1 "register_operand" "x")
		  (parallel [(const_int 0)]))
		(vec_select:SI (match_dup 1) (parallel [(const_int 1)])))
	      (plusminus:SI
		(vec_select:SI (match_dup 1) (parallel [(const_int 2)]))
		(vec_select:SI (match_dup 1) (parallel [(const_int 3)]))))
	    (vec_concat:V2SI
	      (plusminus:SI
		(vec_select:SI (match_dup 1) (parallel [(const_int 4)]))
		(vec_select:SI (match_dup 1) (parallel [(const_int 5)])))
	      (plusminus:SI
		(vec_select:SI (match_dup 1) (parallel [(const_int 6)]))
		(vec_select:SI (match_dup 1) (parallel [(const_int 7)])))))
	  (vec_concat:V4SI
	    (vec_concat:V2SI
	      (plusminus:SI
		(vec_select:SI
		  (match_operand:V8SI 2 "nonimmediate_operand" "xm")
		  (parallel [(const_int 0)]))
		(vec_select:SI (match_dup 2) (parallel [(const_int 1)])))
	      (plusminus:SI
		(vec_select:SI (match_dup 2) (parallel [(const_int 2)]))
		(vec_select:SI (match_dup 2) (parallel [(const_int 3)]))))
	    (vec_concat:V2SI
	      (plusminus:SI
		(vec_select:SI (match_dup 2) (parallel [(const_int 4)]))
		(vec_select:SI (match_dup 2) (parallel [(const_int 5)])))
	      (plusminus:SI
		(vec_select:SI (match_dup 2) (parallel [(const_int 6)]))
		(vec_select:SI (match_dup 2) (parallel [(const_int 7)])))))))]
  "TARGET_AVX2"
  "vph<plusminus_mnemonic>d\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "OI")])

(define_insn "ssse3_ph<plusminus_mnemonic>dv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=x,x")
	(vec_concat:V4SI
	  (vec_concat:V2SI
	    (plusminus:SI
	      (vec_select:SI
		(match_operand:V4SI 1 "register_operand" "0,x")
		(parallel [(const_int 0)]))
	      (vec_select:SI (match_dup 1) (parallel [(const_int 1)])))
	    (plusminus:SI
	      (vec_select:SI (match_dup 1) (parallel [(const_int 2)]))
	      (vec_select:SI (match_dup 1) (parallel [(const_int 3)]))))
	  (vec_concat:V2SI
	    (plusminus:SI
	      (vec_select:SI
		(match_operand:V4SI 2 "vector_operand" "xBm,xm")
		(parallel [(const_int 0)]))
	      (vec_select:SI (match_dup 2) (parallel [(const_int 1)])))
	    (plusminus:SI
	      (vec_select:SI (match_dup 2) (parallel [(const_int 2)]))
	      (vec_select:SI (match_dup 2) (parallel [(const_int 3)]))))))]
  "TARGET_SSSE3"
  "@
   ph<plusminus_mnemonic>d\t{%2, %0|%0, %2}
   vph<plusminus_mnemonic>d\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "TI")])

(define_insn "ssse3_ph<plusminus_mnemonic>dv2si3"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(vec_concat:V2SI
	  (plusminus:SI
	    (vec_select:SI
	      (match_operand:V2SI 1 "register_operand" "0")
	      (parallel [(const_int 0)]))
	    (vec_select:SI (match_dup 1) (parallel [(const_int 1)])))
	  (plusminus:SI
	    (vec_select:SI
	      (match_operand:V2SI 2 "nonimmediate_operand" "ym")
	      (parallel [(const_int 0)]))
	    (vec_select:SI (match_dup 2) (parallel [(const_int 1)])))))]
  "TARGET_SSSE3"
  "ph<plusminus_mnemonic>d\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_insn "avx2_pmaddubsw256"
  [(set (match_operand:V16HI 0 "register_operand" "=x,v")
	(ss_plus:V16HI
	  (mult:V16HI
	    (zero_extend:V16HI
	      (vec_select:V16QI
		(match_operand:V32QI 1 "register_operand" "x,v")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)
			   (const_int 16) (const_int 18)
			   (const_int 20) (const_int 22)
			   (const_int 24) (const_int 26)
			   (const_int 28) (const_int 30)])))
	    (sign_extend:V16HI
	      (vec_select:V16QI
		(match_operand:V32QI 2 "nonimmediate_operand" "xm,vm")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)
			   (const_int 16) (const_int 18)
			   (const_int 20) (const_int 22)
			   (const_int 24) (const_int 26)
			   (const_int 28) (const_int 30)]))))
	  (mult:V16HI
	    (zero_extend:V16HI
	      (vec_select:V16QI (match_dup 1)
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)
			   (const_int 17) (const_int 19)
			   (const_int 21) (const_int 23)
			   (const_int 25) (const_int 27)
			   (const_int 29) (const_int 31)])))
	    (sign_extend:V16HI
	      (vec_select:V16QI (match_dup 2)
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)
			   (const_int 17) (const_int 19)
			   (const_int 21) (const_int 23)
			   (const_int 25) (const_int 27)
			   (const_int 29) (const_int 31)]))))))]
  "TARGET_AVX2"
  "vpmaddubsw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "*,avx512bw")
   (set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex,evex")
   (set_attr "mode" "OI")])

;; The correct representation for this is absolutely enormous, and
;; surely not generally useful.
(define_insn "avx512bw_pmaddubsw512<mode><mask_name>"
  [(set (match_operand:VI2_AVX512VL 0 "register_operand" "=v")
          (unspec:VI2_AVX512VL
            [(match_operand:<dbpsadbwmode> 1 "register_operand" "v")
             (match_operand:<dbpsadbwmode> 2 "nonimmediate_operand" "vm")]
             UNSPEC_PMADDUBSW512))]
   "TARGET_AVX512BW"
   "vpmaddubsw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}";
  [(set_attr "type" "sseiadd")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "avx512bw_umulhrswv32hi3<mask_name>"
  [(set (match_operand:V32HI 0 "register_operand" "=v")
	(truncate:V32HI
	  (lshiftrt:V32SI
	    (plus:V32SI
	      (lshiftrt:V32SI
		(mult:V32SI
		  (sign_extend:V32SI
		    (match_operand:V32HI 1 "nonimmediate_operand" "%v"))
		  (sign_extend:V32SI
		    (match_operand:V32HI 2 "nonimmediate_operand" "vm")))
		(const_int 14))
	      (const_vector:V32HI [(const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "TARGET_AVX512BW"
  "vpmulhrsw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "ssse3_pmaddubsw128"
  [(set (match_operand:V8HI 0 "register_operand" "=x,x,v")
	(ss_plus:V8HI
	  (mult:V8HI
	    (zero_extend:V8HI
	      (vec_select:V8QI
		(match_operand:V16QI 1 "register_operand" "0,x,v")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)])))
	    (sign_extend:V8HI
	      (vec_select:V8QI
		(match_operand:V16QI 2 "vector_operand" "xBm,xm,vm")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)]))))
	  (mult:V8HI
	    (zero_extend:V8HI
	      (vec_select:V8QI (match_dup 1)
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)])))
	    (sign_extend:V8HI
	      (vec_select:V8QI (match_dup 2)
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)]))))))]
  "TARGET_SSSE3"
  "@
   pmaddubsw\t{%2, %0|%0, %2}
   vpmaddubsw\t{%2, %1, %0|%0, %1, %2}
   vpmaddubsw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx,avx512bw")
   (set_attr "type" "sseiadd")
   (set_attr "atom_unit" "simul")
   (set_attr "prefix_data16" "1,*,*")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,vex,evex")
   (set_attr "mode" "TI")])

(define_insn "ssse3_pmaddubsw"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(ss_plus:V4HI
	  (mult:V4HI
	    (zero_extend:V4HI
	      (vec_select:V4QI
		(match_operand:V8QI 1 "register_operand" "0")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)])))
	    (sign_extend:V4HI
	      (vec_select:V4QI
		(match_operand:V8QI 2 "nonimmediate_operand" "ym")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)]))))
	  (mult:V4HI
	    (zero_extend:V4HI
	      (vec_select:V4QI (match_dup 1)
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)])))
	    (sign_extend:V4HI
	      (vec_select:V4QI (match_dup 2)
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)]))))))]
  "TARGET_SSSE3"
  "pmaddubsw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "simul")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_mode_iterator PMULHRSW
  [V4HI V8HI (V16HI "TARGET_AVX2")])

(define_expand "<ssse3_avx2>_pmulhrsw<mode>3_mask"
  [(set (match_operand:PMULHRSW 0 "register_operand")
	(vec_merge:PMULHRSW
	  (truncate:PMULHRSW
	    (lshiftrt:<ssedoublemode>
	      (plus:<ssedoublemode>
	        (lshiftrt:<ssedoublemode>
		  (mult:<ssedoublemode>
		    (sign_extend:<ssedoublemode>
		      (match_operand:PMULHRSW 1 "nonimmediate_operand"))
		    (sign_extend:<ssedoublemode>
		      (match_operand:PMULHRSW 2 "nonimmediate_operand")))
		  (const_int 14))
	        (match_dup 5))
	      (const_int 1)))
	  (match_operand:PMULHRSW 3 "register_operand")
	  (match_operand:<avx512fmaskmode> 4 "register_operand")))]
  "TARGET_AVX512BW && TARGET_AVX512VL"
{
  operands[5] = CONST1_RTX(<MODE>mode);
  ix86_fixup_binary_operands_no_copy (MULT, <MODE>mode, operands);
})

(define_expand "<ssse3_avx2>_pmulhrsw<mode>3"
  [(set (match_operand:PMULHRSW 0 "register_operand")
	(truncate:PMULHRSW
	  (lshiftrt:<ssedoublemode>
	    (plus:<ssedoublemode>
	      (lshiftrt:<ssedoublemode>
		(mult:<ssedoublemode>
		  (sign_extend:<ssedoublemode>
		    (match_operand:PMULHRSW 1 "nonimmediate_operand"))
		  (sign_extend:<ssedoublemode>
		    (match_operand:PMULHRSW 2 "nonimmediate_operand")))
		(const_int 14))
	      (match_dup 3))
	    (const_int 1))))]
  "TARGET_AVX2"
{
  operands[3] = CONST1_RTX(<MODE>mode);
  ix86_fixup_binary_operands_no_copy (MULT, <MODE>mode, operands);
})

(define_insn "*<ssse3_avx2>_pmulhrsw<mode>3<mask_name>"
  [(set (match_operand:VI2_AVX2 0 "register_operand" "=x,x,v")
	(truncate:VI2_AVX2
	  (lshiftrt:<ssedoublemode>
	    (plus:<ssedoublemode>
	      (lshiftrt:<ssedoublemode>
		(mult:<ssedoublemode>
		  (sign_extend:<ssedoublemode>
		    (match_operand:VI2_AVX2 1 "vector_operand" "%0,x,v"))
		  (sign_extend:<ssedoublemode>
		    (match_operand:VI2_AVX2 2 "vector_operand" "xBm,xm,vm")))
		(const_int 14))
	      (match_operand:VI2_AVX2 3 "const1_operand"))
	    (const_int 1))))]
  "TARGET_SSSE3 && <mask_mode512bit_condition> && <mask_avx512bw_condition>
   && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   pmulhrsw\t{%2, %0|%0, %2}
   vpmulhrsw\t{%2, %1, %0<mask_operand4>|%0<mask_operand4>, %1, %2}
   vpmulhrsw\t{%2, %1, %0<mask_operand4>|%0<mask_operand4>, %1, %2}"
  [(set_attr "isa" "noavx,avx,avx512bw")
   (set_attr "type" "sseimul")
   (set_attr "prefix_data16" "1,*,*")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,maybe_evex,evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "*ssse3_pmulhrswv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(truncate:V4HI
	  (lshiftrt:V4SI
	    (plus:V4SI
	      (lshiftrt:V4SI
		(mult:V4SI
		  (sign_extend:V4SI
		    (match_operand:V4HI 1 "nonimmediate_operand" "%0"))
		  (sign_extend:V4SI
		    (match_operand:V4HI 2 "nonimmediate_operand" "ym")))
		(const_int 14))
	      (match_operand:V4HI 3 "const1_operand"))
	    (const_int 1))))]
  "TARGET_SSSE3 && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "pmulhrsw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_insn "<ssse3_avx2>_pshufb<mode>3<mask_name>"
  [(set (match_operand:VI1_AVX512 0 "register_operand" "=x,x,v")
	(unspec:VI1_AVX512
	  [(match_operand:VI1_AVX512 1 "register_operand" "0,x,v")
	   (match_operand:VI1_AVX512 2 "vector_operand" "xBm,xm,vm")]
	  UNSPEC_PSHUFB))]
  "TARGET_SSSE3 && <mask_mode512bit_condition> && <mask_avx512bw_condition>"
  "@
   pshufb\t{%2, %0|%0, %2}
   vpshufb\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}
   vpshufb\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx,avx512bw")
   (set_attr "type" "sselog1")
   (set_attr "prefix_data16" "1,*,*")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,maybe_evex,evex")
   (set_attr "btver2_decode" "vector")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "ssse3_pshufbv8qi3"
  [(set (match_operand:V8QI 0 "register_operand" "=y")
	(unspec:V8QI [(match_operand:V8QI 1 "register_operand" "0")
		      (match_operand:V8QI 2 "nonimmediate_operand" "ym")]
		     UNSPEC_PSHUFB))]
  "TARGET_SSSE3"
  "pshufb\t{%2, %0|%0, %2}";
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_insn "<ssse3_avx2>_psign<mode>3"
  [(set (match_operand:VI124_AVX2 0 "register_operand" "=x,x")
	(unspec:VI124_AVX2
	  [(match_operand:VI124_AVX2 1 "register_operand" "0,x")
	   (match_operand:VI124_AVX2 2 "vector_operand" "xBm,xm")]
	  UNSPEC_PSIGN))]
  "TARGET_SSSE3"
  "@
   psign<ssemodesuffix>\t{%2, %0|%0, %2}
   vpsign<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog1")
   (set_attr "prefix_data16" "1,*")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "ssse3_psign<mode>3"
  [(set (match_operand:MMXMODEI 0 "register_operand" "=y")
	(unspec:MMXMODEI
	  [(match_operand:MMXMODEI 1 "register_operand" "0")
	   (match_operand:MMXMODEI 2 "nonimmediate_operand" "ym")]
	  UNSPEC_PSIGN))]
  "TARGET_SSSE3"
  "psign<mmxvecsize>\t{%2, %0|%0, %2}";
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_insn "<ssse3_avx2>_palignr<mode>_mask"
  [(set (match_operand:VI1_AVX512 0 "register_operand" "=v")
        (vec_merge:VI1_AVX512
	  (unspec:VI1_AVX512
	    [(match_operand:VI1_AVX512 1 "register_operand" "v")
	     (match_operand:VI1_AVX512 2 "nonimmediate_operand" "vm")
	     (match_operand:SI 3 "const_0_to_255_mul_8_operand" "n")]
	    UNSPEC_PALIGNR)
	(match_operand:VI1_AVX512 4 "nonimm_or_0_operand" "0C")
	(match_operand:<avx512fmaskmode> 5 "register_operand" "Yk")))]
  "TARGET_AVX512BW && (<MODE_SIZE> == 64 || TARGET_AVX512VL)"
{
  operands[3] = GEN_INT (INTVAL (operands[3]) / 8);
  return "vpalignr\t{%3, %2, %1, %0%{%5%}%N4|%0%{%5%}%N4, %1, %2, %3}";
}
  [(set_attr "type" "sseishft")
   (set_attr "atom_unit" "sishuf")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<ssse3_avx2>_palignr<mode>"
  [(set (match_operand:SSESCALARMODE 0 "register_operand" "=x,x,v")
	(unspec:SSESCALARMODE
	  [(match_operand:SSESCALARMODE 1 "register_operand" "0,x,v")
	   (match_operand:SSESCALARMODE 2 "vector_operand" "xBm,xm,vm")
	   (match_operand:SI 3 "const_0_to_255_mul_8_operand" "n,n,n")]
	  UNSPEC_PALIGNR))]
  "TARGET_SSSE3"
{
  operands[3] = GEN_INT (INTVAL (operands[3]) / 8);

  switch (which_alternative)
    {
    case 0:
      return "palignr\t{%3, %2, %0|%0, %2, %3}";
    case 1:
    case 2:
      return "vpalignr\t{%3, %2, %1, %0|%0, %1, %2, %3}";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "isa" "noavx,avx,avx512bw")
   (set_attr "type" "sseishft")
   (set_attr "atom_unit" "sishuf")
   (set_attr "prefix_data16" "1,*,*")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "orig,vex,evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "ssse3_palignrdi"
  [(set (match_operand:DI 0 "register_operand" "=y")
	(unspec:DI [(match_operand:DI 1 "register_operand" "0")
		    (match_operand:DI 2 "nonimmediate_operand" "ym")
		    (match_operand:SI 3 "const_0_to_255_mul_8_operand" "n")]
		   UNSPEC_PALIGNR))]
  "TARGET_SSSE3"
{
  operands[3] = GEN_INT (INTVAL (operands[3]) / 8);
  return "palignr\t{%3, %2, %0|%0, %2, %3}";
}
  [(set_attr "type" "sseishft")
   (set_attr "atom_unit" "sishuf")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

;; Mode iterator to handle singularity w/ absence of V2DI and V4DI
;; modes for abs instruction on pre AVX-512 targets.
(define_mode_iterator VI1248_AVX512VL_AVX512BW
  [(V64QI "TARGET_AVX512BW") (V32QI "TARGET_AVX2") V16QI
   (V32HI "TARGET_AVX512BW") (V16HI "TARGET_AVX2") V8HI
   (V16SI "TARGET_AVX512F") (V8SI "TARGET_AVX2") V4SI
   (V8DI "TARGET_AVX512F") (V4DI "TARGET_AVX512VL") (V2DI "TARGET_AVX512VL")])

(define_insn "*abs<mode>2"
  [(set (match_operand:VI1248_AVX512VL_AVX512BW 0 "register_operand" "=v")
	(abs:VI1248_AVX512VL_AVX512BW
	  (match_operand:VI1248_AVX512VL_AVX512BW 1 "vector_operand" "vBm")))]
  "TARGET_SSSE3"
  "%vpabs<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "abs<mode>2_mask"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI48_AVX512VL
	  (abs:VI48_AVX512VL
	    (match_operand:VI48_AVX512VL 1 "nonimmediate_operand" "vm"))
	  (match_operand:VI48_AVX512VL 2 "nonimm_or_0_operand" "0C")
	  (match_operand:<avx512fmaskmode> 3 "register_operand" "Yk")))]
  "TARGET_AVX512F"
  "vpabs<ssemodesuffix>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "abs<mode>2_mask"
  [(set (match_operand:VI12_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI12_AVX512VL
	  (abs:VI12_AVX512VL
	    (match_operand:VI12_AVX512VL 1 "nonimmediate_operand" "vm"))
	  (match_operand:VI12_AVX512VL 2 "nonimm_or_0_operand" "0C")
	  (match_operand:<avx512fmaskmode> 3 "register_operand" "Yk")))]
  "TARGET_AVX512BW"
  "vpabs<ssemodesuffix>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "abs<mode>2"
  [(set (match_operand:VI_AVX2 0 "register_operand")
	(abs:VI_AVX2
	  (match_operand:VI_AVX2 1 "vector_operand")))]
  "TARGET_SSE2"
{
  if (!TARGET_SSSE3
      || ((<MODE>mode == V2DImode || <MODE>mode == V4DImode)
	  && !TARGET_AVX512VL))
    {
      ix86_expand_sse2_abs (operands[0], operands[1]);
      DONE;
    }
})

(define_insn "abs<mode>2"
  [(set (match_operand:MMXMODEI 0 "register_operand" "=y")
	(abs:MMXMODEI
	  (match_operand:MMXMODEI 1 "nonimmediate_operand" "ym")))]
  "TARGET_SSSE3"
  "pabs<mmxvecsize>\t{%1, %0|%0, %1}";
  [(set_attr "type" "sselog1")
   (set_attr "prefix_rep" "0")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AMD SSE4A instructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "sse4a_movnt<mode>"
  [(set (match_operand:MODEF 0 "memory_operand" "=m")
	(unspec:MODEF
	  [(match_operand:MODEF 1 "register_operand" "x")]
	  UNSPEC_MOVNT))]
  "TARGET_SSE4A"
  "movnt<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "<MODE>")])

(define_insn "sse4a_vmmovnt<mode>"
  [(set (match_operand:<ssescalarmode> 0 "memory_operand" "=m")
	(unspec:<ssescalarmode>
	  [(vec_select:<ssescalarmode>
	     (match_operand:VF_128 1 "register_operand" "x")
	     (parallel [(const_int 0)]))]
	  UNSPEC_MOVNT))]
  "TARGET_SSE4A"
  "movnt<ssescalarmodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "sse4a_extrqi"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		      (match_operand 2 "const_0_to_255_operand")
		      (match_operand 3 "const_0_to_255_operand")]
		     UNSPEC_EXTRQI))]
  "TARGET_SSE4A"
  "extrq\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "sse")
   (set_attr "prefix_data16" "1")
   (set_attr "length_immediate" "2")
   (set_attr "mode" "TI")])

(define_insn "sse4a_extrq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		      (match_operand:V16QI 2 "register_operand" "x")]
		     UNSPEC_EXTRQ))]
  "TARGET_SSE4A"
  "extrq\t{%2, %0|%0, %2}"
  [(set_attr "type" "sse")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "sse4a_insertqi"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		      (match_operand:V2DI 2 "register_operand" "x")
		      (match_operand 3 "const_0_to_255_operand")
		      (match_operand 4 "const_0_to_255_operand")]
		     UNSPEC_INSERTQI))]
  "TARGET_SSE4A"
  "insertq\t{%4, %3, %2, %0|%0, %2, %3, %4}"
  [(set_attr "type" "sseins")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_rep" "1")
   (set_attr "length_immediate" "2")
   (set_attr "mode" "TI")])

(define_insn "sse4a_insertq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		      (match_operand:V2DI 2 "register_operand" "x")]
		     UNSPEC_INSERTQ))]
  "TARGET_SSE4A"
  "insertq\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseins")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_rep" "1")
   (set_attr "mode" "TI")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Intel SSE4.1 instructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mapping of immediate bits for blend instructions
(define_mode_attr blendbits
  [(V8SF "255") (V4SF "15") (V4DF "15") (V2DF "3")])

(define_insn "<sse4_1>_blend<ssemodesuffix><avxsizesuffix>"
  [(set (match_operand:VF_128_256 0 "register_operand" "=Yr,*x,x")
	(vec_merge:VF_128_256
	  (match_operand:VF_128_256 2 "vector_operand" "YrBm,*xBm,xm")
	  (match_operand:VF_128_256 1 "register_operand" "0,0,x")
	  (match_operand:SI 3 "const_0_to_<blendbits>_operand")))]
  "TARGET_SSE4_1"
  "@
   blend<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   blend<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   vblend<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssemov")
   (set_attr "length_immediate" "1")
   (set_attr "prefix_data16" "1,1,*")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "mode" "<MODE>")])

(define_insn "<sse4_1>_blendv<ssemodesuffix><avxsizesuffix>"
  [(set (match_operand:VF_128_256 0 "register_operand" "=Yr,*x,x")
	(unspec:VF_128_256
	  [(match_operand:VF_128_256 1 "register_operand" "0,0,x")
	   (match_operand:VF_128_256 2 "vector_operand" "YrBm,*xBm,xm")
	   (match_operand:VF_128_256 3 "register_operand" "Yz,Yz,x")]
	  UNSPEC_BLENDV))]
  "TARGET_SSE4_1"
  "@
   blendv<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   blendv<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   vblendv<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssemov")
   (set_attr "length_immediate" "1")
   (set_attr "prefix_data16" "1,1,*")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "btver2_decode" "vector,vector,vector") 
   (set_attr "mode" "<MODE>")])

(define_insn "<sse4_1>_dp<ssemodesuffix><avxsizesuffix>"
  [(set (match_operand:VF_128_256 0 "register_operand" "=Yr,*x,x")
	(unspec:VF_128_256
	  [(match_operand:VF_128_256 1 "vector_operand" "%0,0,x")
	   (match_operand:VF_128_256 2 "vector_operand" "YrBm,*xBm,xm")
	   (match_operand:SI 3 "const_0_to_255_operand" "n,n,n")]
	  UNSPEC_DP))]
  "TARGET_SSE4_1"
  "@
   dp<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   dp<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   vdp<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssemul")
   (set_attr "length_immediate" "1")
   (set_attr "prefix_data16" "1,1,*")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "btver2_decode" "vector,vector,vector")
   (set_attr "znver1_decode" "vector,vector,vector")
   (set_attr "mode" "<MODE>")])

;; Mode attribute used by `vmovntdqa' pattern
(define_mode_attr vi8_sse4_1_avx2_avx512
   [(V2DI "sse4_1") (V4DI "avx2") (V8DI "avx512f")])

(define_insn "<vi8_sse4_1_avx2_avx512>_movntdqa"
  [(set (match_operand:VI8_AVX2_AVX512F 0 "register_operand" "=Yr,*x,v")
	(unspec:VI8_AVX2_AVX512F [(match_operand:VI8_AVX2_AVX512F 1 "memory_operand" "m,m,m")]
		     UNSPEC_MOVNTDQA))]
  "TARGET_SSE4_1"
  "%vmovntdqa\t{%1, %0|%0, %1}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1,1,*")
   (set_attr "prefix" "orig,orig,maybe_evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<sse4_1_avx2>_mpsadbw"
  [(set (match_operand:VI1_AVX2 0 "register_operand" "=Yr,*x,x")
	(unspec:VI1_AVX2
	  [(match_operand:VI1_AVX2 1 "register_operand" "0,0,x")
	   (match_operand:VI1_AVX2 2 "vector_operand" "YrBm,*xBm,xm")
	   (match_operand:SI 3 "const_0_to_255_operand" "n,n,n")]
	  UNSPEC_MPSADBW))]
  "TARGET_SSE4_1"
  "@
   mpsadbw\t{%3, %2, %0|%0, %2, %3}
   mpsadbw\t{%3, %2, %0|%0, %2, %3}
   vmpsadbw\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "sselog1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "btver2_decode" "vector,vector,vector")
   (set_attr "znver1_decode" "vector,vector,vector")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<sse4_1_avx2>_packusdw<mask_name>"
  [(set (match_operand:VI2_AVX2 0 "register_operand" "=Yr,*x,x,v")
	(vec_concat:VI2_AVX2
	  (us_truncate:<ssehalfvecmode>
	    (match_operand:<sseunpackmode> 1 "register_operand" "0,0,x,v"))
	  (us_truncate:<ssehalfvecmode>
	    (match_operand:<sseunpackmode> 2 "vector_operand" "YrBm,*xBm,xm,vm"))))]
  "TARGET_SSE4_1 && <mask_mode512bit_condition> && <mask_avx512bw_condition>"
  "@
   packusdw\t{%2, %0|%0, %2}
   packusdw\t{%2, %0|%0, %2}
   vpackusdw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}
   vpackusdw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,noavx,avx,avx512bw")
   (set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,<mask_prefix>,evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<sse4_1_avx2>_pblendvb"
  [(set (match_operand:VI1_AVX2 0 "register_operand" "=Yr,*x,x")
	(unspec:VI1_AVX2
	  [(match_operand:VI1_AVX2 1 "register_operand"  "0,0,x")
	   (match_operand:VI1_AVX2 2 "vector_operand" "YrBm,*xBm,xm")
	   (match_operand:VI1_AVX2 3 "register_operand" "Yz,Yz,x")]
	  UNSPEC_BLENDV))]
  "TARGET_SSE4_1"
  "@
   pblendvb\t{%3, %2, %0|%0, %2, %3}
   pblendvb\t{%3, %2, %0|%0, %2, %3}
   vpblendvb\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "*,*,1")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "btver2_decode" "vector,vector,vector")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "sse4_1_pblendw"
  [(set (match_operand:V8HI 0 "register_operand" "=Yr,*x,x")
	(vec_merge:V8HI
	  (match_operand:V8HI 2 "vector_operand" "YrBm,*xBm,xm")
	  (match_operand:V8HI 1 "register_operand" "0,0,x")
	  (match_operand:SI 3 "const_0_to_255_operand" "n,n,n")))]
  "TARGET_SSE4_1"
  "@
   pblendw\t{%3, %2, %0|%0, %2, %3}
   pblendw\t{%3, %2, %0|%0, %2, %3}
   vpblendw\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "mode" "TI")])

;; The builtin uses an 8-bit immediate.  Expand that.
(define_expand "avx2_pblendw"
  [(set (match_operand:V16HI 0 "register_operand")
	(vec_merge:V16HI
	  (match_operand:V16HI 2 "nonimmediate_operand")
	  (match_operand:V16HI 1 "register_operand")
	  (match_operand:SI 3 "const_0_to_255_operand")))]
  "TARGET_AVX2"
{
  HOST_WIDE_INT val = INTVAL (operands[3]) & 0xff;
  operands[3] = GEN_INT (val << 8 | val);
})

(define_insn "*avx2_pblendw"
  [(set (match_operand:V16HI 0 "register_operand" "=x")
	(vec_merge:V16HI
	  (match_operand:V16HI 2 "nonimmediate_operand" "xm")
	  (match_operand:V16HI 1 "register_operand" "x")
	  (match_operand:SI 3 "avx2_pblendw_operand" "n")))]
  "TARGET_AVX2"
{
  operands[3] = GEN_INT (INTVAL (operands[3]) & 0xff);
  return "vpblendw\t{%3, %2, %1, %0|%0, %1, %2, %3}";
}
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "OI")])

(define_insn "avx2_pblendd<mode>"
  [(set (match_operand:VI4_AVX2 0 "register_operand" "=x")
	(vec_merge:VI4_AVX2
	  (match_operand:VI4_AVX2 2 "nonimmediate_operand" "xm")
	  (match_operand:VI4_AVX2 1 "register_operand" "x")
	  (match_operand:SI 3 "const_0_to_255_operand" "n")))]
  "TARGET_AVX2"
  "vpblendd\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "sse4_1_phminposuw"
  [(set (match_operand:V8HI 0 "register_operand" "=Yr,*x,x")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_operand" "YrBm,*xBm,xm")]
		     UNSPEC_PHMINPOSUW))]
  "TARGET_SSE4_1"
  "%vphminposuw\t{%1, %0|%0, %1}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "mode" "TI")])

(define_insn "avx2_<code>v16qiv16hi2<mask_name>"
  [(set (match_operand:V16HI 0 "register_operand" "=v")
	(any_extend:V16HI
	  (match_operand:V16QI 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX2 && <mask_avx512bw_condition> && <mask_avx512vl_condition>"
  "vpmov<extsuffix>bw\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "OI")])

(define_insn "avx512bw_<code>v32qiv32hi2<mask_name>"
  [(set (match_operand:V32HI 0 "register_operand" "=v")
	(any_extend:V32HI
	  (match_operand:V32QI 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512BW"
  "vpmov<extsuffix>bw\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "sse4_1_<code>v8qiv8hi2<mask_name>"
  [(set (match_operand:V8HI 0 "register_operand" "=Yr,*x,v")
	(any_extend:V8HI
	  (vec_select:V8QI
	    (match_operand:V16QI 1 "nonimmediate_operand" "Yrm,*xm,vm")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)
		       (const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))))]
  "TARGET_SSE4_1 && <mask_avx512bw_condition> && <mask_avx512vl_condition>"
  "%vpmov<extsuffix>bw\t{%1, %0<mask_operand2>|%0<mask_operand2>, %q1}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,maybe_evex")
   (set_attr "mode" "TI")])

(define_insn "<mask_codefor>avx512f_<code>v16qiv16si2<mask_name>"
  [(set (match_operand:V16SI 0 "register_operand" "=v")
	(any_extend:V16SI
	  (match_operand:V16QI 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512F"
  "vpmov<extsuffix>bd\t{%1, %0<mask_operand2>|%0<mask_operand2>, %q1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "avx2_<code>v8qiv8si2<mask_name>"
  [(set (match_operand:V8SI 0 "register_operand" "=v")
	(any_extend:V8SI
	  (vec_select:V8QI
	    (match_operand:V16QI 1 "nonimmediate_operand" "vm")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)
		       (const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))))]
  "TARGET_AVX2 && <mask_avx512vl_condition>"
  "vpmov<extsuffix>bd\t{%1, %0<mask_operand2>|%0<mask_operand2>, %q1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "OI")])

(define_insn "sse4_1_<code>v4qiv4si2<mask_name>"
  [(set (match_operand:V4SI 0 "register_operand" "=Yr,*x,v")
	(any_extend:V4SI
	  (vec_select:V4QI
	    (match_operand:V16QI 1 "nonimmediate_operand" "Yrm,*xm,vm")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)]))))]
  "TARGET_SSE4_1 && <mask_avx512vl_condition>"
  "%vpmov<extsuffix>bd\t{%1, %0<mask_operand2>|%0<mask_operand2>, %k1}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,maybe_evex")
   (set_attr "mode" "TI")])

(define_insn "avx512f_<code>v16hiv16si2<mask_name>"
  [(set (match_operand:V16SI 0 "register_operand" "=v")
	(any_extend:V16SI
	  (match_operand:V16HI 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512F"
  "vpmov<extsuffix>wd\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "avx2_<code>v8hiv8si2<mask_name>"
  [(set (match_operand:V8SI 0 "register_operand" "=v")
	(any_extend:V8SI
	    (match_operand:V8HI 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX2 && <mask_avx512vl_condition>"
  "vpmov<extsuffix>wd\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "OI")])

(define_insn "sse4_1_<code>v4hiv4si2<mask_name>"
  [(set (match_operand:V4SI 0 "register_operand" "=Yr,*x,v")
	(any_extend:V4SI
	  (vec_select:V4HI
	    (match_operand:V8HI 1 "nonimmediate_operand" "Yrm,*xm,vm")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)]))))]
  "TARGET_SSE4_1 && <mask_avx512vl_condition>"
  "%vpmov<extsuffix>wd\t{%1, %0<mask_operand2>|%0<mask_operand2>, %q1}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,maybe_evex")
   (set_attr "mode" "TI")])

(define_insn "avx512f_<code>v8qiv8di2<mask_name>"
  [(set (match_operand:V8DI 0 "register_operand" "=v")
	(any_extend:V8DI
	  (vec_select:V8QI
	    (match_operand:V16QI 1 "nonimmediate_operand" "vm")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)
		       (const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))))]
  "TARGET_AVX512F"
  "vpmov<extsuffix>bq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %k1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "avx2_<code>v4qiv4di2<mask_name>"
  [(set (match_operand:V4DI 0 "register_operand" "=v")
	(any_extend:V4DI
	  (vec_select:V4QI
	    (match_operand:V16QI 1 "nonimmediate_operand" "vm")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)]))))]
  "TARGET_AVX2 && <mask_avx512vl_condition>"
  "vpmov<extsuffix>bq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %k1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "OI")])

(define_insn "sse4_1_<code>v2qiv2di2<mask_name>"
  [(set (match_operand:V2DI 0 "register_operand" "=Yr,*x,v")
	(any_extend:V2DI
	  (vec_select:V2QI
	    (match_operand:V16QI 1 "nonimmediate_operand" "Yrm,*xm,vm")
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SSE4_1 && <mask_avx512vl_condition>"
  "%vpmov<extsuffix>bq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %w1}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,maybe_evex")
   (set_attr "mode" "TI")])

(define_insn "avx512f_<code>v8hiv8di2<mask_name>"
  [(set (match_operand:V8DI 0 "register_operand" "=v")
	(any_extend:V8DI
	  (match_operand:V8HI 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512F"
  "vpmov<extsuffix>wq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %q1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "avx2_<code>v4hiv4di2<mask_name>"
  [(set (match_operand:V4DI 0 "register_operand" "=v")
	(any_extend:V4DI
	  (vec_select:V4HI
	    (match_operand:V8HI 1 "nonimmediate_operand" "vm")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)]))))]
  "TARGET_AVX2 && <mask_avx512vl_condition>"
  "vpmov<extsuffix>wq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %q1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "OI")])

(define_insn "sse4_1_<code>v2hiv2di2<mask_name>"
  [(set (match_operand:V2DI 0 "register_operand" "=Yr,*x,v")
	(any_extend:V2DI
	  (vec_select:V2HI
	    (match_operand:V8HI 1 "nonimmediate_operand" "Yrm,*xm,vm")
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SSE4_1 && <mask_avx512vl_condition>"
  "%vpmov<extsuffix>wq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %k1}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,maybe_evex")
   (set_attr "mode" "TI")])

(define_insn "avx512f_<code>v8siv8di2<mask_name>"
  [(set (match_operand:V8DI 0 "register_operand" "=v")
	(any_extend:V8DI
	  (match_operand:V8SI 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512F"
  "vpmov<extsuffix>dq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "avx2_<code>v4siv4di2<mask_name>"
  [(set (match_operand:V4DI 0 "register_operand" "=v")
	(any_extend:V4DI
	    (match_operand:V4SI 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX2 && <mask_avx512vl_condition>"
  "vpmov<extsuffix>dq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "maybe_evex")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "OI")])

(define_insn "sse4_1_<code>v2siv2di2<mask_name>"
  [(set (match_operand:V2DI 0 "register_operand" "=Yr,*x,v")
	(any_extend:V2DI
	  (vec_select:V2SI
	    (match_operand:V4SI 1 "nonimmediate_operand" "Yrm,*xm,vm")
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SSE4_1 && <mask_avx512vl_condition>"
  "%vpmov<extsuffix>dq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %q1}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,maybe_evex")
   (set_attr "mode" "TI")])

;; ptestps/ptestpd are very similar to comiss and ucomiss when
;; setting FLAGS_REG. But it is not a really compare instruction.
(define_insn "avx_vtest<ssemodesuffix><avxsizesuffix>"
  [(set (reg:CC FLAGS_REG)
	(unspec:CC [(match_operand:VF_128_256 0 "register_operand" "x")
		    (match_operand:VF_128_256 1 "nonimmediate_operand" "xm")]
		   UNSPEC_VTESTP))]
  "TARGET_AVX"
  "vtest<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecomi")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

;; ptest is very similar to comiss and ucomiss when setting FLAGS_REG.
;; But it is not a really compare instruction.
(define_insn "<sse4_1>_ptest<mode>"
  [(set (reg:CC FLAGS_REG)
	(unspec:CC [(match_operand:V_AVX 0 "register_operand" "Yr, *x, x")
		    (match_operand:V_AVX 1 "vector_operand" "YrBm, *xBm, xm")]
		   UNSPEC_PTEST))]
  "TARGET_SSE4_1"
  "%vptest\t{%1, %0|%0, %1}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssecomi")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,vex")
   (set (attr "btver2_decode")
     (if_then_else
       (match_test "<sseinsnmode>mode==OImode")
     (const_string "vector")
     (const_string "*")))
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "ptesttf2"
  [(set (reg:CC FLAGS_REG)
	(unspec:CC [(match_operand:TF 0 "register_operand" "Yr, *x, x")
		    (match_operand:TF 1 "vector_operand" "YrBm, *xBm, xm")]
		   UNSPEC_PTEST))]
  "TARGET_SSE4_1"
  "%vptest\t{%1, %0|%0, %1}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssecomi")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "mode" "TI")])

(define_insn "<sse4_1>_round<ssemodesuffix><avxsizesuffix>"
  [(set (match_operand:VF_128_256 0 "register_operand" "=Yr,*x,x")
	(unspec:VF_128_256
	  [(match_operand:VF_128_256 1 "vector_operand" "YrBm,*xBm,xm")
	   (match_operand:SI 2 "const_0_to_15_operand" "n,n,n")]
	  UNSPEC_ROUND))]
  "TARGET_SSE4_1"
  "%vround<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,noavx,avx")
   (set_attr "type" "ssecvt")
   (set_attr "prefix_data16" "1,1,*")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "orig,orig,vex")
   (set_attr "mode" "<MODE>")])

(define_expand "<sse4_1>_round<ssemodesuffix>_sfix<avxsizesuffix>"
  [(match_operand:<sseintvecmode> 0 "register_operand")
   (match_operand:VF1_128_256 1 "vector_operand")
   (match_operand:SI 2 "const_0_to_15_operand")]
  "TARGET_SSE4_1"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);

  emit_insn
    (gen_<sse4_1>_round<ssemodesuffix><avxsizesuffix> (tmp, operands[1],
						       operands[2]));
  emit_insn
    (gen_fix_trunc<mode><sseintvecmodelower>2 (operands[0], tmp));
  DONE;
})

(define_expand "avx512f_round<castmode>512"
  [(match_operand:VF_512 0 "register_operand")
   (match_operand:VF_512 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_15_operand")]
  "TARGET_AVX512F"
{
  emit_insn (gen_avx512f_rndscale<mode> (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "avx512f_roundps512_sfix"
  [(match_operand:V16SI 0 "register_operand")
   (match_operand:V16SF 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_15_operand")]
  "TARGET_AVX512F"
{
  rtx tmp = gen_reg_rtx (V16SFmode);
  emit_insn (gen_avx512f_rndscalev16sf (tmp, operands[1], operands[2]));
  emit_insn (gen_fix_truncv16sfv16si2 (operands[0], tmp));
  DONE;
})

(define_expand "<sse4_1>_round<ssemodesuffix>_vec_pack_sfix<avxsizesuffix>"
  [(match_operand:<ssepackfltmode> 0 "register_operand")
   (match_operand:VF2 1 "vector_operand")
   (match_operand:VF2 2 "vector_operand")
   (match_operand:SI 3 "const_0_to_15_operand")]
  "TARGET_SSE4_1"
{
  rtx tmp0, tmp1;

  if (<MODE>mode == V2DFmode
      && TARGET_AVX && !TARGET_PREFER_AVX128 && optimize_insn_for_speed_p ())
    {
      rtx tmp2 = gen_reg_rtx (V4DFmode);

      tmp0 = gen_reg_rtx (V4DFmode);
      tmp1 = force_reg (V2DFmode, operands[1]);

      emit_insn (gen_avx_vec_concatv4df (tmp0, tmp1, operands[2]));
      emit_insn (gen_avx_roundpd256 (tmp2, tmp0, operands[3]));
      emit_insn (gen_fix_truncv4dfv4si2 (operands[0], tmp2));
    }
  else
    {
      tmp0 = gen_reg_rtx (<MODE>mode);
      tmp1 = gen_reg_rtx (<MODE>mode);

      emit_insn
       (gen_<sse4_1>_round<ssemodesuffix><avxsizesuffix> (tmp0, operands[1],
							  operands[3]));
      emit_insn
       (gen_<sse4_1>_round<ssemodesuffix><avxsizesuffix> (tmp1, operands[2],
							  operands[3]));
      emit_insn
       (gen_vec_pack_sfix_trunc_<mode> (operands[0], tmp0, tmp1));
    }
  DONE;
})

(define_insn "sse4_1_round<ssescalarmodesuffix>"
  [(set (match_operand:VF_128 0 "register_operand" "=Yr,*x,x,v")
	(vec_merge:VF_128
	  (unspec:VF_128
	    [(match_operand:VF_128 2 "register_operand" "Yr,*x,x,v")
	     (match_operand:SI 3 "const_0_to_15_operand" "n,n,n,n")]
	    UNSPEC_ROUND)
	  (match_operand:VF_128 1 "register_operand" "0,0,x,v")
	  (const_int 1)))]
  "TARGET_SSE4_1"
  "@
   round<ssescalarmodesuffix>\t{%3, %2, %0|%0, %2, %3}
   round<ssescalarmodesuffix>\t{%3, %2, %0|%0, %2, %3}
   vround<ssescalarmodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}
   vrndscale<ssescalarmodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "isa" "noavx,noavx,avx,avx512f")
   (set_attr "type" "ssecvt")
   (set_attr "length_immediate" "1")
   (set_attr "prefix_data16" "1,1,*,*")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,orig,vex,evex")
   (set_attr "mode" "<MODE>")])

(define_expand "round<mode>2"
  [(set (match_dup 3)
	(plus:VF
	  (match_operand:VF 1 "register_operand")
	  (match_dup 2)))
   (set (match_operand:VF 0 "register_operand")
	(unspec:VF
	  [(match_dup 3) (match_dup 4)]
	  UNSPEC_ROUND))]
  "TARGET_SSE4_1 && !flag_trapping_math"
{
  machine_mode scalar_mode;
  const struct real_format *fmt;
  REAL_VALUE_TYPE pred_half, half_minus_pred_half;
  rtx half, vec_half;

  scalar_mode = GET_MODE_INNER (<MODE>mode);

  /* load nextafter (0.5, 0.0) */
  fmt = REAL_MODE_FORMAT (scalar_mode);
  real_2expN (&half_minus_pred_half, -(fmt->p) - 1, scalar_mode);
  real_arithmetic (&pred_half, MINUS_EXPR, &dconsthalf, &half_minus_pred_half);
  half = const_double_from_real_value (pred_half, scalar_mode);

  vec_half = ix86_build_const_vector (<MODE>mode, true, half);
  vec_half = force_reg (<MODE>mode, vec_half);

  operands[2] = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_copysign<mode>3 (operands[2], vec_half, operands[1]));

  operands[3] = gen_reg_rtx (<MODE>mode);
  operands[4] = GEN_INT (ROUND_TRUNC);
})

(define_expand "round<mode>2_sfix"
  [(match_operand:<sseintvecmode> 0 "register_operand")
   (match_operand:VF1 1 "register_operand")]
  "TARGET_SSE4_1 && !flag_trapping_math"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);

  emit_insn (gen_round<mode>2 (tmp, operands[1]));

  emit_insn
    (gen_fix_trunc<mode><sseintvecmodelower>2 (operands[0], tmp));
  DONE;
})

(define_expand "round<mode>2_vec_pack_sfix"
  [(match_operand:<ssepackfltmode> 0 "register_operand")
   (match_operand:VF2 1 "register_operand")
   (match_operand:VF2 2 "register_operand")]
  "TARGET_SSE4_1 && !flag_trapping_math"
{
  rtx tmp0, tmp1;

  if (<MODE>mode == V2DFmode
      && TARGET_AVX && !TARGET_PREFER_AVX128 && optimize_insn_for_speed_p ())
    {
      rtx tmp2 = gen_reg_rtx (V4DFmode);

      tmp0 = gen_reg_rtx (V4DFmode);
      tmp1 = force_reg (V2DFmode, operands[1]);

      emit_insn (gen_avx_vec_concatv4df (tmp0, tmp1, operands[2]));
      emit_insn (gen_roundv4df2 (tmp2, tmp0));
      emit_insn (gen_fix_truncv4dfv4si2 (operands[0], tmp2));
    }
  else
    {
      tmp0 = gen_reg_rtx (<MODE>mode);
      tmp1 = gen_reg_rtx (<MODE>mode);

      emit_insn (gen_round<mode>2 (tmp0, operands[1]));
      emit_insn (gen_round<mode>2 (tmp1, operands[2]));

      emit_insn
       (gen_vec_pack_sfix_trunc_<mode> (operands[0], tmp0, tmp1));
    }
  DONE;
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Intel SSE4.2 string/text processing instructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_and_split "sse4_2_pcmpestr"
  [(set (match_operand:SI 0 "register_operand" "=c,c")
	(unspec:SI
	  [(match_operand:V16QI 2 "register_operand" "x,x")
	   (match_operand:SI 3 "register_operand" "a,a")
	   (match_operand:V16QI 4 "nonimmediate_operand" "x,m")
	   (match_operand:SI 5 "register_operand" "d,d")
	   (match_operand:SI 6 "const_0_to_255_operand" "n,n")]
	  UNSPEC_PCMPESTR))
   (set (match_operand:V16QI 1 "register_operand" "=Yz,Yz")
	(unspec:V16QI
	  [(match_dup 2)
	   (match_dup 3)
	   (match_dup 4)
	   (match_dup 5)
	   (match_dup 6)]
	  UNSPEC_PCMPESTR))
   (set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_dup 2)
	   (match_dup 3)
	   (match_dup 4)
	   (match_dup 5)
	   (match_dup 6)]
	  UNSPEC_PCMPESTR))]
  "TARGET_SSE4_2
   && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  int ecx = !find_regno_note (curr_insn, REG_UNUSED, REGNO (operands[0]));
  int xmm0 = !find_regno_note (curr_insn, REG_UNUSED, REGNO (operands[1]));
  int flags = !find_regno_note (curr_insn, REG_UNUSED, FLAGS_REG);

  if (ecx)
    emit_insn (gen_sse4_2_pcmpestri (operands[0], operands[2],
				     operands[3], operands[4],
				     operands[5], operands[6]));
  if (xmm0)
    emit_insn (gen_sse4_2_pcmpestrm (operands[1], operands[2],
				     operands[3], operands[4],
				     operands[5], operands[6]));
  if (flags && !(ecx || xmm0))
    emit_insn (gen_sse4_2_pcmpestr_cconly (NULL, NULL,
					   operands[2], operands[3],
					   operands[4], operands[5],
					   operands[6]));
  if (!(flags || ecx || xmm0))
    emit_note (NOTE_INSN_DELETED);

  DONE;
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,load")
   (set_attr "mode" "TI")])

(define_insn "sse4_2_pcmpestri"
  [(set (match_operand:SI 0 "register_operand" "=c,c")
	(unspec:SI
	  [(match_operand:V16QI 1 "register_operand" "x,x")
	   (match_operand:SI 2 "register_operand" "a,a")
	   (match_operand:V16QI 3 "nonimmediate_operand" "x,m")
	   (match_operand:SI 4 "register_operand" "d,d")
	   (match_operand:SI 5 "const_0_to_255_operand" "n,n")]
	  UNSPEC_PCMPESTR))
   (set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_dup 1)
	   (match_dup 2)
	   (match_dup 3)
	   (match_dup 4)
	   (match_dup 5)]
	  UNSPEC_PCMPESTR))]
  "TARGET_SSE4_2"
  "%vpcmpestri\t{%5, %3, %1|%1, %3, %5}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "length_immediate" "1")
   (set_attr "btver2_decode" "vector")
   (set_attr "memory" "none,load")
   (set_attr "mode" "TI")])

(define_insn "sse4_2_pcmpestrm"
  [(set (match_operand:V16QI 0 "register_operand" "=Yz,Yz")
	(unspec:V16QI
	  [(match_operand:V16QI 1 "register_operand" "x,x")
	   (match_operand:SI 2 "register_operand" "a,a")
	   (match_operand:V16QI 3 "nonimmediate_operand" "x,m")
	   (match_operand:SI 4 "register_operand" "d,d")
	   (match_operand:SI 5 "const_0_to_255_operand" "n,n")]
	  UNSPEC_PCMPESTR))
   (set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_dup 1)
	   (match_dup 2)
	   (match_dup 3)
	   (match_dup 4)
	   (match_dup 5)]
	  UNSPEC_PCMPESTR))]
  "TARGET_SSE4_2"
  "%vpcmpestrm\t{%5, %3, %1|%1, %3, %5}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "btver2_decode" "vector")
   (set_attr "memory" "none,load")
   (set_attr "mode" "TI")])

(define_insn "sse4_2_pcmpestr_cconly"
  [(set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_operand:V16QI 2 "register_operand" "x,x,x,x")
	   (match_operand:SI 3 "register_operand" "a,a,a,a")
	   (match_operand:V16QI 4 "nonimmediate_operand" "x,m,x,m")
	   (match_operand:SI 5 "register_operand" "d,d,d,d")
	   (match_operand:SI 6 "const_0_to_255_operand" "n,n,n,n")]
	  UNSPEC_PCMPESTR))
   (clobber (match_scratch:V16QI 0 "=Yz,Yz,X,X"))
   (clobber (match_scratch:SI    1 "= X, X,c,c"))]
  "TARGET_SSE4_2"
  "@
   %vpcmpestrm\t{%6, %4, %2|%2, %4, %6}
   %vpcmpestrm\t{%6, %4, %2|%2, %4, %6}
   %vpcmpestri\t{%6, %4, %2|%2, %4, %6}
   %vpcmpestri\t{%6, %4, %2|%2, %4, %6}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,load,none,load")
   (set_attr "btver2_decode" "vector,vector,vector,vector") 
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn_and_split "sse4_2_pcmpistr"
  [(set (match_operand:SI 0 "register_operand" "=c,c")
	(unspec:SI
	  [(match_operand:V16QI 2 "register_operand" "x,x")
	   (match_operand:V16QI 3 "nonimmediate_operand" "x,m")
	   (match_operand:SI 4 "const_0_to_255_operand" "n,n")]
	  UNSPEC_PCMPISTR))
   (set (match_operand:V16QI 1 "register_operand" "=Yz,Yz")
	(unspec:V16QI
	  [(match_dup 2)
	   (match_dup 3)
	   (match_dup 4)]
	  UNSPEC_PCMPISTR))
   (set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_dup 2)
	   (match_dup 3)
	   (match_dup 4)]
	  UNSPEC_PCMPISTR))]
  "TARGET_SSE4_2
   && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  int ecx = !find_regno_note (curr_insn, REG_UNUSED, REGNO (operands[0]));
  int xmm0 = !find_regno_note (curr_insn, REG_UNUSED, REGNO (operands[1]));
  int flags = !find_regno_note (curr_insn, REG_UNUSED, FLAGS_REG);

  if (ecx)
    emit_insn (gen_sse4_2_pcmpistri (operands[0], operands[2],
				     operands[3], operands[4]));
  if (xmm0)
    emit_insn (gen_sse4_2_pcmpistrm (operands[1], operands[2],
				     operands[3], operands[4]));
  if (flags && !(ecx || xmm0))
    emit_insn (gen_sse4_2_pcmpistr_cconly (NULL, NULL,
					   operands[2], operands[3],
					   operands[4]));
  if (!(flags || ecx || xmm0))
    emit_note (NOTE_INSN_DELETED);

  DONE;
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,load")
   (set_attr "mode" "TI")])

(define_insn "sse4_2_pcmpistri"
  [(set (match_operand:SI 0 "register_operand" "=c,c")
	(unspec:SI
	  [(match_operand:V16QI 1 "register_operand" "x,x")
	   (match_operand:V16QI 2 "nonimmediate_operand" "x,m")
	   (match_operand:SI 3 "const_0_to_255_operand" "n,n")]
	  UNSPEC_PCMPISTR))
   (set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_dup 1)
	   (match_dup 2)
	   (match_dup 3)]
	  UNSPEC_PCMPISTR))]
  "TARGET_SSE4_2"
  "%vpcmpistri\t{%3, %2, %1|%1, %2, %3}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "memory" "none,load")
   (set_attr "btver2_decode" "vector")
   (set_attr "mode" "TI")])

(define_insn "sse4_2_pcmpistrm"
  [(set (match_operand:V16QI 0 "register_operand" "=Yz,Yz")
	(unspec:V16QI
	  [(match_operand:V16QI 1 "register_operand" "x,x")
	   (match_operand:V16QI 2 "nonimmediate_operand" "x,m")
	   (match_operand:SI 3 "const_0_to_255_operand" "n,n")]
	  UNSPEC_PCMPISTR))
   (set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_dup 1)
	   (match_dup 2)
	   (match_dup 3)]
	  UNSPEC_PCMPISTR))]
  "TARGET_SSE4_2"
  "%vpcmpistrm\t{%3, %2, %1|%1, %2, %3}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "memory" "none,load")
   (set_attr "btver2_decode" "vector")
   (set_attr "mode" "TI")])

(define_insn "sse4_2_pcmpistr_cconly"
  [(set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_operand:V16QI 2 "register_operand" "x,x,x,x")
	   (match_operand:V16QI 3 "nonimmediate_operand" "x,m,x,m")
	   (match_operand:SI 4 "const_0_to_255_operand" "n,n,n,n")]
	  UNSPEC_PCMPISTR))
   (clobber (match_scratch:V16QI 0 "=Yz,Yz,X,X"))
   (clobber (match_scratch:SI    1 "= X, X,c,c"))]
  "TARGET_SSE4_2"
  "@
   %vpcmpistrm\t{%4, %3, %2|%2, %3, %4}
   %vpcmpistrm\t{%4, %3, %2|%2, %3, %4}
   %vpcmpistri\t{%4, %3, %2|%2, %3, %4}
   %vpcmpistri\t{%4, %3, %2|%2, %3, %4}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,load,none,load")
   (set_attr "prefix" "maybe_vex")
   (set_attr "btver2_decode" "vector,vector,vector,vector")
   (set_attr "mode" "TI")])

;; Packed float variants
(define_mode_attr GATHER_SCATTER_SF_MEM_MODE
		      [(V8DI "V8SF") (V16SI "V16SF")])

(define_expand "avx512pf_gatherpf<mode>sf"
  [(unspec
     [(match_operand:<avx512fmaskmode> 0 "register_operand")
      (mem:<GATHER_SCATTER_SF_MEM_MODE>
	(match_par_dup 5
	  [(match_operand 2 "vsib_address_operand")
	   (match_operand:VI48_512 1 "register_operand")
	   (match_operand:SI 3 "const1248_operand")]))
      (match_operand:SI 4 "const_2_to_3_operand")]
     UNSPEC_GATHER_PREFETCH)]
  "TARGET_AVX512PF"
{
  operands[5]
    = gen_rtx_UNSPEC (Pmode, gen_rtvec (3, operands[2], operands[1],
					operands[3]), UNSPEC_VSIBADDR);
})

(define_insn "*avx512pf_gatherpf<mode>sf_mask"
  [(unspec
     [(match_operand:<avx512fmaskmode> 0 "register_operand" "Yk")
      (match_operator:<GATHER_SCATTER_SF_MEM_MODE> 5 "vsib_mem_operator"
	[(unspec:P
	   [(match_operand:P 2 "vsib_address_operand" "Tv")
	    (match_operand:VI48_512 1 "register_operand" "v")
	    (match_operand:SI 3 "const1248_operand" "n")]
	   UNSPEC_VSIBADDR)])
      (match_operand:SI 4 "const_2_to_3_operand" "n")]
     UNSPEC_GATHER_PREFETCH)]
  "TARGET_AVX512PF"
{
  switch (INTVAL (operands[4]))
    {
    case 3:
      return "vgatherpf0<ssemodesuffix>ps\t{%5%{%0%}|%5%{%0%}}";
    case 2:
      return "vgatherpf1<ssemodesuffix>ps\t{%5%{%0%}|%5%{%0%}}";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

;; Packed double variants
(define_expand "avx512pf_gatherpf<mode>df"
  [(unspec
     [(match_operand:<avx512fmaskmode> 0 "register_operand")
      (mem:V8DF
	(match_par_dup 5
	  [(match_operand 2 "vsib_address_operand")
	   (match_operand:VI4_256_8_512 1 "register_operand")
	   (match_operand:SI 3 "const1248_operand")]))
      (match_operand:SI 4 "const_2_to_3_operand")]
     UNSPEC_GATHER_PREFETCH)]
  "TARGET_AVX512PF"
{
  operands[5]
    = gen_rtx_UNSPEC (Pmode, gen_rtvec (3, operands[2], operands[1],
					operands[3]), UNSPEC_VSIBADDR);
})

(define_insn "*avx512pf_gatherpf<mode>df_mask"
  [(unspec
     [(match_operand:<avx512fmaskmode> 0 "register_operand" "Yk")
      (match_operator:V8DF 5 "vsib_mem_operator"
	[(unspec:P
	   [(match_operand:P 2 "vsib_address_operand" "Tv")
	    (match_operand:VI4_256_8_512 1 "register_operand" "v")
	    (match_operand:SI 3 "const1248_operand" "n")]
	   UNSPEC_VSIBADDR)])
      (match_operand:SI 4 "const_2_to_3_operand" "n")]
     UNSPEC_GATHER_PREFETCH)]
  "TARGET_AVX512PF"
{
  switch (INTVAL (operands[4]))
    {
    case 3:
      return "vgatherpf0<ssemodesuffix>pd\t{%5%{%0%}|%5%{%0%}}";
    case 2:
      return "vgatherpf1<ssemodesuffix>pd\t{%5%{%0%}|%5%{%0%}}";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

;; Packed float variants
(define_expand "avx512pf_scatterpf<mode>sf"
  [(unspec
     [(match_operand:<avx512fmaskmode> 0 "register_operand")
      (mem:<GATHER_SCATTER_SF_MEM_MODE>
	(match_par_dup 5
	  [(match_operand 2 "vsib_address_operand")
	   (match_operand:VI48_512 1 "register_operand")
	   (match_operand:SI 3 "const1248_operand")]))
      (match_operand:SI 4 "const2367_operand")]
     UNSPEC_SCATTER_PREFETCH)]
  "TARGET_AVX512PF"
{
  operands[5]
    = gen_rtx_UNSPEC (Pmode, gen_rtvec (3, operands[2], operands[1],
					operands[3]), UNSPEC_VSIBADDR);
})

(define_insn "*avx512pf_scatterpf<mode>sf_mask"
  [(unspec
     [(match_operand:<avx512fmaskmode> 0 "register_operand" "Yk")
      (match_operator:<GATHER_SCATTER_SF_MEM_MODE> 5 "vsib_mem_operator"
	[(unspec:P
	   [(match_operand:P 2 "vsib_address_operand" "Tv")
	    (match_operand:VI48_512 1 "register_operand" "v")
	    (match_operand:SI 3 "const1248_operand" "n")]
	   UNSPEC_VSIBADDR)])
      (match_operand:SI 4 "const2367_operand" "n")]
     UNSPEC_SCATTER_PREFETCH)]
  "TARGET_AVX512PF"
{
  switch (INTVAL (operands[4]))
    {
    case 3:
    case 7:
      return "vscatterpf0<ssemodesuffix>ps\t{%5%{%0%}|%5%{%0%}}";
    case 2:
    case 6:
      return "vscatterpf1<ssemodesuffix>ps\t{%5%{%0%}|%5%{%0%}}";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

;; Packed double variants
(define_expand "avx512pf_scatterpf<mode>df"
  [(unspec
     [(match_operand:<avx512fmaskmode> 0 "register_operand")
      (mem:V8DF
	(match_par_dup 5
	  [(match_operand 2 "vsib_address_operand")
	   (match_operand:VI4_256_8_512 1 "register_operand")
	   (match_operand:SI 3 "const1248_operand")]))
      (match_operand:SI 4 "const2367_operand")]
     UNSPEC_SCATTER_PREFETCH)]
  "TARGET_AVX512PF"
{
  operands[5]
    = gen_rtx_UNSPEC (Pmode, gen_rtvec (3, operands[2], operands[1],
					operands[3]), UNSPEC_VSIBADDR);
})

(define_insn "*avx512pf_scatterpf<mode>df_mask"
  [(unspec
     [(match_operand:<avx512fmaskmode> 0 "register_operand" "Yk")
      (match_operator:V8DF 5 "vsib_mem_operator"
	[(unspec:P
	   [(match_operand:P 2 "vsib_address_operand" "Tv")
	    (match_operand:VI4_256_8_512 1 "register_operand" "v")
	    (match_operand:SI 3 "const1248_operand" "n")]
	   UNSPEC_VSIBADDR)])
      (match_operand:SI 4 "const2367_operand" "n")]
     UNSPEC_SCATTER_PREFETCH)]
  "TARGET_AVX512PF"
{
  switch (INTVAL (operands[4]))
    {
    case 3:
    case 7:
      return "vscatterpf0<ssemodesuffix>pd\t{%5%{%0%}|%5%{%0%}}";
    case 2:
    case 6:
      return "vscatterpf1<ssemodesuffix>pd\t{%5%{%0%}|%5%{%0%}}";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "avx512er_exp2<mode><mask_name><round_saeonly_name>"
  [(set (match_operand:VF_512 0 "register_operand" "=v")
	(unspec:VF_512
	  [(match_operand:VF_512 1 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")]
	  UNSPEC_EXP2))]
  "TARGET_AVX512ER"
  "vexp2<ssemodesuffix>\t{<round_saeonly_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_saeonly_mask_op2>}"
  [(set_attr "prefix" "evex")
   (set_attr "type" "sse")
   (set_attr "mode" "<MODE>")])

(define_insn "<mask_codefor>avx512er_rcp28<mode><mask_name><round_saeonly_name>"
  [(set (match_operand:VF_512 0 "register_operand" "=v")
	(unspec:VF_512
	  [(match_operand:VF_512 1 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")]
	  UNSPEC_RCP28))]
  "TARGET_AVX512ER"
  "vrcp28<ssemodesuffix>\t{<round_saeonly_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_saeonly_mask_op2>}"
  [(set_attr "prefix" "evex")
   (set_attr "type" "sse")
   (set_attr "mode" "<MODE>")])

(define_insn "avx512er_vmrcp28<mode><round_saeonly_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
	  (unspec:VF_128
	    [(match_operand:VF_128 1 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")]
	    UNSPEC_RCP28)
	  (match_operand:VF_128 2 "register_operand" "v")
	  (const_int 1)))]
  "TARGET_AVX512ER"
  "vrcp28<ssescalarmodesuffix>\t{<round_saeonly_op3>%1, %2, %0|%0, %2, %<iptr>1<round_saeonly_op3>}"
  [(set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "type" "sse")
   (set_attr "mode" "<MODE>")])

(define_insn "<mask_codefor>avx512er_rsqrt28<mode><mask_name><round_saeonly_name>"
  [(set (match_operand:VF_512 0 "register_operand" "=v")
	(unspec:VF_512
	  [(match_operand:VF_512 1 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")]
	  UNSPEC_RSQRT28))]
  "TARGET_AVX512ER"
  "vrsqrt28<ssemodesuffix>\t{<round_saeonly_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_saeonly_mask_op2>}"
  [(set_attr "prefix" "evex")
   (set_attr "type" "sse")
   (set_attr "mode" "<MODE>")])

(define_insn "avx512er_vmrsqrt28<mode><round_saeonly_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
	  (unspec:VF_128
	    [(match_operand:VF_128 1 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")]
	    UNSPEC_RSQRT28)
	  (match_operand:VF_128 2 "register_operand" "v")
	  (const_int 1)))]
  "TARGET_AVX512ER"
  "vrsqrt28<ssescalarmodesuffix>\t{<round_saeonly_op3>%1, %2, %0|%0, %2, %<iptr>1<round_saeonly_op3>}"
  [(set_attr "length_immediate" "1")
   (set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; XOP instructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_code_iterator xop_plus [plus ss_plus])

(define_code_attr macs [(plus "macs") (ss_plus "macss")])
(define_code_attr madcs [(plus "madcs") (ss_plus "madcss")])

;; XOP parallel integer multiply/add instructions.

(define_insn "xop_p<macs><ssemodesuffix><ssemodesuffix>"
  [(set (match_operand:VI24_128 0 "register_operand" "=x")
	(xop_plus:VI24_128
	 (mult:VI24_128
	  (match_operand:VI24_128 1 "nonimmediate_operand" "%x")
	  (match_operand:VI24_128 2 "nonimmediate_operand" "xm"))
	 (match_operand:VI24_128 3 "register_operand" "x")))]
  "TARGET_XOP"
  "vp<macs><ssemodesuffix><ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

(define_insn "xop_p<macs>dql"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(xop_plus:V2DI
	 (mult:V2DI
	  (sign_extend:V2DI
	   (vec_select:V2SI
	    (match_operand:V4SI 1 "nonimmediate_operand" "%x")
	    (parallel [(const_int 0) (const_int 2)])))
	  (sign_extend:V2DI
	   (vec_select:V2SI
	    (match_operand:V4SI 2 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0) (const_int 2)]))))
	 (match_operand:V2DI 3 "register_operand" "x")))]
  "TARGET_XOP"
  "vp<macs>dql\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

(define_insn "xop_p<macs>dqh"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(xop_plus:V2DI
	 (mult:V2DI
	  (sign_extend:V2DI
	   (vec_select:V2SI
	    (match_operand:V4SI 1 "nonimmediate_operand" "%x")
	    (parallel [(const_int 1) (const_int 3)])))
	  (sign_extend:V2DI
	   (vec_select:V2SI
	    (match_operand:V4SI 2 "nonimmediate_operand" "xm")
	    (parallel [(const_int 1) (const_int 3)]))))
	 (match_operand:V2DI 3 "register_operand" "x")))]
  "TARGET_XOP"
  "vp<macs>dqh\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

;; XOP parallel integer multiply/add instructions for the intrinisics
(define_insn "xop_p<macs>wd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(xop_plus:V4SI
	 (mult:V4SI
	  (sign_extend:V4SI
	   (vec_select:V4HI
	    (match_operand:V8HI 1 "nonimmediate_operand" "%x")
	    (parallel [(const_int 1) (const_int 3)
		       (const_int 5) (const_int 7)])))
	  (sign_extend:V4SI
	   (vec_select:V4HI
	    (match_operand:V8HI 2 "nonimmediate_operand" "xm")
	    (parallel [(const_int 1) (const_int 3)
		       (const_int 5) (const_int 7)]))))
	 (match_operand:V4SI 3 "register_operand" "x")))]
  "TARGET_XOP"
  "vp<macs>wd\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

(define_insn "xop_p<madcs>wd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(xop_plus:V4SI
	 (plus:V4SI
	  (mult:V4SI
	   (sign_extend:V4SI
	    (vec_select:V4HI
	     (match_operand:V8HI 1 "nonimmediate_operand" "%x")
	     (parallel [(const_int 0) (const_int 2)
			(const_int 4) (const_int 6)])))
	   (sign_extend:V4SI
	    (vec_select:V4HI
	     (match_operand:V8HI 2 "nonimmediate_operand" "xm")
	     (parallel [(const_int 0) (const_int 2)
			(const_int 4) (const_int 6)]))))
	  (mult:V4SI
	   (sign_extend:V4SI
	    (vec_select:V4HI
	     (match_dup 1)
	     (parallel [(const_int 1) (const_int 3)
			(const_int 5) (const_int 7)])))
	   (sign_extend:V4SI
	    (vec_select:V4HI
	     (match_dup 2)
	     (parallel [(const_int 1) (const_int 3)
			(const_int 5) (const_int 7)])))))
	 (match_operand:V4SI 3 "register_operand" "x")))]
  "TARGET_XOP"
  "vp<madcs>wd\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

;; XOP parallel XMM conditional moves
(define_insn "xop_pcmov_<mode><avxsizesuffix>"
  [(set (match_operand:V_128_256 0 "register_operand" "=x,x")
	(if_then_else:V_128_256
	  (match_operand:V_128_256 3 "nonimmediate_operand" "x,m")
	  (match_operand:V_128_256 1 "register_operand" "x,x")
	  (match_operand:V_128_256 2 "nonimmediate_operand" "xm,x")))]
  "TARGET_XOP"
  "vpcmov\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sse4arg")])

;; XOP horizontal add/subtract instructions
(define_insn "xop_phadd<u>bw"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(plus:V8HI
	 (any_extend:V8HI
	  (vec_select:V8QI
	   (match_operand:V16QI 1 "nonimmediate_operand" "xm")
	   (parallel [(const_int 0) (const_int 2)
		      (const_int 4) (const_int 6)
		      (const_int 8) (const_int 10)
		      (const_int 12) (const_int 14)])))
	 (any_extend:V8HI
	  (vec_select:V8QI
	   (match_dup 1)
	   (parallel [(const_int 1) (const_int 3)
		      (const_int 5) (const_int 7)
		      (const_int 9) (const_int 11)
		      (const_int 13) (const_int 15)])))))]
  "TARGET_XOP"
  "vphadd<u>bw\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phadd<u>bd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(plus:V4SI
	 (plus:V4SI
	  (any_extend:V4SI
	   (vec_select:V4QI
	    (match_operand:V16QI 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0) (const_int 4)
		       (const_int 8) (const_int 12)])))
	  (any_extend:V4SI
	   (vec_select:V4QI
	    (match_dup 1)
	    (parallel [(const_int 1) (const_int 5)
		       (const_int 9) (const_int 13)]))))
	 (plus:V4SI
	  (any_extend:V4SI
	   (vec_select:V4QI
	    (match_dup 1)
	    (parallel [(const_int 2) (const_int 6)
		       (const_int 10) (const_int 14)])))
	  (any_extend:V4SI
	   (vec_select:V4QI
	    (match_dup 1)
	    (parallel [(const_int 3) (const_int 7)
		       (const_int 11) (const_int 15)]))))))]
  "TARGET_XOP"
  "vphadd<u>bd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phadd<u>bq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(plus:V2DI
	 (plus:V2DI
	  (plus:V2DI
	   (any_extend:V2DI
	    (vec_select:V2QI
	     (match_operand:V16QI 1 "nonimmediate_operand" "xm")
	     (parallel [(const_int 0) (const_int 8)])))
	   (any_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 1) (const_int 9)]))))
	  (plus:V2DI
	   (any_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 2) (const_int 10)])))
	   (any_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 3) (const_int 11)])))))
	 (plus:V2DI
	  (plus:V2DI
	   (any_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 4) (const_int 12)])))
	   (any_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 5) (const_int 13)]))))
	  (plus:V2DI
	   (any_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 6) (const_int 14)])))
	   (any_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 7) (const_int 15)])))))))]
  "TARGET_XOP"
  "vphadd<u>bq\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phadd<u>wd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(plus:V4SI
	 (any_extend:V4SI
	  (vec_select:V4HI
	   (match_operand:V8HI 1 "nonimmediate_operand" "xm")
	   (parallel [(const_int 0) (const_int 2)
		      (const_int 4) (const_int 6)])))
	 (any_extend:V4SI
	  (vec_select:V4HI
	   (match_dup 1)
	   (parallel [(const_int 1) (const_int 3)
		      (const_int 5) (const_int 7)])))))]
  "TARGET_XOP"
  "vphadd<u>wd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phadd<u>wq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(plus:V2DI
	 (plus:V2DI
	  (any_extend:V2DI
	   (vec_select:V2HI
	    (match_operand:V8HI 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0) (const_int 4)])))
	  (any_extend:V2DI
	   (vec_select:V2HI
	    (match_dup 1)
	    (parallel [(const_int 1) (const_int 5)]))))
	 (plus:V2DI
	  (any_extend:V2DI
	   (vec_select:V2HI
	    (match_dup 1)
	    (parallel [(const_int 2) (const_int 6)])))
	  (any_extend:V2DI
	   (vec_select:V2HI
	    (match_dup 1)
	    (parallel [(const_int 3) (const_int 7)]))))))]
  "TARGET_XOP"
  "vphadd<u>wq\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phadd<u>dq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(plus:V2DI
	 (any_extend:V2DI
	  (vec_select:V2SI
	   (match_operand:V4SI 1 "nonimmediate_operand" "xm")
	   (parallel [(const_int 0) (const_int 2)])))
	 (any_extend:V2DI
	  (vec_select:V2SI
	   (match_dup 1)
	   (parallel [(const_int 1) (const_int 3)])))))]
  "TARGET_XOP"
  "vphadd<u>dq\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phsubbw"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(minus:V8HI
	 (sign_extend:V8HI
	  (vec_select:V8QI
	   (match_operand:V16QI 1 "nonimmediate_operand" "xm")
	   (parallel [(const_int 0) (const_int 2)
		      (const_int 4) (const_int 6)
		      (const_int 8) (const_int 10)
		      (const_int 12) (const_int 14)])))
	 (sign_extend:V8HI
	  (vec_select:V8QI
	   (match_dup 1)
	   (parallel [(const_int 1) (const_int 3)
		      (const_int 5) (const_int 7)
		      (const_int 9) (const_int 11)
		      (const_int 13) (const_int 15)])))))]
  "TARGET_XOP"
  "vphsubbw\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phsubwd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(minus:V4SI
	 (sign_extend:V4SI
	  (vec_select:V4HI
	   (match_operand:V8HI 1 "nonimmediate_operand" "xm")
	   (parallel [(const_int 0) (const_int 2)
		      (const_int 4) (const_int 6)])))
	 (sign_extend:V4SI
	  (vec_select:V4HI
	   (match_dup 1)
	   (parallel [(const_int 1) (const_int 3)
		      (const_int 5) (const_int 7)])))))]
  "TARGET_XOP"
  "vphsubwd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phsubdq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(minus:V2DI
	 (sign_extend:V2DI
	  (vec_select:V2SI
	   (match_operand:V4SI 1 "nonimmediate_operand" "xm")
	   (parallel [(const_int 0) (const_int 2)])))
	 (sign_extend:V2DI
	  (vec_select:V2SI
	   (match_dup 1)
	   (parallel [(const_int 1) (const_int 3)])))))]
  "TARGET_XOP"
  "vphsubdq\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

;; XOP permute instructions
(define_insn "xop_pperm"
  [(set (match_operand:V16QI 0 "register_operand" "=x,x")
	(unspec:V16QI
	  [(match_operand:V16QI 1 "register_operand" "x,x")
	   (match_operand:V16QI 2 "nonimmediate_operand" "x,m")
	   (match_operand:V16QI 3 "nonimmediate_operand" "xm,x")]
	  UNSPEC_XOP_PERMUTE))]
  "TARGET_XOP && !(MEM_P (operands[2]) && MEM_P (operands[3]))"
  "vpperm\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sse4arg")
   (set_attr "mode" "TI")])

;; XOP pack instructions that combine two vectors into a smaller vector
(define_insn "xop_pperm_pack_v2di_v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=x,x")
	(vec_concat:V4SI
	 (truncate:V2SI
	  (match_operand:V2DI 1 "register_operand" "x,x"))
	 (truncate:V2SI
	  (match_operand:V2DI 2 "nonimmediate_operand" "x,m"))))
   (use (match_operand:V16QI 3 "nonimmediate_operand" "xm,x"))]
  "TARGET_XOP && !(MEM_P (operands[2]) && MEM_P (operands[3]))"
  "vpperm\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sse4arg")
   (set_attr "mode" "TI")])

(define_insn "xop_pperm_pack_v4si_v8hi"
  [(set (match_operand:V8HI 0 "register_operand" "=x,x")
	(vec_concat:V8HI
	 (truncate:V4HI
	  (match_operand:V4SI 1 "register_operand" "x,x"))
	 (truncate:V4HI
	  (match_operand:V4SI 2 "nonimmediate_operand" "x,m"))))
   (use (match_operand:V16QI 3 "nonimmediate_operand" "xm,x"))]
  "TARGET_XOP && !(MEM_P (operands[2]) && MEM_P (operands[3]))"
  "vpperm\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sse4arg")
   (set_attr "mode" "TI")])

(define_insn "xop_pperm_pack_v8hi_v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=x,x")
	(vec_concat:V16QI
	 (truncate:V8QI
	  (match_operand:V8HI 1 "register_operand" "x,x"))
	 (truncate:V8QI
	  (match_operand:V8HI 2 "nonimmediate_operand" "x,m"))))
   (use (match_operand:V16QI 3 "nonimmediate_operand" "xm,x"))]
  "TARGET_XOP && !(MEM_P (operands[2]) && MEM_P (operands[3]))"
  "vpperm\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sse4arg")
   (set_attr "mode" "TI")])

;; XOP packed rotate instructions
(define_expand "rotl<mode>3"
  [(set (match_operand:VI_128 0 "register_operand")
	(rotate:VI_128
	 (match_operand:VI_128 1 "nonimmediate_operand")
	 (match_operand:SI 2 "general_operand")))]
  "TARGET_XOP"
{
  /* If we were given a scalar, convert it to parallel */
  if (! const_0_to_<sserotatemax>_operand (operands[2], SImode))
    {
      rtvec vs = rtvec_alloc (<ssescalarnum>);
      rtx par = gen_rtx_PARALLEL (<MODE>mode, vs);
      rtx reg = gen_reg_rtx (<MODE>mode);
      rtx op2 = operands[2];
      int i;

      if (GET_MODE (op2) != <ssescalarmode>mode)
	{
	  op2 = gen_reg_rtx (<ssescalarmode>mode);
	  convert_move (op2, operands[2], false);
	}

      for (i = 0; i < <ssescalarnum>; i++)
	RTVEC_ELT (vs, i) = op2;

      emit_insn (gen_vec_init<mode><ssescalarmodelower> (reg, par));
      emit_insn (gen_xop_vrotl<mode>3 (operands[0], operands[1], reg));
      DONE;
    }
})

(define_expand "rotr<mode>3"
  [(set (match_operand:VI_128 0 "register_operand")
	(rotatert:VI_128
	 (match_operand:VI_128 1 "nonimmediate_operand")
	 (match_operand:SI 2 "general_operand")))]
  "TARGET_XOP"
{
  /* If we were given a scalar, convert it to parallel */
  if (! const_0_to_<sserotatemax>_operand (operands[2], SImode))
    {
      rtvec vs = rtvec_alloc (<ssescalarnum>);
      rtx par = gen_rtx_PARALLEL (<MODE>mode, vs);
      rtx neg = gen_reg_rtx (<MODE>mode);
      rtx reg = gen_reg_rtx (<MODE>mode);
      rtx op2 = operands[2];
      int i;

      if (GET_MODE (op2) != <ssescalarmode>mode)
	{
	  op2 = gen_reg_rtx (<ssescalarmode>mode);
	  convert_move (op2, operands[2], false);
	}

      for (i = 0; i < <ssescalarnum>; i++)
	RTVEC_ELT (vs, i) = op2;

      emit_insn (gen_vec_init<mode><ssescalarmodelower> (reg, par));
      emit_insn (gen_neg<mode>2 (neg, reg));
      emit_insn (gen_xop_vrotl<mode>3 (operands[0], operands[1], neg));
      DONE;
    }
})

(define_insn "xop_rotl<mode>3"
  [(set (match_operand:VI_128 0 "register_operand" "=x")
	(rotate:VI_128
	 (match_operand:VI_128 1 "nonimmediate_operand" "xm")
	 (match_operand:SI 2 "const_0_to_<sserotatemax>_operand" "n")))]
  "TARGET_XOP"
  "vprot<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "xop_rotr<mode>3"
  [(set (match_operand:VI_128 0 "register_operand" "=x")
	(rotatert:VI_128
	 (match_operand:VI_128 1 "nonimmediate_operand" "xm")
	 (match_operand:SI 2 "const_0_to_<sserotatemax>_operand" "n")))]
  "TARGET_XOP"
{
  operands[3]
    = GEN_INT (GET_MODE_BITSIZE (<ssescalarmode>mode) - INTVAL (operands[2]));
  return \"vprot<ssemodesuffix>\t{%3, %1, %0|%0, %1, %3}\";
}
  [(set_attr "type" "sseishft")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_expand "vrotr<mode>3"
  [(match_operand:VI_128 0 "register_operand")
   (match_operand:VI_128 1 "register_operand")
   (match_operand:VI_128 2 "register_operand")]
  "TARGET_XOP"
{
  rtx reg = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neg<mode>2 (reg, operands[2]));
  emit_insn (gen_xop_vrotl<mode>3 (operands[0], operands[1], reg));
  DONE;
})

(define_expand "vrotl<mode>3"
  [(match_operand:VI_128 0 "register_operand")
   (match_operand:VI_128 1 "register_operand")
   (match_operand:VI_128 2 "register_operand")]
  "TARGET_XOP"
{
  emit_insn (gen_xop_vrotl<mode>3 (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "xop_vrotl<mode>3"
  [(set (match_operand:VI_128 0 "register_operand" "=x,x")
	(if_then_else:VI_128
	 (ge:VI_128
	  (match_operand:VI_128 2 "nonimmediate_operand" "x,m")
	  (const_int 0))
	 (rotate:VI_128
	  (match_operand:VI_128 1 "nonimmediate_operand" "xm,x")
	  (match_dup 2))
	 (rotatert:VI_128
	  (match_dup 1)
	  (neg:VI_128 (match_dup 2)))))]
  "TARGET_XOP && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "vprot<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_extra" "2")
   (set_attr "mode" "TI")])

;; XOP packed shift instructions.
(define_expand "vlshr<mode>3"
  [(set (match_operand:VI12_128 0 "register_operand")
	(lshiftrt:VI12_128
	  (match_operand:VI12_128 1 "register_operand")
	  (match_operand:VI12_128 2 "nonimmediate_operand")))]
  "TARGET_XOP"
{
  rtx neg = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neg<mode>2 (neg, operands[2]));
  emit_insn (gen_xop_shl<mode>3 (operands[0], operands[1], neg));
  DONE;
})

(define_expand "vlshr<mode>3"
  [(set (match_operand:VI48_128 0 "register_operand")
	(lshiftrt:VI48_128
	  (match_operand:VI48_128 1 "register_operand")
	  (match_operand:VI48_128 2 "nonimmediate_operand")))]
  "TARGET_AVX2 || TARGET_XOP"
{
  if (!TARGET_AVX2)
    {
      rtx neg = gen_reg_rtx (<MODE>mode);
      emit_insn (gen_neg<mode>2 (neg, operands[2]));
      emit_insn (gen_xop_shl<mode>3 (operands[0], operands[1], neg));
      DONE;
    }
})

(define_expand "vlshr<mode>3"
  [(set (match_operand:VI48_512 0 "register_operand")
	(lshiftrt:VI48_512
	  (match_operand:VI48_512 1 "register_operand")
	  (match_operand:VI48_512 2 "nonimmediate_operand")))]
  "TARGET_AVX512F")

(define_expand "vlshr<mode>3"
  [(set (match_operand:VI48_256 0 "register_operand")
	(lshiftrt:VI48_256
	  (match_operand:VI48_256 1 "register_operand")
	  (match_operand:VI48_256 2 "nonimmediate_operand")))]
  "TARGET_AVX2")

(define_expand "vashrv8hi3<mask_name>"
  [(set (match_operand:V8HI 0 "register_operand")
	(ashiftrt:V8HI
	  (match_operand:V8HI 1 "register_operand")
	  (match_operand:V8HI 2 "nonimmediate_operand")))]
  "TARGET_XOP || (TARGET_AVX512BW && TARGET_AVX512VL)"
{
  if (TARGET_XOP)
    {
      rtx neg = gen_reg_rtx (V8HImode);
      emit_insn (gen_negv8hi2 (neg, operands[2]));
      emit_insn (gen_xop_shav8hi3 (operands[0], operands[1], neg));
      DONE;
    }
})

(define_expand "vashrv16qi3"
  [(set (match_operand:V16QI 0 "register_operand")
	(ashiftrt:V16QI
	  (match_operand:V16QI 1 "register_operand")
	  (match_operand:V16QI 2 "nonimmediate_operand")))]
  "TARGET_XOP"
{
   rtx neg = gen_reg_rtx (V16QImode);
   emit_insn (gen_negv16qi2 (neg, operands[2]));
   emit_insn (gen_xop_shav16qi3 (operands[0], operands[1], neg));
   DONE;
})

(define_expand "vashrv2di3<mask_name>"
  [(set (match_operand:V2DI 0 "register_operand")
	(ashiftrt:V2DI
	  (match_operand:V2DI 1 "register_operand")
	  (match_operand:V2DI 2 "nonimmediate_operand")))]
  "TARGET_XOP || TARGET_AVX512VL"
{
  if (TARGET_XOP)
    {
      rtx neg = gen_reg_rtx (V2DImode);
      emit_insn (gen_negv2di2 (neg, operands[2]));
      emit_insn (gen_xop_shav2di3 (operands[0], operands[1], neg));
      DONE;
    }
})

(define_expand "vashrv4si3"
  [(set (match_operand:V4SI 0 "register_operand")
	(ashiftrt:V4SI (match_operand:V4SI 1 "register_operand")
		       (match_operand:V4SI 2 "nonimmediate_operand")))]
  "TARGET_AVX2 || TARGET_XOP"
{
  if (!TARGET_AVX2)
    {
      rtx neg = gen_reg_rtx (V4SImode);
      emit_insn (gen_negv4si2 (neg, operands[2]));
      emit_insn (gen_xop_shav4si3 (operands[0], operands[1], neg));
      DONE;
    }
})

(define_expand "vashrv16si3"
  [(set (match_operand:V16SI 0 "register_operand")
	(ashiftrt:V16SI (match_operand:V16SI 1 "register_operand")
		        (match_operand:V16SI 2 "nonimmediate_operand")))]
  "TARGET_AVX512F")

(define_expand "vashrv8si3"
  [(set (match_operand:V8SI 0 "register_operand")
	(ashiftrt:V8SI (match_operand:V8SI 1 "register_operand")
		       (match_operand:V8SI 2 "nonimmediate_operand")))]
  "TARGET_AVX2")

(define_expand "vashl<mode>3"
  [(set (match_operand:VI12_128 0 "register_operand")
	(ashift:VI12_128
	  (match_operand:VI12_128 1 "register_operand")
	  (match_operand:VI12_128 2 "nonimmediate_operand")))]
  "TARGET_XOP"
{
  emit_insn (gen_xop_sha<mode>3 (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "vashl<mode>3"
  [(set (match_operand:VI48_128 0 "register_operand")
	(ashift:VI48_128
	  (match_operand:VI48_128 1 "register_operand")
	  (match_operand:VI48_128 2 "nonimmediate_operand")))]
  "TARGET_AVX2 || TARGET_XOP"
{
  if (!TARGET_AVX2)
    {
      operands[2] = force_reg (<MODE>mode, operands[2]);
      emit_insn (gen_xop_sha<mode>3 (operands[0], operands[1], operands[2]));
      DONE;
    }
})

(define_expand "vashl<mode>3"
  [(set (match_operand:VI48_512 0 "register_operand")
	(ashift:VI48_512
	  (match_operand:VI48_512 1 "register_operand")
	  (match_operand:VI48_512 2 "nonimmediate_operand")))]
  "TARGET_AVX512F")

(define_expand "vashl<mode>3"
  [(set (match_operand:VI48_256 0 "register_operand")
	(ashift:VI48_256
	  (match_operand:VI48_256 1 "register_operand")
	  (match_operand:VI48_256 2 "nonimmediate_operand")))]
  "TARGET_AVX2")

(define_insn "xop_sha<mode>3"
  [(set (match_operand:VI_128 0 "register_operand" "=x,x")
	(if_then_else:VI_128
	 (ge:VI_128
	  (match_operand:VI_128 2 "nonimmediate_operand" "x,m")
	  (const_int 0))
	 (ashift:VI_128
	  (match_operand:VI_128 1 "nonimmediate_operand" "xm,x")
	  (match_dup 2))
	 (ashiftrt:VI_128
	  (match_dup 1)
	  (neg:VI_128 (match_dup 2)))))]
  "TARGET_XOP && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "vpsha<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_extra" "2")
   (set_attr "mode" "TI")])

(define_insn "xop_shl<mode>3"
  [(set (match_operand:VI_128 0 "register_operand" "=x,x")
	(if_then_else:VI_128
	 (ge:VI_128
	  (match_operand:VI_128 2 "nonimmediate_operand" "x,m")
	  (const_int 0))
	 (ashift:VI_128
	  (match_operand:VI_128 1 "nonimmediate_operand" "xm,x")
	  (match_dup 2))
	 (lshiftrt:VI_128
	  (match_dup 1)
	  (neg:VI_128 (match_dup 2)))))]
  "TARGET_XOP && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "vpshl<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_extra" "2")
   (set_attr "mode" "TI")])

(define_expand "<shift_insn><mode>3"
  [(set (match_operand:VI1_AVX512 0 "register_operand")
	(any_shift:VI1_AVX512
	  (match_operand:VI1_AVX512 1 "register_operand")
	  (match_operand:SI 2 "nonmemory_operand")))]
  "TARGET_SSE2"
{
  if (TARGET_XOP && <MODE>mode == V16QImode)
    {
      bool negate = false;
      rtx (*gen) (rtx, rtx, rtx);
      rtx tmp, par;
      int i;

      if (<CODE> != ASHIFT)
	{
	  if (CONST_INT_P (operands[2]))
	    operands[2] = GEN_INT (-INTVAL (operands[2]));
	  else
	    negate = true;
	}
      par = gen_rtx_PARALLEL (V16QImode, rtvec_alloc (16));
      for (i = 0; i < 16; i++)
        XVECEXP (par, 0, i) = operands[2];

      tmp = gen_reg_rtx (V16QImode);
      emit_insn (gen_vec_initv16qiqi (tmp, par));

      if (negate)
	emit_insn (gen_negv16qi2 (tmp, tmp));

      gen = (<CODE> == LSHIFTRT ? gen_xop_shlv16qi3 : gen_xop_shav16qi3);
      emit_insn (gen (operands[0], operands[1], tmp));
    }
  else
    ix86_expand_vecop_qihi (<CODE>, operands[0], operands[1], operands[2]);
  DONE;
})

(define_expand "ashrv2di3"
  [(set (match_operand:V2DI 0 "register_operand")
	(ashiftrt:V2DI
	  (match_operand:V2DI 1 "register_operand")
	  (match_operand:DI 2 "nonmemory_operand")))]
  "TARGET_XOP || TARGET_AVX512VL"
{
  if (!TARGET_AVX512VL)
    {
      rtx reg = gen_reg_rtx (V2DImode);
      rtx par;
      bool negate = false;
      int i;

      if (CONST_INT_P (operands[2]))
	operands[2] = GEN_INT (-INTVAL (operands[2]));
      else
	negate = true;

      par = gen_rtx_PARALLEL (V2DImode, rtvec_alloc (2));
      for (i = 0; i < 2; i++)
	XVECEXP (par, 0, i) = operands[2];

      emit_insn (gen_vec_initv2didi (reg, par));

      if (negate)
	emit_insn (gen_negv2di2 (reg, reg));

      emit_insn (gen_xop_shav2di3 (operands[0], operands[1], reg));
      DONE;
    }
})

;; XOP FRCZ support
(define_insn "xop_frcz<mode>2"
  [(set (match_operand:FMAMODE 0 "register_operand" "=x")
	(unspec:FMAMODE
	 [(match_operand:FMAMODE 1 "nonimmediate_operand" "xm")]
	 UNSPEC_FRCZ))]
  "TARGET_XOP"
  "vfrcz<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt1")
   (set_attr "mode" "<MODE>")])

(define_expand "xop_vmfrcz<mode>2"
  [(set (match_operand:VF_128 0 "register_operand")
	(vec_merge:VF_128
	  (unspec:VF_128
	   [(match_operand:VF_128 1 "nonimmediate_operand")]
	   UNSPEC_FRCZ)
	  (match_dup 2)
	  (const_int 1)))]
  "TARGET_XOP"
  "operands[2] = CONST0_RTX (<MODE>mode);")

(define_insn "*xop_vmfrcz<mode>2"
  [(set (match_operand:VF_128 0 "register_operand" "=x")
	(vec_merge:VF_128
	  (unspec:VF_128
	   [(match_operand:VF_128 1 "nonimmediate_operand" "xm")]
	   UNSPEC_FRCZ)
	  (match_operand:VF_128 2 "const0_operand")
	  (const_int 1)))]
  "TARGET_XOP"
  "vfrcz<ssescalarmodesuffix>\t{%1, %0|%0, %<iptr>1}"
  [(set_attr "type" "ssecvt1")
   (set_attr "mode" "<MODE>")])

(define_insn "xop_maskcmp<mode>3"
  [(set (match_operand:VI_128 0 "register_operand" "=x")
	(match_operator:VI_128 1 "ix86_comparison_int_operator"
	 [(match_operand:VI_128 2 "register_operand" "x")
	  (match_operand:VI_128 3 "nonimmediate_operand" "xm")]))]
  "TARGET_XOP"
  "vpcom%Y1<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "sse4arg")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_rep" "0")
   (set_attr "prefix_extra" "2")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "xop_maskcmp_uns<mode>3"
  [(set (match_operand:VI_128 0 "register_operand" "=x")
	(match_operator:VI_128 1 "ix86_comparison_uns_operator"
	 [(match_operand:VI_128 2 "register_operand" "x")
	  (match_operand:VI_128 3 "nonimmediate_operand" "xm")]))]
  "TARGET_XOP"
  "vpcom%Y1u<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_rep" "0")
   (set_attr "prefix_extra" "2")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

;; Version of pcom*u* that is called from the intrinsics that allows pcomequ*
;; and pcomneu* not to be converted to the signed ones in case somebody needs
;; the exact instruction generated for the intrinsic.
(define_insn "xop_maskcmp_uns2<mode>3"
  [(set (match_operand:VI_128 0 "register_operand" "=x")
	(unspec:VI_128
	 [(match_operator:VI_128 1 "ix86_comparison_uns_operator"
	  [(match_operand:VI_128 2 "register_operand" "x")
	   (match_operand:VI_128 3 "nonimmediate_operand" "xm")])]
	 UNSPEC_XOP_UNSIGNED_CMP))]
  "TARGET_XOP"
  "vpcom%Y1u<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_extra" "2")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

;; Pcomtrue and pcomfalse support.  These are useless instructions, but are
;; being added here to be complete.
(define_insn "xop_pcom_tf<mode>3"
  [(set (match_operand:VI_128 0 "register_operand" "=x")
	(unspec:VI_128
	  [(match_operand:VI_128 1 "register_operand" "x")
	   (match_operand:VI_128 2 "nonimmediate_operand" "xm")
	   (match_operand:SI 3 "const_int_operand" "n")]
	  UNSPEC_XOP_TRUEFALSE))]
  "TARGET_XOP"
{
  return ((INTVAL (operands[3]) != 0)
	  ? "vpcomtrue<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
	  : "vpcomfalse<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}");
}
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_extra" "2")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "xop_vpermil2<mode>3"
  [(set (match_operand:VF_128_256 0 "register_operand" "=x,x")
	(unspec:VF_128_256
	  [(match_operand:VF_128_256 1 "register_operand" "x,x")
	   (match_operand:VF_128_256 2 "nonimmediate_operand" "x,m")
	   (match_operand:<sseintvecmode> 3 "nonimmediate_operand" "xm,x")
	   (match_operand:SI 4 "const_0_to_3_operand" "n,n")]
	  UNSPEC_VPERMIL2))]
  "TARGET_XOP"
  "vpermil2<ssemodesuffix>\t{%4, %3, %2, %1, %0|%0, %1, %2, %3, %4}"
  [(set_attr "type" "sse4arg")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "<MODE>")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "aesenc"
  [(set (match_operand:V2DI 0 "register_operand" "=x,x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0,x")
		       (match_operand:V2DI 2 "vector_operand" "xBm,xm")]
		      UNSPEC_AESENC))]
  "TARGET_AES"
  "@
   aesenc\t{%2, %0|%0, %2}
   vaesenc\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,vex")
   (set_attr "btver2_decode" "double,double")
   (set_attr "mode" "TI")])

(define_insn "aesenclast"
  [(set (match_operand:V2DI 0 "register_operand" "=x,x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0,x")
		       (match_operand:V2DI 2 "vector_operand" "xBm,xm")]
		      UNSPEC_AESENCLAST))]
  "TARGET_AES"
  "@
   aesenclast\t{%2, %0|%0, %2}
   vaesenclast\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,vex")
   (set_attr "btver2_decode" "double,double") 
   (set_attr "mode" "TI")])

(define_insn "aesdec"
  [(set (match_operand:V2DI 0 "register_operand" "=x,x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0,x")
		       (match_operand:V2DI 2 "vector_operand" "xBm,xm")]
		      UNSPEC_AESDEC))]
  "TARGET_AES"
  "@
   aesdec\t{%2, %0|%0, %2}
   vaesdec\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,vex")
   (set_attr "btver2_decode" "double,double") 
   (set_attr "mode" "TI")])

(define_insn "aesdeclast"
  [(set (match_operand:V2DI 0 "register_operand" "=x,x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0,x")
		       (match_operand:V2DI 2 "vector_operand" "xBm,xm")]
		      UNSPEC_AESDECLAST))]
  "TARGET_AES"
  "@
   aesdeclast\t{%2, %0|%0, %2}
   vaesdeclast\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,vex")
   (set_attr "btver2_decode" "double,double")
   (set_attr "mode" "TI")])

(define_insn "aesimc"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "vector_operand" "xBm")]
		      UNSPEC_AESIMC))]
  "TARGET_AES"
  "%vaesimc\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "aeskeygenassist"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "vector_operand" "xBm")
		      (match_operand:SI 2 "const_0_to_255_operand" "n")]
		     UNSPEC_AESKEYGENASSIST))]
  "TARGET_AES"
  "%vaeskeygenassist\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "pclmulqdq"
  [(set (match_operand:V2DI 0 "register_operand" "=x,x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0,x")
		      (match_operand:V2DI 2 "vector_operand" "xBm,xm")
		      (match_operand:SI 3 "const_0_to_255_operand" "n,n")]
		     UNSPEC_PCLMUL))]
  "TARGET_PCLMUL"
  "@
   pclmulqdq\t{%3, %2, %0|%0, %2, %3}
   vpclmulqdq\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "isa" "noavx,avx")
   (set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "orig,vex")
   (set_attr "mode" "TI")])

(define_expand "avx_vzeroall"
  [(match_par_dup 0 [(const_int 0)])]
  "TARGET_AVX"
{
  int nregs = TARGET_64BIT ? 16 : 8;
  int regno;

  operands[0] = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (nregs + 1));

  XVECEXP (operands[0], 0, 0)
    = gen_rtx_UNSPEC_VOLATILE (VOIDmode, gen_rtvec (1, const0_rtx),
			       UNSPECV_VZEROALL);

  for (regno = 0; regno < nregs; regno++)
    XVECEXP (operands[0], 0, regno + 1)
      = gen_rtx_SET (gen_rtx_REG (V8SImode, GET_SSE_REGNO (regno)),
		     CONST0_RTX (V8SImode));
})

(define_insn "*avx_vzeroall"
  [(match_parallel 0 "vzeroall_operation"
    [(unspec_volatile [(const_int 0)] UNSPECV_VZEROALL)])]
  "TARGET_AVX"
  "vzeroall"
  [(set_attr "type" "sse")
   (set_attr "modrm" "0")
   (set_attr "memory" "none")
   (set_attr "prefix" "vex")
   (set_attr "btver2_decode" "vector")
   (set_attr "mode" "OI")])

;; Clear the upper 128bits of AVX registers, equivalent to a NOP
;; if the upper 128bits are unused.
(define_insn "avx_vzeroupper"
  [(unspec_volatile [(const_int 0)] UNSPECV_VZEROUPPER)]
  "TARGET_AVX"
  "vzeroupper"
  [(set_attr "type" "sse")
   (set_attr "modrm" "0")
   (set_attr "memory" "none")
   (set_attr "prefix" "vex")
   (set_attr "btver2_decode" "vector")
   (set_attr "mode" "OI")])

(define_mode_attr pbroadcast_evex_isa
  [(V64QI "avx512bw") (V32QI "avx512bw") (V16QI "avx512bw")
   (V32HI "avx512bw") (V16HI "avx512bw") (V8HI "avx512bw")
   (V16SI "avx512f") (V8SI "avx512f") (V4SI "avx512f")
   (V8DI "avx512f") (V4DI "avx512f") (V2DI "avx512f")])

(define_insn "avx2_pbroadcast<mode>"
  [(set (match_operand:VI 0 "register_operand" "=x,v")
	(vec_duplicate:VI
	  (vec_select:<ssescalarmode>
	    (match_operand:<ssexmmmode> 1 "nonimmediate_operand" "xm,vm")
	    (parallel [(const_int 0)]))))]
  "TARGET_AVX2"
  "vpbroadcast<ssemodesuffix>\t{%1, %0|%0, %<iptr>1}"
  [(set_attr "isa" "*,<pbroadcast_evex_isa>")
   (set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex,evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "avx2_pbroadcast<mode>_1"
  [(set (match_operand:VI_256 0 "register_operand" "=x,x,v,v")
	(vec_duplicate:VI_256
	  (vec_select:<ssescalarmode>
	    (match_operand:VI_256 1 "nonimmediate_operand" "m,x,m,v")
	    (parallel [(const_int 0)]))))]
  "TARGET_AVX2"
  "@
   vpbroadcast<ssemodesuffix>\t{%1, %0|%0, %<iptr>1}
   vpbroadcast<ssemodesuffix>\t{%x1, %0|%0, %x1}
   vpbroadcast<ssemodesuffix>\t{%1, %0|%0, %<iptr>1}
   vpbroadcast<ssemodesuffix>\t{%x1, %0|%0, %x1}"
  [(set_attr "isa" "*,*,<pbroadcast_evex_isa>,<pbroadcast_evex_isa>")
   (set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx2_avx512>_permvar<mode><mask_name>"
  [(set (match_operand:VI48F_256_512 0 "register_operand" "=v")
	(unspec:VI48F_256_512
	  [(match_operand:VI48F_256_512 1 "nonimmediate_operand" "vm")
	   (match_operand:<sseintvecmode> 2 "register_operand" "v")]
	  UNSPEC_VPERMVAR))]
  "TARGET_AVX2 && <mask_mode512bit_condition>"
  "vperm<ssemodesuffix>\t{%1, %2, %0<mask_operand3>|%0<mask_operand3>, %2, %1}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "<mask_prefix2>")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_permvar<mode><mask_name>"
  [(set (match_operand:VI1_AVX512VL 0 "register_operand" "=v")
	(unspec:VI1_AVX512VL
	  [(match_operand:VI1_AVX512VL 1 "nonimmediate_operand" "vm")
	   (match_operand:<sseintvecmode> 2 "register_operand" "v")]
	  UNSPEC_VPERMVAR))]
  "TARGET_AVX512VBMI && <mask_mode512bit_condition>"
  "vperm<ssemodesuffix>\t{%1, %2, %0<mask_operand3>|%0<mask_operand3>, %2, %1}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "<mask_prefix2>")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_permvar<mode><mask_name>"
  [(set (match_operand:VI2_AVX512VL 0 "register_operand" "=v")
	(unspec:VI2_AVX512VL
	  [(match_operand:VI2_AVX512VL 1 "nonimmediate_operand" "vm")
	   (match_operand:<sseintvecmode> 2 "register_operand" "v")]
	  UNSPEC_VPERMVAR))]
  "TARGET_AVX512BW && <mask_mode512bit_condition>"
  "vperm<ssemodesuffix>\t{%1, %2, %0<mask_operand3>|%0<mask_operand3>, %2, %1}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "<mask_prefix2>")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "avx2_perm<mode>"
  [(match_operand:VI8F_256 0 "register_operand")
   (match_operand:VI8F_256 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_255_operand")]
  "TARGET_AVX2"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_avx2_perm<mode>_1 (operands[0], operands[1],
				    GEN_INT ((mask >> 0) & 3),
				    GEN_INT ((mask >> 2) & 3),
				    GEN_INT ((mask >> 4) & 3),
				    GEN_INT ((mask >> 6) & 3)));
  DONE;
})

(define_expand "avx512vl_perm<mode>_mask"
  [(match_operand:VI8F_256 0 "register_operand")
   (match_operand:VI8F_256 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_255_operand")
   (match_operand:VI8F_256 3 "nonimm_or_0_operand")
   (match_operand:<avx512fmaskmode> 4 "register_operand")]
  "TARGET_AVX512VL"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_<avx2_avx512>_perm<mode>_1_mask (operands[0], operands[1],
						  GEN_INT ((mask >> 0) & 3),
						  GEN_INT ((mask >> 2) & 3),
						  GEN_INT ((mask >> 4) & 3),
						  GEN_INT ((mask >> 6) & 3),
						  operands[3], operands[4]));
  DONE;
})

(define_insn "avx2_perm<mode>_1<mask_name>"
  [(set (match_operand:VI8F_256 0 "register_operand" "=v")
	(vec_select:VI8F_256
	  (match_operand:VI8F_256 1 "nonimmediate_operand" "vm")
	  (parallel [(match_operand 2 "const_0_to_3_operand")
		     (match_operand 3 "const_0_to_3_operand")
		     (match_operand 4 "const_0_to_3_operand")
		     (match_operand 5 "const_0_to_3_operand")])))]
  "TARGET_AVX2 && <mask_mode512bit_condition>"
{
  int mask = 0;
  mask |= INTVAL (operands[2]) << 0;
  mask |= INTVAL (operands[3]) << 2;
  mask |= INTVAL (operands[4]) << 4;
  mask |= INTVAL (operands[5]) << 6;
  operands[2] = GEN_INT (mask);
  return "vperm<ssemodesuffix>\t{%2, %1, %0<mask_operand6>|%0<mask_operand6>, %1, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix" "<mask_prefix2>")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "avx512f_perm<mode>"
  [(match_operand:V8FI 0 "register_operand")
   (match_operand:V8FI 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_255_operand")]
  "TARGET_AVX512F"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_avx512f_perm<mode>_1 (operands[0], operands[1],
				       GEN_INT ((mask >> 0) & 3),
				       GEN_INT ((mask >> 2) & 3),
				       GEN_INT ((mask >> 4) & 3),
				       GEN_INT ((mask >> 6) & 3),
				       GEN_INT (((mask >> 0) & 3) + 4),
				       GEN_INT (((mask >> 2) & 3) + 4),
				       GEN_INT (((mask >> 4) & 3) + 4),
				       GEN_INT (((mask >> 6) & 3) + 4)));
  DONE;
})

(define_expand "avx512f_perm<mode>_mask"
  [(match_operand:V8FI 0 "register_operand")
   (match_operand:V8FI 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_0_to_255_operand")
   (match_operand:V8FI 3 "nonimm_or_0_operand")
   (match_operand:<avx512fmaskmode> 4 "register_operand")]
  "TARGET_AVX512F"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_avx512f_perm<mode>_1_mask (operands[0], operands[1],
					    GEN_INT ((mask >> 0) & 3),
					    GEN_INT ((mask >> 2) & 3),
					    GEN_INT ((mask >> 4) & 3),
					    GEN_INT ((mask >> 6) & 3),
					    GEN_INT (((mask >> 0) & 3) + 4),
					    GEN_INT (((mask >> 2) & 3) + 4),
					    GEN_INT (((mask >> 4) & 3) + 4),
					    GEN_INT (((mask >> 6) & 3) + 4),
					    operands[3], operands[4]));
  DONE;
})

(define_insn "avx512f_perm<mode>_1<mask_name>"
  [(set (match_operand:V8FI 0 "register_operand" "=v")
	(vec_select:V8FI
	  (match_operand:V8FI 1 "nonimmediate_operand" "vm")
	  (parallel [(match_operand 2 "const_0_to_3_operand")
		     (match_operand 3 "const_0_to_3_operand")
		     (match_operand 4 "const_0_to_3_operand")
		     (match_operand 5 "const_0_to_3_operand")
		     (match_operand 6 "const_4_to_7_operand")
		     (match_operand 7 "const_4_to_7_operand")
		     (match_operand 8 "const_4_to_7_operand")
		     (match_operand 9 "const_4_to_7_operand")])))]
  "TARGET_AVX512F && <mask_mode512bit_condition>
   && (INTVAL (operands[2]) == (INTVAL (operands[6]) - 4)
       && INTVAL (operands[3]) == (INTVAL (operands[7]) - 4)
       && INTVAL (operands[4]) == (INTVAL (operands[8]) - 4)
       && INTVAL (operands[5]) == (INTVAL (operands[9]) - 4))"
{
  int mask = 0;
  mask |= INTVAL (operands[2]) << 0;
  mask |= INTVAL (operands[3]) << 2;
  mask |= INTVAL (operands[4]) << 4;
  mask |= INTVAL (operands[5]) << 6;
  operands[2] = GEN_INT (mask);
  return "vperm<ssemodesuffix>\t{%2, %1, %0<mask_operand10>|%0<mask_operand10>, %1, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix" "<mask_prefix2>")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "avx2_permv2ti"
  [(set (match_operand:V4DI 0 "register_operand" "=x")
	(unspec:V4DI
	  [(match_operand:V4DI 1 "register_operand" "x")
	   (match_operand:V4DI 2 "nonimmediate_operand" "xm")
	   (match_operand:SI 3 "const_0_to_255_operand" "n")]
	  UNSPEC_VPERMTI))]
  "TARGET_AVX2"
  "vperm2i128\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "OI")])

(define_insn "avx2_vec_dupv4df"
  [(set (match_operand:V4DF 0 "register_operand" "=v")
	(vec_duplicate:V4DF
	  (vec_select:DF
	    (match_operand:V2DF 1 "register_operand" "v")
	    (parallel [(const_int 0)]))))]
  "TARGET_AVX2"
  "vbroadcastsd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "V4DF")])

(define_insn "<avx512>_vec_dup<mode>_1"
  [(set (match_operand:VI_AVX512BW 0 "register_operand" "=v,v")
	(vec_duplicate:VI_AVX512BW
	  (vec_select:<ssescalarmode>
	    (match_operand:VI_AVX512BW 1 "nonimmediate_operand" "v,m")
	    (parallel [(const_int 0)]))))]
  "TARGET_AVX512F"
  "@
   vpbroadcast<ssemodesuffix>\t{%x1, %0|%0, %x1}
   vpbroadcast<ssemodesuffix>\t{%x1, %0|%0, %<iptr>1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_vec_dup<mode><mask_name>"
  [(set (match_operand:V48_AVX512VL 0 "register_operand" "=v")
	(vec_duplicate:V48_AVX512VL
	  (vec_select:<ssescalarmode>
	    (match_operand:<ssexmmmode> 1 "nonimmediate_operand" "vm")
	    (parallel [(const_int 0)]))))]
  "TARGET_AVX512F"
{
  /*  There is no DF broadcast (in AVX-512*) to 128b register.
      Mimic it with integer variant.  */
  if (<MODE>mode == V2DFmode)
    return "vpbroadcastq\t{%1, %0<mask_operand2>|%0<mask_operand2>, %q1}";

  return "v<sseintprefix>broadcast<bcstscalarsuff>\t{%1, %0<mask_operand2>|%0<mask_operand2>, %<iptr>1}";
}
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_vec_dup<mode><mask_name>"
  [(set (match_operand:VI12_AVX512VL 0 "register_operand" "=v")
	(vec_duplicate:VI12_AVX512VL
	  (vec_select:<ssescalarmode>
	    (match_operand:<ssexmmmode> 1 "nonimmediate_operand" "vm")
	    (parallel [(const_int 0)]))))]
  "TARGET_AVX512BW"
  "vpbroadcast<bcstscalarsuff>\t{%1, %0<mask_operand2>|%0<mask_operand2>, %<iptr>1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<mask_codefor>avx512f_broadcast<mode><mask_name>"
  [(set (match_operand:V16FI 0 "register_operand" "=v,v")
	(vec_duplicate:V16FI
	  (match_operand:<ssexmmmode> 1 "nonimmediate_operand" "v,m")))]
  "TARGET_AVX512F"
  "@
   vshuf<shuffletype>32x4\t{$0x0, %g1, %g1, %0<mask_operand2>|%0<mask_operand2>, %g1, %g1, 0x0}
   vbroadcast<shuffletype>32x4\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<mask_codefor>avx512f_broadcast<mode><mask_name>"
  [(set (match_operand:V8FI 0 "register_operand" "=v,v")
	(vec_duplicate:V8FI
	  (match_operand:<ssehalfvecmode> 1 "nonimmediate_operand" "v,m")))]
  "TARGET_AVX512F"
  "@
   vshuf<shuffletype>64x2\t{$0x44, %g1, %g1, %0<mask_operand2>|%0<mask_operand2>, %g1, %g1, 0x44}
   vbroadcast<shuffletype>64x4\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<mask_codefor><avx512>_vec_dup_gpr<mode><mask_name>"
  [(set (match_operand:VI12_AVX512VL 0 "register_operand" "=v,v")
	(vec_duplicate:VI12_AVX512VL
	  (match_operand:<ssescalarmode> 1 "nonimmediate_operand" "vm,r")))]
  "TARGET_AVX512BW"
  "@
   vpbroadcast<bcstscalarsuff>\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}
   vpbroadcast<bcstscalarsuff>\t{%k1, %0<mask_operand2>|%0<mask_operand2>, %k1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<mask_codefor><avx512>_vec_dup_gpr<mode><mask_name>"
  [(set (match_operand:V48_AVX512VL 0 "register_operand" "=v,v")
	(vec_duplicate:V48_AVX512VL
	  (match_operand:<ssescalarmode> 1 "nonimmediate_operand" "vm,r")))]
  "TARGET_AVX512F"
  "v<sseintprefix>broadcast<bcstscalarsuff>\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")
   (set (attr "enabled")
     (if_then_else (eq_attr "alternative" "1")
	(symbol_ref "GET_MODE_CLASS (<ssescalarmode>mode) == MODE_INT
		     && (<ssescalarmode>mode != DImode || TARGET_64BIT)")
	(const_int 1)))])

(define_insn "vec_dupv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=v,v,x")
	(vec_duplicate:V4SF
	  (match_operand:SF 1 "nonimmediate_operand" "Yv,m,0")))]
  "TARGET_SSE"
  "@
   vshufps\t{$0, %1, %1, %0|%0, %1, %1, 0}
   vbroadcastss\t{%1, %0|%0, %1}
   shufps\t{$0, %0, %0|%0, %0, 0}"
  [(set_attr "isa" "avx,avx,noavx")
   (set_attr "type" "sseshuf1,ssemov,sseshuf1")
   (set_attr "length_immediate" "1,0,1")
   (set_attr "prefix_extra" "0,1,*")
   (set_attr "prefix" "maybe_evex,maybe_evex,orig")
   (set_attr "mode" "V4SF")])

(define_insn "*vec_dupv4si"
  [(set (match_operand:V4SI 0 "register_operand"     "=v,v,x")
	(vec_duplicate:V4SI
	  (match_operand:SI 1 "nonimmediate_operand" "Yv,m,0")))]
  "TARGET_SSE"
  "@
   %vpshufd\t{$0, %1, %0|%0, %1, 0}
   vbroadcastss\t{%1, %0|%0, %1}
   shufps\t{$0, %0, %0|%0, %0, 0}"
  [(set_attr "isa" "sse2,avx,noavx")
   (set_attr "type" "sselog1,ssemov,sselog1")
   (set_attr "length_immediate" "1,0,1")
   (set_attr "prefix_extra" "0,1,*")
   (set_attr "prefix" "maybe_vex,maybe_evex,orig")
   (set_attr "mode" "TI,V4SF,V4SF")])

(define_insn "*vec_dupv2di"
  [(set (match_operand:V2DI 0 "register_operand"     "=x,v,v,x")
	(vec_duplicate:V2DI
	  (match_operand:DI 1 "nonimmediate_operand" " 0,Yv,m,0")))]
  "TARGET_SSE"
  "@
   punpcklqdq\t%0, %0
   vpunpcklqdq\t{%d1, %0|%0, %d1}
   %vmovddup\t{%1, %0|%0, %1}
   movlhps\t%0, %0"
  [(set_attr "isa" "sse2_noavx,avx,sse3,noavx")
   (set_attr "type" "sselog1,sselog1,sselog1,ssemov")
   (set_attr "prefix" "orig,maybe_evex,maybe_vex,orig")
   (set_attr "mode" "TI,TI,DF,V4SF")])

(define_insn "avx2_vbroadcasti128_<mode>"
  [(set (match_operand:VI_256 0 "register_operand" "=x,v,v")
	(vec_concat:VI_256
	  (match_operand:<ssehalfvecmode> 1 "memory_operand" "m,m,m")
	  (match_dup 1)))]
  "TARGET_AVX2"
  "@
   vbroadcasti128\t{%1, %0|%0, %1}
   vbroadcast<i128vldq>\t{%1, %0|%0, %1}
   vbroadcast<shuffletype>32x4\t{%1, %0|%0, %1}"
  [(set_attr "isa" "*,avx512dq,avx512vl")
   (set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex,evex,evex")
   (set_attr "mode" "OI")])

;; Modes handled by AVX vec_dup patterns.
(define_mode_iterator AVX_VEC_DUP_MODE
  [V8SI V8SF V4DI V4DF])
(define_mode_attr vecdupssescalarmodesuffix
  [(V8SF "ss") (V4DF "sd") (V8SI "ss") (V4DI "sd")])
;; Modes handled by AVX2 vec_dup patterns.
(define_mode_iterator AVX2_VEC_DUP_MODE
  [V32QI V16QI V16HI V8HI V8SI V4SI])

(define_insn "*vec_dup<mode>"
  [(set (match_operand:AVX2_VEC_DUP_MODE 0 "register_operand" "=x,x,v")
	(vec_duplicate:AVX2_VEC_DUP_MODE
	  (match_operand:<ssescalarmode> 1 "nonimmediate_operand" "m,x,$r")))]
  "TARGET_AVX2"
  "@
   v<sseintprefix>broadcast<bcstscalarsuff>\t{%1, %0|%0, %1}
   v<sseintprefix>broadcast<bcstscalarsuff>\t{%x1, %0|%0, %x1}
   #"
  [(set_attr "isa" "*,*,noavx512vl")
   (set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "<sseinsnmode>")
   (set (attr "preferred_for_speed")
     (cond [(eq_attr "alternative" "2")
	      (symbol_ref "TARGET_INTER_UNIT_MOVES_TO_VEC")
	   ]
	   (symbol_ref "true")))])

(define_insn "vec_dup<mode>"
  [(set (match_operand:AVX_VEC_DUP_MODE 0 "register_operand" "=x,x,x,v,x")
	(vec_duplicate:AVX_VEC_DUP_MODE
	  (match_operand:<ssescalarmode> 1 "nonimmediate_operand" "m,m,x,v,?x")))]
  "TARGET_AVX"
  "@
   v<sseintprefix>broadcast<bcstscalarsuff>\t{%1, %0|%0, %1}
   vbroadcast<vecdupssescalarmodesuffix>\t{%1, %0|%0, %1}
   v<sseintprefix>broadcast<bcstscalarsuff>\t{%x1, %0|%0, %x1}
   v<sseintprefix>broadcast<bcstscalarsuff>\t{%x1, %g0|%g0, %x1}
   #"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_evex")
   (set_attr "isa" "avx2,noavx2,avx2,avx512f,noavx2")
   (set_attr "mode" "<sseinsnmode>,V8SF,<sseinsnmode>,<sseinsnmode>,V8SF")])

(define_split
  [(set (match_operand:AVX2_VEC_DUP_MODE 0 "register_operand")
	(vec_duplicate:AVX2_VEC_DUP_MODE
	  (match_operand:<ssescalarmode> 1 "register_operand")))]
  "TARGET_AVX2
   /* Disable this splitter if avx512vl_vec_dup_gprv*[qhs]i insn is
      available, because then we can broadcast from GPRs directly.
      For V*[QH]I modes it requires both -mavx512vl and -mavx512bw,
      for V*SI mode it requires just -mavx512vl.  */
   && !(TARGET_AVX512VL
	&& (TARGET_AVX512BW || <ssescalarmode>mode == SImode))
   && reload_completed && GENERAL_REG_P (operands[1])"
  [(const_int 0)]
{
  emit_insn (gen_vec_setv4si_0 (gen_lowpart (V4SImode, operands[0]),
				CONST0_RTX (V4SImode),
				gen_lowpart (SImode, operands[1])));
  emit_insn (gen_avx2_pbroadcast<mode> (operands[0],
					gen_lowpart (<ssexmmmode>mode,
						     operands[0])));
  DONE;
})

(define_split
  [(set (match_operand:AVX_VEC_DUP_MODE 0 "register_operand")
	(vec_duplicate:AVX_VEC_DUP_MODE
	  (match_operand:<ssescalarmode> 1 "register_operand")))]
  "TARGET_AVX && !TARGET_AVX2 && reload_completed"
  [(set (match_dup 2)
	(vec_duplicate:<ssehalfvecmode> (match_dup 1)))
   (set (match_dup 0)
	(vec_concat:AVX_VEC_DUP_MODE (match_dup 2) (match_dup 2)))]
  "operands[2] = gen_lowpart (<ssehalfvecmode>mode, operands[0]);")

(define_insn "avx_vbroadcastf128_<mode>"
  [(set (match_operand:V_256 0 "register_operand" "=x,x,x,v,v,v,v")
	(vec_concat:V_256
	  (match_operand:<ssehalfvecmode> 1 "nonimmediate_operand" "m,0,?x,m,0,m,0")
	  (match_dup 1)))]
  "TARGET_AVX"
  "@
   vbroadcast<i128>\t{%1, %0|%0, %1}
   vinsert<i128>\t{$1, %1, %0, %0|%0, %0, %1, 1}
   vperm2<i128>\t{$0, %t1, %t1, %0|%0, %t1, %t1, 0}
   vbroadcast<i128vldq>\t{%1, %0|%0, %1}
   vinsert<i128vldq>\t{$1, %1, %0, %0|%0, %0, %1, 1}
   vbroadcast<shuffletype>32x4\t{%1, %0|%0, %1}
   vinsert<shuffletype>32x4\t{$1, %1, %0, %0|%0, %0, %1, 1}"
  [(set_attr "isa" "*,*,*,avx512dq,avx512dq,avx512vl,avx512vl")
   (set_attr "type" "ssemov,sselog1,sselog1,ssemov,sselog1,ssemov,sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "0,1,1,0,1,0,1")
   (set_attr "prefix" "vex,vex,vex,evex,evex,evex,evex")
   (set_attr "mode" "<sseinsnmode>")])

;; For broadcast[i|f]32x2.  Yes there is no v4sf version, only v4si.
(define_mode_iterator VI4F_BRCST32x2
  [V16SI (V8SI "TARGET_AVX512VL") (V4SI "TARGET_AVX512VL")
   V16SF (V8SF "TARGET_AVX512VL")])

(define_mode_attr 64x2mode
  [(V8DF "V2DF") (V8DI "V2DI") (V4DI "V2DI") (V4DF "V2DF")])

(define_mode_attr 32x2mode
  [(V16SF "V2SF") (V16SI "V2SI") (V8SI "V2SI")
  (V8SF "V2SF") (V4SI "V2SI")])

(define_insn "<mask_codefor>avx512dq_broadcast<mode><mask_name>"
  [(set (match_operand:VI4F_BRCST32x2 0 "register_operand" "=v")
	(vec_duplicate:VI4F_BRCST32x2
	  (vec_select:<32x2mode>
	    (match_operand:<ssexmmmode> 1 "nonimmediate_operand" "vm")
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_AVX512DQ"
  "vbroadcast<shuffletype>32x2\t{%1, %0<mask_operand2>|%0<mask_operand2>, %q1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<mask_codefor>avx512vl_broadcast<mode><mask_name>_1"
  [(set (match_operand:VI4F_256 0 "register_operand" "=v,v")
        (vec_duplicate:VI4F_256
         (match_operand:<ssexmmmode> 1 "nonimmediate_operand" "v,m")))]
  "TARGET_AVX512VL"
  "@
   vshuf<shuffletype>32x4\t{$0x0, %t1, %t1, %0<mask_operand2>|%0<mask_operand2>, %t1, %t1, 0x0}
   vbroadcast<shuffletype>32x4\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<mask_codefor>avx512dq_broadcast<mode><mask_name>_1"
  [(set (match_operand:V16FI 0 "register_operand" "=v,v")
       (vec_duplicate:V16FI
         (match_operand:<ssehalfvecmode> 1 "nonimmediate_operand" "v,m")))]
  "TARGET_AVX512DQ"
  "@
   vshuf<shuffletype>32x4\t{$0x44, %g1, %g1, %0<mask_operand2>|%0<mask_operand2>, %g1, %g1, 0x44}
   vbroadcast<shuffletype>32x8\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

;; For broadcast[i|f]64x2
(define_mode_iterator VI8F_BRCST64x2
  [V8DI V8DF (V4DI "TARGET_AVX512VL") (V4DF "TARGET_AVX512VL")])

(define_insn "<mask_codefor>avx512dq_broadcast<mode><mask_name>_1"
  [(set (match_operand:VI8F_BRCST64x2 0 "register_operand" "=v,v")
       (vec_duplicate:VI8F_BRCST64x2
         (match_operand:<64x2mode> 1 "nonimmediate_operand" "v,m")))]
  "TARGET_AVX512DQ"
  "@
   vshuf<shuffletype>64x2\t{$0x0, %<xtg_mode>1, %<xtg_mode>1, %0<mask_operand2>|%0<mask_operand2>, %<xtg_mode>1, %<xtg_mode>1, 0x0}
   vbroadcast<shuffletype>64x2\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "avx512cd_maskb_vec_dup<mode>"
  [(set (match_operand:VI8_AVX512VL 0 "register_operand" "=v")
	(vec_duplicate:VI8_AVX512VL
	  (zero_extend:DI
	    (match_operand:QI 1 "register_operand" "Yk"))))]
  "TARGET_AVX512CD"
  "vpbroadcastmb2q\t{%1, %0|%0, %1}"
  [(set_attr "type" "mskmov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

(define_insn "avx512cd_maskw_vec_dup<mode>"
  [(set (match_operand:VI4_AVX512VL 0 "register_operand" "=v")
	(vec_duplicate:VI4_AVX512VL
	  (zero_extend:SI
	    (match_operand:HI 1 "register_operand" "Yk"))))]
  "TARGET_AVX512CD"
  "vpbroadcastmw2d\t{%1, %0|%0, %1}"
  [(set_attr "type" "mskmov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "XI")])

;; Recognize broadcast as a vec_select as produced by builtin_vec_perm.
;; If it so happens that the input is in memory, use vbroadcast.
;; Otherwise use vpermilp (and in the case of 256-bit modes, vperm2f128).
(define_insn "*avx_vperm_broadcast_v4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=v,v,v")
	(vec_select:V4SF
	  (match_operand:V4SF 1 "nonimmediate_operand" "m,o,v")
	  (match_parallel 2 "avx_vbroadcast_operand"
	    [(match_operand 3 "const_int_operand" "C,n,n")])))]
  "TARGET_AVX"
{
  int elt = INTVAL (operands[3]);
  switch (which_alternative)
    {
    case 0:
    case 1:
      operands[1] = adjust_address_nv (operands[1], SFmode, elt * 4);
      return "vbroadcastss\t{%1, %0|%0, %k1}";
    case 2:
      operands[2] = GEN_INT (elt * 0x55);
      return "vpermilps\t{%2, %1, %0|%0, %1, %2}";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "ssemov,ssemov,sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "0,0,1")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "SF,SF,V4SF")])

(define_insn_and_split "*avx_vperm_broadcast_<mode>"
  [(set (match_operand:VF_256 0 "register_operand" "=v,v,v")
	(vec_select:VF_256
	  (match_operand:VF_256 1 "nonimmediate_operand" "m,o,?v")
	  (match_parallel 2 "avx_vbroadcast_operand"
	    [(match_operand 3 "const_int_operand" "C,n,n")])))]
  "TARGET_AVX"
  "#"
  "&& reload_completed && (<MODE>mode != V4DFmode || !TARGET_AVX2)"
  [(set (match_dup 0) (vec_duplicate:VF_256 (match_dup 1)))]
{
  rtx op0 = operands[0], op1 = operands[1];
  int elt = INTVAL (operands[3]);

  if (REG_P (op1))
    {
      int mask;

      if (TARGET_AVX2 && elt == 0)
	{
	  emit_insn (gen_vec_dup<mode> (op0, gen_lowpart (<ssescalarmode>mode,
							  op1)));
	  DONE;
	}

      /* Shuffle element we care about into all elements of the 128-bit lane.
	 The other lane gets shuffled too, but we don't care.  */
      if (<MODE>mode == V4DFmode)
	mask = (elt & 1 ? 15 : 0);
      else
	mask = (elt & 3) * 0x55;
      emit_insn (gen_avx_vpermil<mode> (op0, op1, GEN_INT (mask)));

      /* Shuffle the lane we care about into both lanes of the dest.  */
      mask = (elt / (<ssescalarnum> / 2)) * 0x11;
      if (EXT_REX_SSE_REG_P (op0))
	{
	  /* There is no EVEX VPERM2F128, but we can use either VBROADCASTSS
	     or VSHUFF128.  */
	  gcc_assert (<MODE>mode == V8SFmode);
	  if ((mask & 1) == 0)
	    emit_insn (gen_avx2_vec_dupv8sf (op0,
					     gen_lowpart (V4SFmode, op0)));
	  else
	    emit_insn (gen_avx512vl_shuf_f32x4_1 (op0, op0, op0,
						  GEN_INT (4), GEN_INT (5),
						  GEN_INT (6), GEN_INT (7),
						  GEN_INT (12), GEN_INT (13),
						  GEN_INT (14), GEN_INT (15)));
	  DONE;
	}

      emit_insn (gen_avx_vperm2f128<mode>3 (op0, op0, op0, GEN_INT (mask)));
      DONE;
    }

  operands[1] = adjust_address (op1, <ssescalarmode>mode,
				elt * GET_MODE_SIZE (<ssescalarmode>mode));
})

(define_expand "<sse2_avx_avx512f>_vpermil<mode><mask_name>"
  [(set (match_operand:VF2 0 "register_operand")
	(vec_select:VF2
	  (match_operand:VF2 1 "nonimmediate_operand")
	  (match_operand:SI 2 "const_0_to_255_operand")))]
  "TARGET_AVX && <mask_mode512bit_condition>"
{
  int mask = INTVAL (operands[2]);
  rtx perm[<ssescalarnum>];

  int i;
  for (i = 0; i < <ssescalarnum>; i = i + 2)
    {
      perm[i]     = GEN_INT (((mask >> i)       & 1) + i);
      perm[i + 1] = GEN_INT (((mask >> (i + 1)) & 1) + i);
    }

  operands[2]
    = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (<ssescalarnum>, perm));
})

(define_expand "<sse2_avx_avx512f>_vpermil<mode><mask_name>"
  [(set (match_operand:VF1 0 "register_operand")
	(vec_select:VF1
	  (match_operand:VF1 1 "nonimmediate_operand")
	  (match_operand:SI 2 "const_0_to_255_operand")))]
  "TARGET_AVX && <mask_mode512bit_condition>"
{
  int mask = INTVAL (operands[2]);
  rtx perm[<ssescalarnum>];

  int i;
  for (i = 0; i < <ssescalarnum>; i = i + 4)
    {
      perm[i]     = GEN_INT (((mask >> 0) & 3) + i);
      perm[i + 1] = GEN_INT (((mask >> 2) & 3) + i);
      perm[i + 2] = GEN_INT (((mask >> 4) & 3) + i);
      perm[i + 3] = GEN_INT (((mask >> 6) & 3) + i);
    }

  operands[2]
    = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (<ssescalarnum>, perm));
})

(define_insn "*<sse2_avx_avx512f>_vpermilp<mode><mask_name>"
  [(set (match_operand:VF 0 "register_operand" "=v")
	(vec_select:VF
	  (match_operand:VF 1 "nonimmediate_operand" "vm")
	  (match_parallel 2 ""
	    [(match_operand 3 "const_int_operand")])))]
  "TARGET_AVX && <mask_mode512bit_condition>
   && avx_vpermilp_parallel (operands[2], <MODE>mode)"
{
  int mask = avx_vpermilp_parallel (operands[2], <MODE>mode) - 1;
  operands[2] = GEN_INT (mask);
  return "vpermil<ssemodesuffix>\t{%2, %1, %0<mask_operand4>|%0<mask_operand4>, %1, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "<mask_prefix>")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<sse2_avx_avx512f>_vpermilvar<mode>3<mask_name>"
  [(set (match_operand:VF 0 "register_operand" "=v")
	(unspec:VF
	  [(match_operand:VF 1 "register_operand" "v")
	   (match_operand:<sseintvecmode> 2 "nonimmediate_operand" "vm")]
	  UNSPEC_VPERMIL))]
  "TARGET_AVX && <mask_mode512bit_condition>"
  "vpermil<ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "btver2_decode" "vector")
   (set_attr "prefix" "<mask_prefix>")
   (set_attr "mode" "<sseinsnmode>")])

(define_mode_iterator VPERMI2
  [V16SI V16SF V8DI V8DF
   (V8SI "TARGET_AVX512VL") (V8SF "TARGET_AVX512VL")
   (V4DI "TARGET_AVX512VL") (V4DF "TARGET_AVX512VL")
   (V4SI "TARGET_AVX512VL") (V4SF "TARGET_AVX512VL")
   (V2DI "TARGET_AVX512VL") (V2DF "TARGET_AVX512VL")
   (V32HI "TARGET_AVX512BW") (V16HI "TARGET_AVX512BW && TARGET_AVX512VL")
   (V8HI "TARGET_AVX512BW && TARGET_AVX512VL")
   (V64QI "TARGET_AVX512VBMI") (V32QI "TARGET_AVX512VBMI && TARGET_AVX512VL")
   (V16QI "TARGET_AVX512VBMI && TARGET_AVX512VL")])

(define_mode_iterator VPERMI2I
  [V16SI V8DI
   (V8SI "TARGET_AVX512VL") (V4SI "TARGET_AVX512VL")
   (V4DI "TARGET_AVX512VL") (V2DI "TARGET_AVX512VL")
   (V32HI "TARGET_AVX512BW") (V16HI "TARGET_AVX512BW && TARGET_AVX512VL")
   (V8HI "TARGET_AVX512BW && TARGET_AVX512VL")
   (V64QI "TARGET_AVX512VBMI") (V32QI "TARGET_AVX512VBMI && TARGET_AVX512VL")
   (V16QI "TARGET_AVX512VBMI && TARGET_AVX512VL")])

(define_expand "<avx512>_vpermi2var<mode>3_mask"
  [(set (match_operand:VPERMI2 0 "register_operand")
	(vec_merge:VPERMI2
	  (unspec:VPERMI2
	    [(match_operand:<sseintvecmode> 2 "register_operand")
	     (match_operand:VPERMI2 1 "register_operand")
	     (match_operand:VPERMI2 3 "nonimmediate_operand")]
	    UNSPEC_VPERMT2)
	  (match_dup 5)
	  (match_operand:<avx512fmaskmode> 4 "register_operand")))]
  "TARGET_AVX512F"
{
  operands[2] = force_reg (<sseintvecmode>mode, operands[2]);
  operands[5] = gen_lowpart (<MODE>mode, operands[2]);
})

(define_insn "*<avx512>_vpermi2var<mode>3_mask"
  [(set (match_operand:VPERMI2I 0 "register_operand" "=v")
	(vec_merge:VPERMI2I
	  (unspec:VPERMI2I
	    [(match_operand:<sseintvecmode> 2 "register_operand" "0")
	     (match_operand:VPERMI2I 1 "register_operand" "v")
	     (match_operand:VPERMI2I 3 "nonimmediate_operand" "vm")]
	    UNSPEC_VPERMT2)
	  (match_dup 2)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512F"
  "vpermi2<ssemodesuffix>\t{%3, %1, %0%{%4%}|%0%{%4%}, %1, %3}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "*<avx512>_vpermi2var<mode>3_mask"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VF_AVX512VL
	  (unspec:VF_AVX512VL
	    [(match_operand:<sseintvecmode> 2 "register_operand" "0")
	     (match_operand:VF_AVX512VL 1 "register_operand" "v")
	     (match_operand:VF_AVX512VL 3 "nonimmediate_operand" "vm")]
	    UNSPEC_VPERMT2)
	  (subreg:VF_AVX512VL (match_dup 2) 0)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512F"
  "vpermi2<ssemodesuffix>\t{%3, %1, %0%{%4%}|%0%{%4%}, %1, %3}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "<avx512>_vpermt2var<mode>3_maskz"
  [(match_operand:VPERMI2 0 "register_operand")
   (match_operand:<sseintvecmode> 1 "register_operand")
   (match_operand:VPERMI2 2 "register_operand")
   (match_operand:VPERMI2 3 "nonimmediate_operand")
   (match_operand:<avx512fmaskmode> 4 "register_operand")]
  "TARGET_AVX512F"
{
  emit_insn (gen_<avx512>_vpermt2var<mode>3_maskz_1 (
	operands[0], operands[1], operands[2], operands[3],
	CONST0_RTX (<MODE>mode), operands[4]));
  DONE;
})

(define_insn "<avx512>_vpermt2var<mode>3<sd_maskz_name>"
  [(set (match_operand:VPERMI2 0 "register_operand" "=v,v")
	(unspec:VPERMI2
	  [(match_operand:<sseintvecmode> 1 "register_operand" "v,0")
	   (match_operand:VPERMI2 2 "register_operand" "0,v")
	   (match_operand:VPERMI2 3 "nonimmediate_operand" "vm,vm")]
	  UNSPEC_VPERMT2))]
  "TARGET_AVX512F"
  "@
   vpermt2<ssemodesuffix>\t{%3, %1, %0<sd_mask_op4>|%0<sd_mask_op4>, %1, %3}
   vpermi2<ssemodesuffix>\t{%3, %2, %0<sd_mask_op4>|%0<sd_mask_op4>, %2, %3}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_vpermt2var<mode>3_mask"
  [(set (match_operand:VPERMI2 0 "register_operand" "=v")
	(vec_merge:VPERMI2
	  (unspec:VPERMI2
	    [(match_operand:<sseintvecmode> 1 "register_operand" "v")
	    (match_operand:VPERMI2 2 "register_operand" "0")
	    (match_operand:VPERMI2 3 "nonimmediate_operand" "vm")]
	    UNSPEC_VPERMT2)
	  (match_dup 2)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512F"
  "vpermt2<ssemodesuffix>\t{%3, %1, %0%{%4%}|%0%{%4%}, %1, %3}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "avx_vperm2f128<mode>3"
  [(set (match_operand:AVX256MODE2P 0 "register_operand")
	(unspec:AVX256MODE2P
	  [(match_operand:AVX256MODE2P 1 "register_operand")
	   (match_operand:AVX256MODE2P 2 "nonimmediate_operand")
	   (match_operand:SI 3 "const_0_to_255_operand")]
	  UNSPEC_VPERMIL2F128))]
  "TARGET_AVX"
{
  int mask = INTVAL (operands[3]);
  if ((mask & 0x88) == 0)
    {
      rtx perm[<ssescalarnum>], t1, t2;
      int i, base, nelt = <ssescalarnum>, nelt2 = nelt / 2;

      base = (mask & 3) * nelt2;
      for (i = 0; i < nelt2; ++i)
	perm[i] = GEN_INT (base + i);

      base = ((mask >> 4) & 3) * nelt2;
      for (i = 0; i < nelt2; ++i)
	perm[i + nelt2] = GEN_INT (base + i);

      t2 = gen_rtx_VEC_CONCAT (<ssedoublevecmode>mode,
			       operands[1], operands[2]);
      t1 = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (nelt, perm));
      t2 = gen_rtx_VEC_SELECT (<MODE>mode, t2, t1);
      t2 = gen_rtx_SET (operands[0], t2);
      emit_insn (t2);
      DONE;
    }
})

;; Note that bits 7 and 3 of the imm8 allow lanes to be zeroed, which
;; means that in order to represent this properly in rtl we'd have to
;; nest *another* vec_concat with a zero operand and do the select from
;; a 4x wide vector.  That doesn't seem very nice.
(define_insn "*avx_vperm2f128<mode>_full"
  [(set (match_operand:AVX256MODE2P 0 "register_operand" "=x")
	(unspec:AVX256MODE2P
	  [(match_operand:AVX256MODE2P 1 "register_operand" "x")
	   (match_operand:AVX256MODE2P 2 "nonimmediate_operand" "xm")
	   (match_operand:SI 3 "const_0_to_255_operand" "n")]
	  UNSPEC_VPERMIL2F128))]
  "TARGET_AVX"
  "vperm2<i128>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "*avx_vperm2f128<mode>_nozero"
  [(set (match_operand:AVX256MODE2P 0 "register_operand" "=x")
	(vec_select:AVX256MODE2P
	  (vec_concat:<ssedoublevecmode>
	    (match_operand:AVX256MODE2P 1 "register_operand" "x")
	    (match_operand:AVX256MODE2P 2 "nonimmediate_operand" "xm"))
	  (match_parallel 3 ""
	    [(match_operand 4 "const_int_operand")])))]
  "TARGET_AVX
   && avx_vperm2f128_parallel (operands[3], <MODE>mode)"
{
  int mask = avx_vperm2f128_parallel (operands[3], <MODE>mode) - 1;
  if (mask == 0x12)
    return "vinsert<i128>\t{$0, %x2, %1, %0|%0, %1, %x2, 0}";
  if (mask == 0x20)
    return "vinsert<i128>\t{$1, %x2, %1, %0|%0, %1, %x2, 1}";
  operands[3] = GEN_INT (mask);
  return "vperm2<i128>\t{%3, %2, %1, %0|%0, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "*ssse3_palignr<mode>_perm"
  [(set (match_operand:V_128 0 "register_operand" "=x,x,v")
      (vec_select:V_128
	(match_operand:V_128 1 "register_operand" "0,x,v")
	(match_parallel 2 "palignr_operand"
	  [(match_operand 3 "const_int_operand" "n,n,n")])))]
  "TARGET_SSSE3"
{
  operands[2] = (GEN_INT (INTVAL (operands[3])
		 * GET_MODE_UNIT_SIZE (GET_MODE (operands[0]))));

  switch (which_alternative)
    {
    case 0:
      return "palignr\t{%2, %1, %0|%0, %1, %2}";
    case 1:
    case 2:
      return "vpalignr\t{%2, %1, %1, %0|%0, %1, %1, %2}";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "isa" "noavx,avx,avx512bw")
   (set_attr "type" "sseishft")
   (set_attr "atom_unit" "sishuf")
   (set_attr "prefix_data16" "1,*,*")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "orig,vex,evex")])

(define_expand "avx512vl_vinsert<mode>"
  [(match_operand:VI48F_256 0 "register_operand")
   (match_operand:VI48F_256 1 "register_operand")
   (match_operand:<ssehalfvecmode> 2 "nonimmediate_operand")
   (match_operand:SI 3 "const_0_to_1_operand")
   (match_operand:VI48F_256 4 "register_operand")
   (match_operand:<avx512fmaskmode> 5 "register_operand")]
  "TARGET_AVX512VL"
{
  rtx (*insn)(rtx, rtx, rtx, rtx, rtx);

  switch (INTVAL (operands[3]))
    {
    case 0:
      insn = gen_vec_set_lo_<mode>_mask;
      break;
    case 1:
      insn = gen_vec_set_hi_<mode>_mask;
      break;
    default:
      gcc_unreachable ();
    }

  emit_insn (insn (operands[0], operands[1], operands[2], operands[4],
		   operands[5]));
  DONE;
})

(define_expand "avx_vinsertf128<mode>"
  [(match_operand:V_256 0 "register_operand")
   (match_operand:V_256 1 "register_operand")
   (match_operand:<ssehalfvecmode> 2 "nonimmediate_operand")
   (match_operand:SI 3 "const_0_to_1_operand")]
  "TARGET_AVX"
{
  rtx (*insn)(rtx, rtx, rtx);

  switch (INTVAL (operands[3]))
    {
    case 0:
      insn = gen_vec_set_lo_<mode>;
      break;
    case 1:
      insn = gen_vec_set_hi_<mode>;
      break;
    default:
      gcc_unreachable ();
    }

  emit_insn (insn (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "vec_set_lo_<mode><mask_name>"
  [(set (match_operand:VI8F_256 0 "register_operand" "=v")
	(vec_concat:VI8F_256
	  (match_operand:<ssehalfvecmode> 2 "nonimmediate_operand" "vm")
	  (vec_select:<ssehalfvecmode>
	    (match_operand:VI8F_256 1 "register_operand" "v")
	    (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_AVX && <mask_avx512dq_condition>"
{
  if (TARGET_AVX512DQ)
    return "vinsert<shuffletype>64x2\t{$0x0, %2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2, 0x0}";
  else if (TARGET_AVX512VL)
    return "vinsert<shuffletype>32x4\t{$0x0, %2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2, 0x0}";
  else
    return "vinsert<i128>\t{$0x0, %2, %1, %0|%0, %1, %2, 0x0}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vec_set_hi_<mode><mask_name>"
  [(set (match_operand:VI8F_256 0 "register_operand" "=v")
	(vec_concat:VI8F_256
	  (vec_select:<ssehalfvecmode>
	    (match_operand:VI8F_256 1 "register_operand" "v")
	    (parallel [(const_int 0) (const_int 1)]))
	  (match_operand:<ssehalfvecmode> 2 "nonimmediate_operand" "vm")))]
  "TARGET_AVX && <mask_avx512dq_condition>"
{
  if (TARGET_AVX512DQ)
    return "vinsert<shuffletype>64x2\t{$0x1, %2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2, 0x1}";
  else if (TARGET_AVX512VL)
    return "vinsert<shuffletype>32x4\t{$0x1, %2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2, 0x1}";
  else
    return "vinsert<i128>\t{$0x1, %2, %1, %0|%0, %1, %2, 0x1}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vec_set_lo_<mode><mask_name>"
  [(set (match_operand:VI4F_256 0 "register_operand" "=v")
	(vec_concat:VI4F_256
	  (match_operand:<ssehalfvecmode> 2 "nonimmediate_operand" "vm")
	  (vec_select:<ssehalfvecmode>
	    (match_operand:VI4F_256 1 "register_operand" "v")
	    (parallel [(const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))))]
  "TARGET_AVX"
{
  if (TARGET_AVX512VL)
    return "vinsert<shuffletype>32x4\t{$0x0, %2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2, 0x0}";
  else
    return "vinsert<i128>\t{$0x0, %2, %1, %0|%0, %1, %2, 0x0}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vec_set_hi_<mode><mask_name>"
  [(set (match_operand:VI4F_256 0 "register_operand" "=v")
	(vec_concat:VI4F_256
	  (vec_select:<ssehalfvecmode>
	    (match_operand:VI4F_256 1 "register_operand" "v")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)]))
	  (match_operand:<ssehalfvecmode> 2 "nonimmediate_operand" "vm")))]
  "TARGET_AVX"
{
  if (TARGET_AVX512VL)
    return "vinsert<shuffletype>32x4\t{$0x1, %2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2, 0x1}";
  else
    return "vinsert<i128>\t{$0x1, %2, %1, %0|%0, %1, %2, 0x1}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vec_set_lo_v16hi"
  [(set (match_operand:V16HI 0 "register_operand" "=x,v")
	(vec_concat:V16HI
	  (match_operand:V8HI 2 "nonimmediate_operand" "xm,vm")
	  (vec_select:V8HI
	    (match_operand:V16HI 1 "register_operand" "x,v")
	    (parallel [(const_int 8) (const_int 9)
		       (const_int 10) (const_int 11)
		       (const_int 12) (const_int 13)
		       (const_int 14) (const_int 15)]))))]
  "TARGET_AVX"
  "@
   vinsert%~128\t{$0x0, %2, %1, %0|%0, %1, %2, 0x0}
   vinserti32x4\t{$0x0, %2, %1, %0|%0, %1, %2, 0x0}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex,evex")
   (set_attr "mode" "OI")])

(define_insn "vec_set_hi_v16hi"
  [(set (match_operand:V16HI 0 "register_operand" "=x,v")
	(vec_concat:V16HI
	  (vec_select:V8HI
	    (match_operand:V16HI 1 "register_operand" "x,v")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)
		       (const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))
	  (match_operand:V8HI 2 "nonimmediate_operand" "xm,vm")))]
  "TARGET_AVX"
  "@
   vinsert%~128\t{$0x1, %2, %1, %0|%0, %1, %2, 0x1}
   vinserti32x4\t{$0x1, %2, %1, %0|%0, %1, %2, 0x1}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex,evex")
   (set_attr "mode" "OI")])

(define_insn "vec_set_lo_v32qi"
  [(set (match_operand:V32QI 0 "register_operand" "=x,v")
	(vec_concat:V32QI
	  (match_operand:V16QI 2 "nonimmediate_operand" "xm,v")
	  (vec_select:V16QI
	    (match_operand:V32QI 1 "register_operand" "x,v")
	    (parallel [(const_int 16) (const_int 17)
		       (const_int 18) (const_int 19)
		       (const_int 20) (const_int 21)
		       (const_int 22) (const_int 23)
		       (const_int 24) (const_int 25)
		       (const_int 26) (const_int 27)
		       (const_int 28) (const_int 29)
		       (const_int 30) (const_int 31)]))))]
  "TARGET_AVX"
  "@
   vinsert%~128\t{$0x0, %2, %1, %0|%0, %1, %2, 0x0}
   vinserti32x4\t{$0x0, %2, %1, %0|%0, %1, %2, 0x0}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex,evex")
   (set_attr "mode" "OI")])

(define_insn "vec_set_hi_v32qi"
  [(set (match_operand:V32QI 0 "register_operand" "=x,v")
	(vec_concat:V32QI
	  (vec_select:V16QI
	    (match_operand:V32QI 1 "register_operand" "x,v")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)
		       (const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)
		       (const_int 8) (const_int 9)
		       (const_int 10) (const_int 11)
		       (const_int 12) (const_int 13)
		       (const_int 14) (const_int 15)]))
	  (match_operand:V16QI 2 "nonimmediate_operand" "xm,vm")))]
  "TARGET_AVX"
  "@
   vinsert%~128\t{$0x1, %2, %1, %0|%0, %1, %2, 0x1}
   vinserti32x4\t{$0x1, %2, %1, %0|%0, %1, %2, 0x1}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex,evex")
   (set_attr "mode" "OI")])

(define_insn "<avx_avx2>_maskload<ssemodesuffix><avxsizesuffix>"
  [(set (match_operand:V48_AVX2 0 "register_operand" "=x")
	(unspec:V48_AVX2
	  [(match_operand:<sseintvecmode> 2 "register_operand" "x")
	   (match_operand:V48_AVX2 1 "memory_operand" "m")]
	  UNSPEC_MASKMOV))]
  "TARGET_AVX"
  "v<sseintprefix>maskmov<ssemodesuffix>\t{%1, %2, %0|%0, %2, %1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "btver2_decode" "vector")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx_avx2>_maskstore<ssemodesuffix><avxsizesuffix>"
  [(set (match_operand:V48_AVX2 0 "memory_operand" "+m")
	(unspec:V48_AVX2
	  [(match_operand:<sseintvecmode> 1 "register_operand" "x")
	   (match_operand:V48_AVX2 2 "register_operand" "x")
	   (match_dup 0)]
	  UNSPEC_MASKMOV))]
  "TARGET_AVX"
  "v<sseintprefix>maskmov<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "btver2_decode" "vector") 
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "maskload<mode><sseintvecmodelower>"
  [(set (match_operand:V48_AVX2 0 "register_operand")
	(unspec:V48_AVX2
	  [(match_operand:<sseintvecmode> 2 "register_operand")
	   (match_operand:V48_AVX2 1 "memory_operand")]
	  UNSPEC_MASKMOV))]
  "TARGET_AVX")

(define_expand "maskload<mode><avx512fmaskmodelower>"
  [(set (match_operand:V48_AVX512VL 0 "register_operand")
	(vec_merge:V48_AVX512VL
	  (match_operand:V48_AVX512VL 1 "memory_operand")
	  (match_dup 0)
	  (match_operand:<avx512fmaskmode> 2 "register_operand")))]
  "TARGET_AVX512F")

(define_expand "maskload<mode><avx512fmaskmodelower>"
  [(set (match_operand:VI12_AVX512VL 0 "register_operand")
	(vec_merge:VI12_AVX512VL
	  (match_operand:VI12_AVX512VL 1 "memory_operand")
	  (match_dup 0)
	  (match_operand:<avx512fmaskmode> 2 "register_operand")))]
  "TARGET_AVX512BW")

(define_expand "maskstore<mode><sseintvecmodelower>"
  [(set (match_operand:V48_AVX2 0 "memory_operand")
	(unspec:V48_AVX2
	  [(match_operand:<sseintvecmode> 2 "register_operand")
	   (match_operand:V48_AVX2 1 "register_operand")
	   (match_dup 0)]
	  UNSPEC_MASKMOV))]
  "TARGET_AVX")

(define_expand "maskstore<mode><avx512fmaskmodelower>"
  [(set (match_operand:V48_AVX512VL 0 "memory_operand")
	(vec_merge:V48_AVX512VL
	  (match_operand:V48_AVX512VL 1 "register_operand")
	  (match_dup 0)
	  (match_operand:<avx512fmaskmode> 2 "register_operand")))]
  "TARGET_AVX512F")

(define_expand "maskstore<mode><avx512fmaskmodelower>"
  [(set (match_operand:VI12_AVX512VL 0 "memory_operand")
	(vec_merge:VI12_AVX512VL
	  (match_operand:VI12_AVX512VL 1 "register_operand")
	  (match_dup 0)
	  (match_operand:<avx512fmaskmode> 2 "register_operand")))]
  "TARGET_AVX512BW")

(define_expand "cbranch<mode>4"
  [(set (reg:CC FLAGS_REG)
	(compare:CC (match_operand:VI48_AVX 1 "register_operand")
		    (match_operand:VI48_AVX 2 "nonimmediate_operand")))
   (set (pc) (if_then_else
	       (match_operator 0 "bt_comparison_operator"
		[(reg:CC FLAGS_REG) (const_int 0)])
	       (label_ref (match_operand 3))
	       (pc)))]
  "TARGET_SSE4_1"
{
  ix86_expand_branch (GET_CODE (operands[0]),
		      operands[1], operands[2], operands[3]);
  DONE;
})


(define_insn_and_split "avx_<castmode><avxsizesuffix>_<castmode>"
  [(set (match_operand:AVX256MODE2P 0 "nonimmediate_operand" "=x,m")
	(unspec:AVX256MODE2P
	  [(match_operand:<ssehalfvecmode> 1 "nonimmediate_operand" "xm,x")]
	  UNSPEC_CAST))]
  "TARGET_AVX && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 1))]
{
  if (REG_P (operands[0]))
    operands[0] = gen_lowpart (<ssehalfvecmode>mode, operands[0]);
  else
    operands[1] = lowpart_subreg (<MODE>mode, operands[1],
				  <ssehalfvecmode>mode);
})

;; Modes handled by vec_init expanders.
(define_mode_iterator VEC_INIT_MODE
  [(V64QI "TARGET_AVX512F") (V32QI "TARGET_AVX") V16QI
   (V32HI "TARGET_AVX512F") (V16HI "TARGET_AVX") V8HI
   (V16SI "TARGET_AVX512F") (V8SI "TARGET_AVX") V4SI
   (V8DI "TARGET_AVX512F") (V4DI "TARGET_AVX") V2DI
   (V16SF "TARGET_AVX512F") (V8SF "TARGET_AVX") V4SF
   (V8DF "TARGET_AVX512F") (V4DF "TARGET_AVX") (V2DF "TARGET_SSE2")
   (V4TI "TARGET_AVX512F") (V2TI "TARGET_AVX")])

;; Likewise, but for initialization from half sized vectors.
;; Thus, these are all VEC_INIT_MODE modes except V2??.
(define_mode_iterator VEC_INIT_HALF_MODE
  [(V64QI "TARGET_AVX512F") (V32QI "TARGET_AVX") V16QI
   (V32HI "TARGET_AVX512F") (V16HI "TARGET_AVX") V8HI
   (V16SI "TARGET_AVX512F") (V8SI "TARGET_AVX") V4SI
   (V8DI "TARGET_AVX512F") (V4DI "TARGET_AVX")
   (V16SF "TARGET_AVX512F") (V8SF "TARGET_AVX") V4SF
   (V8DF "TARGET_AVX512F") (V4DF "TARGET_AVX")
   (V4TI "TARGET_AVX512F")])

(define_expand "vec_init<mode><ssescalarmodelower>"
  [(match_operand:VEC_INIT_MODE 0 "register_operand")
   (match_operand 1)]
  "TARGET_SSE"
{
  ix86_expand_vector_init (false, operands[0], operands[1]);
  DONE;
})

(define_expand "vec_init<mode><ssehalfvecmodelower>"
  [(match_operand:VEC_INIT_HALF_MODE 0 "register_operand")
   (match_operand 1)]
  "TARGET_SSE"
{
  ix86_expand_vector_init (false, operands[0], operands[1]);
  DONE;
})

(define_insn "<avx2_avx512>_ashrv<mode><mask_name>"
  [(set (match_operand:VI48_AVX512F_AVX512VL 0 "register_operand" "=v")
	(ashiftrt:VI48_AVX512F_AVX512VL
	  (match_operand:VI48_AVX512F_AVX512VL 1 "register_operand" "v")
	  (match_operand:VI48_AVX512F_AVX512VL 2 "nonimmediate_operand" "vm")))]
  "TARGET_AVX2 && <mask_mode512bit_condition>"
  "vpsrav<ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx2_avx512>_ashrv<mode><mask_name>"
  [(set (match_operand:VI2_AVX512VL 0 "register_operand" "=v")
	(ashiftrt:VI2_AVX512VL
	  (match_operand:VI2_AVX512VL 1 "register_operand" "v")
	  (match_operand:VI2_AVX512VL 2 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512BW"
  "vpsravw\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx2_avx512>_<shift_insn>v<mode><mask_name>"
  [(set (match_operand:VI48_AVX512F 0 "register_operand" "=v")
	(any_lshift:VI48_AVX512F
	  (match_operand:VI48_AVX512F 1 "register_operand" "v")
	  (match_operand:VI48_AVX512F 2 "nonimmediate_operand" "vm")))]
  "TARGET_AVX2 && <mask_mode512bit_condition>"
  "vp<vshift>v<ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx2_avx512>_<shift_insn>v<mode><mask_name>"
  [(set (match_operand:VI2_AVX512VL 0 "register_operand" "=v")
	(any_lshift:VI2_AVX512VL
	  (match_operand:VI2_AVX512VL 1 "register_operand" "v")
	  (match_operand:VI2_AVX512VL 2 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512BW"
  "vp<vshift>v<ssemodesuffix>\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "avx_vec_concat<mode>"
  [(set (match_operand:V_256_512 0 "register_operand" "=x,v,x,Yv")
	(vec_concat:V_256_512
	  (match_operand:<ssehalfvecmode> 1 "register_operand" "x,v,x,v")
	  (match_operand:<ssehalfvecmode> 2 "nonimm_or_0_operand" "xm,vm,C,C")))]
  "TARGET_AVX"
{
  switch (which_alternative)
    {
    case 0:
      return "vinsert<i128>\t{$0x1, %2, %<xtg_mode>1, %0|%0, %<xtg_mode>1, %2, 0x1}";
    case 1:
      if (<MODE_SIZE> == 64)
	{
	  if (TARGET_AVX512DQ && GET_MODE_SIZE (<ssescalarmode>mode) == 4)
	    return "vinsert<shuffletype>32x8\t{$0x1, %2, %<xtg_mode>1, %0|%0, %<xtg_mode>1, %2, 0x1}";
	  else
	    return "vinsert<shuffletype>64x4\t{$0x1, %2, %<xtg_mode>1, %0|%0, %<xtg_mode>1, %2, 0x1}";
	}
      else
	{
	  if (TARGET_AVX512DQ && GET_MODE_SIZE (<ssescalarmode>mode) == 8)
	    return "vinsert<shuffletype>64x2\t{$0x1, %2, %<xtg_mode>1, %0|%0, %<xtg_mode>1, %2, 0x1}";
	  else
	    return "vinsert<shuffletype>32x4\t{$0x1, %2, %<xtg_mode>1, %0|%0, %<xtg_mode>1, %2, 0x1}";
	}
    case 2:
    case 3:
      switch (get_attr_mode (insn))
	{
	case MODE_V16SF:
	  return "vmovaps\t{%1, %t0|%t0, %1}";
	case MODE_V8DF:
	  return "vmovapd\t{%1, %t0|%t0, %1}";
	case MODE_V8SF:
	  return "vmovaps\t{%1, %x0|%x0, %1}";
	case MODE_V4DF:
	  return "vmovapd\t{%1, %x0|%x0, %1}";
	case MODE_XI:
	  if (which_alternative == 2)
	    return "vmovdqa\t{%1, %t0|%t0, %1}";
	  else if (GET_MODE_SIZE (<ssescalarmode>mode) == 8)
	    return "vmovdqa64\t{%1, %t0|%t0, %1}";
	  else
	    return "vmovdqa32\t{%1, %t0|%t0, %1}";
	case MODE_OI:
	  if (which_alternative == 2)
	    return "vmovdqa\t{%1, %x0|%x0, %1}";
	  else if (GET_MODE_SIZE (<ssescalarmode>mode) == 8)
	    return "vmovdqa64\t{%1, %x0|%x0, %1}";
	  else
	    return "vmovdqa32\t{%1, %x0|%x0, %1}";
	default:
	  gcc_unreachable ();
	}
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "sselog,sselog,ssemov,ssemov")
   (set_attr "prefix_extra" "1,1,*,*")
   (set_attr "length_immediate" "1,1,*,*")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vcvtph2ps<mask_name>"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(vec_select:V4SF
	  (unspec:V8SF [(match_operand:V8HI 1 "register_operand" "v")]
		       UNSPEC_VCVTPH2PS)
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)])))]
  "TARGET_F16C || TARGET_AVX512VL"
  "vcvtph2ps\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "V4SF")])

(define_insn "*vcvtph2ps_load<mask_name>"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(unspec:V4SF [(match_operand:V4HI 1 "memory_operand" "m")]
		     UNSPEC_VCVTPH2PS))]
  "TARGET_F16C || TARGET_AVX512VL"
  "vcvtph2ps\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "vcvtph2ps256<mask_name>"
  [(set (match_operand:V8SF 0 "register_operand" "=v")
	(unspec:V8SF [(match_operand:V8HI 1 "nonimmediate_operand" "vm")]
		     UNSPEC_VCVTPH2PS))]
  "TARGET_F16C || TARGET_AVX512VL"
  "vcvtph2ps\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "btver2_decode" "double")
   (set_attr "mode" "V8SF")])

(define_insn "<mask_codefor>avx512f_vcvtph2ps512<mask_name><round_saeonly_name>"
  [(set (match_operand:V16SF 0 "register_operand" "=v")
	(unspec:V16SF
	  [(match_operand:V16HI 1 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")]
	  UNSPEC_VCVTPH2PS))]
  "TARGET_AVX512F"
  "vcvtph2ps\t{<round_saeonly_mask_op2>%1, %0<mask_operand2>|%0<mask_operand2>, %1<round_saeonly_mask_op2>}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V16SF")])

(define_expand "vcvtps2ph_mask"
  [(set (match_operand:V8HI 0 "register_operand")
	(vec_merge:V8HI
	  (vec_concat:V8HI
	    (unspec:V4HI [(match_operand:V4SF 1 "register_operand")
			  (match_operand:SI 2 "const_0_to_255_operand")]
			  UNSPEC_VCVTPS2PH)
	    (match_dup 5))
	   (match_operand:V8HI 3 "nonimm_or_0_operand")
	   (match_operand:QI 4 "register_operand")))]
  "TARGET_AVX512VL"
  "operands[5] = CONST0_RTX (V4HImode);")

(define_expand "vcvtps2ph"
  [(set (match_operand:V8HI 0 "register_operand")
	(vec_concat:V8HI
	  (unspec:V4HI [(match_operand:V4SF 1 "register_operand")
			(match_operand:SI 2 "const_0_to_255_operand")]
		       UNSPEC_VCVTPS2PH)
	  (match_dup 3)))]
  "TARGET_F16C"
  "operands[3] = CONST0_RTX (V4HImode);")

(define_insn "*vcvtps2ph<mask_name>"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
	(vec_concat:V8HI
	  (unspec:V4HI [(match_operand:V4SF 1 "register_operand" "v")
			(match_operand:SI 2 "const_0_to_255_operand" "N")]
		       UNSPEC_VCVTPS2PH)
	  (match_operand:V4HI 3 "const0_operand")))]
  "(TARGET_F16C || TARGET_AVX512VL) && <mask_avx512vl_condition>"
  "vcvtps2ph\t{%2, %1, %0<mask_operand4>|%0<mask_operand4>, %1, %2}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "V4SF")])

(define_insn "*vcvtps2ph_store<mask_name>"
  [(set (match_operand:V4HI 0 "memory_operand" "=m")
	(unspec:V4HI [(match_operand:V4SF 1 "register_operand" "v")
		      (match_operand:SI 2 "const_0_to_255_operand" "N")]
		     UNSPEC_VCVTPS2PH))]
  "TARGET_F16C || TARGET_AVX512VL"
  "vcvtps2ph\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "maybe_evex")
   (set_attr "mode" "V4SF")])

(define_insn "vcvtps2ph256<mask_name>"
  [(set (match_operand:V8HI 0 "nonimmediate_operand" "=vm")
	(unspec:V8HI [(match_operand:V8SF 1 "register_operand" "v")
		      (match_operand:SI 2 "const_0_to_255_operand" "N")]
		     UNSPEC_VCVTPS2PH))]
  "TARGET_F16C || TARGET_AVX512VL"
  "vcvtps2ph\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "maybe_evex")
   (set_attr "btver2_decode" "vector")
   (set_attr "mode" "V8SF")])

(define_insn "<mask_codefor>avx512f_vcvtps2ph512<mask_name>"
  [(set (match_operand:V16HI 0 "nonimmediate_operand" "=vm")
	(unspec:V16HI
	  [(match_operand:V16SF 1 "register_operand" "v")
	   (match_operand:SI 2 "const_0_to_255_operand" "N")]
	  UNSPEC_VCVTPS2PH))]
  "TARGET_AVX512F"
  "vcvtps2ph\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "evex")
   (set_attr "mode" "V16SF")])

;; For gather* insn patterns
(define_mode_iterator VEC_GATHER_MODE
		      [V2DI V2DF V4DI V4DF V4SI V4SF V8SI V8SF])
(define_mode_attr VEC_GATHER_IDXSI
		      [(V2DI "V4SI") (V4DI "V4SI") (V8DI "V8SI")
		       (V2DF "V4SI") (V4DF "V4SI") (V8DF "V8SI")
		       (V4SI "V4SI") (V8SI "V8SI") (V16SI "V16SI")
		       (V4SF "V4SI") (V8SF "V8SI") (V16SF "V16SI")])

(define_mode_attr VEC_GATHER_IDXDI
		      [(V2DI "V2DI") (V4DI "V4DI") (V8DI "V8DI")
		       (V2DF "V2DI") (V4DF "V4DI") (V8DF "V8DI")
		       (V4SI "V2DI") (V8SI "V4DI") (V16SI "V8DI")
		       (V4SF "V2DI") (V8SF "V4DI") (V16SF "V8DI")])

(define_mode_attr VEC_GATHER_SRCDI
		      [(V2DI "V2DI") (V4DI "V4DI") (V8DI "V8DI")
		       (V2DF "V2DF") (V4DF "V4DF") (V8DF "V8DF")
		       (V4SI "V4SI") (V8SI "V4SI") (V16SI "V8SI")
		       (V4SF "V4SF") (V8SF "V4SF") (V16SF "V8SF")])

(define_expand "avx2_gathersi<mode>"
  [(parallel [(set (match_operand:VEC_GATHER_MODE 0 "register_operand")
		   (unspec:VEC_GATHER_MODE
		     [(match_operand:VEC_GATHER_MODE 1 "register_operand")
		      (mem:<ssescalarmode>
			(match_par_dup 6
			  [(match_operand 2 "vsib_address_operand")
			   (match_operand:<VEC_GATHER_IDXSI>
			      3 "register_operand")
			   (match_operand:SI 5 "const1248_operand ")]))
		      (mem:BLK (scratch))
		      (match_operand:VEC_GATHER_MODE 4 "register_operand")]
		     UNSPEC_GATHER))
	      (clobber (match_scratch:VEC_GATHER_MODE 7))])]
  "TARGET_AVX2"
{
  operands[6]
    = gen_rtx_UNSPEC (Pmode, gen_rtvec (3, operands[2], operands[3],
					operands[5]), UNSPEC_VSIBADDR);
})

(define_insn "*avx2_gathersi<mode>"
  [(set (match_operand:VEC_GATHER_MODE 0 "register_operand" "=&x")
	(unspec:VEC_GATHER_MODE
	  [(match_operand:VEC_GATHER_MODE 2 "register_operand" "0")
	   (match_operator:<ssescalarmode> 7 "vsib_mem_operator"
	     [(unspec:P
		[(match_operand:P 3 "vsib_address_operand" "Tv")
		 (match_operand:<VEC_GATHER_IDXSI> 4 "register_operand" "x")
		 (match_operand:SI 6 "const1248_operand" "n")]
		UNSPEC_VSIBADDR)])
	   (mem:BLK (scratch))
	   (match_operand:VEC_GATHER_MODE 5 "register_operand" "1")]
	  UNSPEC_GATHER))
   (clobber (match_scratch:VEC_GATHER_MODE 1 "=&x"))]
  "TARGET_AVX2"
  "v<sseintprefix>gatherd<ssemodesuffix>\t{%1, %7, %0|%0, %7, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "*avx2_gathersi<mode>_2"
  [(set (match_operand:VEC_GATHER_MODE 0 "register_operand" "=&x")
	(unspec:VEC_GATHER_MODE
	  [(pc)
	   (match_operator:<ssescalarmode> 6 "vsib_mem_operator"
	     [(unspec:P
		[(match_operand:P 2 "vsib_address_operand" "Tv")
		 (match_operand:<VEC_GATHER_IDXSI> 3 "register_operand" "x")
		 (match_operand:SI 5 "const1248_operand" "n")]
		UNSPEC_VSIBADDR)])
	   (mem:BLK (scratch))
	   (match_operand:VEC_GATHER_MODE 4 "register_operand" "1")]
	  UNSPEC_GATHER))
   (clobber (match_scratch:VEC_GATHER_MODE 1 "=&x"))]
  "TARGET_AVX2"
  "v<sseintprefix>gatherd<ssemodesuffix>\t{%1, %6, %0|%0, %6, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "avx2_gatherdi<mode>"
  [(parallel [(set (match_operand:VEC_GATHER_MODE 0 "register_operand")
		   (unspec:VEC_GATHER_MODE
		     [(match_operand:<VEC_GATHER_SRCDI> 1 "register_operand")
		      (mem:<ssescalarmode>
			(match_par_dup 6
			  [(match_operand 2 "vsib_address_operand")
			   (match_operand:<VEC_GATHER_IDXDI>
			      3 "register_operand")
			   (match_operand:SI 5 "const1248_operand ")]))
		      (mem:BLK (scratch))
		      (match_operand:<VEC_GATHER_SRCDI> 4 "register_operand")]
		     UNSPEC_GATHER))
	      (clobber (match_scratch:VEC_GATHER_MODE 7))])]
  "TARGET_AVX2"
{
  operands[6]
    = gen_rtx_UNSPEC (Pmode, gen_rtvec (3, operands[2], operands[3],
					operands[5]), UNSPEC_VSIBADDR);
})

(define_insn "*avx2_gatherdi<mode>"
  [(set (match_operand:VEC_GATHER_MODE 0 "register_operand" "=&x")
	(unspec:VEC_GATHER_MODE
	  [(match_operand:<VEC_GATHER_SRCDI> 2 "register_operand" "0")
	   (match_operator:<ssescalarmode> 7 "vsib_mem_operator"
	     [(unspec:P
		[(match_operand:P 3 "vsib_address_operand" "Tv")
		 (match_operand:<VEC_GATHER_IDXDI> 4 "register_operand" "x")
		 (match_operand:SI 6 "const1248_operand" "n")]
		UNSPEC_VSIBADDR)])
	   (mem:BLK (scratch))
	   (match_operand:<VEC_GATHER_SRCDI> 5 "register_operand" "1")]
	  UNSPEC_GATHER))
   (clobber (match_scratch:VEC_GATHER_MODE 1 "=&x"))]
  "TARGET_AVX2"
  "v<sseintprefix>gatherq<ssemodesuffix>\t{%5, %7, %2|%2, %7, %5}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "*avx2_gatherdi<mode>_2"
  [(set (match_operand:VEC_GATHER_MODE 0 "register_operand" "=&x")
	(unspec:VEC_GATHER_MODE
	  [(pc)
	   (match_operator:<ssescalarmode> 6 "vsib_mem_operator"
	     [(unspec:P
		[(match_operand:P 2 "vsib_address_operand" "Tv")
		 (match_operand:<VEC_GATHER_IDXDI> 3 "register_operand" "x")
		 (match_operand:SI 5 "const1248_operand" "n")]
		UNSPEC_VSIBADDR)])
	   (mem:BLK (scratch))
	   (match_operand:<VEC_GATHER_SRCDI> 4 "register_operand" "1")]
	  UNSPEC_GATHER))
   (clobber (match_scratch:VEC_GATHER_MODE 1 "=&x"))]
  "TARGET_AVX2"
{
  if (<MODE>mode != <VEC_GATHER_SRCDI>mode)
    return "v<sseintprefix>gatherq<ssemodesuffix>\t{%4, %6, %x0|%x0, %6, %4}";
  return "v<sseintprefix>gatherq<ssemodesuffix>\t{%4, %6, %0|%0, %6, %4}";
}
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "*avx2_gatherdi<mode>_3"
  [(set (match_operand:<VEC_GATHER_SRCDI> 0 "register_operand" "=&x")
	(vec_select:<VEC_GATHER_SRCDI>
	  (unspec:VI4F_256
	    [(match_operand:<VEC_GATHER_SRCDI> 2 "register_operand" "0")
	     (match_operator:<ssescalarmode> 7 "vsib_mem_operator"
	       [(unspec:P
		  [(match_operand:P 3 "vsib_address_operand" "Tv")
		   (match_operand:<VEC_GATHER_IDXDI> 4 "register_operand" "x")
		   (match_operand:SI 6 "const1248_operand" "n")]
		  UNSPEC_VSIBADDR)])
	     (mem:BLK (scratch))
	     (match_operand:<VEC_GATHER_SRCDI> 5 "register_operand" "1")]
	     UNSPEC_GATHER)
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)])))
   (clobber (match_scratch:VI4F_256 1 "=&x"))]
  "TARGET_AVX2"
  "v<sseintprefix>gatherq<ssemodesuffix>\t{%5, %7, %0|%0, %7, %5}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "*avx2_gatherdi<mode>_4"
  [(set (match_operand:<VEC_GATHER_SRCDI> 0 "register_operand" "=&x")
	(vec_select:<VEC_GATHER_SRCDI>
	  (unspec:VI4F_256
	    [(pc)
	     (match_operator:<ssescalarmode> 6 "vsib_mem_operator"
	       [(unspec:P
		  [(match_operand:P 2 "vsib_address_operand" "Tv")
		   (match_operand:<VEC_GATHER_IDXDI> 3 "register_operand" "x")
		   (match_operand:SI 5 "const1248_operand" "n")]
		  UNSPEC_VSIBADDR)])
	     (mem:BLK (scratch))
	     (match_operand:<VEC_GATHER_SRCDI> 4 "register_operand" "1")]
	    UNSPEC_GATHER)
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)])))
   (clobber (match_scratch:VI4F_256 1 "=&x"))]
  "TARGET_AVX2"
  "v<sseintprefix>gatherq<ssemodesuffix>\t{%4, %6, %0|%0, %6, %4}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<sseinsnmode>")])

;; Memory operand override for -masm=intel of the v*gatherq* patterns.
(define_mode_attr gatherq_mode
  [(V4SI "q") (V2DI "x") (V4SF "q") (V2DF "x")
   (V8SI "x") (V4DI "t") (V8SF "x") (V4DF "t")
   (V16SI "t") (V8DI "g") (V16SF "t") (V8DF "g")])

(define_expand "<avx512>_gathersi<mode>"
  [(parallel [(set (match_operand:VI48F 0 "register_operand")
		   (unspec:VI48F
		     [(match_operand:VI48F 1 "register_operand")
		      (match_operand:<avx512fmaskmode> 4 "register_operand")
		      (mem:<ssescalarmode>
			(match_par_dup 6
			  [(match_operand 2 "vsib_address_operand")
			   (match_operand:<VEC_GATHER_IDXSI> 3 "register_operand")
			   (match_operand:SI 5 "const1248_operand")]))]
		     UNSPEC_GATHER))
	      (clobber (match_scratch:<avx512fmaskmode> 7))])]
  "TARGET_AVX512F"
{
  operands[6]
    = gen_rtx_UNSPEC (Pmode, gen_rtvec (3, operands[2], operands[3],
					operands[5]), UNSPEC_VSIBADDR);
})

(define_insn "*avx512f_gathersi<mode>"
  [(set (match_operand:VI48F 0 "register_operand" "=&v")
	(unspec:VI48F
	  [(match_operand:VI48F 1 "register_operand" "0")
	   (match_operand:<avx512fmaskmode> 7 "register_operand" "2")
	   (match_operator:<ssescalarmode> 6 "vsib_mem_operator"
	     [(unspec:P
		[(match_operand:P 4 "vsib_address_operand" "Tv")
		 (match_operand:<VEC_GATHER_IDXSI> 3 "register_operand" "v")
		 (match_operand:SI 5 "const1248_operand" "n")]
		UNSPEC_VSIBADDR)])]
	  UNSPEC_GATHER))
   (clobber (match_scratch:<avx512fmaskmode> 2 "=&Yk"))]
  "TARGET_AVX512F"
  "v<sseintprefix>gatherd<ssemodesuffix>\t{%6, %0%{%2%}|%0%{%2%}, %<xtg_mode>6}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "*avx512f_gathersi<mode>_2"
  [(set (match_operand:VI48F 0 "register_operand" "=&v")
	(unspec:VI48F
	  [(pc)
	   (match_operand:<avx512fmaskmode> 6 "register_operand" "1")
	   (match_operator:<ssescalarmode> 5 "vsib_mem_operator"
	     [(unspec:P
		[(match_operand:P 3 "vsib_address_operand" "Tv")
		 (match_operand:<VEC_GATHER_IDXSI> 2 "register_operand" "v")
		 (match_operand:SI 4 "const1248_operand" "n")]
		UNSPEC_VSIBADDR)])]
	  UNSPEC_GATHER))
   (clobber (match_scratch:<avx512fmaskmode> 1 "=&Yk"))]
  "TARGET_AVX512F"
  "v<sseintprefix>gatherd<ssemodesuffix>\t{%5, %0%{%1%}|%0%{%1%}, %<xtg_mode>5}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])


(define_expand "<avx512>_gatherdi<mode>"
  [(parallel [(set (match_operand:VI48F 0 "register_operand")
		   (unspec:VI48F
		     [(match_operand:<VEC_GATHER_SRCDI> 1 "register_operand")
		      (match_operand:QI 4 "register_operand")
		      (mem:<ssescalarmode>
			(match_par_dup 6
			  [(match_operand 2 "vsib_address_operand")
			   (match_operand:<VEC_GATHER_IDXDI> 3 "register_operand")
			   (match_operand:SI 5 "const1248_operand")]))]
		     UNSPEC_GATHER))
	      (clobber (match_scratch:QI 7))])]
  "TARGET_AVX512F"
{
  operands[6]
    = gen_rtx_UNSPEC (Pmode, gen_rtvec (3, operands[2], operands[3],
					operands[5]), UNSPEC_VSIBADDR);
})

(define_insn "*avx512f_gatherdi<mode>"
  [(set (match_operand:VI48F 0 "register_operand" "=&v")
	(unspec:VI48F
	  [(match_operand:<VEC_GATHER_SRCDI> 1 "register_operand" "0")
	   (match_operand:QI 7 "register_operand" "2")
	   (match_operator:<ssescalarmode> 6 "vsib_mem_operator"
	     [(unspec:P
		[(match_operand:P 4 "vsib_address_operand" "Tv")
		 (match_operand:<VEC_GATHER_IDXDI> 3 "register_operand" "v")
		 (match_operand:SI 5 "const1248_operand" "n")]
		UNSPEC_VSIBADDR)])]
	  UNSPEC_GATHER))
   (clobber (match_scratch:QI 2 "=&Yk"))]
  "TARGET_AVX512F"
{
  return "v<sseintprefix>gatherq<ssemodesuffix>\t{%6, %1%{%2%}|%1%{%2%}, %<gatherq_mode>6}";
}
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "*avx512f_gatherdi<mode>_2"
  [(set (match_operand:VI48F 0 "register_operand" "=&v")
	(unspec:VI48F
	  [(pc)
	   (match_operand:QI 6 "register_operand" "1")
	   (match_operator:<ssescalarmode> 5 "vsib_mem_operator"
	     [(unspec:P
		[(match_operand:P 3 "vsib_address_operand" "Tv")
		 (match_operand:<VEC_GATHER_IDXDI> 2 "register_operand" "v")
		 (match_operand:SI 4 "const1248_operand" "n")]
		UNSPEC_VSIBADDR)])]
	  UNSPEC_GATHER))
   (clobber (match_scratch:QI 1 "=&Yk"))]
  "TARGET_AVX512F"
{
  if (<MODE>mode != <VEC_GATHER_SRCDI>mode)
    {
      if (<MODE_SIZE> != 64)
	return "v<sseintprefix>gatherq<ssemodesuffix>\t{%5, %x0%{%1%}|%x0%{%1%}, %<gatherq_mode>5}";
      else
	return "v<sseintprefix>gatherq<ssemodesuffix>\t{%5, %t0%{%1%}|%t0%{%1%}, %t5}";
    }
  return "v<sseintprefix>gatherq<ssemodesuffix>\t{%5, %0%{%1%}|%0%{%1%}, %<gatherq_mode>5}";
}
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "<avx512>_scattersi<mode>"
  [(parallel [(set (mem:VI48F
		     (match_par_dup 5
		       [(match_operand 0 "vsib_address_operand")
			(match_operand:<VEC_GATHER_IDXSI> 2 "register_operand")
			(match_operand:SI 4 "const1248_operand")]))
		   (unspec:VI48F
		     [(match_operand:<avx512fmaskmode> 1 "register_operand")
		      (match_operand:VI48F 3 "register_operand")]
		     UNSPEC_SCATTER))
	      (clobber (match_scratch:<avx512fmaskmode> 6))])]
  "TARGET_AVX512F"
{
  operands[5]
    = gen_rtx_UNSPEC (Pmode, gen_rtvec (3, operands[0], operands[2],
					operands[4]), UNSPEC_VSIBADDR);
})

(define_insn "*avx512f_scattersi<mode>"
  [(set (match_operator:VI48F 5 "vsib_mem_operator"
	  [(unspec:P
	     [(match_operand:P 0 "vsib_address_operand" "Tv")
	      (match_operand:<VEC_GATHER_IDXSI> 2 "register_operand" "v")
	      (match_operand:SI 4 "const1248_operand" "n")]
	     UNSPEC_VSIBADDR)])
	(unspec:VI48F
	  [(match_operand:<avx512fmaskmode> 6 "register_operand" "1")
	   (match_operand:VI48F 3 "register_operand" "v")]
	  UNSPEC_SCATTER))
   (clobber (match_scratch:<avx512fmaskmode> 1 "=&Yk"))]
  "TARGET_AVX512F"
  "v<sseintprefix>scatterd<ssemodesuffix>\t{%3, %5%{%1%}|%5%{%1%}, %3}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "<avx512>_scatterdi<mode>"
  [(parallel [(set (mem:VI48F
		     (match_par_dup 5
		       [(match_operand 0 "vsib_address_operand")
			(match_operand:<VEC_GATHER_IDXDI> 2 "register_operand")
			(match_operand:SI 4 "const1248_operand")]))
		   (unspec:VI48F
		     [(match_operand:QI 1 "register_operand")
		      (match_operand:<VEC_GATHER_SRCDI> 3 "register_operand")]
		     UNSPEC_SCATTER))
	      (clobber (match_scratch:QI 6))])]
  "TARGET_AVX512F"
{
  operands[5]
    = gen_rtx_UNSPEC (Pmode, gen_rtvec (3, operands[0], operands[2],
					operands[4]), UNSPEC_VSIBADDR);
})

(define_insn "*avx512f_scatterdi<mode>"
  [(set (match_operator:VI48F 5 "vsib_mem_operator"
	  [(unspec:P
	     [(match_operand:P 0 "vsib_address_operand" "Tv")
	      (match_operand:<VEC_GATHER_IDXDI> 2 "register_operand" "v")
	      (match_operand:SI 4 "const1248_operand" "n")]
	     UNSPEC_VSIBADDR)])
	(unspec:VI48F
	  [(match_operand:QI 6 "register_operand" "1")
	   (match_operand:<VEC_GATHER_SRCDI> 3 "register_operand" "v")]
	  UNSPEC_SCATTER))
   (clobber (match_scratch:QI 1 "=&Yk"))]
  "TARGET_AVX512F"
{
  if (GET_MODE_SIZE (GET_MODE_INNER (<MODE>mode)) == 8)
    return "v<sseintprefix>scatterq<ssemodesuffix>\t{%3, %5%{%1%}|%5%{%1%}, %3}";
  return "v<sseintprefix>scatterq<ssemodesuffix>\t{%3, %5%{%1%}|%t5%{%1%}, %3}";
}
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_compress<mode>_mask"
  [(set (match_operand:VI48F 0 "register_operand" "=v")
	(unspec:VI48F
	  [(match_operand:VI48F 1 "register_operand" "v")
	   (match_operand:VI48F 2 "nonimm_or_0_operand" "0C")
	   (match_operand:<avx512fmaskmode> 3 "register_operand" "Yk")]
	  UNSPEC_COMPRESS))]
  "TARGET_AVX512F"
  "v<sseintprefix>compress<ssemodesuffix>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "compress<mode>_mask"
  [(set (match_operand:VI12_AVX512VLBW 0 "register_operand" "=v")
	(unspec:VI12_AVX512VLBW
	  [(match_operand:VI12_AVX512VLBW 1 "register_operand" "v")
	   (match_operand:VI12_AVX512VLBW 2 "nonimm_or_0_operand" "0C")
	   (match_operand:<avx512fmaskmode> 3 "register_operand" "Yk")]
	  UNSPEC_COMPRESS))]
  "TARGET_AVX512VBMI2"
  "vpcompress<ssemodesuffix>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<avx512>_compressstore<mode>_mask"
  [(set (match_operand:VI48F 0 "memory_operand" "=m")
	(unspec:VI48F
	  [(match_operand:VI48F 1 "register_operand" "x")
	   (match_dup 0)
	   (match_operand:<avx512fmaskmode> 2 "register_operand" "Yk")]
	  UNSPEC_COMPRESS_STORE))]
  "TARGET_AVX512F"
  "v<sseintprefix>compress<ssemodesuffix>\t{%1, %0%{%2%}|%0%{%2%}, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "memory" "store")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "compressstore<mode>_mask"
  [(set (match_operand:VI12_AVX512VLBW 0 "memory_operand" "=m")
	(unspec:VI12_AVX512VLBW
	  [(match_operand:VI12_AVX512VLBW 1 "register_operand" "x")
	   (match_dup 0)
	   (match_operand:<avx512fmaskmode> 2 "register_operand" "Yk")]
	  UNSPEC_COMPRESS_STORE))]
  "TARGET_AVX512VBMI2"
  "vpcompress<ssemodesuffix>\t{%1, %0%{%2%}|%0%{%2%}, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "memory" "store")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "<avx512>_expand<mode>_maskz"
  [(set (match_operand:VI48F 0 "register_operand")
	(unspec:VI48F
	  [(match_operand:VI48F 1 "nonimmediate_operand")
	   (match_operand:VI48F 2 "nonimm_or_0_operand")
	   (match_operand:<avx512fmaskmode> 3 "register_operand")]
	  UNSPEC_EXPAND))]
  "TARGET_AVX512F"
  "operands[2] = CONST0_RTX (<MODE>mode);")

(define_insn "<avx512>_expand<mode>_mask"
  [(set (match_operand:VI48F 0 "register_operand" "=v,v")
	(unspec:VI48F
	  [(match_operand:VI48F 1 "nonimmediate_operand" "v,m")
	   (match_operand:VI48F 2 "nonimm_or_0_operand" "0C,0C")
	   (match_operand:<avx512fmaskmode> 3 "register_operand" "Yk,Yk")]
	  UNSPEC_EXPAND))]
  "TARGET_AVX512F"
  "v<sseintprefix>expand<ssemodesuffix>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "memory" "none,load")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "expand<mode>_mask"
  [(set (match_operand:VI12_AVX512VLBW 0 "register_operand" "=v,v")
	(unspec:VI12_AVX512VLBW
	  [(match_operand:VI12_AVX512VLBW 1 "nonimmediate_operand" "v,m")
	   (match_operand:VI12_AVX512VLBW 2 "nonimm_or_0_operand" "0C,0C")
	   (match_operand:<avx512fmaskmode> 3 "register_operand" "Yk,Yk")]
	  UNSPEC_EXPAND))]
  "TARGET_AVX512VBMI2"
  "v<sseintprefix>expand<ssemodesuffix>\t{%1, %0%{%3%}%N2|%0%{%3%}%N2, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "evex")
   (set_attr "memory" "none,load")
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "expand<mode>_maskz"
  [(set (match_operand:VI12_AVX512VLBW 0 "register_operand")
	(unspec:VI12_AVX512VLBW
	  [(match_operand:VI12_AVX512VLBW 1 "nonimmediate_operand")
	   (match_operand:VI12_AVX512VLBW 2 "nonimm_or_0_operand")
	   (match_operand:<avx512fmaskmode> 3 "register_operand")]
	  UNSPEC_EXPAND))]
  "TARGET_AVX512VBMI2"
  "operands[2] = CONST0_RTX (<MODE>mode);")

(define_insn "avx512dq_rangep<mode><mask_name><round_saeonly_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
	(unspec:VF_AVX512VL
	  [(match_operand:VF_AVX512VL 1 "register_operand" "v")
	   (match_operand:VF_AVX512VL 2 "<round_saeonly_nimm_predicate>" "<round_saeonly_constraint>")
	   (match_operand:SI 3 "const_0_to_15_operand")]
	  UNSPEC_RANGE))]
  "TARGET_AVX512DQ && <round_saeonly_mode512bit_condition>"
  "vrange<ssemodesuffix>\t{%3, <round_saeonly_mask_op4>%2, %1, %0<mask_operand4>|%0<mask_operand4>, %1, %2<round_saeonly_mask_op4>, %3}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "avx512dq_ranges<mode><mask_scalar_name><round_saeonly_scalar_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
	  (unspec:VF_128
	    [(match_operand:VF_128 1 "register_operand" "v")
	     (match_operand:VF_128 2 "<round_saeonly_scalar_nimm_predicate>" "<round_saeonly_scalar_constraint>")
	     (match_operand:SI 3 "const_0_to_15_operand")]
	    UNSPEC_RANGE)
	  (match_dup 1)
	  (const_int 1)))]
  "TARGET_AVX512DQ"
  "vrange<ssescalarmodesuffix>\t{%3, <round_saeonly_scalar_mask_op4>%2, %1, %0<mask_scalar_operand4>|%0<mask_scalar_operand4>, %1, %<iptr>2<round_saeonly_scalar_mask_op4>, %3}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "avx512dq_fpclass<mode><mask_scalar_merge_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
          (unspec:<avx512fmaskmode>
            [(match_operand:VF_AVX512VL 1 "register_operand" "v")
             (match_operand:QI 2 "const_0_to_255_operand" "n")]
             UNSPEC_FPCLASS))]
   "TARGET_AVX512DQ"
   "vfpclass<ssemodesuffix>\t{%2, %1, %0<mask_scalar_merge_operand3>|%0<mask_scalar_merge_operand3>, %1, %2}";
  [(set_attr "type" "sse")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "avx512dq_vmfpclass<mode>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(and:<avx512fmaskmode>
	  (unspec:<avx512fmaskmode>
	    [(match_operand:VF_128 1 "register_operand" "v")
             (match_operand:QI 2 "const_0_to_255_operand" "n")]
	    UNSPEC_FPCLASS)
	  (const_int 1)))]
   "TARGET_AVX512DQ"
   "vfpclass<ssescalarmodesuffix>\t{%2, %1, %0|%0, %1, %2}";
  [(set_attr "type" "sse")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "<avx512>_getmant<mode><mask_name><round_saeonly_name>"
  [(set (match_operand:VF_AVX512VL 0 "register_operand" "=v")
	(unspec:VF_AVX512VL
	  [(match_operand:VF_AVX512VL 1 "nonimmediate_operand" "<round_saeonly_constraint>")
	   (match_operand:SI 2 "const_0_to_15_operand")]
	  UNSPEC_GETMANT))]
  "TARGET_AVX512F"
  "vgetmant<ssemodesuffix>\t{%2, <round_saeonly_mask_op3>%1, %0<mask_operand3>|%0<mask_operand3>, %1<round_saeonly_mask_op3>, %2}";
  [(set_attr "prefix" "evex")
   (set_attr "mode" "<MODE>")])

(define_insn "avx512f_vgetmant<mode><mask_scalar_name><round_saeonly_scalar_name>"
  [(set (match_operand:VF_128 0 "register_operand" "=v")
	(vec_merge:VF_128
	  (unspec:VF_128
	    [(match_operand:VF_128 1 "register_operand" "v")
	     (match_operand:VF_128 2 "<round_saeonly_scalar_nimm_predicate>" "<round_saeonly_scalar_constraint>")
	     (match_operand:SI 3 "const_0_to_15_operand")]
	    UNSPEC_GETMANT)
	  (match_dup 1)
	  (const_int 1)))]
   "TARGET_AVX512F"
   "vgetmant<ssescalarmodesuffix>\t{%3, <round_saeonly_scalar_mask_op4>%2, %1, %0<mask_scalar_operand4>|%0<mask_scalar_operand4>, %1, %<iptr>2<round_saeonly_scalar_mask_op4>, %3}";
   [(set_attr "prefix" "evex")
   (set_attr "mode" "<ssescalarmode>")])

;; The correct representation for this is absolutely enormous, and
;; surely not generally useful.
(define_insn "<mask_codefor>avx512bw_dbpsadbw<mode><mask_name>"
  [(set (match_operand:VI2_AVX512VL 0 "register_operand" "=v")
	(unspec:VI2_AVX512VL
	  [(match_operand:<dbpsadbwmode> 1 "register_operand" "v")
	   (match_operand:<dbpsadbwmode> 2 "nonimmediate_operand" "vm")
	   (match_operand:SI 3 "const_0_to_255_operand")]
	  UNSPEC_DBPSADBW))]
   "TARGET_AVX512BW"
  "vdbpsadbw\t{%3, %2, %1, %0<mask_operand4>|%0<mask_operand4>, %1, %2, %3}"
  [(set_attr "type" "sselog1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "clz<mode>2<mask_name>"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand" "=v")
	(clz:VI48_AVX512VL
	  (match_operand:VI48_AVX512VL 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512CD"
  "vplzcnt<ssemodesuffix>\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "<mask_codefor>conflict<mode><mask_name>"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand" "=v")
	(unspec:VI48_AVX512VL
	  [(match_operand:VI48_AVX512VL 1 "nonimmediate_operand" "vm")]
	  UNSPEC_CONFLICT))]
  "TARGET_AVX512CD"
  "vpconflict<ssemodesuffix>\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "sha1msg1"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(unspec:V4SI
	  [(match_operand:V4SI 1 "register_operand" "0")
	   (match_operand:V4SI 2 "vector_operand" "xBm")]
	  UNSPEC_SHA1MSG1))]
  "TARGET_SHA"
  "sha1msg1\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "mode" "TI")])

(define_insn "sha1msg2"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(unspec:V4SI
	  [(match_operand:V4SI 1 "register_operand" "0")
	   (match_operand:V4SI 2 "vector_operand" "xBm")]
	  UNSPEC_SHA1MSG2))]
  "TARGET_SHA"
  "sha1msg2\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "mode" "TI")])

(define_insn "sha1nexte"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(unspec:V4SI
	  [(match_operand:V4SI 1 "register_operand" "0")
	   (match_operand:V4SI 2 "vector_operand" "xBm")]
	  UNSPEC_SHA1NEXTE))]
  "TARGET_SHA"
  "sha1nexte\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "mode" "TI")])

(define_insn "sha1rnds4"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(unspec:V4SI
	  [(match_operand:V4SI 1 "register_operand" "0")
	   (match_operand:V4SI 2 "vector_operand" "xBm")
	   (match_operand:SI 3 "const_0_to_3_operand" "n")]
	  UNSPEC_SHA1RNDS4))]
  "TARGET_SHA"
  "sha1rnds4\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "sselog1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "sha256msg1"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(unspec:V4SI
	  [(match_operand:V4SI 1 "register_operand" "0")
	   (match_operand:V4SI 2 "vector_operand" "xBm")]
	  UNSPEC_SHA256MSG1))]
  "TARGET_SHA"
  "sha256msg1\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "mode" "TI")])

(define_insn "sha256msg2"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(unspec:V4SI
	  [(match_operand:V4SI 1 "register_operand" "0")
	   (match_operand:V4SI 2 "vector_operand" "xBm")]
	  UNSPEC_SHA256MSG2))]
  "TARGET_SHA"
  "sha256msg2\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "mode" "TI")])

(define_insn "sha256rnds2"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(unspec:V4SI
	  [(match_operand:V4SI 1 "register_operand" "0")
	   (match_operand:V4SI 2 "vector_operand" "xBm")
	   (match_operand:V4SI 3 "register_operand" "Yz")]
	  UNSPEC_SHA256RNDS2))]
  "TARGET_SHA"
  "sha256rnds2\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "sselog1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn_and_split "avx512f_<castmode><avxsizesuffix>_<castmode>"
  [(set (match_operand:AVX512MODE2P 0 "nonimmediate_operand" "=x,m")
	(unspec:AVX512MODE2P
	  [(match_operand:<ssequartermode> 1 "nonimmediate_operand" "xm,x")]
	  UNSPEC_CAST))]
  "TARGET_AVX512F && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 1))]
{
  if (REG_P (operands[0]))
    operands[0] = gen_lowpart (<ssequartermode>mode, operands[0]);
  else
    operands[1] = lowpart_subreg (<MODE>mode, operands[1],
				  <ssequartermode>mode);
})

(define_insn_and_split "avx512f_<castmode><avxsizesuffix>_256<castmode>"
  [(set (match_operand:AVX512MODE2P 0 "nonimmediate_operand" "=x,m")
	(unspec:AVX512MODE2P
	  [(match_operand:<ssehalfvecmode> 1 "nonimmediate_operand" "xm,x")]
	  UNSPEC_CAST))]
  "TARGET_AVX512F && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 1))]
{
  if (REG_P (operands[0]))
    operands[0] = gen_lowpart (<ssehalfvecmode>mode, operands[0]);
  else
    operands[1] = lowpart_subreg (<MODE>mode, operands[1],
				  <ssehalfvecmode>mode);
})

(define_int_iterator VPMADD52
	[UNSPEC_VPMADD52LUQ
	 UNSPEC_VPMADD52HUQ])

(define_int_attr vpmadd52type
  [(UNSPEC_VPMADD52LUQ "luq") (UNSPEC_VPMADD52HUQ "huq")])

(define_expand "vpamdd52huq<mode>_maskz"
  [(match_operand:VI8_AVX512VL 0 "register_operand")
   (match_operand:VI8_AVX512VL 1 "register_operand")
   (match_operand:VI8_AVX512VL 2 "register_operand")
   (match_operand:VI8_AVX512VL 3 "nonimmediate_operand")
   (match_operand:<avx512fmaskmode> 4 "register_operand")]
  "TARGET_AVX512IFMA"
{
  emit_insn (gen_vpamdd52huq<mode>_maskz_1 (
    operands[0], operands[1], operands[2], operands[3],
    CONST0_RTX (<MODE>mode), operands[4]));
  DONE;
})

(define_expand "vpamdd52luq<mode>_maskz"
  [(match_operand:VI8_AVX512VL 0 "register_operand")
   (match_operand:VI8_AVX512VL 1 "register_operand")
   (match_operand:VI8_AVX512VL 2 "register_operand")
   (match_operand:VI8_AVX512VL 3 "nonimmediate_operand")
   (match_operand:<avx512fmaskmode> 4 "register_operand")]
  "TARGET_AVX512IFMA"
{
  emit_insn (gen_vpamdd52luq<mode>_maskz_1 (
    operands[0], operands[1], operands[2], operands[3],
    CONST0_RTX (<MODE>mode), operands[4]));
  DONE;
})

(define_insn "vpamdd52<vpmadd52type><mode><sd_maskz_name>"
  [(set (match_operand:VI8_AVX512VL 0 "register_operand" "=v")
	(unspec:VI8_AVX512VL
	  [(match_operand:VI8_AVX512VL 1 "register_operand" "0")
	   (match_operand:VI8_AVX512VL 2 "register_operand" "v")
	   (match_operand:VI8_AVX512VL 3 "nonimmediate_operand" "vm")]
	  VPMADD52))]
  "TARGET_AVX512IFMA"
  "vpmadd52<vpmadd52type>\t{%3, %2, %0<sd_mask_op4>|%0<sd_mask_op4>, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vpamdd52<vpmadd52type><mode>_mask"
  [(set (match_operand:VI8_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI8_AVX512VL
	  (unspec:VI8_AVX512VL
	    [(match_operand:VI8_AVX512VL 1 "register_operand" "0")
	     (match_operand:VI8_AVX512VL 2 "register_operand" "v")
	     (match_operand:VI8_AVX512VL 3 "nonimmediate_operand" "vm")]
	    VPMADD52)
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512IFMA"
  "vpmadd52<vpmadd52type>\t{%3, %2, %0%{%4%}|%0%{%4%}, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vpmultishiftqb<mode><mask_name>"
  [(set (match_operand:VI1_AVX512VL 0 "register_operand" "=v")
	(unspec:VI1_AVX512VL
	  [(match_operand:VI1_AVX512VL 1 "register_operand" "v")
	   (match_operand:VI1_AVX512VL 2 "nonimmediate_operand" "vm")]
	  UNSPEC_VPMULTISHIFT))]
  "TARGET_AVX512VBMI"
  "vpmultishiftqb\t{%2, %1, %0<mask_operand3>|%0<mask_operand3>, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_mode_iterator IMOD4
  [(V64SF "TARGET_AVX5124FMAPS") (V64SI "TARGET_AVX5124VNNIW")])

(define_mode_attr imod4_narrow
  [(V64SF "V16SF") (V64SI "V16SI")])

(define_expand "mov<mode>"
  [(set (match_operand:IMOD4 0 "nonimmediate_operand")
	(match_operand:IMOD4 1 "nonimm_or_0_operand"))]
  "TARGET_AVX512F"
{
  ix86_expand_vector_move (<MODE>mode, operands);
  DONE;
})

(define_insn_and_split "*mov<mode>_internal"
  [(set (match_operand:IMOD4 0 "nonimmediate_operand" "=v,v ,m")
	(match_operand:IMOD4 1 "nonimm_or_0_operand"  " C,vm,v"))]
  "TARGET_AVX512F
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx op0, op1;
  int i;

  for (i = 0; i < 4; i++)
    {
      op0 = simplify_subreg
	     (<imod4_narrow>mode, operands[0], <MODE>mode, i * 64);
      op1 = simplify_subreg
	     (<imod4_narrow>mode, operands[1], <MODE>mode, i * 64);
      emit_move_insn (op0, op1);
    }
  DONE;
})

(define_insn "avx5124fmaddps_4fmaddps"
  [(set (match_operand:V16SF 0 "register_operand" "=v")
	(unspec:V16SF
	  [(match_operand:V16SF 1 "register_operand" "0")
	   (match_operand:V64SF 2 "register_operand" "v")
	   (match_operand:V4SF 3 "memory_operand" "m")] UNSPEC_VP4FMADD))]
  "TARGET_AVX5124FMAPS"
  "v4fmaddps\t{%3, %g2, %0|%0, %g2, %3}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("V16SF"))])

(define_insn "avx5124fmaddps_4fmaddps_mask"
  [(set (match_operand:V16SF 0 "register_operand" "=v")
	(vec_merge:V16SF
	  (unspec:V16SF
	     [(match_operand:V64SF 1 "register_operand" "v")
	      (match_operand:V4SF 2 "memory_operand" "m")] UNSPEC_VP4FMADD)
	  (match_operand:V16SF 3 "register_operand" "0")
	  (match_operand:HI 4 "register_operand" "Yk")))]
  "TARGET_AVX5124FMAPS"
  "v4fmaddps\t{%2, %g1, %0%{%4%}|%0%{%4%}, %g1, %2}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("V16SF"))])

(define_insn "avx5124fmaddps_4fmaddps_maskz"
  [(set (match_operand:V16SF 0 "register_operand" "=v")
	(vec_merge:V16SF
	  (unspec:V16SF
	    [(match_operand:V16SF 1 "register_operand" "0")
	     (match_operand:V64SF 2 "register_operand" "v")
	     (match_operand:V4SF 3 "memory_operand" "m")] UNSPEC_VP4FMADD)
	  (match_operand:V16SF 4 "const0_operand" "C")
	  (match_operand:HI 5 "register_operand" "Yk")))]
  "TARGET_AVX5124FMAPS"
  "v4fmaddps\t{%3, %g2, %0%{%5%}%{z%}|%0%{%5%}%{z%}, %g2, %3}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("V16SF"))])

(define_insn "avx5124fmaddps_4fmaddss"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(unspec:V4SF
	  [(match_operand:V4SF 1 "register_operand" "0")
	   (match_operand:V64SF 2 "register_operand" "v")
	   (match_operand:V4SF 3 "memory_operand" "m")] UNSPEC_VP4FMADD))]
  "TARGET_AVX5124FMAPS"
  "v4fmaddss\t{%3, %x2, %0|%0, %x2, %3}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("SF"))])

(define_insn "avx5124fmaddps_4fmaddss_mask"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(vec_merge:V4SF
	  (unspec:V4SF
	    [(match_operand:V64SF 1 "register_operand" "v")
	     (match_operand:V4SF 2 "memory_operand" "m")] UNSPEC_VP4FMADD)
	  (match_operand:V4SF 3 "register_operand" "0")
	  (match_operand:QI 4 "register_operand" "Yk")))]
  "TARGET_AVX5124FMAPS"
  "v4fmaddss\t{%2, %x1, %0%{%4%}|%0%{%4%}, %x1, %2}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("SF"))])

(define_insn "avx5124fmaddps_4fmaddss_maskz"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(vec_merge:V4SF
	  (unspec:V4SF
	    [(match_operand:V4SF 1 "register_operand" "0")
	     (match_operand:V64SF 2 "register_operand" "v")
	     (match_operand:V4SF 3 "memory_operand" "m")] UNSPEC_VP4FMADD)
	  (match_operand:V4SF 4 "const0_operand" "C")
	  (match_operand:QI 5 "register_operand" "Yk")))]
  "TARGET_AVX5124FMAPS"
  "v4fmaddss\t{%3, %x2, %0%{%5%}%{z%}|%0%{%5%}%{z%}, %x2, %3}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("SF"))])

(define_insn "avx5124fmaddps_4fnmaddps"
  [(set (match_operand:V16SF 0 "register_operand" "=v")
	(unspec:V16SF
	  [(match_operand:V16SF 1 "register_operand" "0")
	   (match_operand:V64SF 2 "register_operand" "v")
	   (match_operand:V4SF 3 "memory_operand" "m")] UNSPEC_VP4FNMADD))]
  "TARGET_AVX5124FMAPS"
  "v4fnmaddps\t{%3, %g2, %0|%0, %g2, %3}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("V16SF"))])

(define_insn "avx5124fmaddps_4fnmaddps_mask"
  [(set (match_operand:V16SF 0 "register_operand" "=v")
	(vec_merge:V16SF
	  (unspec:V16SF
	     [(match_operand:V64SF 1 "register_operand" "v")
	      (match_operand:V4SF 2 "memory_operand" "m")] UNSPEC_VP4FNMADD)
	  (match_operand:V16SF 3 "register_operand" "0")
	  (match_operand:HI 4 "register_operand" "Yk")))]
  "TARGET_AVX5124FMAPS"
  "v4fnmaddps\t{%2, %g1, %0%{%4%}|%0%{%4%}, %g1, %2}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("V16SF"))])

(define_insn "avx5124fmaddps_4fnmaddps_maskz"
  [(set (match_operand:V16SF 0 "register_operand" "=v")
	(vec_merge:V16SF
	  (unspec:V16SF
	    [(match_operand:V16SF 1 "register_operand" "0")
	     (match_operand:V64SF 2 "register_operand" "v")
	     (match_operand:V4SF 3 "memory_operand" "m")] UNSPEC_VP4FNMADD)
	  (match_operand:V16SF 4 "const0_operand" "C")
	  (match_operand:HI 5 "register_operand" "Yk")))]
  "TARGET_AVX5124FMAPS"
  "v4fnmaddps\t{%3, %g2, %0%{%5%}%{z%}|%0%{%5%}%{z%}, %g2, %3}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("V16SF"))])

(define_insn "avx5124fmaddps_4fnmaddss"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(unspec:V4SF
	  [(match_operand:V4SF 1 "register_operand" "0")
	   (match_operand:V64SF 2 "register_operand" "v")
	   (match_operand:V4SF 3 "memory_operand" "m")] UNSPEC_VP4FNMADD))]
  "TARGET_AVX5124FMAPS"
  "v4fnmaddss\t{%3, %x2, %0|%0, %x2, %3}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("SF"))])

(define_insn "avx5124fmaddps_4fnmaddss_mask"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(vec_merge:V4SF
	  (unspec:V4SF
	    [(match_operand:V64SF 1 "register_operand" "v")
	     (match_operand:V4SF 2 "memory_operand" "m")] UNSPEC_VP4FNMADD)
	  (match_operand:V4SF 3 "register_operand" "0")
	  (match_operand:QI 4 "register_operand" "Yk")))]
  "TARGET_AVX5124FMAPS"
  "v4fnmaddss\t{%2, %x1, %0%{%4%}|%0%{%4%}, %x1, %2}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("SF"))])

(define_insn "avx5124fmaddps_4fnmaddss_maskz"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(vec_merge:V4SF
	  (unspec:V4SF
	    [(match_operand:V4SF 1 "register_operand" "0")
	     (match_operand:V64SF 2 "register_operand" "v")
	     (match_operand:V4SF 3 "memory_operand" "m")] UNSPEC_VP4FNMADD)
	  (match_operand:V4SF 4 "const0_operand" "C")
	  (match_operand:QI 5 "register_operand" "Yk")))]
  "TARGET_AVX5124FMAPS"
  "v4fnmaddss\t{%3, %x2, %0%{%5%}%{z%}|%0%{%5%}%{z%}, %x2, %3}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("SF"))])

(define_insn "avx5124vnniw_vp4dpwssd"
  [(set (match_operand:V16SI 0 "register_operand" "=v")
	(unspec:V16SI
	  [(match_operand:V16SI 1 "register_operand" "0")
	   (match_operand:V64SI 2 "register_operand" "v")
	   (match_operand:V4SI 3 "memory_operand" "m")] UNSPEC_VP4DPWSSD))]
  "TARGET_AVX5124VNNIW"
  "vp4dpwssd\t{%3, %g2, %0|%0, %g2, %3}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("TI"))])

(define_insn "avx5124vnniw_vp4dpwssd_mask"
  [(set (match_operand:V16SI 0 "register_operand" "=v")
	(vec_merge:V16SI
	  (unspec:V16SI
	     [(match_operand:V64SI 1 "register_operand" "v")
	      (match_operand:V4SI 2 "memory_operand" "m")] UNSPEC_VP4DPWSSD)
	  (match_operand:V16SI 3 "register_operand" "0")
	  (match_operand:HI 4 "register_operand" "Yk")))]
  "TARGET_AVX5124VNNIW"
  "vp4dpwssd\t{%2, %g1, %0%{%4%}|%0%{%4%}, %g1, %2}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("TI"))])

(define_insn "avx5124vnniw_vp4dpwssd_maskz"
  [(set (match_operand:V16SI 0 "register_operand" "=v")
	(vec_merge:V16SI
	  (unspec:V16SI
	    [(match_operand:V16SI 1 "register_operand" "0")
	     (match_operand:V64SI 2 "register_operand" "v")
	     (match_operand:V4SI 3 "memory_operand" "m")] UNSPEC_VP4DPWSSD)
	  (match_operand:V16SI 4 "const0_operand" "C")
	  (match_operand:HI 5 "register_operand" "Yk")))]
  "TARGET_AVX5124VNNIW"
  "vp4dpwssd\t{%3, %g2, %0%{%5%}%{z%}|%0%{%5%}%{z%}, %g2, %3}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("TI"))])

(define_insn "avx5124vnniw_vp4dpwssds"
  [(set (match_operand:V16SI 0 "register_operand" "=v")
	(unspec:V16SI
	  [(match_operand:V16SI 1 "register_operand" "0")
	   (match_operand:V64SI 2 "register_operand" "v")
	   (match_operand:V4SI 3 "memory_operand" "m")] UNSPEC_VP4DPWSSDS))]
  "TARGET_AVX5124VNNIW"
  "vp4dpwssds\t{%3, %g2, %0|%0, %g2, %3}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("TI"))])

(define_insn "avx5124vnniw_vp4dpwssds_mask"
  [(set (match_operand:V16SI 0 "register_operand" "=v")
	(vec_merge:V16SI
	  (unspec:V16SI
	     [(match_operand:V64SI 1 "register_operand" "v")
	      (match_operand:V4SI 2 "memory_operand" "m")] UNSPEC_VP4DPWSSDS)
	  (match_operand:V16SI 3 "register_operand" "0")
	  (match_operand:HI 4 "register_operand" "Yk")))]
  "TARGET_AVX5124VNNIW"
  "vp4dpwssds\t{%2, %g1, %0%{%4%}|%0%{%4%}, %g1, %2}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("TI"))])

(define_insn "avx5124vnniw_vp4dpwssds_maskz"
  [(set (match_operand:V16SI 0 "register_operand" "=v")
	(vec_merge:V16SI
	  (unspec:V16SI
	    [(match_operand:V16SI 1 "register_operand" "0")
	     (match_operand:V64SI 2 "register_operand" "v")
	     (match_operand:V4SI 3 "memory_operand" "m")] UNSPEC_VP4DPWSSDS)
	  (match_operand:V16SI 4 "const0_operand" "C")
	  (match_operand:HI 5 "register_operand" "Yk")))]
  "TARGET_AVX5124VNNIW"
  "vp4dpwssds\t{%3, %g2, %0%{%5%}%{z%}|%0%{%5%}%{z%}, %g2, %3}"
   [(set_attr ("type") ("ssemuladd"))
    (set_attr ("prefix") ("evex"))
    (set_attr ("mode") ("TI"))])

(define_insn "vpopcount<mode><mask_name>"
  [(set (match_operand:VI48_AVX512VL 0 "register_operand" "=v")
	(popcount:VI48_AVX512VL
	  (match_operand:VI48_AVX512VL 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512VPOPCNTDQ"
  "vpopcnt<ssemodesuffix>\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}")

;; Save multiple registers out-of-line.
(define_insn "save_multiple<mode>"
  [(match_parallel 0 "save_multiple"
    [(use (match_operand:P 1 "symbol_operand"))])]
  "TARGET_SSE && TARGET_64BIT"
  "call\t%P1")

;; Restore multiple registers out-of-line.
(define_insn "restore_multiple<mode>"
  [(match_parallel 0 "restore_multiple"
    [(use (match_operand:P 1 "symbol_operand"))])]
  "TARGET_SSE && TARGET_64BIT"
  "call\t%P1")

;; Restore multiple registers out-of-line and return.
(define_insn "restore_multiple_and_return<mode>"
  [(match_parallel 0 "restore_multiple"
    [(return)
     (use (match_operand:P 1 "symbol_operand"))
     (set (reg:DI SP_REG) (reg:DI R10_REG))
    ])]
  "TARGET_SSE && TARGET_64BIT"
  "jmp\t%P1")

;; Restore multiple registers out-of-line when hard frame pointer is used,
;; perform the leave operation prior to returning (from the function).
(define_insn "restore_multiple_leave_return<mode>"
  [(match_parallel 0 "restore_multiple"
    [(return)
     (use (match_operand:P 1 "symbol_operand"))
     (set (reg:DI SP_REG) (plus:DI (reg:DI BP_REG) (const_int 8)))
     (set (reg:DI BP_REG) (mem:DI (reg:DI BP_REG)))
     (clobber (mem:BLK (scratch)))
    ])]
  "TARGET_SSE && TARGET_64BIT"
  "jmp\t%P1")

(define_insn "vpopcount<mode><mask_name>"
  [(set (match_operand:VI12_AVX512VL 0 "register_operand" "=v")
	(popcount:VI12_AVX512VL
	  (match_operand:VI12_AVX512VL 1 "nonimmediate_operand" "vm")))]
  "TARGET_AVX512BITALG"
  "vpopcnt<ssemodesuffix>\t{%1, %0<mask_operand2>|%0<mask_operand2>, %1}")

(define_insn "vgf2p8affineinvqb_<mode><mask_name>"
  [(set (match_operand:VI1_AVX512F 0 "register_operand" "=x,x,v")
	(unspec:VI1_AVX512F
	  [(match_operand:VI1_AVX512F 1 "register_operand" "%0,x,v")
	   (match_operand:VI1_AVX512F 2 "nonimmediate_operand" "xBm,xm,vm")
	   (match_operand:QI 3 "const_0_to_255_operand" "n,n,n")]
	  UNSPEC_GF2P8AFFINEINV))]
  "TARGET_GFNI"
  "@
   gf2p8affineinvqb\t{%3, %2, %0| %0, %2, %3}
   vgf2p8affineinvqb\t{%3, %2, %1, %0<mask_operand4>| %0<mask_operand4>, %1, %2, %3}
   vgf2p8affineinvqb\t{%3, %2, %1, %0<mask_operand4>| %0<mask_operand4>, %1, %2, %3}"
  [(set_attr "isa" "noavx,avx,avx512f")
   (set_attr "prefix_data16" "1,*,*")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,maybe_evex,evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vgf2p8affineqb_<mode><mask_name>"
  [(set (match_operand:VI1_AVX512F 0 "register_operand" "=x,x,v")
	(unspec:VI1_AVX512F
	  [(match_operand:VI1_AVX512F 1 "register_operand" "%0,x,v")
	   (match_operand:VI1_AVX512F 2 "nonimmediate_operand" "xBm,xm,vm")
	   (match_operand:QI 3 "const_0_to_255_operand" "n,n,n")]
	  UNSPEC_GF2P8AFFINE))]
  "TARGET_GFNI"
  "@
   gf2p8affineqb\t{%3, %2, %0| %0, %2, %3}
   vgf2p8affineqb\t{%3, %2, %1, %0<mask_operand4>| %0<mask_operand4>, %1, %2, %3}
   vgf2p8affineqb\t{%3, %2, %1, %0<mask_operand4>| %0<mask_operand4>, %1, %2, %3}"
  [(set_attr "isa" "noavx,avx,avx512f")
   (set_attr "prefix_data16" "1,*,*")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,maybe_evex,evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vgf2p8mulb_<mode><mask_name>"
  [(set (match_operand:VI1_AVX512F 0 "register_operand" "=x,x,v")
	(unspec:VI1_AVX512F
	  [(match_operand:VI1_AVX512F 1 "register_operand" "%0,x,v")
	   (match_operand:VI1_AVX512F 2 "nonimmediate_operand" "xBm,xm,vm")]
	  UNSPEC_GF2P8MUL))]
  "TARGET_GFNI"
  "@
   gf2p8mulb\t{%2, %0| %0, %2}
   vgf2p8mulb\t{%2, %1, %0<mask_operand3>| %0<mask_operand3>, %1, %2}
   vgf2p8mulb\t{%2, %1, %0<mask_operand3>| %0<mask_operand3>, %1, %2}"
  [(set_attr "isa" "noavx,avx,avx512f")
   (set_attr "prefix_data16" "1,*,*")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "orig,maybe_evex,evex")
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vpshrd_<mode><mask_name>"
  [(set (match_operand:VI248_AVX512VL 0 "register_operand" "=v")
	(unspec:VI248_AVX512VL
	  [(match_operand:VI248_AVX512VL 1 "register_operand" "v")
	   (match_operand:VI248_AVX512VL 2 "nonimmediate_operand" "vm")
	   (match_operand:SI 3 "const_0_to_255_operand" "n")]
	  UNSPEC_VPSHRD))]
  "TARGET_AVX512VBMI2"
  "vpshrd<ssemodesuffix>\t{%3, %2, %1, %0<mask_operand4>|%0<mask_operand4>, %1, %2, %3 }"
   [(set_attr ("prefix") ("evex"))])

(define_insn "vpshld_<mode><mask_name>"
  [(set (match_operand:VI248_AVX512VL 0 "register_operand" "=v")
	(unspec:VI248_AVX512VL
	  [(match_operand:VI248_AVX512VL 1 "register_operand" "v")
	   (match_operand:VI248_AVX512VL 2 "nonimmediate_operand" "vm")
	   (match_operand:SI 3 "const_0_to_255_operand" "n")]
	  UNSPEC_VPSHLD))]
  "TARGET_AVX512VBMI2"
  "vpshld<ssemodesuffix>\t{%3, %2, %1, %0<mask_operand4>|%0<mask_operand4>, %1, %2, %3 }"
   [(set_attr ("prefix") ("evex"))])

(define_insn "vpshrdv_<mode>"
  [(set (match_operand:VI248_AVX512VL 0 "register_operand" "=v")
	(unspec:VI248_AVX512VL
	  [(match_operand:VI248_AVX512VL 1 "register_operand" "0")
	   (match_operand:VI248_AVX512VL 2 "register_operand" "v")
	   (match_operand:VI248_AVX512VL 3 "nonimmediate_operand" "vm")]
	  UNSPEC_VPSHRDV))]
  "TARGET_AVX512VBMI2"
  "vpshrdv<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3 }"
   [(set_attr ("prefix") ("evex"))
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vpshrdv_<mode>_mask"
  [(set (match_operand:VI248_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI248_AVX512VL
	  (unspec:VI248_AVX512VL
	    [(match_operand:VI248_AVX512VL 1 "register_operand" "0")
	     (match_operand:VI248_AVX512VL 2 "register_operand" "v")
	     (match_operand:VI248_AVX512VL 3 "nonimmediate_operand" "vm")]
	    UNSPEC_VPSHRDV)
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512VBMI2"
  "vpshrdv<ssemodesuffix>\t{%3, %2, %0%{%4%}|%0%{%4%}, %2, %3 }"
   [(set_attr ("prefix") ("evex"))
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "vpshrdv_<mode>_maskz"
  [(match_operand:VI248_AVX512VL 0 "register_operand")
   (match_operand:VI248_AVX512VL 1 "register_operand")
   (match_operand:VI248_AVX512VL 2 "register_operand")
   (match_operand:VI248_AVX512VL 3 "nonimmediate_operand")
   (match_operand:<avx512fmaskmode> 4 "register_operand")]
  "TARGET_AVX512VBMI2"
{
  emit_insn (gen_vpshrdv_<mode>_maskz_1 (operands[0], operands[1],
					 operands[2], operands[3],
					 CONST0_RTX (<MODE>mode),
						     operands[4]));
  DONE;
})

(define_insn "vpshrdv_<mode>_maskz_1"
  [(set (match_operand:VI248_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI248_AVX512VL
	  (unspec:VI248_AVX512VL
	    [(match_operand:VI248_AVX512VL 1 "register_operand" "0")
	     (match_operand:VI248_AVX512VL 2 "register_operand" "v")
	     (match_operand:VI248_AVX512VL 3 "nonimmediate_operand" "vm")]
	    UNSPEC_VPSHRDV)
	  (match_operand:VI248_AVX512VL 4 "const0_operand" "C")
	  (match_operand:<avx512fmaskmode> 5 "register_operand" "Yk")))]
  "TARGET_AVX512VBMI2"
  "vpshrdv<ssemodesuffix>\t{%3, %2, %0%{%5%}%{z%}|%0%{%5%}%{z%}, %2, %3 }"
   [(set_attr ("prefix") ("evex"))
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vpshldv_<mode>"
  [(set (match_operand:VI248_AVX512VL 0 "register_operand" "=v")
	(unspec:VI248_AVX512VL
	  [(match_operand:VI248_AVX512VL 1 "register_operand" "0")
	   (match_operand:VI248_AVX512VL 2 "register_operand" "v")
	   (match_operand:VI248_AVX512VL 3 "nonimmediate_operand" "vm")]
	  UNSPEC_VPSHLDV))]
  "TARGET_AVX512VBMI2"
  "vpshldv<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3 }"
   [(set_attr ("prefix") ("evex"))
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vpshldv_<mode>_mask"
  [(set (match_operand:VI248_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI248_AVX512VL
	  (unspec:VI248_AVX512VL
	    [(match_operand:VI248_AVX512VL 1 "register_operand" "0")
	     (match_operand:VI248_AVX512VL 2 "register_operand" "v")
	     (match_operand:VI248_AVX512VL 3 "nonimmediate_operand" "vm")]
	    UNSPEC_VPSHLDV)
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512VBMI2"
  "vpshldv<ssemodesuffix>\t{%3, %2, %0%{%4%}|%0%{%4%}, %2, %3 }"
   [(set_attr ("prefix") ("evex"))
   (set_attr "mode" "<sseinsnmode>")])

(define_expand "vpshldv_<mode>_maskz"
  [(match_operand:VI248_AVX512VL 0 "register_operand")
   (match_operand:VI248_AVX512VL 1 "register_operand")
   (match_operand:VI248_AVX512VL 2 "register_operand")
   (match_operand:VI248_AVX512VL 3 "nonimmediate_operand")
   (match_operand:<avx512fmaskmode> 4 "register_operand")]
  "TARGET_AVX512VBMI2"
{
  emit_insn (gen_vpshldv_<mode>_maskz_1 (operands[0], operands[1],
					 operands[2], operands[3],
					 CONST0_RTX (<MODE>mode),
						     operands[4]));
  DONE;
})

(define_insn "vpshldv_<mode>_maskz_1"
  [(set (match_operand:VI248_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI248_AVX512VL
	  (unspec:VI248_AVX512VL
	    [(match_operand:VI248_AVX512VL 1 "register_operand" "0")
	     (match_operand:VI248_AVX512VL 2 "register_operand" "v")
	     (match_operand:VI248_AVX512VL 3 "nonimmediate_operand" "vm")]
	    UNSPEC_VPSHLDV)
	  (match_operand:VI248_AVX512VL 4 "const0_operand" "C")
	  (match_operand:<avx512fmaskmode> 5 "register_operand" "Yk")))]
  "TARGET_AVX512VBMI2"
  "vpshldv<ssemodesuffix>\t{%3, %2, %0%{%5%}%{z%}|%0%{%5%}%{z%}, %2, %3 }"
   [(set_attr ("prefix") ("evex"))
   (set_attr "mode" "<sseinsnmode>")])

(define_insn "vpdpbusd_<mode>"
  [(set (match_operand:VI4_AVX512VL 0 "register_operand" "=v")
	(unspec:VI4_AVX512VL
	  [(match_operand:VI4_AVX512VL 1 "register_operand" "0")
	   (match_operand:VI4_AVX512VL 2 "register_operand" "v")
	   (match_operand:VI4_AVX512VL 3 "nonimmediate_operand" "vm")]
	  UNSPEC_VPMADDUBSWACCD))]
  "TARGET_AVX512VNNI"
  "vpdpbusd\t{%3, %2, %0|%0, %2, %3 }"
   [(set_attr ("prefix") ("evex"))])

(define_insn "vpdpbusd_<mode>_mask"
  [(set (match_operand:VI4_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI4_AVX512VL
	  (unspec:VI4_AVX512VL
	    [(match_operand:VI4_AVX512VL 1 "register_operand" "0")
	     (match_operand:VI4_AVX512VL 2 "register_operand" "v")
	     (match_operand:VI4_AVX512VL 3 "nonimmediate_operand" "vm")]
	    UNSPEC_VPMADDUBSWACCD)
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512VNNI"
  "vpdpbusd\t{%3, %2, %0%{%4%}|%0%{%4%}, %2, %3 }"
   [(set_attr ("prefix") ("evex"))])

(define_expand "vpdpbusd_<mode>_maskz"
  [(match_operand:VI4_AVX512VL 0 "register_operand")
   (match_operand:VI4_AVX512VL 1 "register_operand")
   (match_operand:VI4_AVX512VL 2 "register_operand")
   (match_operand:VI4_AVX512VL 3 "nonimmediate_operand")
   (match_operand:<avx512fmaskmode> 4 "register_operand")]
  "TARGET_AVX512VNNI"
{
  emit_insn (gen_vpdpbusd_<mode>_maskz_1 (operands[0], operands[1],
					  operands[2], operands[3],
					  CONST0_RTX (<MODE>mode),
						      operands[4]));
  DONE;
})

(define_insn "vpdpbusd_<mode>_maskz_1"
  [(set (match_operand:VI4_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI4_AVX512VL
	  (unspec:VI4_AVX512VL
	    [(match_operand:VI4_AVX512VL 1 "register_operand" "0")
	     (match_operand:VI4_AVX512VL 2 "register_operand" "v")
	     (match_operand:VI4_AVX512VL 3 "nonimmediate_operand" "vm")
	    ] UNSPEC_VPMADDUBSWACCD)
	  (match_operand:VI4_AVX512VL 4 "const0_operand" "C")
	  (match_operand:<avx512fmaskmode> 5 "register_operand" "Yk")))]
  "TARGET_AVX512VNNI"
  "vpdpbusd\t{%3, %2, %0%{%5%}%{z%}|%0%{%5%}%{z%}, %2, %3 }"
   [(set_attr ("prefix") ("evex"))])


(define_insn "vpdpbusds_<mode>"
  [(set (match_operand:VI4_AVX512VL 0 "register_operand" "=v")
	(unspec:VI4_AVX512VL
	  [(match_operand:VI4_AVX512VL 1 "register_operand" "0")
	   (match_operand:VI4_AVX512VL 2 "register_operand" "v")
	   (match_operand:VI4_AVX512VL 3 "nonimmediate_operand" "vm")]
	  UNSPEC_VPMADDUBSWACCSSD))]
  "TARGET_AVX512VNNI"
  "vpdpbusds\t{%3, %2, %0|%0, %2, %3 }"
   [(set_attr ("prefix") ("evex"))])

(define_insn "vpdpbusds_<mode>_mask"
  [(set (match_operand:VI4_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI4_AVX512VL
	  (unspec:VI4_AVX512VL
	    [(match_operand:VI4_AVX512VL 1 "register_operand" "0")
	     (match_operand:VI4_AVX512VL 2 "register_operand" "v")
	     (match_operand:VI4_AVX512VL 3 "nonimmediate_operand" "vm")]
	    UNSPEC_VPMADDUBSWACCSSD)
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512VNNI"
  "vpdpbusds\t{%3, %2, %0%{%4%}|%0%{%4%}, %2, %3 }"
   [(set_attr ("prefix") ("evex"))])

(define_expand "vpdpbusds_<mode>_maskz"
  [(match_operand:VI4_AVX512VL 0 "register_operand")
   (match_operand:VI4_AVX512VL 1 "register_operand")
   (match_operand:VI4_AVX512VL 2 "register_operand")
   (match_operand:VI4_AVX512VL 3 "nonimmediate_operand")
   (match_operand:<avx512fmaskmode> 4 "register_operand")]
  "TARGET_AVX512VNNI"
{
  emit_insn (gen_vpdpbusds_<mode>_maskz_1 (operands[0], operands[1],
					   operands[2], operands[3],
					   CONST0_RTX (<MODE>mode),
						       operands[4]));
  DONE;
})

(define_insn "vpdpbusds_<mode>_maskz_1"
  [(set (match_operand:VI4_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI4_AVX512VL
	  (unspec:VI4_AVX512VL
	    [(match_operand:VI4_AVX512VL 1 "register_operand" "0")
	     (match_operand:VI4_AVX512VL 2 "register_operand" "v")
	     (match_operand:VI4_AVX512VL 3 "nonimmediate_operand" "vm")]
	    UNSPEC_VPMADDUBSWACCSSD)
	  (match_operand:VI4_AVX512VL 4 "const0_operand" "C")
	  (match_operand:<avx512fmaskmode> 5 "register_operand" "Yk")))]
  "TARGET_AVX512VNNI"
  "vpdpbusds\t{%3, %2, %0%{%5%}%{z%}|%0%{%5%}%{z%}, %2, %3 }"
   [(set_attr ("prefix") ("evex"))])


(define_insn "vpdpwssd_<mode>"
  [(set (match_operand:VI4_AVX512VL 0 "register_operand" "=v")
	(unspec:VI4_AVX512VL
	  [(match_operand:VI4_AVX512VL 1 "register_operand" "0")
	   (match_operand:VI4_AVX512VL 2 "register_operand" "v")
	   (match_operand:VI4_AVX512VL 3 "nonimmediate_operand" "vm")]
	  UNSPEC_VPMADDWDACCD))]
  "TARGET_AVX512VNNI"
  "vpdpwssd\t{%3, %2, %0|%0, %2, %3 }"
   [(set_attr ("prefix") ("evex"))])

(define_insn "vpdpwssd_<mode>_mask"
  [(set (match_operand:VI4_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI4_AVX512VL
	  (unspec:VI4_AVX512VL
	    [(match_operand:VI4_AVX512VL 1 "register_operand" "0")
	     (match_operand:VI4_AVX512VL 2 "register_operand" "v")
	     (match_operand:VI4_AVX512VL 3 "nonimmediate_operand" "vm")]
	    UNSPEC_VPMADDWDACCD)
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512VNNI"
  "vpdpwssd\t{%3, %2, %0%{%4%}|%0%{%4%}, %2, %3 }"
   [(set_attr ("prefix") ("evex"))])

(define_expand "vpdpwssd_<mode>_maskz"
  [(match_operand:VI4_AVX512VL 0 "register_operand")
   (match_operand:VI4_AVX512VL 1 "register_operand")
   (match_operand:VI4_AVX512VL 2 "register_operand")
   (match_operand:VI4_AVX512VL 3 "nonimmediate_operand")
   (match_operand:<avx512fmaskmode> 4 "register_operand")]
  "TARGET_AVX512VNNI"
{
  emit_insn (gen_vpdpwssd_<mode>_maskz_1 (operands[0], operands[1],
					  operands[2], operands[3],
					  CONST0_RTX (<MODE>mode),
						      operands[4]));
  DONE;
})

(define_insn "vpdpwssd_<mode>_maskz_1"
  [(set (match_operand:VI4_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI4_AVX512VL
	  (unspec:VI4_AVX512VL
	    [(match_operand:VI4_AVX512VL 1 "register_operand" "0")
	     (match_operand:VI4_AVX512VL 2 "register_operand" "v")
	     (match_operand:VI4_AVX512VL 3 "nonimmediate_operand" "vm")]
	    UNSPEC_VPMADDWDACCD)
	  (match_operand:VI4_AVX512VL 4 "const0_operand" "C")
	  (match_operand:<avx512fmaskmode> 5 "register_operand" "Yk")))]
  "TARGET_AVX512VNNI"
  "vpdpwssd\t{%3, %2, %0%{%5%}%{z%}|%0%{%5%}%{z%}, %2, %3 }"
   [(set_attr ("prefix") ("evex"))])


(define_insn "vpdpwssds_<mode>"
  [(set (match_operand:VI4_AVX512VL 0 "register_operand" "=v")
	(unspec:VI4_AVX512VL
	  [(match_operand:VI4_AVX512VL 1 "register_operand" "0")
	   (match_operand:VI4_AVX512VL 2 "register_operand" "v")
	   (match_operand:VI4_AVX512VL 3 "nonimmediate_operand" "vm")]
	  UNSPEC_VPMADDWDACCSSD))]
  "TARGET_AVX512VNNI"
  "vpdpwssds\t{%3, %2, %0|%0, %2, %3 }"
   [(set_attr ("prefix") ("evex"))])

(define_insn "vpdpwssds_<mode>_mask"
  [(set (match_operand:VI4_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI4_AVX512VL
	  (unspec:VI4_AVX512VL
	    [(match_operand:VI4_AVX512VL 1 "register_operand" "0")
	     (match_operand:VI4_AVX512VL 2 "register_operand" "v")
	     (match_operand:VI4_AVX512VL 3 "nonimmediate_operand" "vm")]
	    UNSPEC_VPMADDWDACCSSD)
	  (match_dup 1)
	  (match_operand:<avx512fmaskmode> 4 "register_operand" "Yk")))]
  "TARGET_AVX512VNNI"
  "vpdpwssds\t{%3, %2, %0%{%4%}|%0%{%4%}, %2, %3 }"
   [(set_attr ("prefix") ("evex"))])

(define_expand "vpdpwssds_<mode>_maskz"
  [(match_operand:VI4_AVX512VL 0 "register_operand")
   (match_operand:VI4_AVX512VL 1 "register_operand")
   (match_operand:VI4_AVX512VL 2 "register_operand")
   (match_operand:VI4_AVX512VL 3 "nonimmediate_operand")
   (match_operand:<avx512fmaskmode> 4 "register_operand")]
  "TARGET_AVX512VNNI"
{
  emit_insn (gen_vpdpwssds_<mode>_maskz_1 (operands[0], operands[1],
					   operands[2], operands[3],
					   CONST0_RTX (<MODE>mode),
						       operands[4]));
  DONE;
})

(define_insn "vpdpwssds_<mode>_maskz_1"
  [(set (match_operand:VI4_AVX512VL 0 "register_operand" "=v")
	(vec_merge:VI4_AVX512VL
	  (unspec:VI4_AVX512VL
	    [(match_operand:VI4_AVX512VL 1 "register_operand" "0")
	     (match_operand:VI4_AVX512VL 2 "register_operand" "v")
	     (match_operand:VI4_AVX512VL 3 "nonimmediate_operand" "vm")]
	    UNSPEC_VPMADDWDACCSSD)
	  (match_operand:VI4_AVX512VL 4 "const0_operand" "C")
	  (match_operand:<avx512fmaskmode> 5 "register_operand" "Yk")))]
  "TARGET_AVX512VNNI"
  "vpdpwssds\t{%3, %2, %0%{%5%}%{z%}|%0%{%5%}%{z%}, %2, %3 }"
   [(set_attr ("prefix") ("evex"))])

(define_insn "vaesdec_<mode>"
  [(set (match_operand:VI1_AVX512VL_F 0 "register_operand" "=v")
	(unspec:VI1_AVX512VL_F
	  [(match_operand:VI1_AVX512VL_F 1 "register_operand" "v")
	   (match_operand:VI1_AVX512VL_F 2 "vector_operand" "v")]
	  UNSPEC_VAESDEC))]
  "TARGET_VAES"
  "vaesdec\t{%2, %1, %0|%0, %1, %2}"
)

(define_insn "vaesdeclast_<mode>"
  [(set (match_operand:VI1_AVX512VL_F 0 "register_operand" "=v")
	(unspec:VI1_AVX512VL_F
	  [(match_operand:VI1_AVX512VL_F 1 "register_operand" "v")
	   (match_operand:VI1_AVX512VL_F 2 "vector_operand" "v")]
	  UNSPEC_VAESDECLAST))]
  "TARGET_VAES"
  "vaesdeclast\t{%2, %1, %0|%0, %1, %2}"
)

(define_insn "vaesenc_<mode>"
  [(set (match_operand:VI1_AVX512VL_F 0 "register_operand" "=v")
	(unspec:VI1_AVX512VL_F
	  [(match_operand:VI1_AVX512VL_F 1 "register_operand" "v")
	   (match_operand:VI1_AVX512VL_F 2 "vector_operand" "vm")]
	  UNSPEC_VAESENC))]
  "TARGET_VAES"
  "vaesenc\t{%2, %1, %0|%0, %1, %2}"
)

(define_insn "vaesenclast_<mode>"
  [(set (match_operand:VI1_AVX512VL_F 0 "register_operand" "=v")
	(unspec:VI1_AVX512VL_F
	  [(match_operand:VI1_AVX512VL_F 1 "register_operand" "v")
	   (match_operand:VI1_AVX512VL_F 2 "vector_operand" "vm")]
	  UNSPEC_VAESENCLAST))]
  "TARGET_VAES"
  "vaesenclast\t{%2, %1, %0|%0, %1, %2}"
)

(define_insn "vpclmulqdq_<mode>"
  [(set (match_operand:VI8_FVL 0 "register_operand" "=v")
	(unspec:VI8_FVL [(match_operand:VI8_FVL 1 "register_operand" "v")
			 (match_operand:VI8_FVL 2 "vector_operand" "vm")
			 (match_operand:SI 3 "const_0_to_255_operand" "n")]
			UNSPEC_VPCLMULQDQ))]
  "TARGET_VPCLMULQDQ"
  "vpclmulqdq\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "mode" "DI")])

(define_insn "avx512vl_vpshufbitqmb<mode><mask_scalar_merge_name>"
  [(set (match_operand:<avx512fmaskmode> 0 "register_operand" "=Yk")
	(unspec:<avx512fmaskmode>
	  [(match_operand:VI1_AVX512VLBW 1 "register_operand" "v")
	   (match_operand:VI1_AVX512VLBW 2 "nonimmediate_operand" "vm")]
	  UNSPEC_VPSHUFBIT))]
  "TARGET_AVX512BITALG"
  "vpshufbitqmb\t{%2, %1, %0<mask_scalar_merge_operand3>|%0<mask_scalar_merge_operand3>, %1, %2}"
  [(set_attr "prefix" "evex")
   (set_attr "mode" "<sseinsnmode>")])
