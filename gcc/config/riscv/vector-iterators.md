;; Iterators for RISC-V 'V' Extension for GNU compiler.
;; Copyright (C) 2022-2022 Free Software Foundation, Inc.
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

(define_mode_iterator V [
  VNx1QI VNx2QI VNx4QI VNx8QI VNx16QI VNx32QI (VNx64QI "TARGET_MIN_VLEN > 32")
  VNx1HI VNx2HI VNx4HI VNx8HI VNx16HI (VNx32HI "TARGET_MIN_VLEN > 32")
  VNx1SI VNx2SI VNx4SI VNx8SI (VNx16SI "TARGET_MIN_VLEN > 32")
  VNx1DI VNx2DI VNx4DI (VNx8DI "TARGET_MIN_VLEN > 32")
  (VNx1SF "TARGET_VECTOR_ELEN_FP_32")
  (VNx2SF "TARGET_VECTOR_ELEN_FP_32")
  (VNx4SF "TARGET_VECTOR_ELEN_FP_32")
  (VNx8SF "TARGET_VECTOR_ELEN_FP_32")
  (VNx16SF "TARGET_VECTOR_ELEN_FP_32 && TARGET_MIN_VLEN > 32")
  (VNx1DF "TARGET_VECTOR_ELEN_FP_64")
  (VNx2DF "TARGET_VECTOR_ELEN_FP_64")
  (VNx4DF "TARGET_VECTOR_ELEN_FP_64")
  (VNx8DF "TARGET_VECTOR_ELEN_FP_64")
])

(define_mode_iterator VB [
  VNx1BI VNx2BI VNx4BI VNx8BI VNx16BI VNx32BI
  (VNx64BI "TARGET_MIN_VLEN > 32")
])

(define_mode_attr VM [
  (VNx1QI "VNx1BI") (VNx2QI "VNx2BI") (VNx4QI "VNx4BI") (VNx8QI "VNx8BI") (VNx16QI "VNx16BI") (VNx32QI "VNx32BI") (VNx64QI "VNx64BI")
  (VNx1HI "VNx1BI") (VNx2HI "VNx2BI") (VNx4HI "VNx4BI") (VNx8HI "VNx8BI") (VNx16HI "VNx16BI") (VNx32HI "VNx32BI")
  (VNx1SI "VNx1BI") (VNx2SI "VNx2BI") (VNx4SI "VNx4BI") (VNx8SI "VNx8BI") (VNx16SI "VNx16BI")
  (VNx1DI "VNx1BI") (VNx2DI "VNx2BI") (VNx4DI "VNx4BI") (VNx8DI "VNx8BI")
  (VNx1SF "VNx1BI") (VNx2SF "VNx2BI") (VNx4SF "VNx4BI") (VNx8SF "VNx8BI") (VNx16SF "VNx16BI")
  (VNx1DF "VNx1BI") (VNx2DF "VNx2BI") (VNx4DF "VNx4BI") (VNx8DF "VNx8BI")
])

(define_mode_attr sew [
  (VNx1QI "8") (VNx2QI "8") (VNx4QI "8") (VNx8QI "8") (VNx16QI "8") (VNx32QI "8") (VNx64QI "8")
  (VNx1HI "16") (VNx2HI "16") (VNx4HI "16") (VNx8HI "16") (VNx16HI "16") (VNx32HI "16")
  (VNx1SI "32") (VNx2SI "32") (VNx4SI "32") (VNx8SI "32") (VNx16SI "32")
  (VNx1DI "64") (VNx2DI "64") (VNx4DI "64") (VNx8DI "64")
  (VNx1SF "32") (VNx2SF "32") (VNx4SF "32") (VNx8SF "32") (VNx16SF "32")
  (VNx1DF "64") (VNx2DF "64") (VNx4DF "64") (VNx8DF "64")
])
