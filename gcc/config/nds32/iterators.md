;; Code and mode itertator and attribute definitions
;; of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2020 Free Software Foundation, Inc.
;; Contributed by Andes Technology Corporation.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;;----------------------------------------------------------------------------
;; Mode iterators.
;;----------------------------------------------------------------------------

;; A list of integer modes that are up to one word long.
(define_mode_iterator QIHISI [QI HI SI])

;; A list of integer modes for one word and double word.
(define_mode_iterator SIDI [SI DI])

;; A list of integer modes that are up to one half-word long.
(define_mode_iterator QIHI [QI HI])

;; A list of the modes that are up to double-word long.
(define_mode_iterator DIDF [DI DF])

;; A list of the modes that are up to one word long vector.
(define_mode_iterator VQIHI [V4QI V2HI])

;; A list of the modes that are up to one word long vector
;; and scalar for varies mode.
(define_mode_iterator VSHI [V2HI HI])
(define_mode_iterator VSQIHI [V4QI V2HI QI HI])
(define_mode_iterator VSQIHIDI [V4QI V2HI QI HI DI])
(define_mode_iterator VQIHIDI [V4QI V2HI DI])

;; A list of the modes that are up to double-word long.
(define_mode_iterator ANYF [(SF "TARGET_FPU_SINGLE")
			    (DF "TARGET_FPU_DOUBLE")])

;;----------------------------------------------------------------------------
;; Mode attributes.
;;----------------------------------------------------------------------------

(define_mode_attr size [(QI "b") (HI "h") (SI "w") (SF "s") (DF "d")])

(define_mode_attr byte [(QI "1") (HI "2") (SI "4") (V4QI "4") (V2HI "4")])

(define_mode_attr bits [(V4QI "8") (QI "8") (V2HI "16") (HI "16") (DI "64")])

(define_mode_attr VELT [(V4QI "QI") (V2HI "HI")])

;;----------------------------------------------------------------------------
;; Code iterators.
;;----------------------------------------------------------------------------

;; shifts
(define_code_iterator shift_rotate [ashift ashiftrt lshiftrt rotatert])

(define_code_iterator shifts [ashift ashiftrt lshiftrt])

(define_code_iterator shiftrt [ashiftrt lshiftrt])

(define_code_iterator sat_plus [ss_plus us_plus])

(define_code_iterator all_plus [plus ss_plus us_plus])

(define_code_iterator sat_minus [ss_minus us_minus])

(define_code_iterator all_minus [minus ss_minus us_minus])

(define_code_iterator plus_minus [plus minus])

(define_code_iterator extend [sign_extend zero_extend])

(define_code_iterator sumax [smax umax])

(define_code_iterator sumin [smin umin])

(define_code_iterator sumin_max [smax umax smin umin])

;;----------------------------------------------------------------------------
;; Code attributes.
;;----------------------------------------------------------------------------

;; shifts
(define_code_attr shift
  [(ashift "ashl") (ashiftrt "ashr") (lshiftrt "lshr") (rotatert "rotr")])

(define_code_attr su
  [(ashiftrt "") (lshiftrt "u") (sign_extend "s") (zero_extend "u")])

(define_code_attr zs
  [(sign_extend "s") (zero_extend "z")])

(define_code_attr uk
  [(plus "") (ss_plus "k") (us_plus "uk")
   (minus "") (ss_minus "k") (us_minus "uk")])

(define_code_attr opcode
  [(plus "add") (minus "sub") (smax "smax") (umax "umax") (smin "smin") (umin "umin")])

(define_code_attr add_rsub
  [(plus "a") (minus "rs")])

(define_code_attr add_sub
  [(plus "a") (minus "s")])

;;----------------------------------------------------------------------------
