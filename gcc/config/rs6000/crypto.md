;; Cryptographic instructions added in ISA 2.07
;; Copyright (C) 2012-2013 Free Software Foundation, Inc.
;; Contributed by Michael Meissner (meissner@linux.vnet.ibm.com)

;; This file is part of GCC.

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

(define_c_enum "unspec"
  [UNSPEC_VCIPHER
   UNSPEC_VNCIPHER
   UNSPEC_VCIPHERLAST
   UNSPEC_VNCIPHERLAST
   UNSPEC_VSBOX
   UNSPEC_VSHASIGMA
   UNSPEC_VPERMXOR
   UNSPEC_VPMSUM])

;; Iterator for VPMSUM/VPERMXOR
(define_mode_iterator CR_mode [V16QI V8HI V4SI V2DI])

(define_mode_attr CR_char [(V16QI "b")
			   (V8HI  "h")
			   (V4SI  "w")
			   (V2DI  "d")])

;; Iterator for VSHASIGMAD/VSHASIGMAW
(define_mode_iterator CR_hash [V4SI V2DI])

;; Iterator for the other crypto functions
(define_int_iterator CR_code   [UNSPEC_VCIPHER
				UNSPEC_VNCIPHER
				UNSPEC_VCIPHERLAST
				UNSPEC_VNCIPHERLAST])

(define_int_attr CR_insn [(UNSPEC_VCIPHER      "vcipher")
			  (UNSPEC_VNCIPHER     "vncipher")
			  (UNSPEC_VCIPHERLAST  "vcipherlast")
			  (UNSPEC_VNCIPHERLAST "vncipherlast")])

;; 2 operand crypto instructions
(define_insn "crypto_<CR_insn>"
  [(set (match_operand:V2DI 0 "register_operand" "=v")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "v")
		      (match_operand:V2DI 2 "register_operand" "v")]
		     CR_code))]
  "TARGET_CRYPTO"
  "<CR_insn> %0,%1,%2"
  [(set_attr "type" "crypto")])

(define_insn "crypto_vpmsum<CR_char>"
  [(set (match_operand:CR_mode 0 "register_operand" "=v")
	(unspec:CR_mode [(match_operand:CR_mode 1 "register_operand" "v")
			 (match_operand:CR_mode 2 "register_operand" "v")]
			UNSPEC_VPMSUM))]
  "TARGET_CRYPTO"
  "vpmsum<CR_char> %0,%1,%2"
  [(set_attr "type" "crypto")])

;; 3 operand crypto instructions
(define_insn "crypto_vpermxor_<mode>"
  [(set (match_operand:CR_mode 0 "register_operand" "=v")
	(unspec:CR_mode [(match_operand:CR_mode 1 "register_operand" "v")
			 (match_operand:CR_mode 2 "register_operand" "v")
			 (match_operand:CR_mode 3 "register_operand" "v")]
			UNSPEC_VPERMXOR))]
  "TARGET_CRYPTO"
  "vpermxor %0,%1,%2,%3"
  [(set_attr "type" "crypto")])

;; 1 operand crypto instruction
(define_insn "crypto_vsbox"
  [(set (match_operand:V2DI 0 "register_operand" "=v")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "v")]
		     UNSPEC_VSBOX))]
  "TARGET_CRYPTO"
  "vsbox %0,%1"
  [(set_attr "type" "crypto")])

;; Hash crypto instructions
(define_insn "crypto_vshasigma<CR_char>"
  [(set (match_operand:CR_hash 0 "register_operand" "=v")
	(unspec:CR_hash [(match_operand:CR_hash 1 "register_operand" "v")
			 (match_operand:SI 2 "const_0_to_1_operand" "n")
			 (match_operand:SI 3 "const_0_to_15_operand" "n")]
			UNSPEC_VSHASIGMA))]
  "TARGET_CRYPTO"
  "vshasigma<CR_char> %0,%1,%2,%3"
  [(set_attr "type" "crypto")])
