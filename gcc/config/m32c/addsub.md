;; Machine Descriptions for R8C/M16C/M32C
;; Copyright (C) 2005
;; Free Software Foundation, Inc.
;; Contributed by Red Hat.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;; add, sub

(define_insn "addqi3"
  [(set (match_operand:QI 0 "mra_or_sp_operand"
		  "=SdRhl,SdRhl,??Rmm,??Rmm, Raa,Raa,SdRhl,??Rmm")
	(plus:QI (match_operand:QI 1 "mra_operand"
		  "%0,0,0,0, 0,0,0,0")
		 (match_operand:QI 2 "mrai_operand"
		  "iSdRhl,?Rmm,iSdRhl,?Rmm, iSdRhl,?Rmm,Raa,Raa")))]
  ""
  "add.b\t%2,%0"
  [(set_attr "flags" "oszc")]
  )

(define_insn "addhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand"
	 	  "=SdRhi,SdRhi,??Rmm,??Rmm, SdRhi,??Rmm, Rhi, Raw, Raw, !Rsp")
	(plus:HI (match_operand:HI 1 "general_operand"
		  "%0,0,0,0, 0,0, Raw, Rfb, Rfb, 0")
		 (match_operand:HI 2 "general_operand"
		  "IU2sSdRhi,?Rmm,IU2sSdRhi,?Rmm, IM2,IM2, IS2IU2, I00, IS1, i")))]
  ""
  "@
   add.w\t%2,%0
   add.w\t%2,%0
   add.w\t%2,%0
   add.w\t%2,%0
   sub.w\t%m2,%0
   sub.w\t%m2,%0
   mova\t%d2[%1],%0
   stc\t%1,%0
   mova\t%D2[%1],%0
   add.w\t%2,%0"
  [(set_attr "flags" "oszc,oszc,oszc,oszc,oszc,oszc,oszc,oszc,oszc,oszc")]
  )

(define_insn "addpsi3"
  [(set (match_operand:PSI 0 "nonimmediate_operand" "=SdRpi,SdRpi,Rsp*Rmm, Rpi,Rpi,Rhi,&Rhi")
	(plus:PSI (match_operand:PSI 1 "nonimmediate_operand" "0,0,0, Raa,Rad,!Rcl,Rhi")
		  (match_operand:PSI 2 "general_operand" "iSdRpi,?Rmm,i, i,IS2,i,!Rcl")))]
  "TARGET_A24"
  "@
   add.%&\t%2,%0
   add.%&\t%2,%0
   add.%&\t%2,%0
   mova\t%d2[%1],%0
   mova\t%D2[%1],%0
   #
   #"
  [(set_attr "flags" "oszc,oszc,oszc,*,*,oszc,oszc")]
  )

; This is needed for reloading large frames.
(define_split
  [(set (match_operand:PSI 0 "ra_operand" "")
	(plus:PSI (match_operand:PSI 1 "cr_operand" "")
		 (match_operand:PSI 2 "immediate_operand" "")))]
  ""
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0)
	(plus:PSI (match_dup 0)
		 (match_dup 2)))]
  ""
  )

; This is needed for reloading large frames.
(define_split
  [(set (match_operand:PSI 0 "ra_operand" "")
	(plus:PSI (match_operand:PSI 1 "ra_operand" "")
		 (match_operand:PSI 2 "cr_operand" "")))]
  ""
  [(set (match_dup 0) (match_dup 2))
   (set (match_dup 0)
	(plus:PSI (match_dup 0)
		 (match_dup 1)))]
  ""
  )

(define_insn "subqi3"
  [(set (match_operand:QI 0 "mra_or_sp_operand"
		   "=SdRhl,SdRhl,??Rmm,??Rmm, Raa,Raa,SdRhl,??Rmm, *Rsp")
	(minus:QI (match_operand:QI 1 "mra_operand"
		   "0,0,0,0, 0,0,0,0, 0")
		  (match_operand:QI 2 "mrai_operand"
		   "iSdRhl,?Rmm,iSdRhl,?Rmm, iSdRhl,?Rmm,Raa,Raa, i")))]
  ""
  "sub.b\t%2,%0"
  [(set_attr "flags" "oszc")]
  )

(define_insn "subhi3"
  [(set (match_operand:HI 0 "mra_operand"
		   "=SdRhi,SdRhi,??Rmm,??Rmm, SdRhi,??Rmm")
	(minus:HI (match_operand:HI 1 "mras_operand"
		   "0,0,0,0, 0,0")
		  (match_operand:HI 2 "mrai_operand"
		   "IU2SdRhi,?Rmm,IU2SdRhi,?Rmm, IM2,IM2")))]
  ""
  "@
   sub.w\t%2,%0
   sub.w\t%2,%0
   sub.w\t%2,%0
   sub.w\t%2,%0
   add.w\t%m2,%0
   add.w\t%m2,%0"
  [(set_attr "flags" "oszc,oszc,oszc,oszc,oszc,oszc")]
  )

(define_insn "subpsi3"
  [(set (match_operand:PSI 0 "mra_operand" "=RpiSd,RpiSd,??Rmm,??Rmm")
	(minus:PSI (match_operand:PSI 1 "mra_operand" "0,0,0,0")
		   (match_operand:PSI 2 "mrai_operand" "iRpiSd,?Rmm,iRpiSd,?Rmm")))]
  "TARGET_A24"
  "sub.%&\t%2,%0"
  [(set_attr "flags" "oszc")]
  )

(define_insn "negqi2"
  [(set (match_operand:QI 0 "mra_operand" "=SdRhl,??Rmm")
	(neg:QI (match_operand:QI 1 "mra_operand" "0,0")))]
  ""
  "neg.b\t%0"
  [(set_attr "flags" "oszc,oszc")]
  )

(define_insn "neghi2"
  [(set (match_operand:HI 0 "mra_operand" "=SdRhi,??Rmm")
	(neg:HI (match_operand:HI 1 "mra_operand" "0,0")))]
  ""
  "neg.w\t%0"
  [(set_attr "flags" "oszc,oszc")]
  )

; We can negate an SImode by operating on the subparts.  GCC deals
; with this itself for larger modes, but not SI.
(define_insn "negsi2"
  [(set (match_operand:SI 0 "mra_operand" "=SdR03,??Rmm")
	(neg:SI (match_operand:SI 1 "mra_operand" "0,0")))]
  ""
  "not.w %h0 | not.w %H0 | add.w #1,%h0 | adcf.w %H0"
  [(set_attr "flags" "oszc,oszc")]
  )

(define_insn "absqi2"
  [(set (match_operand:QI 0 "mra_operand" "=RhlSd,??Rmm")
	(abs:QI (match_operand:QI 1 "mra_operand" "0,0")))]
  ""
  "abs.b\t%0"
  [(set_attr "flags" "oszc")]
  )

(define_insn "abshi2"
  [(set (match_operand:HI 0 "mra_operand" "=RhiSd,??Rmm")
	(abs:HI (match_operand:HI 1 "mra_operand" "0,0")))]
  ""
  "abs.w\t%0"
  [(set_attr "flags" "oszc")]
  )
