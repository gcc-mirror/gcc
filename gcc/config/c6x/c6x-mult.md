;; -*- buffer-read-only: t -*-
;; Generated automatically from c6x-mult.md.in by genmult.sh
;; Multiplication patterns for TI C6X.
;; This file is processed by genmult.sh to produce two variants of each
;; pattern, a normal one and a real_mult variant for modulo scheduling.
;; Copyright (C) 2010-2023 Free Software Foundation, Inc.
;; Contributed by Bernd Schmidt <bernds@codesourcery.com>
;; Contributed by CodeSourcery.
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

;; -------------------------------------------------------------------------
;; Miscellaneous insns that execute on the M units
;; -------------------------------------------------------------------------

(define_insn "rotlsi3"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
        (rotate:SI (match_operand:SI 1 "register_operand" "a,b,?b,?a")
		   (match_operand:SI 2 "reg_or_ucst5_operand" "aIu5,bIu5,aIu5,bIu5")))]
  "TARGET_INSNS_64"
  "%|%.\\trotl\\t%$\\t%1, %2, %0"
  [(set_attr "units" "m")
   (set_attr "type" "mpy2")
   (set_attr "cross" "n,n,y,y")])

(define_insn "bitrevsi2"
  [(set (match_operand:SI 0 "register_operand" "=a,a,b,b")
	(unspec:SI [(match_operand:SI 1 "register_operand" "a,?b,b,?a")]
		   UNSPEC_BITREV))]
  "TARGET_INSNS_64"
  "%|%.\\tbitr\\t%$\\t%1, %0"
  [(set_attr "units" "m")
   (set_attr "type" "mpy2")
   (set_attr "cross" "n,y,n,y")])

;; Vector average.

(define_insn "avgv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=a,b,a,b")
        (unspec:V2HI [(match_operand:V2HI 1 "register_operand" "a,b,?b,?a")
		      (match_operand:V2HI 2 "register_operand" "a,b,a,b")] UNSPEC_AVG))]
  "TARGET_INSNS_64"
  "%|%.\\tavg2\\t%$\\t%1, %2, %0"
  [(set_attr "units" "m")
   (set_attr "type" "mpy2")
   (set_attr "cross" "n,n,y,y")])

(define_insn "uavgv4qi3"
  [(set (match_operand:V4QI 0 "register_operand" "=a,b,a,b")
        (unspec:V4QI [(match_operand:V4QI 1 "register_operand" "a,b,?b,?a")
		      (match_operand:V4QI 2 "register_operand" "a,b,a,b")] UNSPEC_AVG))]
  "TARGET_INSNS_64"
  "%|%.\\tavgu4\\t%$\\t%1, %2, %0"
  [(set_attr "units" "m")
   (set_attr "type" "mpy2")
   (set_attr "cross" "n,n,y,y")])

;; -------------------------------------------------------------------------
;; Multiplication
;; -------------------------------------------------------------------------

(define_insn "mulhi3"
  [(set (match_operand:HI 0 "register_operand" "=a,b,a,b")
        (mult:HI (match_operand:HI 1 "register_operand" "a,b,?b,?a")
                 (match_operand:HI 2 "reg_or_scst5_operand" "aIs5,bIs5,aIs5,bIs5")))]
  ""
  "%|%.\\tmpy\\t%$\\t%2, %1, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "op_pattern" "sxs")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhisi3_const"
  [(set (match_operand:SI 0 "register_operand" "=a,b,ab")
        (mult:SI (sign_extend:SI
		  (match_operand:HI 1 "register_operand" "a,b,?ab"))
                 (match_operand:HI 2 "scst5_operand" "Is5,Is5,Is5")))]
  ""
  "%|%.\\tmpy\\t%$\\t%2, %1, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y")])

(define_insn "*mulhisi3_insn"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
        (mult:SI (sign_extend:SI
		  (match_operand:HI 1 "register_operand" "%a,b,?a,?b"))
                 (sign_extend:SI
		  (match_operand:HI 2 "reg_or_scst5_operand" "a,b,b,a"))))]
  ""
  "%|%.\\tmpy\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "op_pattern" "ssx")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhisi3_lh"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
        (mult:SI (sign_extend:SI
		  (match_operand:HI 1 "register_operand" "a,b,?a,?b"))
                 (ashiftrt:SI
		  (match_operand:SI 2 "register_operand" "a,b,b,a")
		  (const_int 16))))]
  ""
  "%|%.\\tmpylh\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhisi3_hl"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
        (mult:SI (ashiftrt:SI
		  (match_operand:SI 1 "register_operand" "a,b,?a,?b")
		  (const_int 16))
                 (sign_extend:SI
		  (match_operand:HI 2 "register_operand" "a,b,b,a"))))]
  ""
  "%|%.\\tmpyhl\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhisi3_hh"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
        (mult:SI (ashiftrt:SI
		  (match_operand:SI 1 "register_operand" "%a,b,?a,?b")
		  (const_int 16))
                 (ashiftrt:SI
		  (match_operand:SI 2 "register_operand" "a,b,b,a")
		  (const_int 16))))]
  ""
  "%|%.\\tmpyh\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "umulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
        (mult:SI (zero_extend:SI
		  (match_operand:HI 1 "register_operand" "%a,b,?a,?b"))
                 (zero_extend:SI
		  (match_operand:HI 2 "register_operand" "a,b,b,a"))))]
  ""
  "%|%.\\tmpyu\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "umulhisi3_lh"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
        (mult:SI (zero_extend:SI
		  (match_operand:HI 1 "register_operand" "a,b,?a,?b"))
                 (lshiftrt:SI
		  (match_operand:SI 2 "register_operand" "a,b,b,a")
		  (const_int 16))))]
  ""
  "%|%.\\tmpylhu\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "umulhisi3_hl"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
        (mult:SI (lshiftrt:SI
		  (match_operand:SI 1 "register_operand" "a,b,?a,?b")
		  (const_int 16))
                 (zero_extend:SI
		  (match_operand:HI 2 "register_operand" "a,b,b,a"))))]
  ""
  "%|%.\\tmpyhlu\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "umulhisi3_hh"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
        (mult:SI (lshiftrt:SI
		  (match_operand:SI 1 "register_operand" "%a,b,?a,?b")
		  (const_int 16))
                 (lshiftrt:SI
		  (match_operand:SI 2 "register_operand" "a,b,b,a")
		  (const_int 16))))]
  ""
  "%|%.\\tmpyhu\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "usmulhisi3_const"
  [(set (match_operand:SI 0 "register_operand" "=a,b,ab")
        (mult:SI (zero_extend:SI
		  (match_operand:HI 1 "register_operand" "a,b,?ab"))
                 (match_operand:SI 2 "scst5_operand" "Is5,Is5,Is5")))]
  ""
  "%|%.\\tmpysu\\t%$\\t%2, %1, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y")])

(define_insn "*usmulhisi3_insn"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
        (mult:SI (zero_extend:SI
		  (match_operand:HI 1 "register_operand" "a,b,?a,?b"))
                 (sign_extend:SI
		  (match_operand:HI 2 "reg_or_scst5_operand" "aIs5,bIs5,bIs5,aIs5"))))]
  ""
  "%|%.\\tmpyus\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "usmulhisi3_lh"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
	(mult:SI (zero_extend:SI
		  (match_operand:HI 1 "register_operand" "a,b,?a,?b"))
                 (ashiftrt:SI
		  (match_operand:SI 2 "register_operand" "a,b,b,a")
		  (const_int 16))))]
  ""
  "%|%.\\tmpyluhs\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "usmulhisi3_hl"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
        (mult:SI (lshiftrt:SI
		  (match_operand:SI 1 "register_operand" "a,b,?a,?b")
		  (const_int 16))
                 (sign_extend:SI
		  (match_operand:HI 2 "register_operand" "a,b,b,a"))))]
  ""
  "%|%.\\tmpyhuls\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "usmulhisi3_hh"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
        (mult:SI (lshiftrt:SI
		  (match_operand:SI 1 "register_operand" "a,b,?a,?b")
		  (const_int 16))
                 (ashiftrt:SI
		  (match_operand:SI 2 "register_operand" "a,b,b,a")
		  (const_int 16))))]
  ""
  "%|%.\\tmpyhus\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulsi3_insn"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
	(mult:SI (match_operand:SI 1 "register_operand" "%a,b,?a,?b")
		 (match_operand:SI 2 "register_operand" "a,b,b,a")))]
  "TARGET_MPY32"
  "%|%.\\tmpy32\\t%$\\t%1, %2, %0"
 [(set_attr "type" "mpy4")
  (set_attr "units" "m")
  (set_attr "cross" "n,n,y,y")])

(define_insn "<u>mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=a,b,a,b")
        (mult:DI (any_ext:DI
		  (match_operand:SI 1 "register_operand" "%a,b,?a,?b"))
                 (any_ext:DI
		  (match_operand:SI 2 "register_operand" "a,b,b,a"))))]
  "TARGET_MPY32"
  "%|%.\\tmpy32<u>\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy4")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "usmulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=a,b,a,b")
        (mult:DI (zero_extend:DI
		  (match_operand:SI 1 "register_operand" "a,b,?a,?b"))
                 (sign_extend:DI
		  (match_operand:SI 2 "register_operand" "a,b,b,a"))))]
  "TARGET_MPY32"
  "%|%.\\tmpy32us\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy4")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

;; Widening vector multiply and dot product

(define_insn "mulv2hiv2si3"
  [(set (match_operand:V2SI 0 "register_operand" "=a,b,a,b")
	(mult:V2SI
	 (sign_extend:V2SI (match_operand:V2HI 1 "register_operand" "a,b,a,b"))
	 (sign_extend:V2SI (match_operand:V2HI 2 "register_operand" "a,b,?b,?a"))))]
  "TARGET_INSNS_64"
  "%|%.\\tmpy2\\t%$\\t%1, %2, %0"
 [(set_attr "type" "mpy4")
  (set_attr "units" "m")
  (set_attr "cross" "n,n,y,y")])

(define_insn "umulv4qiv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=a,b,a,b")
	(mult:V4HI
	 (zero_extend:V4HI (match_operand:V4QI 1 "register_operand" "a,b,a,b"))
	 (zero_extend:V4HI (match_operand:V4QI 2 "register_operand" "a,b,?b,?a"))))]
  "TARGET_INSNS_64"
  "%|%.\\tmpyu4\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy4")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "usmulv4qiv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=a,b,a,b")
	(mult:V4HI
	 (zero_extend:V4HI (match_operand:V4QI 1 "register_operand" "a,b,?b,?a"))
	 (sign_extend:V4HI (match_operand:V4QI 2 "register_operand" "a,b,a,b"))))]
  "TARGET_INSNS_64"
  "%|%.\\tmpyus4\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy4")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "dotv2hi"
  [(set (match_operand:SI 0 "register_operand" "=a,b,a,b")
	(plus:SI
	  (mult:SI
	    (sign_extend:SI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand" "a,b,a,b")
		(parallel [(const_int 0)])))
	    (sign_extend:SI
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" "a,b,?b,?a")
		(parallel [(const_int 0)]))))
	  (mult:SI
	    (sign_extend:SI
	      (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	    (sign_extend:SI
	      (vec_select:HI (match_dup 2) (parallel [(const_int 1)]))))))]
  "TARGET_INSNS_64"
  "%|%.\\tdotp2\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy4")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

;; Fractional multiply

(define_insn "mulv2hqv2sq3"
  [(set (match_operand:V2SQ 0 "register_operand" "=a,b,a,b")
        (ss_mult:V2SQ
	 (fract_convert:V2SQ
	  (match_operand:V2HQ 1 "register_operand" "%a,b,?a,?b"))
	 (fract_convert:V2SQ
	  (match_operand:V2HQ 2 "register_operand" "a,b,b,a"))))]
  ""
  "%|%.\\tsmpy2\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy4")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhqsq3"
  [(set (match_operand:SQ 0 "register_operand" "=a,b,a,b")
        (ss_mult:SQ
	 (fract_convert:SQ
	  (match_operand:HQ 1 "register_operand" "%a,b,?a,?b"))
	 (fract_convert:SQ
	  (match_operand:HQ 2 "register_operand" "a,b,b,a"))))]
  ""
  "%|%.\\tsmpy\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhqsq3_lh"
  [(set (match_operand:SQ 0 "register_operand" "=a,b,a,b")
        (ss_mult:SQ
	 (fract_convert:SQ
	  (match_operand:HQ 1 "register_operand" "a,b,?a,?b"))
	 (fract_convert:SQ
	  (truncate:HQ (match_operand:SQ 2 "register_operand" "a,b,b,a")))))]
  ""
  "%|%.\\tsmpylh\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhqsq3_hl"
  [(set (match_operand:SQ 0 "register_operand" "=a,b,a,b")
        (ss_mult:SQ
	 (fract_convert:SQ
	  (truncate:HQ (match_operand:SQ 1 "register_operand" "a,b,b,a")))
	 (fract_convert:SQ
	  (match_operand:HQ 2 "register_operand" "a,b,b,a"))))]
  ""
  "%|%.\\tsmpyhl\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhqsq3_hh"
  [(set (match_operand:SQ 0 "register_operand" "=a,b,a,b")
        (ss_mult:SQ
	 (fract_convert:SQ
	  (truncate:HQ (match_operand:SQ 1 "register_operand" "a,b,b,a")))
	 (fract_convert:SQ
	  (truncate:HQ (match_operand:SQ 2 "register_operand" "a,b,b,a")))))]
  ""
  "%|%.\\tsmpyh\\t%$\\t%1, %2, %0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])
;; Multiplication patterns for TI C6X.
;; This file is processed by genmult.sh to produce two variants of each
;; pattern, a normal one and a real_mult variant for modulo scheduling.
;; Copyright (C) 2010-2023 Free Software Foundation, Inc.
;; Contributed by Bernd Schmidt <bernds@codesourcery.com>
;; Contributed by CodeSourcery.
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

;; -------------------------------------------------------------------------
;; Miscellaneous insns that execute on the M units
;; -------------------------------------------------------------------------

(define_insn "rotlsi3_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (rotate:SI (match_operand:SI 1 "register_operand" "a,b,?b,?a")
		   (match_operand:SI 2 "reg_or_ucst5_operand" "aIu5,bIu5,aIu5,bIu5"))] UNSPEC_REAL_MULT)]
  "TARGET_INSNS_64"
  "%|%.\\trotl\\t%$\\t%1, %2, %k0"
  [(set_attr "units" "m")
   (set_attr "type" "mpy2")
   (set_attr "cross" "n,n,y,y")])

(define_insn "bitrevsi2_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JA,JB,JB")
	(unspec:SI [(match_operand:SI 1 "register_operand" "a,?b,b,?a")]
		   UNSPEC_BITREV)] UNSPEC_REAL_MULT)]
  "TARGET_INSNS_64"
  "%|%.\\tbitr\\t%$\\t%1, %k0"
  [(set_attr "units" "m")
   (set_attr "type" "mpy2")
   (set_attr "cross" "n,y,n,y")])

;; Vector average.

(define_insn "avgv2hi3_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (unspec:V2HI [(match_operand:V2HI 1 "register_operand" "a,b,?b,?a")
		      (match_operand:V2HI 2 "register_operand" "a,b,a,b")] UNSPEC_AVG)] UNSPEC_REAL_MULT)]
  "TARGET_INSNS_64"
  "%|%.\\tavg2\\t%$\\t%1, %2, %k0"
  [(set_attr "units" "m")
   (set_attr "type" "mpy2")
   (set_attr "cross" "n,n,y,y")])

(define_insn "uavgv4qi3_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (unspec:V4QI [(match_operand:V4QI 1 "register_operand" "a,b,?b,?a")
		      (match_operand:V4QI 2 "register_operand" "a,b,a,b")] UNSPEC_AVG)] UNSPEC_REAL_MULT)]
  "TARGET_INSNS_64"
  "%|%.\\tavgu4\\t%$\\t%1, %2, %k0"
  [(set_attr "units" "m")
   (set_attr "type" "mpy2")
   (set_attr "cross" "n,n,y,y")])

;; -------------------------------------------------------------------------
;; Multiplication
;; -------------------------------------------------------------------------

(define_insn "mulhi3_real"
  [(unspec [(match_operand:HI 0 "const_int_operand" "=JA,JB,JA,JB")
        (mult:HI (match_operand:HI 1 "register_operand" "a,b,?b,?a")
                 (match_operand:HI 2 "reg_or_scst5_operand" "aIs5,bIs5,aIs5,bIs5"))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tmpy\\t%$\\t%2, %1, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "op_pattern" "sxs")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhisi3_const_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JAJB")
        (mult:SI (sign_extend:SI
		  (match_operand:HI 1 "register_operand" "a,b,?ab"))
                 (match_operand:HI 2 "scst5_operand" "Is5,Is5,Is5"))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tmpy\\t%$\\t%2, %1, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y")])

(define_insn "*mulhisi3_insn_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (mult:SI (sign_extend:SI
		  (match_operand:HI 1 "register_operand" "%a,b,?a,?b"))
                 (sign_extend:SI
		  (match_operand:HI 2 "reg_or_scst5_operand" "a,b,b,a")))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tmpy\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "op_pattern" "ssx")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhisi3_lh_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (mult:SI (sign_extend:SI
		  (match_operand:HI 1 "register_operand" "a,b,?a,?b"))
                 (ashiftrt:SI
		  (match_operand:SI 2 "register_operand" "a,b,b,a")
		  (const_int 16)))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tmpylh\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhisi3_hl_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (mult:SI (ashiftrt:SI
		  (match_operand:SI 1 "register_operand" "a,b,?a,?b")
		  (const_int 16))
                 (sign_extend:SI
		  (match_operand:HI 2 "register_operand" "a,b,b,a")))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tmpyhl\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhisi3_hh_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (mult:SI (ashiftrt:SI
		  (match_operand:SI 1 "register_operand" "%a,b,?a,?b")
		  (const_int 16))
                 (ashiftrt:SI
		  (match_operand:SI 2 "register_operand" "a,b,b,a")
		  (const_int 16)))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tmpyh\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "umulhisi3_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (mult:SI (zero_extend:SI
		  (match_operand:HI 1 "register_operand" "%a,b,?a,?b"))
                 (zero_extend:SI
		  (match_operand:HI 2 "register_operand" "a,b,b,a")))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tmpyu\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "umulhisi3_lh_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (mult:SI (zero_extend:SI
		  (match_operand:HI 1 "register_operand" "a,b,?a,?b"))
                 (lshiftrt:SI
		  (match_operand:SI 2 "register_operand" "a,b,b,a")
		  (const_int 16)))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tmpylhu\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "umulhisi3_hl_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (mult:SI (lshiftrt:SI
		  (match_operand:SI 1 "register_operand" "a,b,?a,?b")
		  (const_int 16))
                 (zero_extend:SI
		  (match_operand:HI 2 "register_operand" "a,b,b,a")))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tmpyhlu\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "umulhisi3_hh_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (mult:SI (lshiftrt:SI
		  (match_operand:SI 1 "register_operand" "%a,b,?a,?b")
		  (const_int 16))
                 (lshiftrt:SI
		  (match_operand:SI 2 "register_operand" "a,b,b,a")
		  (const_int 16)))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tmpyhu\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "usmulhisi3_const_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JAJB")
        (mult:SI (zero_extend:SI
		  (match_operand:HI 1 "register_operand" "a,b,?ab"))
                 (match_operand:SI 2 "scst5_operand" "Is5,Is5,Is5"))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tmpysu\\t%$\\t%2, %1, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y")])

(define_insn "*usmulhisi3_insn_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (mult:SI (zero_extend:SI
		  (match_operand:HI 1 "register_operand" "a,b,?a,?b"))
                 (sign_extend:SI
		  (match_operand:HI 2 "reg_or_scst5_operand" "aIs5,bIs5,bIs5,aIs5")))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tmpyus\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "usmulhisi3_lh_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
	(mult:SI (zero_extend:SI
		  (match_operand:HI 1 "register_operand" "a,b,?a,?b"))
                 (ashiftrt:SI
		  (match_operand:SI 2 "register_operand" "a,b,b,a")
		  (const_int 16)))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tmpyluhs\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "usmulhisi3_hl_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (mult:SI (lshiftrt:SI
		  (match_operand:SI 1 "register_operand" "a,b,?a,?b")
		  (const_int 16))
                 (sign_extend:SI
		  (match_operand:HI 2 "register_operand" "a,b,b,a")))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tmpyhuls\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "usmulhisi3_hh_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (mult:SI (lshiftrt:SI
		  (match_operand:SI 1 "register_operand" "a,b,?a,?b")
		  (const_int 16))
                 (ashiftrt:SI
		  (match_operand:SI 2 "register_operand" "a,b,b,a")
		  (const_int 16)))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tmpyhus\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulsi3_insn_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
	(mult:SI (match_operand:SI 1 "register_operand" "%a,b,?a,?b")
		 (match_operand:SI 2 "register_operand" "a,b,b,a"))] UNSPEC_REAL_MULT)]
  "TARGET_MPY32"
  "%|%.\\tmpy32\\t%$\\t%1, %2, %k0"
 [(set_attr "type" "mpy4")
  (set_attr "units" "m")
  (set_attr "cross" "n,n,y,y")])

(define_insn "<u>mulsidi3_real"
  [(unspec [(match_operand:DI 0 "const_int_operand" "=JA,JB,JA,JB")
        (mult:DI (any_ext:DI
		  (match_operand:SI 1 "register_operand" "%a,b,?a,?b"))
                 (any_ext:DI
		  (match_operand:SI 2 "register_operand" "a,b,b,a")))] UNSPEC_REAL_MULT)]
  "TARGET_MPY32"
  "%|%.\\tmpy32<u>\\t%$\\t%1, %2, %K0"
  [(set_attr "type" "mpy4")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "usmulsidi3_real"
  [(unspec [(match_operand:DI 0 "const_int_operand" "=JA,JB,JA,JB")
        (mult:DI (zero_extend:DI
		  (match_operand:SI 1 "register_operand" "a,b,?a,?b"))
                 (sign_extend:DI
		  (match_operand:SI 2 "register_operand" "a,b,b,a")))] UNSPEC_REAL_MULT)]
  "TARGET_MPY32"
  "%|%.\\tmpy32us\\t%$\\t%1, %2, %K0"
  [(set_attr "type" "mpy4")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

;; Widening vector multiply and dot product

(define_insn "mulv2hiv2si3_real"
  [(unspec [(match_operand:V2SI 0 "const_int_operand" "=JA,JB,JA,JB")
	(mult:V2SI
	 (sign_extend:V2SI (match_operand:V2HI 1 "register_operand" "a,b,a,b"))
	 (sign_extend:V2SI (match_operand:V2HI 2 "register_operand" "a,b,?b,?a")))] UNSPEC_REAL_MULT)]
  "TARGET_INSNS_64"
  "%|%.\\tmpy2\\t%$\\t%1, %2, %k0"
 [(set_attr "type" "mpy4")
  (set_attr "units" "m")
  (set_attr "cross" "n,n,y,y")])

(define_insn "umulv4qiv4hi3_real"
  [(unspec [(match_operand:V4HI 0 "const_int_operand" "=JA,JB,JA,JB")
	(mult:V4HI
	 (zero_extend:V4HI (match_operand:V4QI 1 "register_operand" "a,b,a,b"))
	 (zero_extend:V4HI (match_operand:V4QI 2 "register_operand" "a,b,?b,?a")))] UNSPEC_REAL_MULT)]
  "TARGET_INSNS_64"
  "%|%.\\tmpyu4\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy4")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "usmulv4qiv4hi3_real"
  [(unspec [(match_operand:V4HI 0 "const_int_operand" "=JA,JB,JA,JB")
	(mult:V4HI
	 (zero_extend:V4HI (match_operand:V4QI 1 "register_operand" "a,b,?b,?a"))
	 (sign_extend:V4HI (match_operand:V4QI 2 "register_operand" "a,b,a,b")))] UNSPEC_REAL_MULT)]
  "TARGET_INSNS_64"
  "%|%.\\tmpyus4\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy4")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "dotv2hi_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
	(plus:SI
	  (mult:SI
	    (sign_extend:SI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand" "a,b,a,b")
		(parallel [(const_int 0)])))
	    (sign_extend:SI
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" "a,b,?b,?a")
		(parallel [(const_int 0)]))))
	  (mult:SI
	    (sign_extend:SI
	      (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	    (sign_extend:SI
	      (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))] UNSPEC_REAL_MULT)]
  "TARGET_INSNS_64"
  "%|%.\\tdotp2\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy4")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

;; Fractional multiply

(define_insn "mulv2hqv2sq3_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (ss_mult:V2SQ
	 (fract_convert:V2SQ
	  (match_operand:V2HQ 1 "register_operand" "%a,b,?a,?b"))
	 (fract_convert:V2SQ
	  (match_operand:V2HQ 2 "register_operand" "a,b,b,a")))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tsmpy2\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy4")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhqsq3_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (ss_mult:SQ
	 (fract_convert:SQ
	  (match_operand:HQ 1 "register_operand" "%a,b,?a,?b"))
	 (fract_convert:SQ
	  (match_operand:HQ 2 "register_operand" "a,b,b,a")))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tsmpy\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhqsq3_lh_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (ss_mult:SQ
	 (fract_convert:SQ
	  (match_operand:HQ 1 "register_operand" "a,b,?a,?b"))
	 (fract_convert:SQ
	  (truncate:HQ (match_operand:SQ 2 "register_operand" "a,b,b,a"))))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tsmpylh\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhqsq3_hl_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (ss_mult:SQ
	 (fract_convert:SQ
	  (truncate:HQ (match_operand:SQ 1 "register_operand" "a,b,b,a")))
	 (fract_convert:SQ
	  (match_operand:HQ 2 "register_operand" "a,b,b,a")))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tsmpyhl\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])

(define_insn "mulhqsq3_hh_real"
  [(unspec [(match_operand:SI 0 "const_int_operand" "=JA,JB,JA,JB")
        (ss_mult:SQ
	 (fract_convert:SQ
	  (truncate:HQ (match_operand:SQ 1 "register_operand" "a,b,b,a")))
	 (fract_convert:SQ
	  (truncate:HQ (match_operand:SQ 2 "register_operand" "a,b,b,a"))))] UNSPEC_REAL_MULT)]
  ""
  "%|%.\\tsmpyh\\t%$\\t%1, %2, %k0"
  [(set_attr "type" "mpy2")
   (set_attr "units" "m")
   (set_attr "cross" "n,n,y,y")])
