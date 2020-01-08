;; Patterns for the Intel Wireless MMX technology architecture.
;; Copyright (C) 2011-2020 Free Software Foundation, Inc.
;; Written by Marvell, Inc.
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

(define_insn "iwmmxt_wabs<mode>3"
  [(set (match_operand:VMMX               0 "register_operand" "=y")
        (unspec:VMMX [(match_operand:VMMX 1 "register_operand"  "y")] UNSPEC_WABS))]
  "TARGET_REALLY_IWMMXT"
  "wabs<MMX_char>%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wabs")]
)

(define_insn "iwmmxt_wabsdiffb"
  [(set (match_operand:V8QI                          0 "register_operand" "=y")
	(truncate:V8QI
	  (abs:V8HI
	    (minus:V8HI
	      (zero_extend:V8HI (match_operand:V8QI  1 "register_operand"  "y"))
	      (zero_extend:V8HI (match_operand:V8QI  2 "register_operand"  "y"))))))]
 "TARGET_REALLY_IWMMXT"
 "wabsdiffb%?\\t%0, %1, %2"
 [(set_attr "predicable" "yes")
  (set_attr "type" "wmmx_wabsdiff")]
)

(define_insn "iwmmxt_wabsdiffh"
  [(set (match_operand:V4HI                          0 "register_operand" "=y")
        (truncate: V4HI
          (abs:V4SI
            (minus:V4SI
              (zero_extend:V4SI (match_operand:V4HI  1 "register_operand"  "y"))
	      (zero_extend:V4SI (match_operand:V4HI  2 "register_operand"  "y"))))))]
  "TARGET_REALLY_IWMMXT"
  "wabsdiffh%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wabsdiff")]
)

(define_insn "iwmmxt_wabsdiffw"
  [(set (match_operand:V2SI                          0 "register_operand" "=y")
        (truncate: V2SI
	  (abs:V2DI
	    (minus:V2DI
	      (zero_extend:V2DI (match_operand:V2SI  1 "register_operand"  "y"))
	      (zero_extend:V2DI (match_operand:V2SI  2 "register_operand"  "y"))))))]
 "TARGET_REALLY_IWMMXT"
 "wabsdiffw%?\\t%0, %1, %2"
 [(set_attr "predicable" "yes")
  (set_attr "type" "wmmx_wabsdiff")]
)

(define_insn "iwmmxt_waddsubhx"
  [(set (match_operand:V4HI                                        0 "register_operand" "=y")
	(vec_merge:V4HI
	  (ss_minus:V4HI
	    (match_operand:V4HI                                    1 "register_operand" "y")
	    (vec_select:V4HI (match_operand:V4HI 2 "register_operand" "y")
	                     (parallel [(const_int 1) (const_int 0) (const_int 3) (const_int 2)])))
	  (ss_plus:V4HI
	    (match_dup 1)
	    (vec_select:V4HI (match_dup 2)
	                     (parallel [(const_int 1) (const_int 0) (const_int 3) (const_int 2)])))
	  (const_int 10)))]
  "TARGET_REALLY_IWMMXT"
  "waddsubhx%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_waddsubhx")]
)

(define_insn "iwmmxt_wsubaddhx"
  [(set (match_operand:V4HI                                        0 "register_operand" "=y")
	(vec_merge:V4HI
	  (ss_plus:V4HI
	    (match_operand:V4HI                                    1 "register_operand" "y")
	    (vec_select:V4HI (match_operand:V4HI 2 "register_operand" "y")
	                     (parallel [(const_int 1) (const_int 0) (const_int 3) (const_int 2)])))
	  (ss_minus:V4HI
	    (match_dup 1)
	    (vec_select:V4HI (match_dup 2)
	                     (parallel [(const_int 1) (const_int 0) (const_int 3) (const_int 2)])))
	  (const_int 10)))]
  "TARGET_REALLY_IWMMXT"
  "wsubaddhx%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wsubaddhx")]
)

(define_insn "addc<mode>3"
  [(set (match_operand:VMMX2      0 "register_operand" "=y")
	(unspec:VMMX2
          [(plus:VMMX2
             (match_operand:VMMX2 1 "register_operand"  "y")
	     (match_operand:VMMX2 2 "register_operand"  "y"))] UNSPEC_WADDC))]
  "TARGET_REALLY_IWMMXT"
  "wadd<MMX_char>c%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wadd")]
)

(define_insn "iwmmxt_avg4"
[(set (match_operand:V8QI                                 0 "register_operand" "=y")
      (truncate:V8QI
        (vec_select:V8HI
	  (vec_merge:V8HI
	    (lshiftrt:V8HI
	      (plus:V8HI
	        (plus:V8HI
		  (plus:V8HI
	            (plus:V8HI
		      (zero_extend:V8HI (match_operand:V8QI 1 "register_operand" "y"))
		      (zero_extend:V8HI (match_operand:V8QI 2 "register_operand" "y")))
		    (vec_select:V8HI (zero_extend:V8HI (match_dup 1))
		                     (parallel [(const_int 7) (const_int 0) (const_int 1) (const_int 2)
				                (const_int 3) (const_int 4) (const_int 5) (const_int 6)])))
		  (vec_select:V8HI (zero_extend:V8HI (match_dup 2))
		                   (parallel [(const_int 7) (const_int 0) (const_int 1) (const_int 2)
				              (const_int 3) (const_int 4) (const_int 5) (const_int 6)])))
	        (const_vector:V8HI [(const_int 1) (const_int 1) (const_int 1) (const_int 1)
	                            (const_int 1) (const_int 1) (const_int 1) (const_int 1)]))
	      (const_int 2))
	    (const_vector:V8HI [(const_int 0) (const_int 0) (const_int 0) (const_int 0)
	                        (const_int 0) (const_int 0) (const_int 0) (const_int 0)])
	    (const_int 254))
	  (parallel [(const_int 1) (const_int 2) (const_int 3) (const_int 4)
	             (const_int 5) (const_int 6) (const_int 7) (const_int 0)]))))]
  "TARGET_REALLY_IWMMXT"
  "wavg4%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wavg4")]
)

(define_insn "iwmmxt_avg4r"
  [(set (match_operand:V8QI                                   0 "register_operand" "=y")
	(truncate:V8QI
	  (vec_select:V8HI
	    (vec_merge:V8HI
	      (lshiftrt:V8HI
	        (plus:V8HI
		  (plus:V8HI
		    (plus:V8HI
		      (plus:V8HI
		        (zero_extend:V8HI (match_operand:V8QI 1 "register_operand" "y"))
		        (zero_extend:V8HI (match_operand:V8QI 2 "register_operand" "y")))
		      (vec_select:V8HI (zero_extend:V8HI (match_dup 1))
		                       (parallel [(const_int 7) (const_int 0) (const_int 1) (const_int 2)
				                  (const_int 3) (const_int 4) (const_int 5) (const_int 6)])))
		    (vec_select:V8HI (zero_extend:V8HI (match_dup 2))
		                     (parallel [(const_int 7) (const_int 0) (const_int 1) (const_int 2)
				                (const_int 3) (const_int 4) (const_int 5) (const_int 6)])))
		  (const_vector:V8HI [(const_int 2) (const_int 2) (const_int 2) (const_int 2)
		                      (const_int 2) (const_int 2) (const_int 2) (const_int 2)]))
	        (const_int 2))
	      (const_vector:V8HI [(const_int 0) (const_int 0) (const_int 0) (const_int 0)
	                          (const_int 0) (const_int 0) (const_int 0) (const_int 0)])
	      (const_int 254))
	    (parallel [(const_int 1) (const_int 2) (const_int 3) (const_int 4)
	               (const_int 5) (const_int 6) (const_int 7) (const_int 0)]))))]
  "TARGET_REALLY_IWMMXT"
  "wavg4r%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wavg4")]
)

(define_insn "iwmmxt_wmaddsx"
  [(set (match_operand:V2SI                                        0 "register_operand" "=y")
	(plus:V2SI
	  (mult:V2SI
	    (vec_select:V2SI (sign_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
	                     (parallel [(const_int 1) (const_int 3)]))
	    (vec_select:V2SI (sign_extend:V4SI (match_operand:V4HI 2 "register_operand" "y"))
	                     (parallel [(const_int 0) (const_int 2)])))
	  (mult:V2SI
	    (vec_select:V2SI (sign_extend:V4SI (match_dup 1))
	                     (parallel [(const_int 0) (const_int 2)]))
	    (vec_select:V2SI (sign_extend:V4SI (match_dup 2))
	                     (parallel [(const_int 1) (const_int 3)])))))]
 "TARGET_REALLY_IWMMXT"
  "wmaddsx%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
	(set_attr "type" "wmmx_wmadd")]
)

(define_insn "iwmmxt_wmaddux"
  [(set (match_operand:V2SI                                        0 "register_operand" "=y")
	(plus:V2SI
	  (mult:V2SI
	    (vec_select:V2SI (zero_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
	                     (parallel [(const_int 1) (const_int 3)]))
	    (vec_select:V2SI (zero_extend:V4SI (match_operand:V4HI 2 "register_operand" "y"))
	                     (parallel [(const_int 0) (const_int 2)])))
	  (mult:V2SI
	    (vec_select:V2SI (zero_extend:V4SI (match_dup 1))
	                     (parallel [(const_int 0) (const_int 2)]))
	    (vec_select:V2SI (zero_extend:V4SI (match_dup 2))
	                     (parallel [(const_int 1) (const_int 3)])))))]
  "TARGET_REALLY_IWMMXT"
  "wmaddux%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmadd")]
)

(define_insn "iwmmxt_wmaddsn"
 [(set (match_operand:V2SI                                     0 "register_operand" "=y")
    (minus:V2SI
      (mult:V2SI
        (vec_select:V2SI (sign_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
	                 (parallel [(const_int 0) (const_int 2)]))
        (vec_select:V2SI (sign_extend:V4SI (match_operand:V4HI 2 "register_operand" "y"))
	                 (parallel [(const_int 0) (const_int 2)])))
      (mult:V2SI
        (vec_select:V2SI (sign_extend:V4SI (match_dup 1))
	                 (parallel [(const_int 1) (const_int 3)]))
        (vec_select:V2SI (sign_extend:V4SI (match_dup 2))
	                 (parallel [(const_int 1) (const_int 3)])))))]
 "TARGET_REALLY_IWMMXT"
 "wmaddsn%?\\t%0, %1, %2"
 [(set_attr "predicable" "yes")
  (set_attr "type" "wmmx_wmadd")]
)

(define_insn "iwmmxt_wmaddun"
  [(set (match_operand:V2SI                                        0 "register_operand" "=y")
	(minus:V2SI
	  (mult:V2SI
	    (vec_select:V2SI (zero_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
	                     (parallel [(const_int 0) (const_int 2)]))
	    (vec_select:V2SI (zero_extend:V4SI (match_operand:V4HI 2 "register_operand" "y"))
	                     (parallel [(const_int 0) (const_int 2)])))
	  (mult:V2SI
	    (vec_select:V2SI (zero_extend:V4SI (match_dup 1))
	                     (parallel [(const_int 1) (const_int 3)]))
	    (vec_select:V2SI (zero_extend:V4SI (match_dup 2))
	                     (parallel [(const_int 1) (const_int 3)])))))]
  "TARGET_REALLY_IWMMXT"
  "wmaddun%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmadd")]
)

(define_insn "iwmmxt_wmulwsm"
  [(set (match_operand:V2SI                         0 "register_operand" "=y")
	(truncate:V2SI
	  (ashiftrt:V2DI
	    (mult:V2DI
	      (sign_extend:V2DI (match_operand:V2SI 1 "register_operand" "y"))
	      (sign_extend:V2DI (match_operand:V2SI 2 "register_operand" "y")))
	    (const_int 32))))]
  "TARGET_REALLY_IWMMXT"
  "wmulwsm%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmulw")]
)

(define_insn "iwmmxt_wmulwum"
  [(set (match_operand:V2SI                         0 "register_operand" "=y")
	(truncate:V2SI
          (lshiftrt:V2DI
	    (mult:V2DI
	      (zero_extend:V2DI (match_operand:V2SI 1 "register_operand" "y"))
	      (zero_extend:V2DI (match_operand:V2SI 2 "register_operand" "y")))
	    (const_int 32))))]
  "TARGET_REALLY_IWMMXT"
  "wmulwum%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmulw")]
)

(define_insn "iwmmxt_wmulsmr"
  [(set (match_operand:V4HI                           0 "register_operand" "=y")
	(truncate:V4HI
	  (ashiftrt:V4SI
	    (plus:V4SI
	      (mult:V4SI
	        (sign_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
		(sign_extend:V4SI (match_operand:V4HI 2 "register_operand" "y")))
	      (const_vector:V4SI [(const_int 32768)
	                          (const_int 32768)
				  (const_int 32768)]))
	    (const_int 16))))]
  "TARGET_REALLY_IWMMXT"
  "wmulsmr%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmul")]
)

(define_insn "iwmmxt_wmulumr"
  [(set (match_operand:V4HI                           0 "register_operand" "=y")
	(truncate:V4HI
	  (lshiftrt:V4SI
	    (plus:V4SI
	      (mult:V4SI
	        (zero_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
		(zero_extend:V4SI (match_operand:V4HI 2 "register_operand" "y")))
	      (const_vector:V4SI [(const_int 32768)
				  (const_int 32768)
				  (const_int 32768)
				  (const_int 32768)]))
	  (const_int 16))))]
  "TARGET_REALLY_IWMMXT"
  "wmulumr%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmul")]
)

(define_insn "iwmmxt_wmulwsmr"
  [(set (match_operand:V2SI                           0 "register_operand" "=y")
	(truncate:V2SI
	  (ashiftrt:V2DI
	    (plus:V2DI
	      (mult:V2DI
	        (sign_extend:V2DI (match_operand:V2SI 1 "register_operand" "y"))
		(sign_extend:V2DI (match_operand:V2SI 2 "register_operand" "y")))
	      (const_vector:V2DI [(const_int 2147483648)
				  (const_int 2147483648)]))
	    (const_int 32))))]
  "TARGET_REALLY_IWMMXT"
  "wmulwsmr%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmul")]
)

(define_insn "iwmmxt_wmulwumr"
  [(set (match_operand:V2SI                           0 "register_operand" "=y")
	(truncate:V2SI
	  (lshiftrt:V2DI
	    (plus:V2DI
	      (mult:V2DI
	        (zero_extend:V2DI (match_operand:V2SI 1 "register_operand" "y"))
		(zero_extend:V2DI (match_operand:V2SI 2 "register_operand" "y")))
	      (const_vector:V2DI [(const_int 2147483648)
			          (const_int 2147483648)]))
	    (const_int 32))))]
  "TARGET_REALLY_IWMMXT"
  "wmulwumr%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmulw")]
)

(define_insn "iwmmxt_wmulwl"
  [(set (match_operand:V2SI   0 "register_operand" "=y")
        (mult:V2SI
          (match_operand:V2SI 1 "register_operand" "y")
	  (match_operand:V2SI 2 "register_operand" "y")))]
  "TARGET_REALLY_IWMMXT"
  "wmulwl%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmulw")]
)

(define_insn "iwmmxt_wqmulm"
  [(set (match_operand:V4HI            0 "register_operand" "=y")
        (unspec:V4HI [(match_operand:V4HI 1 "register_operand" "y")
		      (match_operand:V4HI 2 "register_operand" "y")] UNSPEC_WQMULM))]
  "TARGET_REALLY_IWMMXT"
  "wqmulm%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wqmulm")]
)

(define_insn "iwmmxt_wqmulwm"
  [(set (match_operand:V2SI               0 "register_operand" "=y")
	(unspec:V2SI [(match_operand:V2SI 1 "register_operand" "y")
		      (match_operand:V2SI 2 "register_operand" "y")] UNSPEC_WQMULWM))]
  "TARGET_REALLY_IWMMXT"
  "wqmulwm%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wqmulwm")]
)

(define_insn "iwmmxt_wqmulmr"
  [(set (match_operand:V4HI               0 "register_operand" "=y")
	(unspec:V4HI [(match_operand:V4HI 1 "register_operand" "y")
		      (match_operand:V4HI 2 "register_operand" "y")] UNSPEC_WQMULMR))]
  "TARGET_REALLY_IWMMXT"
  "wqmulmr%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wqmulm")]
)

(define_insn "iwmmxt_wqmulwmr"
  [(set (match_operand:V2SI            0 "register_operand" "=y")
        (unspec:V2SI [(match_operand:V2SI 1 "register_operand" "y")
		      (match_operand:V2SI 2 "register_operand" "y")] UNSPEC_WQMULWMR))]
  "TARGET_REALLY_IWMMXT"
  "wqmulwmr%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wqmulwm")]
)

(define_insn "iwmmxt_waddbhusm"
  [(set (match_operand:V8QI                          0 "register_operand" "=y")
	(vec_concat:V8QI
	  (const_vector:V4QI [(const_int 0) (const_int 0) (const_int 0) (const_int 0)])
	  (us_truncate:V4QI
	    (ss_plus:V4HI
	      (match_operand:V4HI                    1 "register_operand" "y")
	      (zero_extend:V4HI
	        (vec_select:V4QI (match_operand:V8QI 2 "register_operand" "y")
	                         (parallel [(const_int 4) (const_int 5) (const_int 6) (const_int 7)])))))))]
  "TARGET_REALLY_IWMMXT"
  "waddbhusm%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_waddbhus")]
)

(define_insn "iwmmxt_waddbhusl"
  [(set (match_operand:V8QI                          0 "register_operand" "=y")
	(vec_concat:V8QI
	  (us_truncate:V4QI
	    (ss_plus:V4HI
	      (match_operand:V4HI                    1 "register_operand" "y")
	      (zero_extend:V4HI
		(vec_select:V4QI (match_operand:V8QI 2 "register_operand" "y")
		                 (parallel [(const_int 0) (const_int 1) (const_int 2) (const_int 3)])))))
	  (const_vector:V4QI [(const_int 0) (const_int 0) (const_int 0) (const_int 0)])))]
  "TARGET_REALLY_IWMMXT"
  "waddbhusl%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_waddbhus")]
)

(define_insn "iwmmxt_wqmiabb"
  [(set (match_operand:V2SI	                             0 "register_operand" "=y")
	(unspec:V2SI [(match_operand:V2SI                    1 "register_operand" "0")
		      (zero_extract:V4HI (match_operand:V4HI 2 "register_operand" "y") (const_int 16) (const_int 0))
		      (zero_extract:V4HI (match_dup 2) (const_int 16) (const_int 32))
		      (zero_extract:V4HI (match_operand:V4HI 3 "register_operand" "y") (const_int 16) (const_int 0))
		      (zero_extract:V4HI (match_dup 3) (const_int 16) (const_int 32))] UNSPEC_WQMIAxy))]
  "TARGET_REALLY_IWMMXT"
  "wqmiabb%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wqmiaxy")]
)

(define_insn "iwmmxt_wqmiabt"
  [(set (match_operand:V2SI	                             0 "register_operand" "=y")
	(unspec:V2SI [(match_operand:V2SI                    1 "register_operand" "0")
	              (zero_extract:V4HI (match_operand:V4HI 2 "register_operand" "y") (const_int 16) (const_int 0))
		      (zero_extract:V4HI (match_dup 2) (const_int 16) (const_int 32))
		      (zero_extract:V4HI (match_operand:V4HI 3 "register_operand" "y") (const_int 16) (const_int 16))
		      (zero_extract:V4HI (match_dup 3) (const_int 16) (const_int 48))] UNSPEC_WQMIAxy))]
  "TARGET_REALLY_IWMMXT"
  "wqmiabt%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wqmiaxy")]
)

(define_insn "iwmmxt_wqmiatb"
  [(set (match_operand:V2SI                                  0 "register_operand" "=y")
        (unspec:V2SI [(match_operand:V2SI                    1 "register_operand" "0")
	              (zero_extract:V4HI (match_operand:V4HI 2 "register_operand" "y") (const_int 16) (const_int 16))
	              (zero_extract:V4HI (match_dup 2) (const_int 16) (const_int 48))
	              (zero_extract:V4HI (match_operand:V4HI 3 "register_operand" "y") (const_int 16) (const_int 0))
	              (zero_extract:V4HI (match_dup 3) (const_int 16) (const_int 32))] UNSPEC_WQMIAxy))]
  "TARGET_REALLY_IWMMXT"
  "wqmiatb%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wqmiaxy")]
)

(define_insn "iwmmxt_wqmiatt"
  [(set (match_operand:V2SI                                  0 "register_operand" "=y")
        (unspec:V2SI [(match_operand:V2SI                    1 "register_operand" "0")
	              (zero_extract:V4HI (match_operand:V4HI 2 "register_operand" "y") (const_int 16) (const_int 16))
	              (zero_extract:V4HI (match_dup 2) (const_int 16) (const_int 48))
	              (zero_extract:V4HI (match_operand:V4HI 3 "register_operand" "y") (const_int 16) (const_int 16))
	              (zero_extract:V4HI (match_dup 3) (const_int 16) (const_int 48))] UNSPEC_WQMIAxy))]
  "TARGET_REALLY_IWMMXT"
  "wqmiatt%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wqmiaxy")]
)

(define_insn "iwmmxt_wqmiabbn"
  [(set (match_operand:V2SI                                  0 "register_operand" "=y")
        (unspec:V2SI [(match_operand:V2SI                    1 "register_operand" "0")
                      (zero_extract:V4HI (match_operand:V4HI 2 "register_operand" "y") (const_int 16) (const_int 0))
	              (zero_extract:V4HI (match_dup 2) (const_int 16) (const_int 32))
	              (zero_extract:V4HI (match_operand:V4HI 3 "register_operand" "y") (const_int 16) (const_int 0))
	              (zero_extract:V4HI (match_dup 3) (const_int 16) (const_int 32))] UNSPEC_WQMIAxyn))]
  "TARGET_REALLY_IWMMXT"
  "wqmiabbn%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wqmiaxy")]
)

(define_insn "iwmmxt_wqmiabtn"
  [(set (match_operand:V2SI                                  0 "register_operand" "=y")
        (unspec:V2SI [(match_operand:V2SI                    1 "register_operand" "0")
                      (zero_extract:V4HI (match_operand:V4HI 2 "register_operand" "y") (const_int 16) (const_int 0))
	              (zero_extract:V4HI (match_dup 2) (const_int 16) (const_int 32))
	              (zero_extract:V4HI (match_operand:V4HI 3 "register_operand" "y") (const_int 16) (const_int 16))
	              (zero_extract:V4HI (match_dup 3) (const_int 16) (const_int 48))] UNSPEC_WQMIAxyn))]
  "TARGET_REALLY_IWMMXT"
  "wqmiabtn%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wqmiaxy")]
)

(define_insn "iwmmxt_wqmiatbn"
  [(set (match_operand:V2SI                                  0 "register_operand" "=y")
        (unspec:V2SI [(match_operand:V2SI                    1 "register_operand" "0")
                      (zero_extract:V4HI (match_operand:V4HI 2 "register_operand" "y") (const_int 16) (const_int 16))
	              (zero_extract:V4HI (match_dup 2) (const_int 16) (const_int 48))
	              (zero_extract:V4HI (match_operand:V4HI 3 "register_operand" "y") (const_int 16) (const_int 0))
	              (zero_extract:V4HI (match_dup 3) (const_int 16) (const_int 32))] UNSPEC_WQMIAxyn))]
  "TARGET_REALLY_IWMMXT"
  "wqmiatbn%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wqmiaxy")]
)

(define_insn "iwmmxt_wqmiattn"
 [(set (match_operand:V2SI                                  0 "register_operand" "=y")
       (unspec:V2SI [(match_operand:V2SI                    1 "register_operand" "0")
                     (zero_extract:V4HI (match_operand:V4HI 2 "register_operand" "y") (const_int 16) (const_int 16))
	             (zero_extract:V4HI (match_dup 2) (const_int 16) (const_int 48))
	             (zero_extract:V4HI (match_operand:V4HI 3 "register_operand" "y") (const_int 16) (const_int 16))
	             (zero_extract:V4HI (match_dup 3) (const_int 16) (const_int 48))] UNSPEC_WQMIAxyn))]
  "TARGET_REALLY_IWMMXT"
  "wqmiattn%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wqmiaxy")]
)

(define_insn "iwmmxt_wmiabb"
  [(set	(match_operand:DI	                          0 "register_operand" "=y")
	(plus:DI (match_operand:DI	                  1 "register_operand" "0")
		 (plus:DI
		   (mult:DI
		     (sign_extend:DI
		       (vec_select:HI (match_operand:V4HI 2 "register_operand" "y")
				      (parallel [(const_int 0)])))
		     (sign_extend:DI
		       (vec_select:HI (match_operand:V4HI 3 "register_operand" "y")
				      (parallel [(const_int 0)]))))
		   (mult:DI
		     (sign_extend:DI
		       (vec_select:HI (match_dup 2)
			              (parallel [(const_int 2)])))
		     (sign_extend:DI
		       (vec_select:HI (match_dup 3)
				      (parallel [(const_int 2)])))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiabb%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiaxy")]
)

(define_insn "iwmmxt_wmiabt"
  [(set	(match_operand:DI	                          0 "register_operand" "=y")
	(plus:DI (match_operand:DI	                  1 "register_operand" "0")
		 (plus:DI
		   (mult:DI
		     (sign_extend:DI
		       (vec_select:HI (match_operand:V4HI 2 "register_operand" "y")
				      (parallel [(const_int 0)])))
		     (sign_extend:DI
		       (vec_select:HI (match_operand:V4HI 3 "register_operand" "y")
				      (parallel [(const_int 1)]))))
		   (mult:DI
		     (sign_extend:DI
		       (vec_select:HI (match_dup 2)
				      (parallel [(const_int 2)])))
		     (sign_extend:DI
		       (vec_select:HI (match_dup 3)
				      (parallel [(const_int 3)])))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiabt%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiaxy")]
)

(define_insn "iwmmxt_wmiatb"
  [(set	(match_operand:DI	                          0 "register_operand" "=y")
	(plus:DI (match_operand:DI	                  1 "register_operand" "0")
		 (plus:DI
		   (mult:DI
		     (sign_extend:DI
		       (vec_select:HI (match_operand:V4HI 2 "register_operand" "y")
				      (parallel [(const_int 1)])))
		     (sign_extend:DI
		       (vec_select:HI (match_operand:V4HI 3 "register_operand" "y")
				      (parallel [(const_int 0)]))))
		   (mult:DI
		     (sign_extend:DI
		       (vec_select:HI (match_dup 2)
				      (parallel [(const_int 3)])))
		     (sign_extend:DI
		       (vec_select:HI (match_dup 3)
				      (parallel [(const_int 2)])))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiatb%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiaxy")]
)

(define_insn "iwmmxt_wmiatt"
  [(set	(match_operand:DI	                   0 "register_operand" "=y")
        (plus:DI (match_operand:DI	           1 "register_operand" "0")
          (plus:DI
            (mult:DI
              (sign_extend:DI
                (vec_select:HI (match_operand:V4HI 2 "register_operand" "y")
	                       (parallel [(const_int 1)])))
	      (sign_extend:DI
	        (vec_select:HI (match_operand:V4HI 3 "register_operand" "y")
	                       (parallel [(const_int 1)]))))
            (mult:DI
	      (sign_extend:DI
                (vec_select:HI (match_dup 2)
	                       (parallel [(const_int 3)])))
              (sign_extend:DI
                (vec_select:HI (match_dup 3)
	                       (parallel [(const_int 3)])))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiatt%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiaxy")]
)

(define_insn "iwmmxt_wmiabbn"
  [(set	(match_operand:DI	                           0 "register_operand" "=y")
	(minus:DI (match_operand:DI	                   1 "register_operand" "0")
		  (plus:DI
		    (mult:DI
		      (sign_extend:DI
			(vec_select:HI (match_operand:V4HI 2 "register_operand" "y")
				       (parallel [(const_int 0)])))
		      (sign_extend:DI
		        (vec_select:HI (match_operand:V4HI 3 "register_operand" "y")
				       (parallel [(const_int 0)]))))
		    (mult:DI
		      (sign_extend:DI
			(vec_select:HI (match_dup 2)
				       (parallel [(const_int 2)])))
		      (sign_extend:DI
		        (vec_select:HI (match_dup 3)
				       (parallel [(const_int 2)])))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiabbn%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiaxy")]
)

(define_insn "iwmmxt_wmiabtn"
  [(set	(match_operand:DI	                           0 "register_operand" "=y")
	(minus:DI (match_operand:DI	                   1 "register_operand" "0")
		  (plus:DI
		    (mult:DI
		      (sign_extend:DI
			(vec_select:HI (match_operand:V4HI 2 "register_operand" "y")
				       (parallel [(const_int 0)])))
		      (sign_extend:DI
		        (vec_select:HI (match_operand:V4HI 3 "register_operand" "y")
				       (parallel [(const_int 1)]))))
		    (mult:DI
		      (sign_extend:DI
		        (vec_select:HI (match_dup 2)
				       (parallel [(const_int 2)])))
		      (sign_extend:DI
			(vec_select:HI (match_dup 3)
				       (parallel [(const_int 3)])))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiabtn%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiaxy")]
)

(define_insn "iwmmxt_wmiatbn"
  [(set (match_operand:DI	                           0 "register_operand" "=y")
	(minus:DI (match_operand:DI	                   1 "register_operand" "0")
		  (plus:DI
		    (mult:DI
		      (sign_extend:DI
			(vec_select:HI (match_operand:V4HI 2 "register_operand" "y")
				       (parallel [(const_int 1)])))
		      (sign_extend:DI
		        (vec_select:HI (match_operand:V4HI 3 "register_operand" "y")
				       (parallel [(const_int 0)]))))
		    (mult:DI
		      (sign_extend:DI
		        (vec_select:HI (match_dup 2)
				       (parallel [(const_int 3)])))
		      (sign_extend:DI
			(vec_select:HI (match_dup 3)
				       (parallel [(const_int 2)])))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiatbn%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiaxy")]
)

(define_insn "iwmmxt_wmiattn"
  [(set (match_operand:DI	                           0 "register_operand" "=y")
	(minus:DI (match_operand:DI	                   1 "register_operand" "0")
		  (plus:DI
		    (mult:DI
		      (sign_extend:DI
			(vec_select:HI (match_operand:V4HI 2 "register_operand" "y")
				       (parallel [(const_int 1)])))
		      (sign_extend:DI
			(vec_select:HI (match_operand:V4HI 3 "register_operand" "y")
				       (parallel [(const_int 1)]))))
		    (mult:DI
		      (sign_extend:DI
			(vec_select:HI (match_dup 2)
				       (parallel [(const_int 3)])))
		      (sign_extend:DI
			(vec_select:HI (match_dup 3)
				       (parallel [(const_int 3)])))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiattn%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiaxy")]
)

(define_insn "iwmmxt_wmiawbb"
  [(set (match_operand:DI	0 "register_operand" "=y")
	(plus:DI
	  (match_operand:DI      1 "register_operand" "0")
	  (mult:DI
	    (sign_extend:DI (vec_select:SI (match_operand:V2SI 2 "register_operand" "y") (parallel [(const_int 0)])))
	    (sign_extend:DI (vec_select:SI (match_operand:V2SI 3 "register_operand" "y") (parallel [(const_int 0)]))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiawbb%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiawxy")]
)

(define_insn "iwmmxt_wmiawbt"
  [(set (match_operand:DI	                               0 "register_operand" "=y")
	(plus:DI
	  (match_operand:DI                                    1 "register_operand" "0")
	  (mult:DI
	    (sign_extend:DI (vec_select:SI (match_operand:V2SI 2 "register_operand" "y") (parallel [(const_int 0)])))
	    (sign_extend:DI (vec_select:SI (match_operand:V2SI 3 "register_operand" "y") (parallel [(const_int 1)]))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiawbt%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiawxy")]
)

(define_insn "iwmmxt_wmiawtb"
  [(set (match_operand:DI	                               0 "register_operand" "=y")
	(plus:DI
	  (match_operand:DI                                    1 "register_operand" "0")
	  (mult:DI
	    (sign_extend:DI (vec_select:SI (match_operand:V2SI 2 "register_operand" "y") (parallel [(const_int 1)])))
	    (sign_extend:DI (vec_select:SI (match_operand:V2SI 3 "register_operand" "y") (parallel [(const_int 0)]))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiawtb%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiawxy")]
)

(define_insn "iwmmxt_wmiawtt"
[(set (match_operand:DI	                                     0 "register_operand" "=y")
      (plus:DI
	(match_operand:DI                                    1 "register_operand" "0")
	(mult:DI
	  (sign_extend:DI (vec_select:SI (match_operand:V2SI 2 "register_operand" "y") (parallel [(const_int 1)])))
	  (sign_extend:DI (vec_select:SI (match_operand:V2SI 3 "register_operand" "y") (parallel [(const_int 1)]))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiawtt%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiawxy")]
)

(define_insn "iwmmxt_wmiawbbn"
  [(set (match_operand:DI	                               0 "register_operand" "=y")
	(minus:DI
	  (match_operand:DI                                    1 "register_operand" "0")
	  (mult:DI
	    (sign_extend:DI (vec_select:SI (match_operand:V2SI 2 "register_operand" "y") (parallel [(const_int 0)])))
	    (sign_extend:DI (vec_select:SI (match_operand:V2SI 3 "register_operand" "y") (parallel [(const_int 0)]))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiawbbn%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiawxy")]
)

(define_insn "iwmmxt_wmiawbtn"
  [(set (match_operand:DI	                               0 "register_operand" "=y")
	(minus:DI
	  (match_operand:DI                                    1 "register_operand" "0")
	  (mult:DI
	    (sign_extend:DI (vec_select:SI (match_operand:V2SI 2 "register_operand" "y") (parallel [(const_int 0)])))
	    (sign_extend:DI (vec_select:SI (match_operand:V2SI 3 "register_operand" "y") (parallel [(const_int 1)]))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiawbtn%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiawxy")]
)

(define_insn "iwmmxt_wmiawtbn"
  [(set (match_operand:DI	                               0 "register_operand" "=y")
	(minus:DI
	  (match_operand:DI                                    1 "register_operand" "0")
	  (mult:DI
	    (sign_extend:DI (vec_select:SI (match_operand:V2SI 2 "register_operand" "y") (parallel [(const_int 1)])))
	    (sign_extend:DI (vec_select:SI (match_operand:V2SI 3 "register_operand" "y") (parallel [(const_int 0)]))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiawtbn%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiawxy")]
)

(define_insn "iwmmxt_wmiawttn"
  [(set (match_operand:DI	                               0 "register_operand" "=y")
	(minus:DI
	  (match_operand:DI                                    1 "register_operand" "0")
	  (mult:DI
	    (sign_extend:DI (vec_select:SI (match_operand:V2SI 2 "register_operand" "y") (parallel [(const_int 1)])))
	    (sign_extend:DI (vec_select:SI (match_operand:V2SI 3 "register_operand" "y") (parallel [(const_int 1)]))))))]
  "TARGET_REALLY_IWMMXT"
  "wmiawttn%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmiawxy")]
)

(define_insn "iwmmxt_wmerge"
  [(set (match_operand:DI         0 "register_operand" "=y")
	(ior:DI
	  (ashift:DI
	    (match_operand:DI     2 "register_operand" "y")
	    (minus:SI
	      (const_int 64)
	      (mult:SI
	        (match_operand:SI 3 "immediate_operand" "i")
		(const_int 8))))
	  (lshiftrt:DI
	    (ashift:DI
	      (match_operand:DI   1 "register_operand" "y")
	      (mult:SI
	        (match_dup 3)
		(const_int 8)))
	    (mult:SI
	      (match_dup 3)
	      (const_int 8)))))]
  "TARGET_REALLY_IWMMXT"
  "wmerge%?\\t%0, %1, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmerge")]
)

(define_insn "iwmmxt_tandc<mode>3"
  [(set (reg:CC CC_REGNUM)
	(subreg:CC (unspec:VMMX [(const_int 0)] UNSPEC_TANDC) 0))
   (unspec:CC [(reg:SI 15)] UNSPEC_TANDC)]
  "TARGET_REALLY_IWMMXT"
  "tandc<MMX_char>%?\\t r15"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tandc")]
)

(define_insn "iwmmxt_torc<mode>3"
  [(set (reg:CC CC_REGNUM)
	(subreg:CC (unspec:VMMX [(const_int 0)] UNSPEC_TORC) 0))
   (unspec:CC [(reg:SI 15)] UNSPEC_TORC)]
  "TARGET_REALLY_IWMMXT"
  "torc<MMX_char>%?\\t r15"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_torc")]
)

(define_insn "iwmmxt_torvsc<mode>3"
  [(set (reg:CC CC_REGNUM)
	(subreg:CC (unspec:VMMX [(const_int 0)] UNSPEC_TORVSC) 0))
   (unspec:CC [(reg:SI 15)] UNSPEC_TORVSC)]
  "TARGET_REALLY_IWMMXT"
  "torvsc<MMX_char>%?\\t r15"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_torvsc")]
)

(define_insn "iwmmxt_textrc<mode>3"
  [(set (reg:CC CC_REGNUM)
	(subreg:CC (unspec:VMMX [(const_int 0)
		                 (match_operand:SI 0 "immediate_operand" "i")] UNSPEC_TEXTRC) 0))
   (unspec:CC [(reg:SI 15)] UNSPEC_TEXTRC)]
  "TARGET_REALLY_IWMMXT"
  "textrc<MMX_char>%?\\t r15, %0"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_textrc")]
)
