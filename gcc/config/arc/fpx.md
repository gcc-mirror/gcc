;; Machine description of the Synopsys DesignWare ARC cpu Floating Point
;; extensions for GNU C compiler
;; Copyright (C) 2007-2021 Free Software Foundation, Inc.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODOs:
;;        dpfp blocks?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheduler descriptions for the fpx instructions
(define_insn_reservation "spfp_compact" 3
  (and (match_test "TARGET_SPFP_COMPACT_SET")
       (eq_attr "type" "spfp"))
  "issue+core, nothing*2, write_port")

(define_insn_reservation "spfp_fast" 6
  (and (match_test "TARGET_SPFP_FAST_SET")
       (eq_attr "type" "spfp"))
  "issue+core, nothing*5, write_port")

(define_insn_reservation "dpfp_compact_mult" 7
  (and (match_test "TARGET_DPFP_COMPACT_SET")
       (eq_attr "type" "dpfp_mult"))
  "issue+core, nothing*6, write_port")

(define_insn_reservation "dpfp_compact_addsub" 5
  (and (match_test "TARGET_DPFP_COMPACT_SET")
       (eq_attr "type" "dpfp_addsub"))
  "issue+core, nothing*4, write_port")

(define_insn_reservation "dpfp_fast" 5
  (and (match_test "TARGET_DPFP_FAST_SET")
       (eq_attr "type" "dpfp_mult,dpfp_addsub"))
  "issue+core, nothing*4, write_port")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "*addsf3_fpx"
  [(set (match_operand:SF 0 "register_operand"          "=r,r,r,r,r ")
	(plus:SF (match_operand:SF 1 "nonmemory_operand" "0,r,GCal,r,0")
		 (match_operand:SF 2 "nonmemory_operand" "I,rL,r,GCal,LrCal")))]
;  "(TARGET_ARC700 || TARGET_ARC600) && TARGET_SPFP_SET";Add flag for float
  "TARGET_SPFP"
  "@
   fadd %0,%1,%2
   fadd %0,%1,%2
   fadd   %0,%1,%2
   fadd   %0,%1,%2
   fadd%? %0,%1,%2"
  [(set_attr "type" "spfp")
  (set_attr "length" "4,4,8,8,8")])

(define_insn "*subsf3_fpx"
  [(set (match_operand:SF 0 "register_operand"          "=r,r,r,r,r ")
	(minus:SF (match_operand:SF 1 "nonmemory_operand" "r,0,GCal,r,0")
		 (match_operand:SF 2 "nonmemory_operand" "rL,I,r,GCal,LrCal")))]
  ;"(TARGET_ARC700 || TARGET_ARC600) && TARGET_SPFP_SET";Add flag for float
  "TARGET_SPFP"
  "@
   fsub %0,%1,%2
   fsub %0,%1,%2
   fsub   %0,%1,%2
   fsub   %0,%1,%2
   fsub%? %0,%1,%2"
  [(set_attr "type" "spfp")
  (set_attr "length" "4,4,8,8,8")])

(define_insn "*mulsf3_fpx"
  [(set (match_operand:SF 0 "register_operand"          "=r,r,r,r,r ")
	(mult:SF (match_operand:SF 1 "nonmemory_operand" "r,0,GCal,r,0")
		 (match_operand:SF 2 "nonmemory_operand" "rL,I,r,GCal,LrCal")))]
;  "(TARGET_ARC700 || TARGET_ARC600) && TARGET_SPFP_SET"	;Add flag for float
  "TARGET_SPFP"
  "@
   fmul %0,%1,%2
   fmul %0,%1,%2
   fmul   %0,%1,%2
   fmul   %0,%1,%2
   fmul%? %0,%1,%2"
  [(set_attr "type" "spfp")
  (set_attr "length" "4,4,8,8,8")])


;; For comparisons, we can avoid storing the top half of the result into
;; a register since '.f' lets us set the Z bit for the conditional
;; branch insns.

;; ??? FIXME (x-y)==0 is not a correct comparison for floats:
;;     http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm
(define_insn "cmpsfpx_raw"
  [(set (reg:CC_FPX 61)
	(compare:CC_FPX (match_operand:SF 0 "register_operand" "r")
			 (match_operand:SF 1 "register_operand" "r")))]
  "TARGET_ARGONAUT_SET && TARGET_SPFP"
  "fsub.f 0,%0,%1"
  [(set_attr "type" "spfp")
   (set_attr "length" "4")])

;; ??? FIXME (x-y)==0 is not a correct comparison for floats:
;;     http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm
;; ??? FIXME we claim to clobber operand 2, yet the two numbers appended
;; to the actual instructions are incorrect.  The result of the d*subh
;; insn is stored in the Dx register specified by that first number.
(define_insn "cmpdfpx_raw"
  [(set (reg:CC_FPX 61)
	(compare:CC_FPX (match_operand:DF 0 "nonmemory_operand" "D,r")
			 (match_operand:DF 1 "nonmemory_operand" "r,D")))
   (clobber (match_scratch:DF 2 "=D,D"))]
  "TARGET_ARGONAUT_SET && TARGET_DPFP"
  "@
   dsubh%F0%F1.f 0,%H2,%L2
   drsubh%F0%F2.f 0,%H1,%L1"
  [(set_attr "type" "dpfp_addsub")
   (set_attr "length" "4")])

;; ??? FIXME subtraction is not a correct comparison for floats:
;;     http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm
(define_insn "*cmpfpx_gt"
  [(set (reg:CC_FP_GT 61) (compare:CC_FP_GT (reg:CC_FPX 61) (const_int 0)))]
  "TARGET_ARGONAUT_SET"
  "cmp.ls pcl,pcl"
  [(set_attr "type" "compare")
   (set_attr "length" "4")])

;; ??? FIXME subtraction is not a correct comparison for floats:
;;     http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm
(define_insn "*cmpfpx_ge"
  [(set (reg:CC_FP_GE 61) (compare:CC_FP_GE (reg:CC_FPX 61) (const_int 0)))]
  "TARGET_ARGONAUT_SET"
  "rcmp.pnz pcl,0"
  [(set_attr "type" "compare")
   (set_attr "length" "4")])

;; DPFP instructions begin...

;; op0_reg = D1_reg.low
(define_insn "*lr_double_lower"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(match_operand:DF 1 "arc_double_register_operand" "D")] VUNSPEC_ARC_LR ))]
 "TARGET_DPFP && !TARGET_DPFP_DISABLE_LRSR"
"lr %0, [%1l] ; *lr_double_lower"
[(set_attr "length" "8")
(set_attr "type" "lr")]
)

(define_insn "*lr_double_higher"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(match_operand:DF 1 "arc_double_register_operand" "D")]
			    VUNSPEC_ARC_LR_HIGH ))]
 "TARGET_DPFP && !TARGET_DPFP_DISABLE_LRSR"
"lr %0, [%1h] ; *lr_double_higher"
[(set_attr "length" "8")
(set_attr "type" "lr")]
)

(define_insn "*dexcl_3op_peep2_insn"
  [(set (match_operand:SI 0 "dest_reg_operand" "=r") ; not register_operand, to accept SUBREG
	(unspec_volatile:SI
	 [(match_operand:SI 1 "shouldbe_register_operand" "r") ; r1
	  (match_operand:SI 2 "shouldbe_register_operand" "r") ; r0
	  ] VUNSPEC_ARC_DEXCL ))
   (clobber (match_operand:DF 3 "arc_double_register_operand" "=&D"))]
  "TARGET_DPFP"
  "dexcl%F3 %0, %1, %2"
  [(set_attr "type" "move")
   (set_attr "length" "4")]
)

;; version which will not overwrite operand0
(define_insn "dexcl_2op"
  [(set (match_operand:DF 0 "arc_double_register_operand" "=D")
	(unspec_volatile:DF
	 [(match_operand:SI 1 "shouldbe_register_operand" "r") ; r1
	  (match_operand:SI 2 "shouldbe_register_operand" "r") ; r0
	  ] VUNSPEC_ARC_DEXCL_NORES))
  ]
  "TARGET_DPFP"
  "dexcl%F0 0, %1, %2"
  [(set_attr "type" "move")
   (set_attr "length" "4")]
)

;; dexcl a,b,c pattern generated by the peephole2 above
(define_insn "*dexcl_3op_peep2_insn_lr"
  [(parallel [(set (match_operand:SI 0 "register_operand" "=r")
		   (unspec_volatile:SI [(match_operand:DF 1 "arc_double_register_operand" "=D")] VUNSPEC_ARC_LR ))
	     (set (match_dup 1) (match_operand:DF 2 "register_operand" "r"))]
	    )
  ]
  "TARGET_DPFP && !TARGET_DPFP_DISABLE_LRSR"
  "dexcl%F1 %0, %H2, %L2"
  [(set_attr "type" "move")
   (set_attr "length" "4")]
)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             doubles support for ARC
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; D0 = D1+{reg_pair}2
;; (define_expand "adddf3"
;;   [(set (match_operand:DF 0 "arc_double_register_operand"          "")
;; 	(plus:DF (match_operand:DF 1 "arc_double_register_operand" "")
;; 		 (match_operand:DF 2 "nonmemory_operand" "")))]
;;  "TARGET_DPFP"
;;  " "
;; )
;; daddh{0}{1} 0, {reg_pair}2.hi, {reg_pair}2.lo
;; OR
;; daddh{0}{1} 0, reg3, limm2.lo
;; daddh{0}{1} 0, {reg_pair}2.hi, {reg_pair}2.lo  /* operand 4 = 1*/
;; OR
;; daddh{0}{1} 0, reg3, limm2.lo /* operand 4 = 0 */
;;
(define_insn "adddf3_insn"
  [(set (match_operand:DF 0 "arc_double_register_operand"          "=D,D")
	(plus:DF (match_operand:DF 1 "arc_double_register_operand" "D,D")
		 (match_operand:DF 2 "nonmemory_operand" "!r,G")))
  (use (match_operand:SI 3 "" "N,r"))
  (use (match_operand:SI 4 "" "N,Q"))
  ; Prevent can_combine_p from combining muldf3_insn patterns with
  ; different USE pairs.
  (use (match_dup 2))
  ]
  "TARGET_DPFP &&
   !(GET_CODE(operands[2]) == CONST_DOUBLE && GET_CODE(operands[3]) == CONST_INT)"
  "@
     daddh%F0%F1 0,%H2,%L2
     daddh%F0%F1 0,%3,%L2"
  [(set_attr "type" "dpfp_addsub")
  (set_attr "length" "4,8")])

;; dmulh{0}{1} 0, {reg_pair}2.hi, {reg_pair}2.lo
;; OR
;; dmulh{0}{1} 0, reg3, limm2.lo
;; dmulh{0}{1} 0, {reg_pair}2.hi, {reg_pair}2.lo /* operand 4 = 1*/
;; OR
;; dmulh{0}{1} 0, reg3, limm2.lo /* operand 4 = 0*/
(define_insn "muldf3_insn"
  [(set (match_operand:DF 0 "arc_double_register_operand"          "=D,D")
	(mult:DF (match_operand:DF 1 "arc_double_register_operand" "D,D")
		 (match_operand:DF 2 "nonmemory_operand" "!r,G")))
  (use (match_operand:SI 3 "" "N,!r"))
  (use (match_operand:SI 4 "" "N,Q"))
  ; Prevent can_combine_p from combining muldf3_insn patterns with
  ; different USE pairs.
  (use (match_dup 2))
  ]
  "TARGET_DPFP &&
   !(GET_CODE(operands[2]) == CONST_DOUBLE && GET_CODE(operands[3]) == CONST_INT)"
  "@
    dmulh%F0%F1 0,%H2,%L2
    dmulh%F0%F1 0,%3, %L2"
  [(set_attr "type" "dpfp_mult")
  (set_attr "length" "4,8")])

;; dsubh{0}{1} 0, {reg_pair}2.hi, {reg_pair}2.lo
;; OR
;; dsubh{0}{1} 0, reg3, limm2.lo
;; OR
;; drsubh{0}{2} 0, {reg_pair}1.hi, {reg_pair}1.lo
;; OR
;; drsubh{0}{2} 0, reg3, limm1.lo
;; dsubh{0}{1} 0, {reg_pair}2.hi, {reg_pair}2.lo /* operand 4 = 1 */
;; OR
;; dsubh{0}{1} 0, reg3, limm2.lo /* operand 4 = 0*/
;; OR
;; drsubh{0}{2} 0, {reg_pair}1.hi, {reg_pair}1.lo /* operand 4 = 1 */
;; OR
;; drsubh{0}{2} 0, reg3, limm1.lo /* operand 4 = 0*/
(define_insn "subdf3_insn"
  [(set (match_operand:DF 0 "arc_double_register_operand"          "=D,D,D,D")
		   (minus:DF (match_operand:DF 1 "nonmemory_operand" "D,D,!r,G")
			    (match_operand:DF 2 "nonmemory_operand" "!r,G,D,D")))
  (use (match_operand:SI 3 "" "N,r,N,r"))
  (use (match_operand:SI 4 "" "N,Q,N,Q"))
  ; Prevent can_combine_p from combining muldf3_insn patterns with
  ; different USE pairs.
  (use (match_dup 2))]
  "TARGET_DPFP &&
   !(GET_CODE(operands[2]) == CONST_DOUBLE && GET_CODE(operands[3]) == CONST_INT) &&
   !(GET_CODE(operands[1]) == CONST_DOUBLE && GET_CODE(operands[3]) == CONST_INT)"
  "@
     dsubh%F0%F1 0,%H2,%L2
     dsubh%F0%F1 0,%3,%L2
     drsubh%F0%F2 0,%H1,%L1
     drsubh%F0%F2 0,%3,%L1"
  [(set_attr "type" "dpfp_addsub")
   (set_attr "length" "4,8,4,8")
   (set_attr "cpu_facility" "*,*,fpx,fpx")])

;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Peephole for following conversion
;; ;;                    D0 = D2<op>{reg_pair}3
;; ;;                    {reg_pair}5 = D0
;; ;;                    D0 = {reg_pair}6
;; ;;                            |
;; ;;                            V
;; ;;            _________________________________________________________
;; ;;           / D0             = D2 <op> {regpair3_or_limmreg34}
;; ;;    ---- +   {reg_pair}5.hi = ( D2<op>{regpair3_or_limmreg34} ).hi
;; ;;   |       \_________________________________________________________
;; ;;   |
;; ;;   |         ________________________________________________________
;; ;;   |      / {reg_pair}5.lo  = ( D2<op>{regpair3_or_limmreg34} ).lo
;; ;;   +-----+  D0              = {reg_pair}6
;; ;;          \ _________________________________________________________
;; ;;                            ||
;; ;;                            ||
;; ;;                            \/
;; ;;  d<op>{0}{2}h {reg_pair}5.hi, {regpair3_or_limmreg34}.lo, {regpair3_or_limmreg34}.hi
;; ;;  dexcl{0}    {reg_pair}5.lo, {reg_pair}6.lo, {reg_pair}6.hi
;; ;; -----------------------------------------------------------------------------------------
;; ;;  where <op> is one of {+,*,-}
;; ;;        <opname> is {add,mult,sub}
;; ;;
;; ;; NOTE: For rsub insns D2 and {regpair3_or_limmreg34} get interchanged as
;; ;;       {regpair2_or_limmreg24} and D3
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define_peephole2
;;   [(parallel [(set (match_operand:DF 0 "register_operand"          "")
;; 	(match_operator:DF 1 "arc_dpfp_operator" [(match_operand:DF 2 "nonmemory_operand" "")
;; 			   (match_operand:DF 3 "nonmemory_operand" "")]))
;; 	     (use (match_operand:SI 4 "" ""))])
;;   (set (match_operand:DF 5 "register_operand" "")
;;        (match_dup 0))
;;   (set (match_dup 0)
;;        (match_operand:DF 6 "register_operand" ""))
;;   ]
;;   "TARGET_DPFP"
;;   [
;;   (parallel [(set (match_dup 0)
;; 		  (match_op_dup:DF 1 [(match_dup 2)
;; 				   (match_dup 3)]))
;; 	    (use (match_dup 4))
;;             (set (match_dup 5)
;; 		 (match_op_dup:DF  1 [(match_dup 2)
;; 				   (match_dup 3)]))])
;;   (parallel [
;; ;;	    (set (subreg:SI (match_dup 5) 0)
;; 	    (set (match_dup 7)
;; 		 (unspec_volatile [(match_dup 0)] VUNSPEC_ARC_LR ))
;; 	    (set (match_dup 0) (match_dup 6))]
;; 	    )
;;   ]
;;   "operands[7] = simplify_gen_subreg(SImode,operands[5],DFmode,0);"
;;   )
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Peephole for following conversion
;;                    D0 = D2<op>{reg_pair}3
;;                    {reg_pair}6 = D0
;;                    D0 = {reg_pair}7
;;                            |
;;                            V
;;            _________________________________________________________
;;           / D0             = D2 <op> {regpair3_or_limmreg34}
;;    ---- +   {reg_pair}6.hi = ( D2<op>{regpair3_or_limmreg34} ).hi
;;   |       \_________________________________________________________
;;   |
;;   |         ________________________________________________________
;;   |      / {reg_pair}6.lo  = ( D2<op>{regpair3_or_limmreg34} ).lo
;;   +-----+  D0              = {reg_pair}7
;;          \ _________________________________________________________
;;                            ||
;;                            ||
;;                            \/
;;  d<op>{0}{2}h {reg_pair}6.hi, {regpair3_or_limmreg34}.lo, {regpair3_or_limmreg34}.hi
;;  dexcl{0}    {reg_pair}6.lo, {reg_pair}7.lo, {reg_pair}7.hi
;; -----------------------------------------------------------------------------------------
;;  where <op> is one of {+,*,-}
;;        <opname> is {add,mult,sub}
;;
;; NOTE: For rsub insns D2 and {regpair3_or_limmreg34} get interchanged as
;;       {regpair2_or_limmreg24} and D3
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define_peephole2
  [(parallel [(set (match_operand:DF 0 "register_operand"          "")
	(match_operator:DF 1 "arc_dpfp_operator" [(match_operand:DF 2 "nonmemory_operand" "")
			   (match_operand:DF 3 "nonmemory_operand" "")]))
	     (use (match_operand:SI 4 "" ""))
	     (use (match_operand:SI 5 "" ""))
	     (use (match_operand:SI 6 "" ""))])
  (set (match_operand:DF 7 "register_operand" "")
       (match_dup 0))
  (set (match_dup 0)
       (match_operand:DF 8 "register_operand" ""))
  ]
  "TARGET_DPFP && !TARGET_DPFP_DISABLE_LRSR"
  [
  (parallel [(set (match_dup 0)
		  (match_op_dup:DF 1 [(match_dup 2)
				   (match_dup 3)]))
	    (use (match_dup 4))
	    (use (match_dup 5))
            (set (match_dup 7)
		 (match_op_dup:DF  1 [(match_dup 2)
				   (match_dup 3)]))])
  (parallel [
;;	    (set (subreg:SI (match_dup 7) 0)
	    (set (match_dup 9)
		 (unspec_volatile:SI [(match_dup 0)] VUNSPEC_ARC_LR ))
	    (set (match_dup 0) (match_dup 8))]
	    )
  ]
  "operands[9] = simplify_gen_subreg(SImode,operands[7],DFmode,0);"
  )

;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Peephole to generate d<opname>{ij}h a,b,c instructions
;; ;;                    D0 = D2<op>{reg_pair}3
;; ;;                    {reg_pair}5 = D0
;; ;;                            |
;; ;;                            V
;; ;;            __________________________________________
;; ;;           / D0             = D2 <op> {regpair3_or_limmreg34}
;; ;;    ---- +   {reg_pair}5.hi = ( D2<op>{regpair3_or_limmreg34} ).hi
;; ;;   |       \__________________________________________
;; ;;   |
;; ;;   + ---    {reg_pair}5.lo     = ( D2<op>{regpair3_or_limmreg34} ).lo
;; ;;                            ||
;; ;;                            ||
;; ;;                            \/
;; ;;  d<op>{0}{2}h {reg_pair}4.hi, {regpair3_or_limmreg34}.lo, {regpair3_or_limmreg34}.hi
;; ;;  lr    {reg_pair}4.lo, {D2l}
;; ;; ----------------------------------------------------------------------------------------
;; ;;  where <op> is one of {+,*,-}
;; ;;        <opname> is {add,mult,sub}
;; ;;
;; ;; NOTE: For rsub insns D2 and {regpair3_or_limmreg34} get interchanged as
;; ;;       {regpair2_or_limmreg24} and D3
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define_peephole2
;;   [(parallel [(set (match_operand:DF 0 "register_operand"          "")
;; 		   (match_operator:DF 1 "arc_dpfp_operator" [(match_operand:DF 2 "nonmemory_operand" "")
;; 				      (match_operand:DF 3 "nonmemory_operand" "")]))
;; 	     (use (match_operand:SI 4 "" ""))])
;;   (set (match_operand:DF 5 "register_operand" "")
;;        (match_dup 0))
;;   ]
;;   "TARGET_DPFP"
;;   [
;;   (parallel [(set (match_dup 0)
;; 		  (match_op_dup:DF 1 [(match_dup 2)
;; 				   (match_dup 3)]))
;; 	    (use (match_dup 4))
;;             (set (match_dup 5)
;; 		 (match_op_dup:DF  1 [(match_dup 2)
;; 				   (match_dup 3)]))])
;; ;  (set (subreg:SI (match_dup 5) 0)
;;   (set (match_dup 6)
;;        (unspec_volatile [(match_dup 0)] VUNSPEC_ARC_LR ))
;;   ]
;;   "operands[6] = simplify_gen_subreg(SImode,operands[5],DFmode,0);"
;;   )
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Peephole to generate d<opname>{ij}h a,b,c instructions
;;                    D0 = D2<op>{reg_pair}3
;;                    {reg_pair}6 = D0
;;                            |
;;                            V
;;            __________________________________________
;;           / D0             = D2 <op> {regpair3_or_limmreg34}
;;    ---- +   {reg_pair}6.hi = ( D2<op>{regpair3_or_limmreg34} ).hi
;;   |       \__________________________________________
;;   |
;;   + ---    {reg_pair}6.lo     = ( D2<op>{regpair3_or_limmreg34} ).lo
;;                            ||
;;                            ||
;;                            \/
;;  d<op>{0}{2}h {reg_pair}4.hi, {regpair3_or_limmreg34}.lo, {regpair3_or_limmreg34}.hi
;;  lr    {reg_pair}4.lo, {D2l}
;; ----------------------------------------------------------------------------------------
;;  where <op> is one of {+,*,-}
;;        <opname> is {add,mult,sub}
;;
;; NOTE: For rsub insns D2 and {regpair3_or_limmreg34} get interchanged as
;;       {regpair2_or_limmreg24} and D3
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define_peephole2
  [(parallel [(set (match_operand:DF 0 "register_operand"          "")
		   (match_operator:DF 1 "arc_dpfp_operator" [(match_operand:DF 2 "nonmemory_operand" "")
				      (match_operand:DF 3 "nonmemory_operand" "")]))
	     (use (match_operand:SI 4 "" ""))
	     (use (match_operand:SI 5 "" ""))
	     (use (match_operand:SI 6 "" ""))])
  (set (match_operand:DF 7 "register_operand" "")
       (match_dup 0))
  ]
  "TARGET_DPFP  && !TARGET_DPFP_DISABLE_LRSR"
  [
  (parallel [(set (match_dup 0)
		  (match_op_dup:DF 1 [(match_dup 2)
				   (match_dup 3)]))
	    (use (match_dup 4))
	    (use (match_dup 5))
            (set (match_dup 7)
		 (match_op_dup:DF  1 [(match_dup 2)
				   (match_dup 3)]))])
;  (set (subreg:SI (match_dup 7) 0)
  (set (match_dup 8)
       (unspec_volatile:SI [(match_dup 0)] VUNSPEC_ARC_LR ))
  ]
  "operands[8] = simplify_gen_subreg(SImode,operands[7],DFmode,0);"
  )

;; ;;            _______________________________________________________
;; ;;           / D0             = D1 + {regpair2_or_limmreg23}
;; ;;         +   {reg_pair}4.hi = ( D1 + {regpair2_or_limmreg23} ).hi
;; ;;           \_______________________________________________________
;; (define_insn "*daddh_peep2_insn"
;;   [(parallel [(set (match_operand:DF 0 "arc_double_register_operand" "=D,D")
;; 		   (plus:DF (match_operand:DF 1 "arc_double_register_operand" "D,D")
;; 			    (match_operand:DF 2 "nonmemory_operand" "r,G")))
;; 	     (use (match_operand:SI 3 "" "N,r"))
;; 	     (set (match_operand:DF 4 "register_operand" "=r,r")
;; 		  (plus:DF (match_dup 1)
;; 			   (match_dup 2)))])]
;;  "TARGET_DPFP"
;;  "@
;;     daddh%F0%F1 %H4, %H2, %L2
;;     daddh%F0%F1 %H4, %3, %L2"
;;  [(set_attr "type" "dpfp_addsub")
;;  (set_attr "length" "4,8")]
;; )
;;            _______________________________________________________
;;           / D0             = D1 + {regpair2_or_limmreg23}
;;         +   {reg_pair}5.hi = ( D1 + {regpair2_or_limmreg23} ).hi
;;           \_______________________________________________________
(define_insn "*daddh_peep2_insn"
  [(parallel [(set (match_operand:DF 0 "arc_double_register_operand" "=D,D")
		   (plus:DF (match_operand:DF 1 "arc_double_register_operand" "D,D")
			    (match_operand:DF 2 "nonmemory_operand" "r,G")))
	     (use (match_operand:SI 3 "" "N,r"))
	     (use (match_operand:SI 4 "" "N,Q"))
	     (use (match_operand:SI 5 "" ""))
	     (set (match_operand:DF 6 "register_operand" "=r,r")
		  (plus:DF (match_dup 1)
			   (match_dup 2)))])]
 "TARGET_DPFP &&
   !(GET_CODE(operands[2]) == CONST_DOUBLE && GET_CODE(operands[3]) == CONST_INT)"
 "@
    daddh%F0%F1 %H6, %H2, %L2
    daddh%F0%F1 %H6, %3, %L2"
 [(set_attr "type" "dpfp_addsub")
 (set_attr "length" "4,8")]
)

;;            _______________________________________________________
;;           / D0             = D1 * {regpair2_or_limmreg23}
;;         +   {reg_pair}5.hi = ( D1 * {regpair2_or_limmreg23} ).hi
;;           \_______________________________________________________
(define_insn "*dmulh_peep2_insn"
  [(parallel [(set (match_operand:DF 0 "arc_double_register_operand" "=D,D")
		   (mult:DF (match_operand:DF 1 "arc_double_register_operand" "D,D")
			    (match_operand:DF 2 "nonmemory_operand" "r,G")))
	     (use (match_operand:SI 3 "" "N,r"))
	     (use (match_operand:SI 4 "" "N,Q"))
	     (use (match_operand:SI 5 "" ""))
	     (set (match_operand:DF 6 "register_operand" "=r,r")
		  (mult:DF (match_dup 1)
				      (match_dup 2)))])]
 "TARGET_DPFP &&
   !(GET_CODE(operands[2]) == CONST_DOUBLE && GET_CODE(operands[3]) == CONST_INT)"
 "@
    dmulh%F0%F1 %H6, %H2, %L2
    dmulh%F0%F1 %H6, %3, %L2"
 [(set_attr "type" "dpfp_mult")
 (set_attr "length" "4,8")]
)

;;            _______________________________________________________
;;           / D0             = D1 - {regpair2_or_limmreg23}
;;         +   {reg_pair}5.hi = ( D1 - {regpair2_or_limmreg23} ).hi
;;           \_______________________________________________________
;;  OR
;;            _______________________________________________________
;;           / D0             = {regpair1_or_limmreg13} - D2
;;         +   {reg_pair}5.hi = ( {regpair1_or_limmreg13} ).hi - D2
;;           \_______________________________________________________
(define_insn "*dsubh_peep2_insn"
  [(parallel [(set (match_operand:DF 0 "arc_double_register_operand" "=D,D,D,D")
		   (minus:DF (match_operand:DF 1 "nonmemory_operand" "D,D,r,G")
			     (match_operand:DF 2 "nonmemory_operand" "r,G,D,D")))
	     (use (match_operand:SI 3 "" "N,r,N,r"))
	     (use (match_operand:SI 4 "" "N,Q,N,Q"))
	     (use (match_operand:SI 5 "" ""))
	     (set (match_operand:DF 6 "register_operand" "=r,r,r,r")
		  (minus:DF (match_dup 1)
				      (match_dup 2)))])]
 "TARGET_DPFP &&
   !(GET_CODE(operands[2]) == CONST_DOUBLE && GET_CODE(operands[3]) == CONST_INT)  &&
   !(GET_CODE(operands[1]) == CONST_DOUBLE && GET_CODE(operands[3]) == CONST_INT)"
 "@
  dsubh%F0%F1 %H6, %H2, %L2
  dsubh%F0%F1 %H6, %3, %L2
  drsubh%F0%F2 %H6, %H1, %L1
  drsubh%F0%F2 %H6, %3, %L1"
 [(set_attr "type" "dpfp_addsub")
  (set_attr "length" "4,8,4,8")
  (set_attr "cpu_facility" "*,*,fpx,fpx")])

;; Intel QUARK SE extensions
(define_mode_iterator QUARK_CMP [CC_FP_GT CC_FP_GE])
(define_mode_attr quark_cmp [(CC_FP_GT "gt") (CC_FP_GE "ge")])

(define_expand "cmp_quark"
  [(parallel [(set (match_operand 0 "")
		   (match_operand 1 ""))
	      (clobber (match_scratch:SI 2 ""))])]
  ""
  "")

(define_insn "*cmpsf_quark_<quark_cmp>"
  [(set (reg:QUARK_CMP CC_REG)
	(compare:QUARK_CMP (match_operand:SF 0 "register_operand" "r")
			   (match_operand:SF 1 "register_operand" "r")))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_FPX_QUARK"
  "dsp_fp_cmp\\t%2,%0,%1\\n\\trsub.f\\t0,%2,7\\n\\tcmp.nc\\t%2,1\\n\\tcmp.hi\\t%2,3"
  [(set_attr "length" "16")
   (set_attr "cond" "set")
   (set_attr "predicable" "no")
   (set_attr "cond" "nocond")])

(define_insn "*cmpsf_quark_ord"
  [(set (reg:CC_FP_ORD CC_REG)
	(compare:CC_FP_ORD (match_operand:SF 0 "register_operand" "r")
			   (match_operand:SF 1 "register_operand" "r")))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_FPX_QUARK"
  "dsp_fp_cmp\\t%2,%0,%1\\n\\tadd.f\\t%2,%2,-8"
  [(set_attr "length" "8")
   (set_attr "cond" "set")
   (set_attr "predicable" "no")
   (set_attr "cond" "nocond")])

(define_insn "*cmpsf_quark_uneq"
  [(set (reg:CC_FP_UNEQ CC_REG)
	(compare:CC_FP_UNEQ (match_operand:SF 0 "register_operand" "r")
			    (match_operand:SF 1 "register_operand" "r")))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_FPX_QUARK"
  "dsp_fp_cmp\\t%2,%0,%1\\n\\ttst\\t%2,6"
  [(set_attr "length" "8")
   (set_attr "cond" "set")
   (set_attr "predicable" "no")
   (set_attr "cond" "nocond")])

(define_insn "*cmpsf_quark_eq"
  [(set (reg:CC_Z CC_REG)
	(compare:CC_Z (match_operand:SF 0 "register_operand" "r")
		      (match_operand:SF 1 "register_operand" "r")))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_FPX_QUARK"
  "dsp_fp_cmp\\t%2,%0,%1\\n\\ttst\\t%2,0x0E"
  [(set_attr "length" "8")
   (set_attr "cond" "set")
   (set_attr "predicable" "no")
   (set_attr "cond" "nocond")])

(define_insn "*divsf3_quark"
  [(set (match_operand:SF 0 "register_operand"        "=r")
	(div:SF (match_operand:SF 1 "register_operand" "r")
		(match_operand:SF 2 "register_operand" "r")))]
  "TARGET_FPX_QUARK"
  "dsp_fp_div\\t%0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")
   (set_attr "cond" "nocond")])

(define_insn "*sqrtsf2_quark"
  [(set (match_operand:SF 0 "register_operand"          "=r")
	(sqrt:SF (match_operand:SF 1 "register_operand" "r")))]
  "TARGET_FPX_QUARK"
  "dsp_fp_sqrt\\t%0,%1"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")
   (set_attr "cond" "nocond")])

;; SF->SI (using rounding towards zero)
(define_insn "*fix_truncsfsi2_quark"
  [(set (match_operand:SI 0 "register_operand"                "=r")
	(fix:SI (fix:SF (match_operand:SF 1 "register_operand" "r"))))]
  "TARGET_FPX_QUARK"
  "dsp_fp_flt2i\\t%0,%1"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")
   (set_attr "cond" "nocond")])

;; SI->SF
(define_insn "*floatsisf2_quark"
  [(set (match_operand:SF 0 "register_operand"          "=r")
	(float:SF (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_FPX_QUARK"
  "dsp_fp_i2flt\\t%0,%1"
  [(set_attr "length" "4")
   (set_attr "predicable" "no")
   (set_attr "cond" "nocond")])

