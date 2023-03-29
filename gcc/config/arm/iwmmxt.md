;; Patterns for the Intel Wireless MMX technology architecture.
;; Copyright (C) 2003-2023 Free Software Foundation, Inc.
;; Contributed by Red Hat.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Register numbers. Need to sync with FIRST_IWMMXT_GR_REGNUM in arm.h
(define_constants
  [(WCGR0           96)
   (WCGR1           97)
   (WCGR2           98)
   (WCGR3           99)
  ]
)

(define_insn "tbcstv8qi"
  [(set (match_operand:V8QI                   0 "register_operand" "=y")
        (vec_duplicate:V8QI (match_operand:QI 1 "s_register_operand" "r")))]
  "TARGET_REALLY_IWMMXT"
  "tbcstb%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tbcst")]
)

(define_insn "tbcstv4hi"
  [(set (match_operand:V4HI                   0 "register_operand" "=y")
        (vec_duplicate:V4HI (match_operand:HI 1 "s_register_operand" "r")))]
  "TARGET_REALLY_IWMMXT"
  "tbcsth%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tbcst")]
)

(define_insn "tbcstv2si"
  [(set (match_operand:V2SI                   0 "register_operand" "=y")
        (vec_duplicate:V2SI (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_REALLY_IWMMXT"
  "tbcstw%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tbcst")]
)

(define_insn "iwmmxt_iordi3"
  [(set (match_operand:DI         0 "register_operand" "=y")
        (ior:DI (match_operand:DI 1 "register_operand" "%y")
		(match_operand:DI 2 "register_operand"  "y")))]
  "TARGET_REALLY_IWMMXT"
  "wor%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "length" "4")
   (set_attr "type" "wmmx_wor")]
)

(define_insn "iwmmxt_xordi3"
  [(set (match_operand:DI         0 "register_operand" "=y")
        (xor:DI (match_operand:DI 1 "register_operand" "%y")
		(match_operand:DI 2 "register_operand"  "y")))]
  "TARGET_REALLY_IWMMXT"
  "wxor%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "length" "4")
   (set_attr "type" "wmmx_wxor")]
)

(define_insn "iwmmxt_anddi3"
  [(set (match_operand:DI         0 "register_operand" "=y")
        (and:DI (match_operand:DI 1 "register_operand" "%y")
		(match_operand:DI 2 "register_operand"  "y")))]
  "TARGET_REALLY_IWMMXT"
  "wand%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "length" "4")
   (set_attr "type" "wmmx_wand")]
)

(define_insn "iwmmxt_nanddi3"
  [(set (match_operand:DI                 0 "register_operand" "=y")
        (and:DI (match_operand:DI         1 "register_operand"  "y")
		(not:DI (match_operand:DI 2 "register_operand"  "y"))))]
  "TARGET_REALLY_IWMMXT"
  "wandn%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wandn")]
)

(define_insn "*iwmmxt_arm_movdi"
  [(set (match_operand:DI 0 "nonimmediate_di_operand" "=r, r, r, r, m,y,y,r, y,Uy,*w, r,*w,*w, *Uv")
        (match_operand:DI 1 "di_operand"              "rDa,Db,Dc,mi,r,y,r,y,Uy,y,  r,*w,*w,*Uvi,*w"))]
  "TARGET_REALLY_IWMMXT
   && (   register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))"
  "*
  switch (which_alternative)
    {
    case 0:
    case 1:
    case 2:
      return \"#\";
    case 3: case 4:
      return output_move_double (operands, true, NULL);
    case 5:
      return \"wmov%?\\t%0,%1\";
    case 6:
      return \"tmcrr%?\\t%0,%Q1,%R1\";
    case 7:
      return \"tmrrc%?\\t%Q0,%R0,%1\";
    case 8:
      return \"wldrd%?\\t%0,%1\";
    case 9:
      return \"wstrd%?\\t%1,%0\";
    case 10:
      return \"fmdrr%?\\t%P0, %Q1, %R1\\t%@ int\";
    case 11:
      return \"fmrrd%?\\t%Q0, %R0, %P1\\t%@ int\";
    case 12:
      if (TARGET_VFP_SINGLE)
	return \"fcpys%?\\t%0, %1\\t%@ int\;fcpys%?\\t%p0, %p1\\t%@ int\";
      else
	return \"fcpyd%?\\t%P0, %P1\\t%@ int\";
    case 13: case 14:
      return output_move_vfp (operands);
    default:
      gcc_unreachable ();
    }
  "
  [(set (attr "length") (cond [(eq_attr "alternative" "0,3,4") (const_int 8)
                              (eq_attr "alternative" "1") (const_int 12)
                              (eq_attr "alternative" "2") (const_int 16)
                              (eq_attr "alternative" "12")
                               (if_then_else
                                 (eq (symbol_ref "TARGET_VFP_SINGLE") (const_int 1))
                                 (const_int 8)
                                 (const_int 4))]
                              (const_int 4)))
   (set_attr "type" "*,*,*,load_8,store_8,*,*,*,*,*,f_mcrr,f_mrrc,\
                     ffarithd,f_loadd,f_stored")
   (set_attr "arm_pool_range" "*,*,*,1020,*,*,*,*,*,*,*,*,*,1020,*")
   (set_attr "arm_neg_pool_range" "*,*,*,1008,*,*,*,*,*,*,*,*,*,1008,*")]
)

(define_insn "*iwmmxt_movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=rk,r,r,r,rk, m,z,r,?z,?Uy,*t, r,*t,*t  ,*Uv")
	(match_operand:SI 1 "general_operand"      " rk,I,K,j,mi,rk,r,z,Uy,  z, r,*t,*t,*Uvi, *t"))]
  "TARGET_REALLY_IWMMXT
   && (   register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode))"
  "*
   switch (which_alternative)
     {
     case 0: return \"mov\\t%0, %1\";
     case 1: return \"mov\\t%0, %1\";
     case 2: return \"mvn\\t%0, #%B1\";
     case 3: return \"movw\\t%0, %1\";
     case 4: return \"ldr\\t%0, %1\";
     case 5: return \"str\\t%1, %0\";
     case 6: return \"tmcr\\t%0, %1\";
     case 7: return \"tmrc\\t%0, %1\";
     case 8: return arm_output_load_gr (operands);
     case 9: return \"wstrw\\t%1, %0\";
     case 10:return \"fmsr\\t%0, %1\";
     case 11:return \"fmrs\\t%0, %1\";
     case 12:return \"fcpys\\t%0, %1\\t%@ int\";
     case 13: case 14:
       return output_move_vfp (operands);
     default:
       gcc_unreachable ();
     }"
  [(set_attr "type"           "*,*,*,*,load_4,store_4,*,*,*,*,f_mcr,f_mrc,\
                               fmov,f_loads,f_stores")
   (set_attr "length"         "*,*,*,*,*,        *,*,*,  16,     *,*,*,*,*,*")
   (set_attr "pool_range"     "*,*,*,*,4096,     *,*,*,1024,     *,*,*,*,1020,*")
   (set_attr "neg_pool_range" "*,*,*,*,4084,     *,*,*,   *,  1012,*,*,*,1008,*")
   ;; Note - the "predicable" attribute is not allowed to have alternatives.
   ;; Since the wSTRw wCx instruction is not predicable, we cannot support
   ;; predicating any of the alternatives in this template.  Instead,
   ;; we do the predication ourselves, in cond_iwmmxt_movsi_insn.
   (set_attr "predicable"     "no")
   ;; Also - we have to pretend that these insns clobber the condition code
   ;; bits as otherwise arm_final_prescan_insn() will try to conditionalize
   ;; them.
   (set_attr "conds" "clob")]
)

;; Because iwmmxt_movsi_insn is not predicable, we provide the
;; cond_exec version explicitly, with appropriate constraints.

(define_insn "*cond_iwmmxt_movsi_insn"
  [(cond_exec
     (match_operator 2 "arm_comparison_operator"
      [(match_operand 3 "cc_register" "")
      (const_int 0)])
     (set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r, m,z,r")
	  (match_operand:SI 1 "general_operand"      "rI,K,mi,r,r,z")))]
  "TARGET_REALLY_IWMMXT
   && (   register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode))"
  "*
   switch (which_alternative)
   {
   case 0: return \"mov%?\\t%0, %1\";
   case 1: return \"mvn%?\\t%0, #%B1\";
   case 2: return \"ldr%?\\t%0, %1\";
   case 3: return \"str%?\\t%1, %0\";
   case 4: return \"tmcr%?\\t%0, %1\";
   default: return \"tmrc%?\\t%0, %1\";
  }"
  [(set_attr "type"           "*,*,load_4,store_4,*,*")
   (set_attr "pool_range"     "*,*,4096,     *,*,*")
   (set_attr "neg_pool_range" "*,*,4084,     *,*,*")]
)

(define_insn "mov<mode>_internal"
  [(set (match_operand:VMMX 0 "nonimmediate_operand" "=y,m,y,?r,?y,?r,?r,?m")
	(match_operand:VMMX 1 "general_operand"       "y,y,mi,y,r,r,mi,r"))]
  "TARGET_REALLY_IWMMXT"
  "*
   switch (which_alternative)
   {
   case 0: return \"wmov%?\\t%0, %1\";
   case 1: return \"wstrd%?\\t%1, %0\";
   case 2: return \"wldrd%?\\t%0, %1\";
   case 3: return \"tmrrc%?\\t%Q0, %R0, %1\";
   case 4: return \"tmcrr%?\\t%0, %Q1, %R1\";
   case 5: return \"#\";
   default: return output_move_double (operands, true, NULL);
   }"
  [(set_attr "predicable" "yes")
   (set_attr "length"         "4,     4,   4,4,4,8,   8,8")
   (set_attr "type"           "wmmx_wmov,wmmx_wstr,wmmx_wldr,wmmx_tmrrc,wmmx_tmcrr,*,load_4,store_4")
   (set_attr "pool_range"     "*,     *, 256,*,*,*, 256,*")
   (set_attr "neg_pool_range" "*,     *, 244,*,*,*, 244,*")]
)

(define_expand "iwmmxt_setwcgr0"
  [(set (reg:SI WCGR0)
	(match_operand:SI 0 "register_operand"))]
  "TARGET_REALLY_IWMMXT"
  {}
)

(define_expand "iwmmxt_setwcgr1"
  [(set (reg:SI WCGR1)
	(match_operand:SI 0 "register_operand"))]
  "TARGET_REALLY_IWMMXT"
  {}
)

(define_expand "iwmmxt_setwcgr2"
  [(set (reg:SI WCGR2)
	(match_operand:SI 0 "register_operand"))]
  "TARGET_REALLY_IWMMXT"
  {}
)

(define_expand "iwmmxt_setwcgr3"
  [(set (reg:SI WCGR3)
	(match_operand:SI 0 "register_operand"))]
  "TARGET_REALLY_IWMMXT"
  {}
)

(define_expand "iwmmxt_getwcgr0"
  [(set (match_operand:SI 0 "register_operand")
        (reg:SI WCGR0))]
  "TARGET_REALLY_IWMMXT"
  {}
)

(define_expand "iwmmxt_getwcgr1"
  [(set (match_operand:SI 0 "register_operand")
        (reg:SI WCGR1))]
  "TARGET_REALLY_IWMMXT"
  {}
)

(define_expand "iwmmxt_getwcgr2"
  [(set (match_operand:SI 0 "register_operand")
        (reg:SI WCGR2))]
  "TARGET_REALLY_IWMMXT"
  {}
)

(define_expand "iwmmxt_getwcgr3"
  [(set (match_operand:SI 0 "register_operand")
        (reg:SI WCGR3))]
  "TARGET_REALLY_IWMMXT"
  {}
)

(define_insn "*and<mode>3_iwmmxt"
  [(set (match_operand:VMMX           0 "register_operand" "=y")
        (and:VMMX (match_operand:VMMX 1 "register_operand"  "y")
	          (match_operand:VMMX 2 "register_operand"  "y")))]
  "TARGET_REALLY_IWMMXT"
  "wand\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wand")]
)

(define_insn "*ior<mode>3_iwmmxt"
  [(set (match_operand:VMMX           0 "register_operand" "=y")
        (ior:VMMX (match_operand:VMMX 1 "register_operand"  "y")
	          (match_operand:VMMX 2 "register_operand"  "y")))]
  "TARGET_REALLY_IWMMXT"
  "wor\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wor")]
)

(define_insn "*xor<mode>3_iwmmxt"
  [(set (match_operand:VMMX           0 "register_operand" "=y")
        (xor:VMMX (match_operand:VMMX 1 "register_operand"  "y")
	          (match_operand:VMMX 2 "register_operand"  "y")))]
  "TARGET_REALLY_IWMMXT"
  "wxor\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wxor")]
)


;; Vector add/subtract

(define_insn "*add<mode>3_iwmmxt"
  [(set (match_operand:VMMX            0 "register_operand" "=y")
        (plus:VMMX (match_operand:VMMX 1 "register_operand" "y")
	           (match_operand:VMMX 2 "register_operand" "y")))]
  "TARGET_REALLY_IWMMXT"
  "wadd<MMX_char>%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wadd")]
)

(define_insn "ssaddv8qi3"
  [(set (match_operand:V8QI               0 "register_operand" "=y")
        (ss_plus:V8QI (match_operand:V8QI 1 "register_operand"  "y")
		      (match_operand:V8QI 2 "register_operand"  "y")))]
  "TARGET_REALLY_IWMMXT"
  "waddbss%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wadd")]
)

(define_insn "ssaddv4hi3"
  [(set (match_operand:V4HI               0 "register_operand" "=y")
        (ss_plus:V4HI (match_operand:V4HI 1 "register_operand"  "y")
		      (match_operand:V4HI 2 "register_operand"  "y")))]
  "TARGET_REALLY_IWMMXT"
  "waddhss%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wadd")]
)

(define_insn "ssaddv2si3"
  [(set (match_operand:V2SI               0 "register_operand" "=y")
        (ss_plus:V2SI (match_operand:V2SI 1 "register_operand"  "y")
		      (match_operand:V2SI 2 "register_operand"  "y")))]
  "TARGET_REALLY_IWMMXT"
  "waddwss%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wadd")]
)

(define_insn "usaddv8qi3"
  [(set (match_operand:V8QI               0 "register_operand" "=y")
        (us_plus:V8QI (match_operand:V8QI 1 "register_operand"  "y")
		      (match_operand:V8QI 2 "register_operand"  "y")))]
  "TARGET_REALLY_IWMMXT"
  "waddbus%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wadd")]
)

(define_insn "usaddv4hi3"
  [(set (match_operand:V4HI               0 "register_operand" "=y")
        (us_plus:V4HI (match_operand:V4HI 1 "register_operand"  "y")
		      (match_operand:V4HI 2 "register_operand"  "y")))]
  "TARGET_REALLY_IWMMXT"
  "waddhus%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wadd")]
)

(define_insn "usaddv2si3"
  [(set (match_operand:V2SI               0 "register_operand" "=y")
        (us_plus:V2SI (match_operand:V2SI 1 "register_operand"  "y")
		      (match_operand:V2SI 2 "register_operand"  "y")))]
  "TARGET_REALLY_IWMMXT"
  "waddwus%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wadd")]
)

(define_insn "*sub<mode>3_iwmmxt"
  [(set (match_operand:VMMX             0 "register_operand" "=y")
        (minus:VMMX (match_operand:VMMX 1 "register_operand"  "y")
		    (match_operand:VMMX 2 "register_operand"  "y")))]
  "TARGET_REALLY_IWMMXT"
  "wsub<MMX_char>%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wsub")]
)

(define_insn "sssubv8qi3"
  [(set (match_operand:V8QI                0 "register_operand" "=y")
        (ss_minus:V8QI (match_operand:V8QI 1 "register_operand"  "y")
		       (match_operand:V8QI 2 "register_operand"  "y")))]
  "TARGET_REALLY_IWMMXT"
  "wsubbss%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wsub")]
)

(define_insn "sssubv4hi3"
  [(set (match_operand:V4HI                0 "register_operand" "=y")
        (ss_minus:V4HI (match_operand:V4HI 1 "register_operand" "y")
		       (match_operand:V4HI 2 "register_operand" "y")))]
  "TARGET_REALLY_IWMMXT"
  "wsubhss%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wsub")]
)

(define_insn "sssubv2si3"
  [(set (match_operand:V2SI                0 "register_operand" "=y")
        (ss_minus:V2SI (match_operand:V2SI 1 "register_operand" "y")
		       (match_operand:V2SI 2 "register_operand" "y")))]
  "TARGET_REALLY_IWMMXT"
  "wsubwss%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wsub")]
)

(define_insn "ussubv8qi3"
  [(set (match_operand:V8QI                0 "register_operand" "=y")
        (us_minus:V8QI (match_operand:V8QI 1 "register_operand" "y")
		       (match_operand:V8QI 2 "register_operand" "y")))]
  "TARGET_REALLY_IWMMXT"
  "wsubbus%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wsub")]
)

(define_insn "ussubv4hi3"
  [(set (match_operand:V4HI                0 "register_operand" "=y")
        (us_minus:V4HI (match_operand:V4HI 1 "register_operand" "y")
		       (match_operand:V4HI 2 "register_operand" "y")))]
  "TARGET_REALLY_IWMMXT"
  "wsubhus%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wsub")]
)

(define_insn "ussubv2si3"
  [(set (match_operand:V2SI                0 "register_operand" "=y")
        (us_minus:V2SI (match_operand:V2SI 1 "register_operand" "y")
		       (match_operand:V2SI 2 "register_operand" "y")))]
  "TARGET_REALLY_IWMMXT"
  "wsubwus%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wsub")]
)

(define_insn "*mulv4hi3_iwmmxt"
  [(set (match_operand:V4HI            0 "register_operand" "=y")
        (mult:V4HI (match_operand:V4HI 1 "register_operand" "y")
		   (match_operand:V4HI 2 "register_operand" "y")))]
  "TARGET_REALLY_IWMMXT"
  "wmulul%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmul")]
)

(define_insn "smulv4hi3_highpart"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	  (truncate:V4HI
	    (lshiftrt:V4SI
	      (mult:V4SI (sign_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
	                 (sign_extend:V4SI (match_operand:V4HI 2 "register_operand" "y")))
	      (const_int 16))))]
  "TARGET_REALLY_IWMMXT"
  "wmulsm%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmul")]
)

(define_insn "umulv4hi3_highpart"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	  (truncate:V4HI
	    (lshiftrt:V4SI
	      (mult:V4SI (zero_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
	                 (zero_extend:V4SI (match_operand:V4HI 2 "register_operand" "y")))
	      (const_int 16))))]
  "TARGET_REALLY_IWMMXT"
  "wmulum%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmul")]
)

(define_insn "iwmmxt_wmacs"
  [(set (match_operand:DI               0 "register_operand" "=y")
	(unspec:DI [(match_operand:DI   1 "register_operand" "0")
	            (match_operand:V4HI 2 "register_operand" "y")
	            (match_operand:V4HI 3 "register_operand" "y")] UNSPEC_WMACS))]
  "TARGET_REALLY_IWMMXT"
  "wmacs%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmac")]
)

(define_insn "iwmmxt_wmacsz"
  [(set (match_operand:DI               0 "register_operand" "=y")
	(unspec:DI [(match_operand:V4HI 1 "register_operand" "y")
	            (match_operand:V4HI 2 "register_operand" "y")] UNSPEC_WMACSZ))]
  "TARGET_REALLY_IWMMXT"
  "wmacsz%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmac")]
)

(define_insn "iwmmxt_wmacu"
  [(set (match_operand:DI               0 "register_operand" "=y")
	(unspec:DI [(match_operand:DI   1 "register_operand" "0")
	            (match_operand:V4HI 2 "register_operand" "y")
	            (match_operand:V4HI 3 "register_operand" "y")] UNSPEC_WMACU))]
  "TARGET_REALLY_IWMMXT"
  "wmacu%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmac")]
)

(define_insn "iwmmxt_wmacuz"
  [(set (match_operand:DI               0 "register_operand" "=y")
	(unspec:DI [(match_operand:V4HI 1 "register_operand" "y")
	            (match_operand:V4HI 2 "register_operand" "y")] UNSPEC_WMACUZ))]
  "TARGET_REALLY_IWMMXT"
  "wmacuz%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmac")]
)

;; Same as xordi3, but don't show input operands so that we don't think
;; they are live.
(define_insn "iwmmxt_clrdi"
  [(set (match_operand:DI 0 "register_operand" "=y")
        (unspec:DI [(const_int 0)] UNSPEC_CLRDI))]
  "TARGET_REALLY_IWMMXT"
  "wxor%?\\t%0, %0, %0"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wxor")]
)

;; Seems like cse likes to generate these, so we have to support them.

(define_insn "iwmmxt_clrv8qi"
  [(set (match_operand:V8QI 0 "s_register_operand" "=y")
        (const_vector:V8QI [(const_int 0) (const_int 0)
			    (const_int 0) (const_int 0)
			    (const_int 0) (const_int 0)
			    (const_int 0) (const_int 0)]))]
  "TARGET_REALLY_IWMMXT"
  "wxor%?\\t%0, %0, %0"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wxor")]
)

(define_insn "iwmmxt_clrv4hi"
  [(set (match_operand:V4HI 0 "s_register_operand" "=y")
        (const_vector:V4HI [(const_int 0) (const_int 0)
			    (const_int 0) (const_int 0)]))]
  "TARGET_REALLY_IWMMXT"
  "wxor%?\\t%0, %0, %0"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wxor")]
)

(define_insn "iwmmxt_clrv2si"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
        (const_vector:V2SI [(const_int 0) (const_int 0)]))]
  "TARGET_REALLY_IWMMXT"
  "wxor%?\\t%0, %0, %0"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wxor")]
)

;; Unsigned averages/sum of absolute differences

(define_insn "iwmmxt_uavgrndv8qi3"
  [(set (match_operand:V8QI                                    0 "register_operand" "=y")
        (truncate:V8QI
	  (lshiftrt:V8HI
	    (plus:V8HI
	      (plus:V8HI (zero_extend:V8HI (match_operand:V8QI 1 "register_operand" "y"))
	                 (zero_extend:V8HI (match_operand:V8QI 2 "register_operand" "y")))
	      (const_vector:V8HI [(const_int 1)
	                          (const_int 1)
	                          (const_int 1)
	                          (const_int 1)
	                          (const_int 1)
	                          (const_int 1)
	                          (const_int 1)
	                          (const_int 1)]))
	    (const_int 1))))]
  "TARGET_REALLY_IWMMXT"
  "wavg2br%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wavg2")]
)

(define_insn "iwmmxt_uavgrndv4hi3"
  [(set (match_operand:V4HI                                    0 "register_operand" "=y")
        (truncate:V4HI
	  (lshiftrt:V4SI
            (plus:V4SI
	      (plus:V4SI (zero_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
	                 (zero_extend:V4SI (match_operand:V4HI 2 "register_operand" "y")))
	      (const_vector:V4SI [(const_int 1)
	                          (const_int 1)
	                          (const_int 1)
	                          (const_int 1)]))
	    (const_int 1))))]
  "TARGET_REALLY_IWMMXT"
  "wavg2hr%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wavg2")]
)

(define_insn "iwmmxt_uavgv8qi3"
  [(set (match_operand:V8QI                                  0 "register_operand" "=y")
        (truncate:V8QI
	  (lshiftrt:V8HI
	    (plus:V8HI (zero_extend:V8HI (match_operand:V8QI 1 "register_operand" "y"))
	               (zero_extend:V8HI (match_operand:V8QI 2 "register_operand" "y")))
	    (const_int 1))))]
  "TARGET_REALLY_IWMMXT"
  "wavg2b%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wavg2")]
)

(define_insn "iwmmxt_uavgv4hi3"
  [(set (match_operand:V4HI                                  0 "register_operand" "=y")
        (truncate:V4HI
	  (lshiftrt:V4SI
	    (plus:V4SI (zero_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
	               (zero_extend:V4SI (match_operand:V4HI 2 "register_operand" "y")))
	    (const_int 1))))]
  "TARGET_REALLY_IWMMXT"
  "wavg2h%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wavg2")]
)

;; Insert/extract/shuffle

(define_insn "iwmmxt_tinsrb"
  [(set (match_operand:V8QI                0 "register_operand" "=y")
        (vec_merge:V8QI
	  (vec_duplicate:V8QI
	    (truncate:QI (match_operand:SI 2 "nonimmediate_operand" "r")))
	  (match_operand:V8QI              1 "register_operand"     "0")
	  (match_operand:SI                3 "immediate_operand"    "i")))]
  "TARGET_REALLY_IWMMXT"
  "*
   {
     return arm_output_iwmmxt_tinsr (operands);
   }
   "
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tinsr")]
)

(define_insn "iwmmxt_tinsrh"
  [(set (match_operand:V4HI                0 "register_operand"    "=y")
        (vec_merge:V4HI
          (vec_duplicate:V4HI
            (truncate:HI (match_operand:SI 2 "nonimmediate_operand" "r")))
	  (match_operand:V4HI              1 "register_operand"     "0")
	  (match_operand:SI                3 "immediate_operand"    "i")))]
  "TARGET_REALLY_IWMMXT"
  "*
   {
     return arm_output_iwmmxt_tinsr (operands);
   }
   "
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tinsr")]
)

(define_insn "iwmmxt_tinsrw"
  [(set (match_operand:V2SI   0 "register_operand"    "=y")
        (vec_merge:V2SI
          (vec_duplicate:V2SI
            (match_operand:SI 2 "nonimmediate_operand" "r"))
          (match_operand:V2SI 1 "register_operand"     "0")
          (match_operand:SI   3 "immediate_operand"    "i")))]
  "TARGET_REALLY_IWMMXT"
  "*
   {
     return arm_output_iwmmxt_tinsr (operands);
   }
   "
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tinsr")]
)

(define_insn "iwmmxt_textrmub"
  [(set (match_operand:SI                                   0 "register_operand" "=r")
        (zero_extend:SI (vec_select:QI (match_operand:V8QI  1 "register_operand" "y")
		                       (parallel
				         [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_REALLY_IWMMXT"
  "textrmub%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_textrm")]
)

(define_insn "iwmmxt_textrmsb"
  [(set (match_operand:SI                                   0 "register_operand" "=r")
        (sign_extend:SI (vec_select:QI (match_operand:V8QI  1 "register_operand" "y")
				       (parallel
				         [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_REALLY_IWMMXT"
  "textrmsb%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_textrm")]
)

(define_insn "iwmmxt_textrmuh"
  [(set (match_operand:SI                                   0 "register_operand" "=r")
        (zero_extend:SI (vec_select:HI (match_operand:V4HI  1 "register_operand" "y")
				       (parallel
				         [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_REALLY_IWMMXT"
  "textrmuh%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_textrm")]
)

(define_insn "iwmmxt_textrmsh"
  [(set (match_operand:SI                                   0 "register_operand" "=r")
        (sign_extend:SI (vec_select:HI (match_operand:V4HI  1 "register_operand" "y")
				       (parallel
				         [(match_operand:SI 2 "immediate_operand" "i")]))))]
  "TARGET_REALLY_IWMMXT"
  "textrmsh%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_textrm")]
)

;; There are signed/unsigned variants of this instruction, but they are
;; pointless.
(define_insn "iwmmxt_textrmw"
  [(set (match_operand:SI                           0 "register_operand" "=r")
        (vec_select:SI (match_operand:V2SI          1 "register_operand" "y")
		       (parallel [(match_operand:SI 2 "immediate_operand" "i")])))]
  "TARGET_REALLY_IWMMXT"
  "textrmsw%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_textrm")]
)

(define_insn "iwmmxt_wshufh"
  [(set (match_operand:V4HI               0 "register_operand" "=y")
        (unspec:V4HI [(match_operand:V4HI 1 "register_operand" "y")
		      (match_operand:SI   2 "immediate_operand" "i")] UNSPEC_WSHUFH))]
  "TARGET_REALLY_IWMMXT"
  "wshufh%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wshufh")]
)

;; Mask-generating comparisons
;;
;; Note - you cannot use patterns like these here:
;;
;;   (set (match:<vector>) (<comparator>:<vector> (match:<vector>) (match:<vector>)))
;;
;; Because GCC will assume that the truth value (1 or 0) is installed
;; into the entire destination vector, (with the '1' going into the least
;; significant element of the vector).  This is not how these instructions
;; behave.

(define_insn "eqv8qi3"
  [(set (match_operand:V8QI                        0 "register_operand" "=y")
	(unspec_volatile:V8QI [(match_operand:V8QI 1 "register_operand"  "y")
	                       (match_operand:V8QI 2 "register_operand"  "y")]
	                      VUNSPEC_WCMP_EQ))]
  "TARGET_REALLY_IWMMXT"
  "wcmpeqb%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wcmpeq")]
)

(define_insn "eqv4hi3"
  [(set (match_operand:V4HI                        0 "register_operand" "=y")
	(unspec_volatile:V4HI [(match_operand:V4HI 1 "register_operand"  "y")
		               (match_operand:V4HI 2 "register_operand"  "y")]
	                       VUNSPEC_WCMP_EQ))]
  "TARGET_REALLY_IWMMXT"
  "wcmpeqh%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wcmpeq")]
)

(define_insn "eqv2si3"
  [(set (match_operand:V2SI    0 "register_operand" "=y")
	(unspec_volatile:V2SI
	  [(match_operand:V2SI 1 "register_operand"  "y")
	   (match_operand:V2SI 2 "register_operand"  "y")]
           VUNSPEC_WCMP_EQ))]
  "TARGET_REALLY_IWMMXT"
  "wcmpeqw%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wcmpeq")]
)

(define_insn "gtuv8qi3"
  [(set (match_operand:V8QI                        0 "register_operand" "=y")
	(unspec_volatile:V8QI [(match_operand:V8QI 1 "register_operand"  "y")
	                       (match_operand:V8QI 2 "register_operand"  "y")]
	                       VUNSPEC_WCMP_GTU))]
  "TARGET_REALLY_IWMMXT"
  "wcmpgtub%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wcmpgt")]
)

(define_insn "gtuv4hi3"
  [(set (match_operand:V4HI                        0 "register_operand" "=y")
        (unspec_volatile:V4HI [(match_operand:V4HI 1 "register_operand"  "y")
                               (match_operand:V4HI 2 "register_operand"  "y")]
                               VUNSPEC_WCMP_GTU))]
  "TARGET_REALLY_IWMMXT"
  "wcmpgtuh%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wcmpgt")]
)

(define_insn "gtuv2si3"
  [(set (match_operand:V2SI                        0 "register_operand" "=y")
	(unspec_volatile:V2SI [(match_operand:V2SI 1 "register_operand"  "y")
	                       (match_operand:V2SI 2 "register_operand"  "y")]
	                       VUNSPEC_WCMP_GTU))]
  "TARGET_REALLY_IWMMXT"
  "wcmpgtuw%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wcmpgt")]
)

(define_insn "gtv8qi3"
  [(set (match_operand:V8QI                        0 "register_operand" "=y")
	(unspec_volatile:V8QI [(match_operand:V8QI 1 "register_operand"  "y")
	                       (match_operand:V8QI 2 "register_operand"  "y")]
	                       VUNSPEC_WCMP_GT))]
  "TARGET_REALLY_IWMMXT"
  "wcmpgtsb%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wcmpgt")]
)

(define_insn "gtv4hi3"
  [(set (match_operand:V4HI                        0 "register_operand" "=y")
	(unspec_volatile:V4HI [(match_operand:V4HI 1 "register_operand"  "y")
	                       (match_operand:V4HI 2 "register_operand"  "y")]
	                       VUNSPEC_WCMP_GT))]
  "TARGET_REALLY_IWMMXT"
  "wcmpgtsh%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wcmpgt")]
)

(define_insn "gtv2si3"
  [(set (match_operand:V2SI                        0 "register_operand" "=y")
	(unspec_volatile:V2SI [(match_operand:V2SI 1 "register_operand"  "y")
	                       (match_operand:V2SI 2 "register_operand"  "y")]
	                       VUNSPEC_WCMP_GT))]
  "TARGET_REALLY_IWMMXT"
  "wcmpgtsw%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wcmpgt")]
)

;; Max/min insns

(define_insn "*smax<mode>3_iwmmxt"
  [(set (match_operand:VMMX            0 "register_operand" "=y")
        (smax:VMMX (match_operand:VMMX 1 "register_operand" "y")
		   (match_operand:VMMX 2 "register_operand" "y")))]
  "TARGET_REALLY_IWMMXT"
  "wmaxs<MMX_char>%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmax")]
)

(define_insn "*umax<mode>3_iwmmxt"
  [(set (match_operand:VMMX            0 "register_operand" "=y")
        (umax:VMMX (match_operand:VMMX 1 "register_operand" "y")
		   (match_operand:VMMX 2 "register_operand" "y")))]
  "TARGET_REALLY_IWMMXT"
  "wmaxu<MMX_char>%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmax")]
)

(define_insn "*smin<mode>3_iwmmxt"
  [(set (match_operand:VMMX            0 "register_operand" "=y")
        (smin:VMMX (match_operand:VMMX 1 "register_operand" "y")
		   (match_operand:VMMX 2 "register_operand" "y")))]
  "TARGET_REALLY_IWMMXT"
  "wmins<MMX_char>%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmin")]
)

(define_insn "*umin<mode>3_iwmmxt"
  [(set (match_operand:VMMX            0 "register_operand" "=y")
        (umin:VMMX (match_operand:VMMX 1 "register_operand" "y")
		   (match_operand:VMMX 2 "register_operand" "y")))]
  "TARGET_REALLY_IWMMXT"
  "wminu<MMX_char>%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmin")]
)

;; Pack/unpack insns.

(define_insn "iwmmxt_wpackhss"
  [(set (match_operand:V8QI                     0 "register_operand" "=y")
	(vec_concat:V8QI
	  (ss_truncate:V4QI (match_operand:V4HI 1 "register_operand" "y"))
	  (ss_truncate:V4QI (match_operand:V4HI 2 "register_operand" "y"))))]
  "TARGET_REALLY_IWMMXT"
  "wpackhss%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wpack")]
)

(define_insn "iwmmxt_wpackwss"
  [(set (match_operand:V4HI                     0 "register_operand" "=y")
        (vec_concat:V4HI
	  (ss_truncate:V2HI (match_operand:V2SI 1 "register_operand" "y"))
	  (ss_truncate:V2HI (match_operand:V2SI 2 "register_operand" "y"))))]
  "TARGET_REALLY_IWMMXT"
  "wpackwss%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wpack")]
)

(define_insn "iwmmxt_wpackdss"
  [(set (match_operand:V2SI                 0 "register_operand" "=y")
	(vec_concat:V2SI
	  (ss_truncate:SI (match_operand:DI 1 "register_operand" "y"))
	  (ss_truncate:SI (match_operand:DI 2 "register_operand" "y"))))]
  "TARGET_REALLY_IWMMXT"
  "wpackdss%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wpack")]
)

(define_insn "iwmmxt_wpackhus"
  [(set (match_operand:V8QI                     0 "register_operand" "=y")
	(vec_concat:V8QI
	  (us_truncate:V4QI (match_operand:V4HI 1 "register_operand" "y"))
	  (us_truncate:V4QI (match_operand:V4HI 2 "register_operand" "y"))))]
  "TARGET_REALLY_IWMMXT"
  "wpackhus%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wpack")]
)

(define_insn "iwmmxt_wpackwus"
  [(set (match_operand:V4HI                     0 "register_operand" "=y")
	(vec_concat:V4HI
	  (us_truncate:V2HI (match_operand:V2SI 1 "register_operand" "y"))
	  (us_truncate:V2HI (match_operand:V2SI 2 "register_operand" "y"))))]
  "TARGET_REALLY_IWMMXT"
  "wpackwus%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wpack")]
)

(define_insn "iwmmxt_wpackdus"
  [(set (match_operand:V2SI                 0 "register_operand" "=y")
	(vec_concat:V2SI
	  (us_truncate:SI (match_operand:DI 1 "register_operand" "y"))
	  (us_truncate:SI (match_operand:DI 2 "register_operand" "y"))))]
  "TARGET_REALLY_IWMMXT"
  "wpackdus%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wpack")]
)

(define_insn "iwmmxt_wunpckihb"
  [(set (match_operand:V8QI                                      0 "register_operand" "=y")
	(vec_merge:V8QI
	  (vec_select:V8QI (match_operand:V8QI 1 "register_operand" "y")
		           (parallel [(const_int 4)
			              (const_int 0)
			              (const_int 5)
			              (const_int 1)
			              (const_int 6)
			              (const_int 2)
			              (const_int 7)
			              (const_int 3)]))
          (vec_select:V8QI (match_operand:V8QI 2 "register_operand" "y")
			   (parallel [(const_int 0)
			              (const_int 4)
			              (const_int 1)
			              (const_int 5)
			              (const_int 2)
			              (const_int 6)
			              (const_int 3)
			              (const_int 7)]))
          (const_int 85)))]
  "TARGET_REALLY_IWMMXT"
  "wunpckihb%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckih")]
)

(define_insn "iwmmxt_wunpckihh"
  [(set (match_operand:V4HI                                      0 "register_operand" "=y")
	(vec_merge:V4HI
	  (vec_select:V4HI (match_operand:V4HI 1 "register_operand" "y")
		           (parallel [(const_int 2)
			              (const_int 0)
			              (const_int 3)
			              (const_int 1)]))
	  (vec_select:V4HI (match_operand:V4HI 2 "register_operand" "y")
		           (parallel [(const_int 0)
			              (const_int 2)
			              (const_int 1)
			              (const_int 3)]))
          (const_int 5)))]
  "TARGET_REALLY_IWMMXT"
  "wunpckihh%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckih")]
)

(define_insn "iwmmxt_wunpckihw"
  [(set (match_operand:V2SI                    0 "register_operand" "=y")
	(vec_merge:V2SI
	  (vec_select:V2SI (match_operand:V2SI 1 "register_operand" "y")
		           (parallel [(const_int 1)
		                      (const_int 0)]))
          (vec_select:V2SI (match_operand:V2SI 2 "register_operand" "y")
		           (parallel [(const_int 0)
			              (const_int 1)]))
          (const_int 1)))]
  "TARGET_REALLY_IWMMXT"
  "wunpckihw%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckih")]
)

(define_insn "iwmmxt_wunpckilb"
  [(set (match_operand:V8QI                                      0 "register_operand" "=y")
	(vec_merge:V8QI
	  (vec_select:V8QI (match_operand:V8QI 1 "register_operand" "y")
		           (parallel [(const_int 0)
			              (const_int 4)
			              (const_int 1)
			              (const_int 5)
		                      (const_int 2)
				      (const_int 6)
				      (const_int 3)
				      (const_int 7)]))
	  (vec_select:V8QI (match_operand:V8QI 2 "register_operand" "y")
		           (parallel [(const_int 4)
			              (const_int 0)
			              (const_int 5)
			              (const_int 1)
			              (const_int 6)
			              (const_int 2)
			              (const_int 7)
			              (const_int 3)]))
	  (const_int 85)))]
  "TARGET_REALLY_IWMMXT"
  "wunpckilb%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckil")]
)

(define_insn "iwmmxt_wunpckilh"
  [(set (match_operand:V4HI                                      0 "register_operand" "=y")
	(vec_merge:V4HI
	  (vec_select:V4HI (match_operand:V4HI 1 "register_operand" "y")
		           (parallel [(const_int 0)
			              (const_int 2)
			              (const_int 1)
			              (const_int 3)]))
	  (vec_select:V4HI (match_operand:V4HI 2 "register_operand" "y")
			   (parallel [(const_int 2)
			              (const_int 0)
			              (const_int 3)
			              (const_int 1)]))
	  (const_int 5)))]
  "TARGET_REALLY_IWMMXT"
  "wunpckilh%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckil")]
)

(define_insn "iwmmxt_wunpckilw"
  [(set (match_operand:V2SI                    0 "register_operand" "=y")
	(vec_merge:V2SI
	  (vec_select:V2SI (match_operand:V2SI 1 "register_operand" "y")
		           (parallel [(const_int 0)
				      (const_int 1)]))
	  (vec_select:V2SI (match_operand:V2SI 2 "register_operand" "y")
		           (parallel [(const_int 1)
			              (const_int 0)]))
	  (const_int 1)))]
  "TARGET_REALLY_IWMMXT"
  "wunpckilw%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckil")]
)

(define_insn "iwmmxt_wunpckehub"
  [(set (match_operand:V4HI                     0 "register_operand" "=y")
	(vec_select:V4HI
	  (zero_extend:V8HI (match_operand:V8QI 1 "register_operand" "y"))
	  (parallel [(const_int 4) (const_int 5)
	             (const_int 6) (const_int 7)])))]
  "TARGET_REALLY_IWMMXT"
  "wunpckehub%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckeh")]
)

(define_insn "iwmmxt_wunpckehuh"
  [(set (match_operand:V2SI                     0 "register_operand" "=y")
	(vec_select:V2SI
	  (zero_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
	  (parallel [(const_int 2) (const_int 3)])))]
  "TARGET_REALLY_IWMMXT"
  "wunpckehuh%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckeh")]
)

(define_insn "iwmmxt_wunpckehuw"
  [(set (match_operand:DI                       0 "register_operand" "=y")
	(vec_select:DI
	  (zero_extend:V2DI (match_operand:V2SI 1 "register_operand" "y"))
	  (parallel [(const_int 1)])))]
  "TARGET_REALLY_IWMMXT"
  "wunpckehuw%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckeh")]
)

(define_insn "iwmmxt_wunpckehsb"
  [(set (match_operand:V4HI                     0 "register_operand" "=y")
        (vec_select:V4HI
	  (sign_extend:V8HI (match_operand:V8QI 1 "register_operand" "y"))
	  (parallel [(const_int 4) (const_int 5)
	             (const_int 6) (const_int 7)])))]
  "TARGET_REALLY_IWMMXT"
  "wunpckehsb%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckeh")]
)

(define_insn "iwmmxt_wunpckehsh"
  [(set (match_operand:V2SI                     0 "register_operand" "=y")
	(vec_select:V2SI
	  (sign_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
	  (parallel [(const_int 2) (const_int 3)])))]
  "TARGET_REALLY_IWMMXT"
  "wunpckehsh%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckeh")]
)

(define_insn "iwmmxt_wunpckehsw"
  [(set (match_operand:DI                       0 "register_operand" "=y")
	(vec_select:DI
	  (sign_extend:V2DI (match_operand:V2SI 1 "register_operand" "y"))
	  (parallel [(const_int 1)])))]
  "TARGET_REALLY_IWMMXT"
  "wunpckehsw%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckeh")]
)

(define_insn "iwmmxt_wunpckelub"
  [(set (match_operand:V4HI                     0 "register_operand" "=y")
	(vec_select:V4HI
	  (zero_extend:V8HI (match_operand:V8QI 1 "register_operand" "y"))
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)])))]
  "TARGET_REALLY_IWMMXT"
  "wunpckelub%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckel")]
)

(define_insn "iwmmxt_wunpckeluh"
  [(set (match_operand:V2SI                     0 "register_operand" "=y")
	(vec_select:V2SI
	  (zero_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
	  (parallel [(const_int 0) (const_int 1)])))]
  "TARGET_REALLY_IWMMXT"
  "wunpckeluh%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckel")]
)

(define_insn "iwmmxt_wunpckeluw"
  [(set (match_operand:DI                       0 "register_operand" "=y")
	(vec_select:DI
	  (zero_extend:V2DI (match_operand:V2SI 1 "register_operand" "y"))
	  (parallel [(const_int 0)])))]
  "TARGET_REALLY_IWMMXT"
  "wunpckeluw%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckel")]
)

(define_insn "iwmmxt_wunpckelsb"
  [(set (match_operand:V4HI                     0 "register_operand" "=y")
	(vec_select:V4HI
	  (sign_extend:V8HI (match_operand:V8QI 1 "register_operand" "y"))
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)])))]
  "TARGET_REALLY_IWMMXT"
  "wunpckelsb%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckel")]
)

(define_insn "iwmmxt_wunpckelsh"
  [(set (match_operand:V2SI                     0 "register_operand" "=y")
	(vec_select:V2SI
	  (sign_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
	  (parallel [(const_int 0) (const_int 1)])))]
  "TARGET_REALLY_IWMMXT"
  "wunpckelsh%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckel")]
)

(define_insn "iwmmxt_wunpckelsw"
  [(set (match_operand:DI                       0 "register_operand" "=y")
        (vec_select:DI
	  (sign_extend:V2DI (match_operand:V2SI 1 "register_operand" "y"))
	  (parallel [(const_int 0)])))]
  "TARGET_REALLY_IWMMXT"
  "wunpckelsw%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wunpckel")]
)

;; Shifts

(define_insn "ror<mode>3"
  [(set (match_operand:VSHFT                 0 "register_operand" "=y,y")
        (rotatert:VSHFT (match_operand:VSHFT 1 "register_operand" "y,y")
		        (match_operand:SI    2 "imm_or_reg_operand" "z,i")))]
  "TARGET_REALLY_IWMMXT"
  "*
  switch  (which_alternative)
    {
    case 0:
      return \"wror<MMX_char>g%?\\t%0, %1, %2\";
    case 1:
      return arm_output_iwmmxt_shift_immediate (\"wror<MMX_char>\", operands, true);
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "predicable" "yes")
   (set_attr "arch" "*, iwmmxt2")
   (set_attr "type" "wmmx_wror, wmmx_wror")]
)

(define_insn "ashr<mode>3_iwmmxt"
  [(set (match_operand:VSHFT                 0 "register_operand" "=y,y")
        (ashiftrt:VSHFT (match_operand:VSHFT 1 "register_operand" "y,y")
			(match_operand:SI    2 "imm_or_reg_operand" "z,i")))]
  "TARGET_REALLY_IWMMXT"
  "*
  switch  (which_alternative)
    {
    case 0:
      return \"wsra<MMX_char>g%?\\t%0, %1, %2\";
    case 1:
      return arm_output_iwmmxt_shift_immediate (\"wsra<MMX_char>\", operands, true);
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "predicable" "yes")
   (set_attr "arch" "*, iwmmxt2")
   (set_attr "type" "wmmx_wsra, wmmx_wsra")]
)

(define_insn "lshr<mode>3_iwmmxt"
  [(set (match_operand:VSHFT                 0 "register_operand" "=y,y")
        (lshiftrt:VSHFT (match_operand:VSHFT 1 "register_operand" "y,y")
			(match_operand:SI    2 "imm_or_reg_operand" "z,i")))]
  "TARGET_REALLY_IWMMXT"
  "*
  switch  (which_alternative)
    {
    case 0:
      return \"wsrl<MMX_char>g%?\\t%0, %1, %2\";
    case 1:
      return arm_output_iwmmxt_shift_immediate (\"wsrl<MMX_char>\", operands, false);
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "predicable" "yes")
   (set_attr "arch" "*, iwmmxt2")
   (set_attr "type" "wmmx_wsrl, wmmx_wsrl")]
)

(define_insn "ashl<mode>3_iwmmxt"
  [(set (match_operand:VSHFT               0 "register_operand" "=y,y")
        (ashift:VSHFT (match_operand:VSHFT 1 "register_operand" "y,y")
		      (match_operand:SI    2 "imm_or_reg_operand" "z,i")))]
  "TARGET_REALLY_IWMMXT"
  "*
  switch  (which_alternative)
    {
    case 0:
      return \"wsll<MMX_char>g%?\\t%0, %1, %2\";
    case 1:
      return arm_output_iwmmxt_shift_immediate (\"wsll<MMX_char>\", operands, false);
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "predicable" "yes")
   (set_attr "arch" "*, iwmmxt2")
   (set_attr "type" "wmmx_wsll, wmmx_wsll")]
)

(define_insn "ror<mode>3_di"
  [(set (match_operand:VSHFT                 0 "register_operand" "=y,y")
        (rotatert:VSHFT (match_operand:VSHFT 1 "register_operand" "y,y")
		        (match_operand:DI    2 "imm_or_reg_operand" "y,i")))]
  "TARGET_REALLY_IWMMXT"
  "*
  switch (which_alternative)
    {
    case 0:
      return \"wror<MMX_char>%?\\t%0, %1, %2\";
    case 1:
      return arm_output_iwmmxt_shift_immediate (\"wror<MMX_char>\", operands, true);
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "predicable" "yes")
   (set_attr "arch" "*, iwmmxt2")
   (set_attr "type" "wmmx_wror, wmmx_wror")]
)

(define_insn "ashr<mode>3_di"
  [(set (match_operand:VSHFT                 0 "register_operand" "=y,y")
        (ashiftrt:VSHFT (match_operand:VSHFT 1 "register_operand" "y,y")
		        (match_operand:DI    2 "imm_or_reg_operand" "y,i")))]
  "TARGET_REALLY_IWMMXT"
  "*
  switch (which_alternative)
    {
    case 0:
      return \"wsra<MMX_char>%?\\t%0, %1, %2\";
    case 1:
      return arm_output_iwmmxt_shift_immediate (\"wsra<MMX_char>\", operands, true);
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "predicable" "yes")
   (set_attr "arch" "*, iwmmxt2")
   (set_attr "type" "wmmx_wsra, wmmx_wsra")]
)

(define_insn "lshr<mode>3_di"
  [(set (match_operand:VSHFT                 0 "register_operand" "=y,y")
        (lshiftrt:VSHFT (match_operand:VSHFT 1 "register_operand" "y,y")
		        (match_operand:DI    2 "register_operand" "y,i")))]
  "TARGET_REALLY_IWMMXT"
  "*
  switch (which_alternative)
    {
    case 0:
      return \"wsrl<MMX_char>%?\\t%0, %1, %2\";
    case 1:
      return arm_output_iwmmxt_shift_immediate (\"wsrl<MMX_char>\", operands, false);
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "predicable" "yes")
   (set_attr "arch" "*, iwmmxt2")
   (set_attr "type" "wmmx_wsrl, wmmx_wsrl")]
)

(define_insn "ashl<mode>3_di"
  [(set (match_operand:VSHFT               0 "register_operand" "=y,y")
        (ashift:VSHFT (match_operand:VSHFT 1 "register_operand" "y,y")
		      (match_operand:DI    2 "imm_or_reg_operand" "y,i")))]
  "TARGET_REALLY_IWMMXT"
  "*
  switch (which_alternative)
    {
    case 0:
      return \"wsll<MMX_char>%?\\t%0, %1, %2\";
    case 1:
      return arm_output_iwmmxt_shift_immediate (\"wsll<MMX_char>\", operands, false);
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "predicable" "yes")
   (set_attr "arch" "*, iwmmxt2")
   (set_attr "type" "wmmx_wsll, wmmx_wsll")]
)

(define_insn "iwmmxt_wmadds"
  [(set (match_operand:V2SI                                        0 "register_operand" "=y")
	(plus:V2SI
	  (mult:V2SI
	    (vec_select:V2SI (sign_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
	                     (parallel [(const_int 1) (const_int 3)]))
	    (vec_select:V2SI (sign_extend:V4SI (match_operand:V4HI 2 "register_operand" "y"))
	                     (parallel [(const_int 1) (const_int 3)])))
	  (mult:V2SI
	    (vec_select:V2SI (sign_extend:V4SI (match_dup 1))
	                     (parallel [(const_int 0) (const_int 2)]))
	    (vec_select:V2SI (sign_extend:V4SI (match_dup 2))
	                     (parallel [(const_int 0) (const_int 2)])))))]
  "TARGET_REALLY_IWMMXT"
  "wmadds%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmadd")]
)

(define_insn "iwmmxt_wmaddu"
  [(set (match_operand:V2SI               0 "register_operand" "=y")
	(plus:V2SI
	  (mult:V2SI
	    (vec_select:V2SI (zero_extend:V4SI (match_operand:V4HI 1 "register_operand" "y"))
	                     (parallel [(const_int 1) (const_int 3)]))
	    (vec_select:V2SI (zero_extend:V4SI (match_operand:V4HI 2 "register_operand" "y"))
	                     (parallel [(const_int 1) (const_int 3)])))
	  (mult:V2SI
	    (vec_select:V2SI (zero_extend:V4SI (match_dup 1))
	                     (parallel [(const_int 0) (const_int 2)]))
	    (vec_select:V2SI (zero_extend:V4SI (match_dup 2))
	                     (parallel [(const_int 0) (const_int 2)])))))]
  "TARGET_REALLY_IWMMXT"
  "wmaddu%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wmadd")]
)

(define_insn "iwmmxt_tmia"
  [(set (match_operand:DI                     0 "register_operand" "=y")
	(plus:DI (match_operand:DI            1 "register_operand" "0")
		 (mult:DI (sign_extend:DI
			    (match_operand:SI 2 "register_operand" "r"))
			  (sign_extend:DI
			    (match_operand:SI 3 "register_operand" "r")))))]
  "TARGET_REALLY_IWMMXT"
  "tmia%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tmia")]
)

(define_insn "iwmmxt_tmiaph"
  [(set (match_operand:DI                                    0 "register_operand" "=y")
	(plus:DI (match_operand:DI                           1 "register_operand" "0")
		 (plus:DI
		   (mult:DI (sign_extend:DI
			      (truncate:HI (match_operand:SI 2 "register_operand" "r")))
			    (sign_extend:DI
			      (truncate:HI (match_operand:SI 3 "register_operand" "r"))))
		   (mult:DI (sign_extend:DI
			      (truncate:HI (ashiftrt:SI (match_dup 2) (const_int 16))))
			    (sign_extend:DI
			      (truncate:HI (ashiftrt:SI (match_dup 3) (const_int 16))))))))]
  "TARGET_REALLY_IWMMXT"
  "tmiaph%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tmiaph")]
)

(define_insn "iwmmxt_tmiabb"
  [(set (match_operand:DI                                  0 "register_operand" "=y")
	(plus:DI (match_operand:DI                         1 "register_operand" "0")
		 (mult:DI (sign_extend:DI
			    (truncate:HI (match_operand:SI 2 "register_operand" "r")))
			  (sign_extend:DI
			    (truncate:HI (match_operand:SI 3 "register_operand" "r"))))))]
  "TARGET_REALLY_IWMMXT"
  "tmiabb%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tmiaxy")]
)

(define_insn "iwmmxt_tmiatb"
  [(set (match_operand:DI                         0 "register_operand" "=y")
	(plus:DI (match_operand:DI                1 "register_operand" "0")
		 (mult:DI (sign_extend:DI
			    (truncate:HI
			      (ashiftrt:SI
				(match_operand:SI 2 "register_operand" "r")
				(const_int 16))))
			  (sign_extend:DI
			    (truncate:HI
			      (match_operand:SI   3 "register_operand" "r"))))))]
  "TARGET_REALLY_IWMMXT"
  "tmiatb%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tmiaxy")]
)

(define_insn "iwmmxt_tmiabt"
  [(set (match_operand:DI                         0 "register_operand" "=y")
	(plus:DI (match_operand:DI                1 "register_operand" "0")
		 (mult:DI (sign_extend:DI
			    (truncate:HI
			      (match_operand:SI   2 "register_operand" "r")))
			  (sign_extend:DI
			    (truncate:HI
			      (ashiftrt:SI
				(match_operand:SI 3 "register_operand" "r")
				(const_int 16)))))))]
  "TARGET_REALLY_IWMMXT"
  "tmiabt%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tmiaxy")]
)

(define_insn "iwmmxt_tmiatt"
  [(set (match_operand:DI          0 "register_operand" "=y")
	(plus:DI (match_operand:DI 1 "register_operand" "0")
		 (mult:DI (sign_extend:DI
			    (truncate:HI
			      (ashiftrt:SI
				(match_operand:SI 2 "register_operand" "r")
				(const_int 16))))
			  (sign_extend:DI
			    (truncate:HI
			      (ashiftrt:SI
				(match_operand:SI 3 "register_operand" "r")
				(const_int 16)))))))]
  "TARGET_REALLY_IWMMXT"
  "tmiatt%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tmiaxy")]
)

(define_insn "iwmmxt_tmovmskb"
  [(set (match_operand:SI               0 "register_operand" "=r")
	(unspec:SI [(match_operand:V8QI 1 "register_operand" "y")] UNSPEC_TMOVMSK))]
  "TARGET_REALLY_IWMMXT"
  "tmovmskb%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tmovmsk")]
)

(define_insn "iwmmxt_tmovmskh"
  [(set (match_operand:SI               0 "register_operand" "=r")
	(unspec:SI [(match_operand:V4HI 1 "register_operand" "y")] UNSPEC_TMOVMSK))]
  "TARGET_REALLY_IWMMXT"
  "tmovmskh%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tmovmsk")]
)

(define_insn "iwmmxt_tmovmskw"
  [(set (match_operand:SI               0 "register_operand" "=r")
	(unspec:SI [(match_operand:V2SI 1 "register_operand" "y")] UNSPEC_TMOVMSK))]
  "TARGET_REALLY_IWMMXT"
  "tmovmskw%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_tmovmsk")]
)

(define_insn "iwmmxt_waccb"
  [(set (match_operand:DI               0 "register_operand" "=y")
	(unspec:DI [(match_operand:V8QI 1 "register_operand" "y")] UNSPEC_WACC))]
  "TARGET_REALLY_IWMMXT"
  "waccb%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wacc")]
)

(define_insn "iwmmxt_wacch"
  [(set (match_operand:DI               0 "register_operand" "=y")
	(unspec:DI [(match_operand:V4HI 1 "register_operand" "y")] UNSPEC_WACC))]
  "TARGET_REALLY_IWMMXT"
  "wacch%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wacc")]
)

(define_insn "iwmmxt_waccw"
  [(set (match_operand:DI               0 "register_operand" "=y")
	(unspec:DI [(match_operand:V2SI 1 "register_operand" "y")] UNSPEC_WACC))]
  "TARGET_REALLY_IWMMXT"
  "waccw%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wacc")]
)

;; use unspec here to prevent 8 * imm to be optimized by cse
(define_insn "iwmmxt_waligni"
  [(set (match_operand:V8QI                                0 "register_operand" "=y")
	(unspec:V8QI [(subreg:V8QI
		        (ashiftrt:TI
		          (subreg:TI (vec_concat:V16QI
				       (match_operand:V8QI 1 "register_operand" "y")
				       (match_operand:V8QI 2 "register_operand" "y")) 0)
		          (mult:SI
		            (match_operand:SI              3 "immediate_operand" "i")
		            (const_int 8))) 0)] UNSPEC_WALIGNI))]
  "TARGET_REALLY_IWMMXT"
  "waligni%?\\t%0, %1, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_waligni")]
)

(define_insn "iwmmxt_walignr"
  [(set (match_operand:V8QI                           0 "register_operand" "=y")
	(subreg:V8QI (ashiftrt:TI
		       (subreg:TI (vec_concat:V16QI
				    (match_operand:V8QI 1 "register_operand" "y")
				    (match_operand:V8QI 2 "register_operand" "y")) 0)
		       (mult:SI
		         (zero_extract:SI (match_operand:SI 3 "register_operand" "z") (const_int 3) (const_int 0))
		         (const_int 8))) 0))]
  "TARGET_REALLY_IWMMXT"
  "walignr%U3%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_walignr")]
)

(define_insn "iwmmxt_walignr0"
  [(set (match_operand:V8QI                           0 "register_operand" "=y")
	(subreg:V8QI (ashiftrt:TI
		       (subreg:TI (vec_concat:V16QI
				    (match_operand:V8QI 1 "register_operand" "y")
				    (match_operand:V8QI 2 "register_operand" "y")) 0)
		       (mult:SI
		         (zero_extract:SI (reg:SI WCGR0) (const_int 3) (const_int 0))
		         (const_int 8))) 0))]
  "TARGET_REALLY_IWMMXT"
  "walignr0%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_walignr")]
)

(define_insn "iwmmxt_walignr1"
  [(set (match_operand:V8QI                           0 "register_operand" "=y")
	(subreg:V8QI (ashiftrt:TI
		       (subreg:TI (vec_concat:V16QI
				    (match_operand:V8QI 1 "register_operand" "y")
				    (match_operand:V8QI 2 "register_operand" "y")) 0)
		       (mult:SI
		         (zero_extract:SI (reg:SI WCGR1) (const_int 3) (const_int 0))
		         (const_int 8))) 0))]
  "TARGET_REALLY_IWMMXT"
  "walignr1%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_walignr")]
)

(define_insn "iwmmxt_walignr2"
  [(set (match_operand:V8QI                           0 "register_operand" "=y")
	(subreg:V8QI (ashiftrt:TI
		       (subreg:TI (vec_concat:V16QI
				    (match_operand:V8QI 1 "register_operand" "y")
				    (match_operand:V8QI 2 "register_operand" "y")) 0)
		       (mult:SI
		         (zero_extract:SI (reg:SI WCGR2) (const_int 3) (const_int 0))
		         (const_int 8))) 0))]
  "TARGET_REALLY_IWMMXT"
  "walignr2%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_walignr")]
)

(define_insn "iwmmxt_walignr3"
  [(set (match_operand:V8QI                           0 "register_operand" "=y")
	(subreg:V8QI (ashiftrt:TI
		       (subreg:TI (vec_concat:V16QI
				    (match_operand:V8QI 1 "register_operand" "y")
				    (match_operand:V8QI 2 "register_operand" "y")) 0)
		       (mult:SI
		         (zero_extract:SI (reg:SI WCGR3) (const_int 3) (const_int 0))
		         (const_int 8))) 0))]
  "TARGET_REALLY_IWMMXT"
  "walignr3%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_walignr")]
)

(define_insn "iwmmxt_wsadb"
  [(set (match_operand:V2SI               0 "register_operand" "=y")
        (unspec:V2SI [
		      (match_operand:V2SI 1 "register_operand" "0")
		      (match_operand:V8QI 2 "register_operand" "y")
		      (match_operand:V8QI 3 "register_operand" "y")] UNSPEC_WSAD))]
  "TARGET_REALLY_IWMMXT"
  "wsadb%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wsad")]
)

(define_insn "iwmmxt_wsadh"
  [(set (match_operand:V2SI               0 "register_operand" "=y")
        (unspec:V2SI [
		      (match_operand:V2SI 1 "register_operand" "0")
		      (match_operand:V4HI 2 "register_operand" "y")
		      (match_operand:V4HI 3 "register_operand" "y")] UNSPEC_WSAD))]
  "TARGET_REALLY_IWMMXT"
  "wsadh%?\\t%0, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wsad")]
)

(define_insn "iwmmxt_wsadbz"
  [(set (match_operand:V2SI               0 "register_operand" "=y")
        (unspec:V2SI [(match_operand:V8QI 1 "register_operand" "y")
		      (match_operand:V8QI 2 "register_operand" "y")] UNSPEC_WSADZ))]
  "TARGET_REALLY_IWMMXT"
  "wsadbz%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wsad")]
)

(define_insn "iwmmxt_wsadhz"
  [(set (match_operand:V2SI               0 "register_operand" "=y")
        (unspec:V2SI [(match_operand:V4HI 1 "register_operand" "y")
		      (match_operand:V4HI 2 "register_operand" "y")] UNSPEC_WSADZ))]
  "TARGET_REALLY_IWMMXT"
  "wsadhz%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "wmmx_wsad")]
)

(include "iwmmxt2.md")
