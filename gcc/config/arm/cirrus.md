;; Cirrus EP9312 "Maverick" ARM floating point co-processor description.
;; Copyright (C) 2003 Free Software Foundation, Inc.
;; Contributed by Red Hat.
;; Written by Aldy Hernandez (aldyh@redhat.com)

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


; Cirrus types for invalid insn combinations
; not		Not a cirrus insn
; normal	Any Cirrus insn not covered by the special cases below
; double	cfldrd, cfldr64, cfstrd, cfstr64
; compare	cfcmps, cfcmpd, cfcmp32, cfcmp64
; move		cfmvdlr, cfmvdhr, cfmvsr, cfmv64lr, cfmv64hr
(define_attr "cirrus" "not,normal,double,compare,move" (const_string "not"))


(define_insn "cirrus_adddi3"
  [(set (match_operand:DI          0 "cirrus_fp_register" "=v")
	(plus:DI (match_operand:DI 1 "cirrus_fp_register"  "v")
		 (match_operand:DI 2 "cirrus_fp_register"  "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfadd64%?\\t%V0, %V1, %V2"
  [(set_attr "type" "mav_farith")
   (set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_addsi3"
  [(set (match_operand:SI          0 "cirrus_fp_register" "=v")
	(plus:SI (match_operand:SI 1 "cirrus_fp_register" "v")
		 (match_operand:SI 2 "cirrus_fp_register" "v")))]
  "TARGET_ARM && TARGET_CIRRUS && 0"
  "cfadd32%?\\t%V0, %V1, %V2"
  [(set_attr "type" "mav_farith")
   (set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_addsf3"
  [(set (match_operand:SF          0 "cirrus_fp_register" "=v")
	(plus:SF (match_operand:SF 1 "cirrus_fp_register" "v")
		 (match_operand:SF 2 "cirrus_fp_register" "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfadds%?\\t%V0, %V1, %V2"
  [(set_attr "type" "mav_farith")
   (set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_adddf3"
  [(set (match_operand:DF          0 "cirrus_fp_register" "=v")
	(plus:DF (match_operand:DF 1 "cirrus_fp_register" "v")
		 (match_operand:DF 2 "cirrus_fp_register" "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfaddd%?\\t%V0, %V1, %V2"
  [(set_attr "type" "mav_farith")
   (set_attr "cirrus" "normal")]
)

(define_insn "cirrus_subdi3"
  [(set (match_operand:DI           0 "cirrus_fp_register" "=v")
	(minus:DI (match_operand:DI 1 "cirrus_fp_register"  "v")
		  (match_operand:DI 2 "cirrus_fp_register"  "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfsub64%?\\t%V0, %V1, %V2"
  [(set_attr "type" "mav_farith")
   (set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_subsi3_insn"
  [(set (match_operand:SI           0 "cirrus_fp_register" "=v")
	(minus:SI (match_operand:SI 1 "cirrus_fp_register" "v")
		  (match_operand:SI 2 "cirrus_fp_register" "v")))]
  "TARGET_ARM && TARGET_CIRRUS && 0"
  "cfsub32%?\\t%V0, %V1, %V2"
  [(set_attr "type" "mav_farith")
   (set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_subsf3"
  [(set (match_operand:SF           0 "cirrus_fp_register" "=v")
	(minus:SF (match_operand:SF 1 "cirrus_fp_register"  "v")
		  (match_operand:SF 2 "cirrus_fp_register"  "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfsubs%?\\t%V0, %V1, %V2"
  [(set_attr "type" "mav_farith")
   (set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_subdf3"
  [(set (match_operand:DF           0 "cirrus_fp_register" "=v")
	(minus:DF (match_operand:DF 1 "cirrus_fp_register" "v")
		  (match_operand:DF 2 "cirrus_fp_register" "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfsubd%?\\t%V0, %V1, %V2"
  [(set_attr "type" "mav_farith")
   (set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_mulsi3"
  [(set (match_operand:SI          0 "cirrus_fp_register" "=v")
	(mult:SI (match_operand:SI 2 "cirrus_fp_register"  "v")
		 (match_operand:SI 1 "cirrus_fp_register"  "v")))]
  "TARGET_ARM && TARGET_CIRRUS && 0"
  "cfmul32%?\\t%V0, %V1, %V2"
  [(set_attr "type" "mav_farith")
   (set_attr "cirrus" "normal")]
)

(define_insn "muldi3"
  [(set (match_operand:DI          0 "cirrus_fp_register" "=v")
	(mult:DI (match_operand:DI 2 "cirrus_fp_register"  "v")
		 (match_operand:DI 1 "cirrus_fp_register"  "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfmul64%?\\t%V0, %V1, %V2"
  [(set_attr "type" "mav_dmult")
   (set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_mulsi3addsi"
  [(set (match_operand:SI            0 "cirrus_fp_register" "=v")
	(plus:SI
	  (mult:SI (match_operand:SI 1 "cirrus_fp_register"  "v")
		   (match_operand:SI 2 "cirrus_fp_register"  "v"))
	  (match_operand:SI          3 "cirrus_fp_register"  "0")))]
  "TARGET_ARM && TARGET_CIRRUS && 0"
  "cfmac32%?\\t%V0, %V1, %V2"
  [(set_attr "type" "mav_farith")
   (set_attr "cirrus" "normal")]
)

;; Cirrus SI multiply-subtract
(define_insn "*cirrus_mulsi3subsi"
  [(set (match_operand:SI            0 "cirrus_fp_register" "=v")
	(minus:SI
	  (match_operand:SI          1 "cirrus_fp_register"  "0")
	  (mult:SI (match_operand:SI 2 "cirrus_fp_register"  "v")
		   (match_operand:SI 3 "cirrus_fp_register"  "v"))))]
  "0 && TARGET_ARM && TARGET_CIRRUS"
  "cfmsc32%?\\t%V0, %V2, %V3"
  [(set_attr "type" "mav_farith")
   (set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_mulsf3"
  [(set (match_operand:SF          0 "cirrus_fp_register" "=v")
	(mult:SF (match_operand:SF 1 "cirrus_fp_register"  "v")
		 (match_operand:SF 2 "cirrus_fp_register"  "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfmuls%?\\t%V0, %V1, %V2"
  [(set_attr "type" "mav_farith")
   (set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_muldf3"
  [(set (match_operand:DF          0 "cirrus_fp_register" "=v")
	(mult:DF (match_operand:DF 1 "cirrus_fp_register"  "v")
		 (match_operand:DF 2 "cirrus_fp_register"  "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfmuld%?\\t%V0, %V1, %V2"
  [(set_attr "type" "mav_dmult")
   (set_attr "cirrus" "normal")]
)

(define_insn "cirrus_ashl_const"
  [(set (match_operand:SI            0 "cirrus_fp_register" "=v")
	(ashift:SI (match_operand:SI 1 "cirrus_fp_register"  "v")
		   (match_operand:SI 2 "cirrus_shift_const"  "")))]
  "TARGET_ARM && TARGET_CIRRUS && 0"
  "cfsh32%?\\t%V0, %V1, #%s2"
  [(set_attr "cirrus" "normal")]
)

(define_insn "cirrus_ashiftrt_const"
  [(set (match_operand:SI	       0 "cirrus_fp_register" "=v")
	(ashiftrt:SI (match_operand:SI 1 "cirrus_fp_register"  "v")
		     (match_operand:SI 2 "cirrus_shift_const"  "")))]
  "TARGET_ARM && TARGET_CIRRUS && 0"
  "cfsh32%?\\t%V0, %V1, #-%s2"
  [(set_attr "cirrus" "normal")]
)

(define_insn "cirrus_ashlsi3"
  [(set (match_operand:SI            0 "cirrus_fp_register" "=v")
	(ashift:SI (match_operand:SI 1 "cirrus_fp_register"  "v")
		   (match_operand:SI 2 "register_operand"    "r")))]
  "TARGET_ARM && TARGET_CIRRUS && 0"
  "cfrshl32%?\\t%V1, %V0, %s2"
  [(set_attr "cirrus" "normal")]
)

(define_insn "ashldi3_cirrus"
  [(set (match_operand:DI            0 "cirrus_fp_register" "=v")
	(ashift:DI (match_operand:DI 1 "cirrus_fp_register"  "v")
		   (match_operand:SI 2 "register_operand"    "r")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfrshl64%?\\t%V1, %V0, %s2"
  [(set_attr "cirrus" "normal")]
)

(define_insn "cirrus_ashldi_const"
  [(set (match_operand:DI            0 "cirrus_fp_register" "=v")
	(ashift:DI (match_operand:DI 1 "cirrus_fp_register"  "v")
		   (match_operand:SI 2 "cirrus_shift_const"  "")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfsh64%?\\t%V0, %V1, #%s2"
  [(set_attr "cirrus" "normal")]
)

(define_insn "cirrus_ashiftrtdi_const"
  [(set (match_operand:DI            0 "cirrus_fp_register" "=v")
	(ashiftrt:DI (match_operand:DI 1 "cirrus_fp_register"  "v")
		     (match_operand:SI 2 "cirrus_shift_const"  "")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfsh64%?\\t%V0, %V1, #-%s2"
  [(set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_absdi2"
  [(set (match_operand:DI         0 "cirrus_fp_register" "=v")
	(abs:DI (match_operand:DI 1 "cirrus_fp_register"  "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfabs64%?\\t%V0, %V1"
  [(set_attr "cirrus" "normal")]
)

;; This doesn't really clobber ``cc''.  Fixme: aldyh.  
(define_insn "*cirrus_negdi2"
  [(set (match_operand:DI         0 "cirrus_fp_register" "=v")
	(neg:DI (match_operand:DI 1 "cirrus_fp_register"  "v")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfneg64%?\\t%V0, %V1"
  [(set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_negsi2"
  [(set (match_operand:SI         0 "cirrus_fp_register" "=v")
	(neg:SI (match_operand:SI 1 "cirrus_fp_register"  "v")))]
  "TARGET_ARM && TARGET_CIRRUS && 0"
  "cfneg32%?\\t%V0, %V1"
  [(set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_negsf2"
  [(set (match_operand:SF         0 "cirrus_fp_register" "=v")
	(neg:SF (match_operand:SF 1 "cirrus_fp_register"  "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfnegs%?\\t%V0, %V1"
  [(set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_negdf2"
  [(set (match_operand:DF         0 "cirrus_fp_register" "=v")
	(neg:DF (match_operand:DF 1 "cirrus_fp_register"  "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfnegd%?\\t%V0, %V1"
  [(set_attr "cirrus" "normal")]
)

;; This doesn't really clobber the condition codes either.  
(define_insn "*cirrus_abssi2"
  [(set (match_operand:SI         0 "cirrus_fp_register" "=v")
        (abs:SI (match_operand:SI 1 "cirrus_fp_register"  "v")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM && TARGET_CIRRUS && 0"
  "cfabs32%?\\t%V0, %V1"
  [(set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_abssf2"
  [(set (match_operand:SF         0 "cirrus_fp_register" "=v")
        (abs:SF (match_operand:SF 1 "cirrus_fp_register"  "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfabss%?\\t%V0, %V1"
  [(set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_absdf2"
  [(set (match_operand:DF         0 "cirrus_fp_register" "=v")
        (abs:DF (match_operand:DF 1 "cirrus_fp_register"  "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfabsd%?\\t%V0, %V1"
  [(set_attr "cirrus" "normal")]
)

;; Convert Cirrus-SI to Cirrus-SF
(define_insn "cirrus_floatsisf2"
  [(set (match_operand:SF           0 "cirrus_fp_register" "=v")
 	(float:SF (match_operand:SI 1 "s_register_operand"  "r")))
   (clobber (match_scratch:DF 2 "=v"))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfmv64lr%?\\t%Z2, %1\;cfcvt32s%?\\t%V0, %Y2"
  [(set_attr "length" "8")
   (set_attr "cirrus" "move")]
)

(define_insn "cirrus_floatsidf2"
  [(set (match_operand:DF           0 "cirrus_fp_register" "=v")
	(float:DF (match_operand:SI 1 "s_register_operand" "r")))
   (clobber (match_scratch:DF 2 "=v"))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfmv64lr%?\\t%Z2, %1\;cfcvt32d%?\\t%V0, %Y2"
  [(set_attr "length" "8")
   (set_attr "cirrus" "move")]
)

(define_insn "floatdisf2"
  [(set (match_operand:SF           0 "cirrus_fp_register" "=v")
	(float:SF (match_operand:DI 1 "cirrus_fp_register" "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfcvt64s%?\\t%V0, %V1"
  [(set_attr "cirrus" "normal")])

(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "cirrus_fp_register" "=v")
	(float:DF (match_operand:DI 1 "cirrus_fp_register" "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfcvt64d%?\\t%V0, %V1"
  [(set_attr "cirrus" "normal")])

(define_insn "cirrus_truncsfsi2"
  [(set (match_operand:SI         0 "s_register_operand" "=r")
	(fix:SI (fix:SF (match_operand:SF 1 "cirrus_fp_register"  "v"))))
   (clobber (match_scratch:DF     2                      "=v"))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cftruncs32%?\\t%Y2, %V1\;cfmvr64l%?\\t%0, %Z2"
  [(set_attr "length" "8")
   (set_attr "cirrus" "normal")]
)

(define_insn "cirrus_truncdfsi2"
  [(set (match_operand:SI         0 "s_register_operand" "=r")
	(fix:SI (fix:DF (match_operand:DF 1 "cirrus_fp_register"  "v"))))
   (clobber (match_scratch:DF     2                      "=v"))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cftruncd32%?\\t%Y2, %V1\;cfmvr64l%?\\t%0, %Z2"
  [(set_attr "length" "8")]
)

(define_insn "*cirrus_truncdfsf2"
  [(set (match_operand:SF  0 "cirrus_fp_register" "=v")
        (float_truncate:SF
         (match_operand:DF 1 "cirrus_fp_register" "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfcvtds%?\\t%V0, %V1"
  [(set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_extendsfdf2"
  [(set (match_operand:DF                  0 "cirrus_fp_register" "=v")
        (float_extend:DF (match_operand:SF 1 "cirrus_fp_register"  "v")))]
  "TARGET_ARM && TARGET_CIRRUS"
  "cfcvtsd%?\\t%V0, %V1"
  [(set_attr "cirrus" "normal")]
)

(define_insn "*cirrus_arm_movdi"
  [(set (match_operand:DI 0 "nonimmediate_di_operand" "=r,r,o<>,v,r,v,m,v")
	(match_operand:DI 1 "di_operand"              "rIK,mi,r,r,v,m,v,v"))]
  "TARGET_ARM && TARGET_CIRRUS"
  "*
  {
  switch (which_alternative)
    {
    case 0:
    case 1:
    case 2:
      return (output_move_double (operands));

    case 3: return \"cfmv64lr%?\\t%V0, %Q1\;cfmv64hr%?\\t%V0, %R1\";
    case 4: return \"cfmvr64l%?\\t%Q0, %V1\;cfmvr64h%?\\t%R0, %V1\";

    case 5: return \"cfldr64%?\\t%V0, %1\";
    case 6: return \"cfstr64%?\\t%V1, %0\";

    /* Shifting by 0 will just copy %1 into %0.  */
    case 7: return \"cfsh64%?\\t%V0, %V1, #0\";

    default: abort ();
    }
  }"
  [(set_attr "length"         "  8,   8,     8,   8,     8,     4,     4,     4")
   (set_attr "type"           "  *,load,store2,   *,     *,  load,store2,     *")
   (set_attr "pool_range"     "  *,1020,     *,   *,     *,     *,     *,     *")
   (set_attr "neg_pool_range" "  *,1012,     *,   *,     *,     *,     *,     *")
   (set_attr "cirrus"         "not, not,   not,move,normal,double,double,normal")]
)

;; Cirrus SI values have been outlawed.  Look in arm.h for the comment
;; on HARD_REGNO_MODE_OK.

(define_insn "*cirrus_arm_movsi_insn"
  [(set (match_operand:SI 0 "general_operand" "=r,r,r,m,*v,r,*v,T,*v")
        (match_operand:SI 1 "general_operand" "rI,K,mi,r,r,*v,T,*v,*v"))]
  "TARGET_ARM && TARGET_CIRRUS && 0
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode))"
  "@
   mov%?\\t%0, %1
   mvn%?\\t%0, #%B1
   ldr%?\\t%0, %1
   str%?\\t%1, %0
   cfmv64lr%?\\t%Z0, %1
   cfmvr64l%?\\t%0, %Z1
   cfldr32%?\\t%V0, %1
   cfstr32%?\\t%V1, %0
   cfsh32%?\\t%V0, %V1, #0"
  [(set_attr "type"           "*,  *,  load,store1,   *,     *,  load,store1,     *")
   (set_attr "pool_range"     "*,  *,  4096,     *,   *,     *,  1024,     *,     *")
   (set_attr "neg_pool_range" "*,  *,  4084,     *,   *,     *,  1012,     *,     *")
   (set_attr "cirrus"         "not,not, not,   not,move,normal,normal,normal,normal")]
)

(define_insn "*cirrus_movsf_hard_insn"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=v,v,v,r,m,r,r,m")
        (match_operand:SF 1 "general_operand"       "v,m,r,v,v,r,mE,r"))]
  "TARGET_ARM && TARGET_CIRRUS
   && (GET_CODE (operands[0]) != MEM
       || register_operand (operands[1], SFmode))"
  "@
   cfcpys%?\\t%V0, %V1
   cfldrs%?\\t%V0, %1
   cfmvsr%?\\t%V0, %1
   cfmvrs%?\\t%0, %V1
   cfstrs%?\\t%V1, %0
   mov%?\\t%0, %1
   ldr%?\\t%0, %1\\t%@ float
   str%?\\t%1, %0\\t%@ float"
  [(set_attr "length"         "     *,     *,   *,     *,     *,  4,   4,     4")
   (set_attr "type"           "     *,  load,   *,     *,store1,  *,load,store1")
   (set_attr "pool_range"     "     *,     *,   *,     *,     *,  *,4096,     *")
   (set_attr "neg_pool_range" "     *,     *,   *,     *,     *,  *,4084,     *")
   (set_attr "cirrus"         "normal,normal,move,normal,normal,not, not,   not")]
)

(define_insn "*cirrus_movdf_hard_insn"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=r,Q,r,m,r,v,v,v,r,m")
	(match_operand:DF 1 "general_operand"       "Q,r,r,r,mF,v,m,r,v,v"))]
  "TARGET_ARM
   && TARGET_CIRRUS
   && (GET_CODE (operands[0]) != MEM
       || register_operand (operands[1], DFmode))"
  "*
  {
  switch (which_alternative)
    {
    case 0: return \"ldm%?ia\\t%m1, %M0\\t%@ double\";
    case 1: return \"stm%?ia\\t%m0, %M1\\t%@ double\";
    case 2: case 3: case 4: return output_move_double (operands);
    case 5: return \"cfcpyd%?\\t%V0, %V1\";
    case 6: return \"cfldrd%?\\t%V0, %1\";
    case 7: return \"cfmvdlr\\t%V0, %Q1\;cfmvdhr%?\\t%V0, %R1\";
    case 8: return \"cfmvrdl%?\\t%Q0, %V1\;cfmvrdh%?\\t%R0, %V1\";
    case 9: return \"cfstrd%?\\t%V1, %0\";
    default: abort ();
    }
  }"
  [(set_attr "type"           "load,store2,  *,store2,load,     *,  load,   *,     *,store2")
   (set_attr "length"         "   4,     4,  8,     8,   8,     4,     4,   8,     8,     4")
   (set_attr "pool_range"     "   *,     *,  *,     *, 252,     *,     *,   *,     *,     *")
   (set_attr "neg_pool_range" "   *,     *,  *,     *, 244,     *,     *,   *,     *,     *")
   (set_attr "cirrus"         " not,   not,not,   not, not,normal,double,move,normal,double")]
)

