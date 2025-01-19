;; microblaze.md -- Machine description for Xilinx MicroBlaze processors.
;; Copyright (C) 2009-2025 Free Software Foundation, Inc.

;; Contributed by Michael Eager <eager@eagercon.com>.

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
;; <http://www.gnu.org/licenses/>.  */

(include "constraints.md")
(include "predicates.md")

;;----------------------------------------------------
;; Constants
;;----------------------------------------------------
(define_constants [
  (R_SP        1)       ;; Stack pointer reg
  (R_SR       15)       ;; Sub-routine return addr reg
  (R_IR       14)       ;; Interrupt return addr reg
  (R_DR       16)       ;; Debug trap return addr reg
  (R_ER       17)       ;; Exception return addr reg
  (R_TMP      18)       ;; Assembler temporary reg
  (R_GOT      20)       ;; GOT ptr reg
  (MB_PIPE_3   0)       ;; Microblaze 3-stage pipeline 
  (MB_PIPE_5   1)       ;; Microblaze 5-stage pipeline 
  (UNSPEC_SET_GOT       101)    ;;
  (UNSPEC_GOTOFF        102)    ;; GOT offset
  (UNSPEC_PLT           103)    ;; jump table
  (UNSPEC_CMP		104)    ;; signed compare
  (UNSPEC_CMPU		105)    ;; unsigned compare
  (UNSPEC_TLS           106)    ;; jump table
  (UNSPEC_SET_TEXT      107)    ;; set text start
  (UNSPEC_TEXT          108)    ;; data text relative
])

(define_c_enum "unspec" [
  UNSPEC_IPREFETCH
])

;;----------------------------------------------------
;; Instruction Attributes
;;----------------------------------------------------

;; Classification of each insn.
;; branch	conditional branch
;; jump		unconditional jump
;; call		unconditional call
;; load		load instruction(s)
;; store	store instruction(s)
;; move		data movement within same register set
;; arith	integer arithmetic instruction
;; darith	double precision integer arithmetic instructions
;; imul		integer multiply
;; idiv		integer divide
;; icmp		integer compare
;; Xfadd		floating point add/subtract
;; Xfmul		floating point multiply
;; Xfmadd	floating point multiply-add
;; Xfdiv		floating point divide
;; Xfabs		floating point absolute value
;; Xfneg		floating point negation
;; Xfcmp		floating point compare
;; Xfcvt		floating point convert
;; Xfsqrt	floating point square root
;; multi	multiword sequence (or user asm statements)
;; nop		no operation
;; bshift 	Shift operations

(define_attr "type"
  "unknown,branch,jump,call,load,store,move,arith,darith,imul,idiv,icmp,multi,nop,no_delay_arith,no_delay_load,no_delay_store,no_delay_imul,no_delay_move,bshift,fadd,frsub,fmul,fdiv,fcmp,fsl,fsqrt,fcvt,trap"
  (const_string "unknown"))

;; Main data type used by the insn
(define_attr "mode" "unknown,none,QI,HI,SI,DI,SF,DF" (const_string "unknown"))

;; # instructions (4 bytes each)
(define_attr "length" "" (const_int 4))

(define_code_iterator any_return [return simple_return])

;; <optab> expands to the name of the optab for a particular code.
(define_code_attr optab [(return "return")
			 (simple_return "simple_return")])


;;----------------------------------------------------
;; Attribute describing the processor.  
;;----------------------------------------------------

;; Describe a user's asm statement.
(define_asm_attributes
  [(set_attr "type" "multi")])

;; whether or not generating calls to position independent functions
(define_attr "abicalls" "no,yes"
  (const (symbol_ref "microblaze_abicalls_attr")))

;;----------------------------------------------------------------
;; Microblaze DFA Pipeline description
;;----------------------------------------------------------------
                  
;;-----------------------------------------------------------------
/*
   This is description of pipeline hazards based on DFA.  The
   following constructions can be used for this:

   o define_cpu_unit string [string]) describes a cpu functional unit
     (separated by comma).

     1st operand: Names of cpu function units.
     2nd operand: Name of automaton (see comments for
     DEFINE_AUTOMATON).

     All define_reservations and define_cpu_units should have unique
     names which cannot be "nothing".

   o (exclusion_set string string) means that each CPU function unit
     in the first string cannot be reserved simultaneously with each
     unit whose name is in the second string and vise versa.  CPU
     units in the string are separated by commas. For example, it is
     useful for description CPU with fully pipelined floating point
     functional unit which can execute simultaneously only single
     floating point insns or only double floating point insns.

   o (presence_set string string) means that each CPU function unit in
     the first string cannot be reserved unless at least one of units
     whose names are in the second string is reserved.  This is an
     asymmetric relation.  CPU units in the string are separated by
     commas.  For example, it is useful for description that slot1 is
     reserved after slot0 reservation for a VLIW processor.

   o (absence_set string string) means that each CPU function unit in
     the first string cannot be reserved only if each unit whose name
     is in the second string is not reserved.  This is an asymmetric
     relation (actually exclusion set is analogous to this one but it
     is symmetric).  CPU units in the string are separated by commas.
     For example, it is useful for description that slot0 cannot be
     reserved after slot1 or slot2 reservation for a VLIW processor.

   o (define_bypass number out_insn_names in_insn_names) names bypass with
     given latency (the first number) from insns given by the first
     string (see define_insn_reservation) into insns given by the
     second string.  Insn names in the strings are separated by
     commas.

   o (define_automaton string) describes names of an automaton
     generated and used for pipeline hazards recognition.  The names
     are separated by comma.  Actually it is possibly to generate the
     single automaton but unfortunately it can be very large.  If we
     use more one automata, the summary size of the automata usually
     is less than the single one.  The automaton name is used in
     define_cpu_unit.  All automata should have unique names.

   o (define_reservation string string) names reservation (the first
     string) of cpu functional units (the 2nd string).  Sometimes unit
     reservations for different insns contain common parts.  In such
     case, you describe common part and use one its name (the 1st
     parameter) in regular expression in define_insn_reservation.  All
     define_reservations, define results and define_cpu_units should
     have unique names which cannot be "nothing".

   o (define_insn_reservation name default_latency condition regexpr)
     describes reservation of cpu functional units (the 3nd operand)
     for instruction which is selected by the condition (the 2nd
     parameter).  The first parameter is used for output of debugging
     information.  The reservations are described by a regular
     expression according the following syntax:

       regexp = regexp "," oneof
              | oneof

       oneof = oneof "|" allof
             | allof

       allof = allof "+" repeat
             | repeat

       repeat = element "*" number
              | element

       element = cpu_function_name
               | reservation_name
               | result_name
               | "nothing"
               | "(" regexp ")"

       1. "," is used for describing start of the next cycle in
          reservation.

       2. "|" is used for describing the reservation described by the
          first regular expression *or* the reservation described by
          the second regular expression *or* etc.

       3. "+" is used for describing the reservation described by the
          first regular expression *and* the reservation described by
          the second regular expression *and* etc.

       4. "*" is used for convenience and simply means sequence in
          which the regular expression are repeated NUMBER times with
          cycle advancing (see ",").

       5. cpu function unit name which means reservation.

       6. reservation name -- see define_reservation.

       7. string "nothing" means no units reservation.

*/
;;-----------------------------------------------------------------


;;----------------------------------------------------------------
;; Microblaze 5-stage pipeline description (v5.00.a and later)
;;----------------------------------------------------------------                 
                    
(define_automaton   "mbpipe_5")
(define_cpu_unit    "mb_issue,mb_iu,mb_wb,mb_fpu,mb_fpu_2,mb_mul,mb_mul_2,mb_div,mb_div_2,mb_bs,mb_bs_2" "mbpipe_5")

(define_insn_reservation "mb-integer" 1 
  (and (eq_attr "type" "branch,jump,call,arith,darith,icmp,nop,no_delay_arith")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_5)))
  "mb_issue,mb_iu,mb_wb")

(define_insn_reservation "mb-special-move" 2
  (and (eq_attr "type" "move")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_5)))
  "mb_issue,mb_iu*2,mb_wb")

(define_insn_reservation "mb-mem-load" 3
  (and (eq_attr "type" "load,no_delay_load")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_5)))
  "mb_issue,mb_iu,mb_wb")

(define_insn_reservation "mb-mem-store" 1
  (and (eq_attr "type" "store,no_delay_store")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_5)))
  "mb_issue,mb_iu,mb_wb")

(define_insn_reservation "mb-mul" 3
  (and (eq_attr "type" "imul,no_delay_imul")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_5)))
  "mb_issue,mb_mul,mb_mul_2*2,mb_wb")

(define_insn_reservation "mb-div" 34            
  (and (eq_attr "type" "idiv")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_5)))
    "mb_issue,mb_div,mb_div_2*33,mb_wb")

(define_insn_reservation "mb-bs" 2 
  (and (eq_attr "type" "bshift")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_5)))
   "mb_issue,mb_bs,mb_bs_2,mb_wb")

(define_insn_reservation "mb-fpu-add-sub-mul" 6
  (and (eq_attr "type" "fadd,frsub,fmul")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_5)))
  "mb_issue,mb_fpu,mb_fpu_2*5,mb_wb")

(define_insn_reservation "mb-fpu-fcmp" 3
  (and (eq_attr "type" "fcmp")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_5)))
  "mb_issue,mb_fpu,mb_fpu*2,mb_wb")

(define_insn_reservation "mb-fpu-div" 30
  (and (eq_attr "type" "fdiv")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_5)))
  "mb_issue,mb_fpu,mb_fpu_2*29,mb_wb")

(define_insn_reservation "mb-fpu-sqrt" 30
  (and (eq_attr "type" "fsqrt")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_5)))
  "mb_issue,mb_fpu,mb_fpu_2*29,mb_wb")

(define_insn_reservation "mb-fpu-fcvt" 4
  (and (eq_attr "type" "fcvt")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_5)))
  "mb_issue,mb_fpu,mb_fpu_2*3,mb_wb")

;;----------------------------------------------------------------
;; Microblaze 3-stage pipeline description (for v4.00.a and earlier)
;;----------------------------------------------------------------

(define_automaton   "mbpipe_3")
(define_cpu_unit    "mb3_iu" "mbpipe_3")

(define_insn_reservation "mb3-integer" 1 
  (and (eq_attr "type" "branch,jump,call,arith,darith,icmp,nop,no_delay_arith")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_3)))
  "mb3_iu")

(define_insn_reservation "mb3-special-move" 2
  (and (eq_attr "type" "move")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_3)))
  "mb3_iu*2")

(define_insn_reservation "mb3-mem-load" 2
  (and (eq_attr "type" "load,no_delay_load")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_3)))
  "mb3_iu")

(define_insn_reservation "mb3-mem-store" 1
  (and (eq_attr "type" "store,no_delay_store")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_3)))
  "mb3_iu")

(define_insn_reservation "mb3-mul" 3
  (and (eq_attr "type" "imul,no_delay_imul")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_3)))
  "mb3_iu")

(define_insn_reservation "mb3-div" 34            
  (and (eq_attr "type" "idiv")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_3)))
    "mb3_iu")

(define_insn_reservation "mb3-bs" 2 
  (and (eq_attr "type" "bshift")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_3)))
   "mb3_iu")

(define_insn_reservation "mb3-fpu-add-sub-mul" 6
  (and (eq_attr "type" "fadd,frsub,fmul")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_3)))
  "mb3_iu")

(define_insn_reservation "mb3-fpu-fcmp" 3
  (and (eq_attr "type" "fcmp")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_3)))
  "mb3_iu")

(define_insn_reservation "mb3-fpu-div" 30
  (and (eq_attr "type" "fdiv")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_3)))
  "mb3_iu")

(define_insn_reservation "mb3-fpu-sqrt" 30
  (and (eq_attr "type" "fsqrt")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_3)))
  "mb3_iu")

(define_insn_reservation "mb3-fpu-fcvt" 4
  (and (eq_attr "type" "fcvt")
       (eq (symbol_ref  "microblaze_pipe") (const_int MB_PIPE_3)))
  "mb3_iu")

(automata_option "v")
(automata_option "time")
(automata_option "progress")

(define_insn "bswapsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (bswap:SI (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_REORDER"
  "swapb %0, %1"
)

(define_insn "bswaphi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (bswap:HI (match_operand:HI 1 "register_operand" "r")))]
  "TARGET_REORDER"
  "swapb %0, %1
   swaph %0, %0"
)

;;----------------------------------------------------------------
;; Microblaze delay slot description
;;----------------------------------------------------------------
(define_delay (eq_attr "type" "branch,call,jump")
  [(and (eq_attr "type" "!branch,call,jump,icmp,multi,no_delay_arith,no_delay_load,no_delay_store,no_delay_imul,no_delay_move,darith") 
        (ior (not (match_test "microblaze_no_unsafe_delay"))
             (eq_attr "type" "!fadd,frsub,fmul,fdiv,fcmp,store,load")
             ))
  (nil) (nil)])


;;----------------------------------------------------------------
;; Microblaze FPU
;;----------------------------------------------------------------

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
        (plus:SF (match_operand:SF 1 "register_operand" "d")
                 (match_operand:SF 2 "register_operand" "d")))]
  "TARGET_HARD_FLOAT"
  "fadd\t%0,%1,%2"
  [(set_attr "type"     "fadd")
  (set_attr "mode"      "SF")
  (set_attr "length"    "4")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
        (minus:SF (match_operand:SF 1 "register_operand" "d")
                  (match_operand:SF 2 "register_operand" "d")))]
  "TARGET_HARD_FLOAT"
  "frsub\t%0,%2,%1"
  [(set_attr "type"     "frsub")
  (set_attr "mode"      "SF")
  (set_attr "length"    "4")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
        (mult:SF (match_operand:SF 1 "register_operand" "d")
                 (match_operand:SF 2 "register_operand" "d")))]
  "TARGET_HARD_FLOAT"
  "fmul\t%0,%1,%2"
  [(set_attr "type"     "fmul")
  (set_attr "mode"      "SF")
  (set_attr "length"    "4")])


(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
        (div:SF (match_operand:SF 1 "register_operand" "d")
                (match_operand:SF 2 "register_operand" "d")))]
  "TARGET_HARD_FLOAT"
  "fdiv\t%0,%2,%1"
  [(set_attr "type"     "fdiv")
  (set_attr "mode"      "SF")
  (set_attr "length"    "4")])

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
        (sqrt:SF (match_operand:SF 1 "register_operand" "d")))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT_SQRT"
  "fsqrt\t%0,%1"
  [(set_attr "type"     "fsqrt")
  (set_attr "mode"      "SF")
  (set_attr "length"    "4")])

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
        (float:SF (match_operand:SI 1 "register_operand" "d")))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT_CONVERT"
  "flt\t%0,%1"
  [(set_attr "type"     "fcvt")
  (set_attr "mode"      "SF")
  (set_attr "length"    "4")])

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (fix:SI (match_operand:SF 1 "register_operand" "d")))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT_CONVERT"
  "fint\t%0,%1"
  [(set_attr "type"     "fcvt")
  (set_attr "mode"      "SF")
  (set_attr "length"    "4")])

;;----------------------------------------------------------------
;; Add
;;----------------------------------------------------------------

;; Add 2 SImode integers [ src1 = reg ; src2 = arith ; dest = reg ]
;; Leave carry as is
(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d,d")
	(plus:SI (match_operand:SI 1 "reg_or_0_operand" "%dJ,dJ,dJ")
		 (match_operand:SI 2 "arith_plus_operand" "d,I,i")))]
  ""
  "@
   addk\t%0,%z1,%2
   addik\t%0,%z1,%2
   addik\t%0,%z1,%2"
  [(set_attr "type"	"arith,arith,no_delay_arith")
  (set_attr "mode"	"SI,SI,SI")
  (set_attr "length"	"4,4,8")])

;;----------------------------------------------------------------
;; Double Precision Additions
;;----------------------------------------------------------------

;; reg_DI_dest = reg_DI_src1 + DI_src2

;; Adding 2 DI operands in register or reg/imm

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(plus:DI (match_operand:DI 1 "register_operand" "%d,d,d")
		 (match_operand:DI 2 "arith_operand32" "d,P,N")))]
  ""
  "@
  add\t%L0,%L1,%L2\;addc\t%M0,%M1,%M2
  addi\t%L0,%L1,%2\;addc\t%M0,%M1,r0
  addi\t%L0,%L1,%2\;addc\t%M0,%M1,r0\;addi\t%M0,%M0,-1"
  [(set_attr "type"	"darith")
  (set_attr "mode"	"DI")
  (set_attr "length"	"8,8,12")])

;;----------------------------------------------------------------
;; Subtraction
;;----------------------------------------------------------------

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(minus:SI (match_operand:SI 1 "arith_operand" "d,d")
		  (match_operand:SI 2 "arith_operand" "d,n")))]
  ""
  "@
   rsubk\t%0,%2,%z1
   addik\t%0,%z1,-%2"
  [(set_attr "type"	"arith,no_delay_arith")
  (set_attr "mode"	"SI")
  (set_attr "length"	"4,8")])

(define_insn "iprefetch"
  [(unspec [(match_operand:SI 0 "const_int_operand" "n")] UNSPEC_IPREFETCH)
   (clobber (mem:BLK (scratch)))]
   "TARGET_PREFETCH"
  {
    operands[2] = gen_rtx_REG (SImode, MB_ABI_ASM_TEMP_REGNUM);
    return "mfs\t%2,rpc\n\twic\t%2,r0";
  }
  [(set_attr "type" "arith")
   (set_attr "mode"  "SI")
   (set_attr "length"    "8")])

;;----------------------------------------------------------------
;; Double Precision Subtraction
;;----------------------------------------------------------------

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=&d")
	(minus:DI (match_operand:DI 1 "register_operand" "d")
		  (match_operand:DI 2 "arith_operand32" "d")))]
  ""
  "rsub\t%L0,%L2,%L1\;rsubc\t%M0,%M2,%M1"
  [(set_attr "type"	"darith")
  (set_attr "mode"	"DI")
  (set_attr "length"	"8")])


;;----------------------------------------------------------------
;; Multiplication
;;----------------------------------------------------------------

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d,d")
	(mult:SI (match_operand:SI 1 "register_operand" "d,d,d")
		 (match_operand:SI 2 "arith_operand" "d,I,i")))]
  "!TARGET_SOFT_MUL"
  "@
  mul\t%0,%1,%2
  muli\t%0,%1,%2
  muli\t%0,%1,%2"
  [(set_attr "type"	"imul,imul,no_delay_imul")
  (set_attr "mode"	"SI")
  (set_attr "length"	"4,4,8")])

(define_insn "mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=&d")
        (mult:DI
         (sign_extend:DI (match_operand:SI 1 "register_operand" "d"))
         (sign_extend:DI (match_operand:SI 2 "register_operand" "d"))))]
  "!TARGET_SOFT_MUL && TARGET_MULTIPLY_HIGH"
  "mul\t%L0,%1,%2\;mulh\t%M0,%1,%2"
  [(set_attr "type"     "no_delay_arith")
   (set_attr "mode"     "DI")
   (set_attr "length"   "8")])

(define_insn "umulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=&d")
        (mult:DI
         (zero_extend:DI (match_operand:SI 1 "register_operand" "d"))
         (zero_extend:DI (match_operand:SI 2 "register_operand" "d"))))]
  "!TARGET_SOFT_MUL && TARGET_MULTIPLY_HIGH"
  "mul\t%L0,%1,%2\;mulhu\t%M0,%1,%2"
  [(set_attr "type"     "no_delay_arith")
   (set_attr "mode"     "DI")
   (set_attr "length"   "8")])

(define_insn "usmulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=&d")
        (mult:DI
         (zero_extend:DI (match_operand:SI 1 "register_operand" "d"))
         (sign_extend:DI (match_operand:SI 2 "register_operand" "d"))))]
  "!TARGET_SOFT_MUL && TARGET_MULTIPLY_HIGH"
  "mul\t%L0,%1,%2\;mulhsu\t%M0,%2,%1"
  [(set_attr "type"     "no_delay_arith")
   (set_attr "mode"     "DI")
   (set_attr "length"   "8")])

(define_insn "*smulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (truncate:SI
         (lshiftrt:DI
          (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand"  "d"))
                   (sign_extend:DI (match_operand:SI 2 "register_operand"  "d")))
          (const_int 32))))]
  "!TARGET_SOFT_MUL && TARGET_MULTIPLY_HIGH"
  "mulh\t%0,%1,%2"
  [(set_attr "type"     "imul")
  (set_attr "mode"      "SI")
  (set_attr "length"    "4")])

(define_insn "*umulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand"                            "=d")
        (truncate:SI
         (lshiftrt:DI
          (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand"  "d"))
                   (zero_extend:DI (match_operand:SI 2 "register_operand"  "d"))
)
          (const_int 32))))]
  "!TARGET_SOFT_MUL && TARGET_MULTIPLY_HIGH"
  "mulhu\t%0,%1,%2"
  [(set_attr "type"     "imul")
  (set_attr "mode"      "SI")
  (set_attr "length"    "4")])

(define_insn "*usmulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand"                            "=d")
        (truncate:SI
         (lshiftrt:DI
          (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand"  "d"))
                   (sign_extend:DI (match_operand:SI 2 "register_operand"  "d"))
)
          (const_int 32))))]
  "!TARGET_SOFT_MUL && TARGET_MULTIPLY_HIGH"
  "mulhsu\t%0,%2,%1"
  [(set_attr "type"     "imul")
  (set_attr "mode"      "SI")
  (set_attr "length"    "4")])


;;----------------------------------------------------------------
;; Division and remainder
;;----------------------------------------------------------------
(define_expand "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(div:SI (match_operand:SI 1 "register_operand" "d")
                (match_operand:SI 2 "register_operand" "d")))
  ]
  "(!TARGET_SOFT_DIV) || (TARGET_BARREL_SHIFT && TARGET_SMALL_DIVIDES)"
  {
    if (TARGET_SOFT_DIV && TARGET_BARREL_SHIFT && TARGET_SMALL_DIVIDES) 
      { 
        microblaze_expand_divide (operands);
        DONE;
      } 
    else if (!TARGET_SOFT_DIV) 
      {
        emit_insn (gen_divsi3_internal (operands[0], operands[1], operands[2]));
        DONE;
      }
  }     
)


(define_insn "divsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(div:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "register_operand" "d")))
  ]
  "!TARGET_SOFT_DIV"
  "idiv\t%0,%2,%1"
  [(set_attr "type"	"idiv")
  (set_attr "mode"	"SI")
  (set_attr "length"	"4")]
)

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(udiv:SI (match_operand:SI 1 "register_operand" "d")
                 (match_operand:SI 2 "register_operand" "d")))
  ]
  "!TARGET_SOFT_DIV"
  "idivu\t%0,%2,%1"
  [(set_attr "type"	"idiv")
  (set_attr "mode"	"SI")
  (set_attr "length"	"4")])

(define_peephole2
  [(set (match_operand:SI 0 "register_operand")
        (fix:SI (match_operand:SF 1 "register_operand")))
   (set (pc)
        (if_then_else (match_operator 2 "ordered_comparison_operator"
                       [(match_operand:SI 3 "register_operand")
                        (match_operand:SI 4 "arith_operand")])
                      (label_ref (match_operand 5))
                      (pc)))]
   "TARGET_HARD_FLOAT"
   [(set (match_dup 1) (match_dup 3))]

  {
    rtx condition;
    rtx cmp_op0 = operands[3];
    rtx cmp_op1 = operands[4];
    rtx comp_reg =  gen_rtx_REG (SImode, MB_ABI_ASM_TEMP_REGNUM);

    emit_insn (gen_cstoresf4 (comp_reg, operands[2],
                              gen_rtx_REG (SFmode, REGNO (cmp_op0)),
                              gen_rtx_REG (SFmode, REGNO (cmp_op1))));
    condition = gen_rtx_NE (SImode, comp_reg, const0_rtx);
    emit_jump_insn (gen_condjump (condition, operands[5]));
  }
)

;;----------------------------------------------------------------
;; Negation and one's complement
;;----------------------------------------------------------------

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(neg:SI (match_operand:SI 1 "register_operand" "d")))]
  ""
  "rsubk\t%0,%1,r0"
  [(set_attr "type"	"arith")
  (set_attr "mode"	"SI")
  (set_attr "length"	"4")])

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(neg:DI (match_operand:DI 1 "register_operand" "d")))]
  ""
  "rsub\t%L0,%L1,r0\;rsubc\t%M0,%M1,r0"
  [(set_attr "type"	"darith")
  (set_attr "mode"	"DI")
  (set_attr "length"	"8")])


(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(not:SI (match_operand:SI 1 "register_operand" "d")))]
  ""
  "xori\t%0,%1,-1"
  [(set_attr "type"	"arith")
  (set_attr "mode"	"SI")
  (set_attr "length"	"4")])

(define_insn "*one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(not:DI (match_operand:DI 1 "register_operand" "d")))]
  ""
  "nor\t%M0,r0,%M1\;nor\t%L0,r0,%L1"
  [(set_attr "type"	"darith")
  (set_attr "mode"	"DI")
  (set_attr "length"    "8")]
)

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(not:DI (match_operand:DI 1 "register_operand" "")))]
  "reload_completed 
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))"

  [(set (subreg:SI (match_dup 0) 0) (not:SI (subreg:SI (match_dup 1) 0)))
  (set (subreg:SI (match_dup 0) 4) (not:SI (subreg:SI (match_dup 1) 4)))]
  "")


;;----------------------------------------------------------------
;; Logical
;;----------------------------------------------------------------

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d,d,d")
	(and:SI (match_operand:SI 1 "arith_operand" "%d,d,d,d")
		(match_operand:SI 2 "arith_operand" "d,I,i,M")))]
  ""
  "@
   and\t%0,%1,%2
   andi\t%0,%1,%2 #and1
   andi\t%0,%1,%2 #and2
   andi\t%0,%1,%2 #and3"
  [(set_attr "type"	"arith,arith,no_delay_arith,no_delay_arith")
  (set_attr "mode"	"SI,SI,SI,SI")
  (set_attr "length"	"4,8,8,8")])


(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d,d,d")
	(ior:SI (match_operand:SI 1 "arith_operand" "%d,d,d,d")
		(match_operand:SI 2 "arith_operand" "d,I,M,i")))]
  ""
  "@
   or\t%0,%1,%2
   ori\t%0,%1,%2
   ori\t%0,%1,%2
   ori\t%0,%1,%2" 
  [(set_attr "type"	"arith,no_delay_arith,no_delay_arith,no_delay_arith")
  (set_attr "mode"	"SI,SI,SI,SI")
  (set_attr "length"	"4,8,8,8")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d,d")
	(xor:SI (match_operand:SI 1 "arith_operand" "%d,d,d")
		(match_operand:SI 2 "arith_operand" "d,I,i")))]
  ""
  "@
   xor\t%0,%1,%2
   xori\t%0,%1,%2
   xori\t%0,%1,%2"
  [(set_attr "type"	"arith,arith,no_delay_arith")
  (set_attr "mode"	"SI,SI,SI")
  (set_attr "length"	"4,8,8")])

;;----------------------------------------------------------------
;; Zero extension
;;----------------------------------------------------------------

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=d,d,d")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "d,R,m")))]
  ""
  "@
  andi\t%0,%1,0xffff
  lhu%i1\t%0,%1
  lhu%i1\t%0,%1"
  [(set_attr "type"	"no_delay_arith,load,no_delay_load")
  (set_attr "mode"	"SI,SI,SI")
  (set_attr "length"	"8,4,8")])

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=d,d,d")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "d,R,m")))]
  ""
  "@
  andi\t%0,%1,0x00ff
  lbu%i1\t%0,%1
  lbu%i1\t%0,%1"
  [(set_attr "type"	"arith,load,no_delay_load")
  (set_attr "mode"	"HI")
  (set_attr "length"	"4,4,8")])
  
(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=d,d,d")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "d,R,m")))]
  ""
  "@
  andi\t%0,%1,0x00ff
  lbu%i1\t%0,%1
  lbu%i1\t%0,%1"
  [(set_attr "type"	"arith,load,no_delay_load")
  (set_attr "mode"	"SI,SI,SI")
  (set_attr "length"	"4,4,8")])

;;----------------------------------------------------------------
;; Sign extension
;;----------------------------------------------------------------

;; basic Sign Extend Operations

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "d")))]
  ""
  "sext8\t%0,%1"
  [(set_attr "type"	"arith")
  (set_attr "mode"	"SI")
  (set_attr "length"	"4")])

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "d")))]
  ""
  "sext16\t%0,%1"
  [(set_attr "type"	"arith")
  (set_attr "mode"	"SI")
  (set_attr "length"	"4")])

;; Those for integer source operand are ordered
;; widest source type first.

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(sign_extend:DI (match_operand:SI 1 "nonimmediate_operand" "d,R,m")))]
  ""
  { 
     if (which_alternative == 0)
       output_asm_insn ("addk\t%L0,r0,%1", operands);
     else
       output_asm_insn ("lw%i1\t%L0,%1", operands);

     output_asm_insn ("add\t%M0,%L0,%L0", operands);
     output_asm_insn ("addc\t%M0,r0,r0", operands);
     output_asm_insn ("beqi\t%M0,.+8", operands);
     return "addi\t%M0,r0,0xffffffff";
  }
  [(set_attr "type"	"multi,multi,multi")
  (set_attr "mode"	"DI")
  (set_attr "length"	"20,20,20")])

;;----------------------------------------------------------------
;; Data movement
;;----------------------------------------------------------------

;; 64-bit integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.

(define_expand "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  {
    /* If operands[1] is a constant address illegal for pic, then we need to
       handle it just like microblaze_legitimize_address does.  */
    if (flag_pic && pic_address_needs_scratch (operands[1]))
    {
        rtx temp = force_reg (DImode, XEXP (XEXP (operands[1], 0), 0));
        rtx temp2 = XEXP (XEXP (operands[1], 0), 1);
        emit_move_insn (operands[0], gen_rtx_PLUS (DImode, temp, temp2));
        DONE;
    }


    if ((reload_in_progress | reload_completed) == 0
        && !register_operand (operands[0], DImode)
        && !register_operand (operands[1], DImode)
        && (((GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) != 0)
	       && operands[1] != CONST0_RTX (DImode))))
    {

      rtx temp = force_reg (DImode, operands[1]);
      emit_move_insn (operands[0], temp);
      DONE;
    }
  }
)



(define_insn "*movdi_internal"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,d,d,d,d,R,o")
	(match_operand:DI 1 "general_operand"      " d,i,J,R,o,d,d"))]
  ""
  { 
    switch (which_alternative)
    {
      case 0:
        return "addk\t%0,%1\n\taddk\t%D0,%d1";
      case 1:
	return "addik\t%M0,r0,%h1\n\taddik\t%L0,r0,%j1 #li => la";
      case 2:
	  return "addk\t%0,r0,r0\n\taddk\t%D0,r0,r0";
      case 3:
      case 4:
        if (reg_mentioned_p (operands[0], operands[1]))
          return "lwi\t%D0,%o1\n\tlwi\t%0,%1";
	else
	  return "lwi\t%0,%1\n\tlwi\t%D0,%o1";
      case 5:
      case 6:
        return "swi\t%1,%0\n\tswi\t%D1,%o0";
    }
    return "unreachable";
  }
  [(set_attr "type"	"no_delay_move,no_delay_arith,no_delay_arith,no_delay_load,no_delay_load,no_delay_store,no_delay_store")
  (set_attr "mode"	"DI")
  (set_attr "length"   "8,8,8,8,12,8,12")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(match_operand:DI 1 "register_operand" ""))]
  "reload_completed 
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1])) 
   && (REGNO(operands[0]) == (REGNO(operands[1]) + 1))"

  [(set (subreg:SI (match_dup 0) 4) (subreg:SI (match_dup 1) 4))
  (set (subreg:SI (match_dup 0) 0) (subreg:SI (match_dup 1) 0))]
  "")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(match_operand:DI 1 "register_operand" ""))]
  "reload_completed 
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1])) 
   && (REGNO (operands[0]) != (REGNO (operands[1]) + 1))"

  [(set (subreg:SI (match_dup 0) 0) (subreg:SI (match_dup 1) 0))
  (set (subreg:SI (match_dup 0) 4) (subreg:SI (match_dup 1) 4))]
  "")

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  {
    if (microblaze_expand_move (SImode, operands)) DONE;
  }
)

;; Added for status registers
(define_insn "movsi_status"
  [(set (match_operand:SI 0 "register_operand" "=d,d,z")
        (match_operand:SI 1 "register_operand" "z,d,d"))]
  "microblaze_is_interrupt_variant ()"
  "@
	mfs\t%0,%1  #mfs
	addk\t%0,%1,r0 #add movsi
	mts\t%0,%1  #mts"	
  [(set_attr "type" "move")
  (set_attr "mode" "SI")
  (set_attr "length" "12")])

;; This move will be not be moved to delay slot.	
(define_insn "*movsi_internal3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d,d,d")
	(match_operand:SI 1 "immediate_operand" "J,I,Mnis"))]
  "(register_operand (operands[0], SImode) && 
           (GET_CODE (operands[1]) == CONST_INT && 
                 (INTVAL (operands[1]) <= 32767 && INTVAL (operands[1]) >= -32768)))"  
  "@
   addk\t%0,r0,r0
   addik\t%0,r0,%1\t# %X1
   addik\t%0,r0,%1\t# %X1"
  [(set_attr "type"	"arith,arith,no_delay_arith")
  (set_attr "mode"	"SI")
  (set_attr "length"	"4")])

;; This move may be used for PLT label operand
(define_insn "*movsi_internal5_pltop"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(match_operand:SI 1 "call_insn_operand" ""))]
  "(register_operand (operands[0], Pmode) && 
           PLT_ADDR_P (operands[1]))"
  { 
     gcc_unreachable ();
  }
  [(set_attr "type"	"load")
  (set_attr "mode"	"SI")
  (set_attr "length"	"4")])

(define_insn "*movsi_internal2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d,d,d,   d,d,R,m")
	(match_operand:SI 1 "move_src_operand"         " d,I,Mnis,R,m,dJ,dJ"))]
  ""
  "@
   addk\t%0,%1,r0
   addik\t%0,r0,%1\t# %X1
   addik\t%0,%a1
   lw%i1\t%0,%1
   lw%i1\t%0,%1
   sw%i0\t%z1,%0
   sw%i0\t%z1,%0"
  [(set_attr "type"	"load,load,no_delay_load,load,no_delay_load,store,no_delay_store")
  (set_attr "mode"	"SI")
  (set_attr "length"	"4,4,8,4,8,4,8")])


;; 16-bit Integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.
;; Unsigned loads are used because BYTE_LOADS_ZERO_EXTEND is defined

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  {
    if ((reload_in_progress | reload_completed) == 0
        && !register_operand (operands[0], HImode)
        && !register_operand (operands[1], HImode)
        && ((GET_CODE (operands[1]) != CONST_INT
  	    || INTVAL (operands[1]) != 0)))
    {
        rtx temp = force_reg (HImode, operands[1]);
        emit_move_insn (operands[0], temp);
        DONE;
    }
  }
)

(define_insn "*movhi_internal2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,d,d,d,R,m")
	(match_operand:HI 1 "general_operand"       "I,d,R,m,dJ,dJ"))]
  ""
  "@
   addik\t%0,r0,%1\t# %X1
   addk\t%0,%1,r0
   lhu%i1\t%0,%1
   lhu%i1\t%0,%1
   sh%i0\t%z1,%0
   sh%i0\t%z1,%0"
  [(set_attr "type"	"arith,move,load,no_delay_load,store,no_delay_store")
  (set_attr "mode"	"HI")
  (set_attr "length"	"4,4,4,8,8,8")])

;; 8-bit Integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.
;; Unsigned loads are used because BYTE_LOADS_ZERO_EXTEND is defined

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  {
    if ((reload_in_progress | reload_completed) == 0
        && !register_operand (operands[0], QImode)
        && !register_operand (operands[1], QImode)
        && ((GET_CODE (operands[1]) != CONST_INT
            || INTVAL (operands[1]) != 0)))
    {
        rtx temp = force_reg (QImode, operands[1]);
        emit_move_insn (operands[0], temp);
        DONE;
    }
  }
)

(define_insn "*movqi_internal2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,d,d,d,d,R,m")
	(match_operand:QI 1 "general_operand"       "J,I,d,R,m,dJ,dJ"))]
  ""
  "@
   addk\t%0,r0,%z1
   addik\t%0,r0,%1\t# %X1
   addk\t%0,%1,r0
   lbu%i1\t%0,%1
   lbu%i1\t%0,%1
   sb%i0\t%z1,%0
   sbi\t%z1,%0"
  [(set_attr "type"	"arith,arith,move,load,no_delay_load,store,no_delay_store")
  (set_attr "mode"	"QI")
  (set_attr "length"	"4,4,8,4,8,4,8")])

;; Block moves, see microblaze.cc for more details.
;; Argument 0 is the destination
;; Argument 1 is the source
;; Argument 2 is the length
;; Argument 3 is the alignment
 
(define_expand "cpymemsi"
  [(parallel [(set (match_operand:BLK 0 "general_operand")
		   (match_operand:BLK 1 "general_operand"))
	      (use (match_operand:SI 2 ""))
	      (use (match_operand:SI 3 "const_int_operand"))])]
  ""
  {
    if (microblaze_expand_block_move (operands[0], operands[1], 
				      operands[2], operands[3]))
        DONE;
    else  
        FAIL;
  }
)

;;Load and store reverse
(define_insn "movsi4_rev"
  [(set (match_operand:SI 0 "reg_or_mem_operand" "=r,Q")
        (bswap:SI (match_operand:SF 1 "reg_or_mem_operand" "Q,r")))]
  "TARGET_REORDER"
  "@
   lwr\t%0,%y1,r0
   swr\t%1,%y0,r0"
  [(set_attr "type"     "load,store")
  (set_attr "mode"      "SI")
  (set_attr "length"    "4,4")])

;; 32-bit floating point moves

(define_expand "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "")
        (match_operand:SF 1 "general_operand" ""))]
  ""
  {
    if ((reload_in_progress | reload_completed) == 0
        && !register_operand (operands[0], SFmode)
        && !register_operand (operands[1], SFmode)
        && ( ((GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) != 0)
                 && operands[1] != CONST0_RTX (SFmode))))
    {
        rtx temp = force_reg (SFmode, operands[1]);
        emit_move_insn (operands[0], temp);
        DONE;
    }
  }
)

;; Applies to both TARGET_SOFT_FLOAT and TARGET_HARD_FLOAT
;;
(define_insn "*movsf_internal"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=d,d,d,d,d,R,m")
        (match_operand:SF 1 "general_operand" "G,d,R,F,m,d,d"))]
  "(register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode)
       || operands[1] == CONST0_RTX (SFmode))"
  "@
   addk\t%0,r0,r0
   addk\t%0,%1,r0
   lw%i1\t%0,%1
   addik\t%0,r0,%F1
   lw%i1\t%0,%1
   sw%i0\t%z1,%0
   swi\t%z1,%0"
  [(set_attr "type"     "move,no_delay_load,load,no_delay_load,no_delay_load,store,no_delay_store")
  (set_attr "mode"      "SF")
  (set_attr "length"    "4,4,4,4,4,4,4")])

;; 64-bit floating point moves
(define_expand "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "")
        (match_operand:DF 1 "general_operand" ""))]
  ""
  {
    if (flag_pic == 2) {
      if (GET_CODE (operands[1]) == MEM 
          && !microblaze_legitimate_address_p (DFmode, XEXP (operands[1],0), 0))
      {
        rtx ptr_reg;
        rtx result;
        ptr_reg = force_reg (Pmode, XEXP (operands[1],0));
        result = gen_rtx_MEM (DFmode, ptr_reg);
        emit_move_insn (operands[0], result);
        DONE;
      }
    }
    if ((reload_in_progress | reload_completed) == 0
        && !register_operand (operands[0], DFmode)
        && !register_operand (operands[1], DFmode)
        && (((GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) != 0)
                 && operands[1] != CONST0_RTX (DFmode))))
    {
        rtx temp = force_reg (DFmode, operands[1]);
        emit_move_insn (operands[0], temp);
        DONE;
    }
  }
)

;; movdf_internal
;; Applies to both TARGET_SOFT_FLOAT and TARGET_HARD_FLOAT
;;
(define_insn "*movdf_internal"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=d,d,d,d,o")
        (match_operand:DF 1 "general_operand" "dG,o,F,T,d"))]
  ""
  {
    switch (which_alternative)
    {
      case 0:
	return "addk\t%0,r0,r0\n\taddk\t%D0,r0,r0";
      case 1:
      case 3:
	if (reg_mentioned_p (operands[0], operands[1]))
          return "lwi\t%D0,%o1\n\tlwi\t%0,%1";
        else
	  return "lwi\t%0,%1\n\tlwi\t%D0,%o1";
      case 2:
      {
	return "addik\t%0,r0,%h1 \n\taddik\t%D0,r0,%j1 #Xfer Lo";
      }
      case 4:
	return "swi\t%1,%0\n\tswi\t%D1,%o0";
    }
    gcc_unreachable ();
  }
  [(set_attr "type"     "no_delay_move,no_delay_load,no_delay_load,no_delay_load,no_delay_store")
  (set_attr "mode"      "DF")
  (set_attr "length"    "4,8,8,16,8")])

(define_split
  [(set (match_operand:DF 0 "register_operand" "")
        (match_operand:DF 1 "register_operand" ""))]
  "reload_completed
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && (REGNO (operands[0]) == (REGNO (operands[1]) + 1))"
  [(set (subreg:SI (match_dup 0) 4) (subreg:SI (match_dup 1) 4))
  (set (subreg:SI (match_dup 0) 0) (subreg:SI (match_dup 1) 0))]
  "")

(define_split
  [(set (match_operand:DF 0 "register_operand" "")
        (match_operand:DF 1 "register_operand" ""))]
  "reload_completed
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && (REGNO (operands[0]) != (REGNO (operands[1]) + 1))"
  [(set (subreg:SI (match_dup 0) 0) (subreg:SI (match_dup 1) 0))
  (set (subreg:SI (match_dup 0) 4) (subreg:SI (match_dup 1) 4))]
  "")

;;----------------------------------------------------------------
;; Shifts
;;----------------------------------------------------------------

;;----------------------------------------------------------------
;; 32-bit left shifts
;;----------------------------------------------------------------
(define_expand "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=&d")
	(ashift:SI (match_operand:SI 1 "register_operand" "d")
		   (match_operand:SI 2 "arith_operand" "")))]
  ""
  { 
    /* Avoid recursion for trivial cases. */
    if (!((GET_CODE (operands [2]) == CONST_INT) && (INTVAL (operands[2]) == 1)))
      if (microblaze_expand_shift (operands))
        DONE;
  }
)

;; Irrespective of if we have a barrel-shifter or not, we want to match 
;; shifts by 1 with a special pattern. When a barrel shifter is present, 
;; saves a cycle. If not, allows us to annotate the instruction for delay 
;; slot optimization
(define_insn "*ashlsi3_byone"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashift:SI (match_operand:SI 1 "register_operand" "d")
                   (match_operand:SI 2 "arith_operand"    "I")))] 
  "(operands[2] == const1_rtx)"
  "addk\t%0,%1,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4")]
)

;; Barrel shift left
(define_insn "ashlsi3_bshift"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ashift:SI (match_operand:SI 1 "register_operand" "d,d")
                   (match_operand:SI 2 "arith_operand"    "I,d")))]
  "TARGET_BARREL_SHIFT"
  "@
  bslli\t%0,%1,%2
  bsll\t%0,%1,%2"
  [(set_attr "type"	"bshift,bshift")
  (set_attr "mode"	"SI,SI")
  (set_attr "length"	"4,4")]
)

;; The following patterns apply when there is no barrel shifter present

(define_insn "*ashlsi3_with_mul_delay"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashift:SI (match_operand:SI 1 "register_operand"  "d")
                   (match_operand:SI 2 "immediate_operand" "I")))] 
  "!TARGET_SOFT_MUL 
   && ((1 << INTVAL (operands[2])) <= 32767 && (1 << INTVAL (operands[2])) >= -32768)"
  "muli\t%0,%1,%m2"
  ;; This MUL will not generate an imm. Can go into a delay slot.
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4")]
)

(define_insn "*ashlsi3_with_mul_nodelay"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashift:SI (match_operand:SI 1 "register_operand"  "d")
                   (match_operand:SI 2 "immediate_operand" "I")))] 
  "!TARGET_SOFT_MUL"
  "muli\t%0,%1,%m2"
  ;; This MUL will generate an IMM. Cannot go into a delay slot
  [(set_attr "type"	"no_delay_arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8")]
)

(define_insn "*ashlsi3_with_size_opt"
  [(set (match_operand:SI 0 "register_operand" "=&d")
       (ashift:SI (match_operand:SI 1 "register_operand"  "d")
                   (match_operand:SI 2 "immediate_operand" "I")))]
  "(INTVAL (operands[2]) > 5 && optimize_size)"
  {
    operands[3] = gen_rtx_REG (SImode, MB_ABI_ASM_TEMP_REGNUM);

    output_asm_insn ("ori\t%3,r0,%2", operands);
    if (REGNO (operands[0]) != REGNO (operands[1]))
        output_asm_insn ("addk\t%0,%1,r0", operands);

    output_asm_insn ("addik\t%3,%3,-1", operands);
    output_asm_insn ("bneid\t%3,.-4", operands);
    return "addk\t%0,%0,%0";
  }
  [(set_attr "type"    "multi")
   (set_attr "mode"    "SI")
   (set_attr "length"  "20")]
)

(define_insn "*ashlsi3_with_rotate"
  [(set (match_operand:SI 0 "register_operand" "=&d")
       (ashift:SI (match_operand:SI 1 "register_operand"  "d")
                   (match_operand:SI 2 "immediate_operand" "I")))]
  "(INTVAL (operands[2]) > 17 && !optimize_size)"
  {
    int i, nshift;
    
    nshift = INTVAL (operands[2]);
    operands[3] = gen_int_mode (0xFFFFFFFF << nshift, SImode);

    /* We do one extra shift so that the first bit (carry) coming into the MSB
       will be masked out */
    output_asm_insn ("src\t%0,%1", operands);
    for (i = 0; i < (32 - nshift); i++)
       output_asm_insn ("src\t%0,%0", operands);

    return "andi\t%0,%0,%3";
  }
  [(set_attr "type"    "multi")
  (set_attr "mode"     "SI")
  (set_attr "length"   "80")]
)

(define_insn "*ashlsi_inline"
  [(set (match_operand:SI 0 "register_operand" "=&d")
       (ashift:SI (match_operand:SI 1 "register_operand"  "d")
                   (match_operand:SI 2 "immediate_operand" "I")))]
  ""
  {
    int i;
    int nshift = INTVAL (operands[2]);
    if (REGNO (operands[0]) != REGNO (operands[1]))
      output_asm_insn ("addk\t%0,r0,%1", operands);
    output_asm_insn ("addk\t%0,%1,%1", operands);
    for (i = 0; i < (nshift - 2); i++)
      output_asm_insn ("addk\t%0,%0,%0", operands);
    return "addk\t%0,%0,%0";
  }
  [(set_attr "type"    "multi")
  (set_attr "mode"     "SI")
  (set_attr "length"   "124")]
)

(define_insn "*ashlsi_reg"
  [(set (match_operand:SI 0 "register_operand" "=&d")
       (ashift:SI (match_operand:SI 1 "register_operand"  "d")
                   (match_operand:SI 2 "register_operand" "d")))]
  ""
  {
    operands[3] = gen_rtx_REG (SImode, MB_ABI_ASM_TEMP_REGNUM);
    output_asm_insn ("andi\t%3,%2,31", operands);
    if (REGNO (operands[0]) != REGNO (operands[1])) 
      output_asm_insn ("addk\t%0,r0,%1", operands);
    /* Exit the loop if zero shift. */
    output_asm_insn ("beqid\t%3,.+20", operands);
    /* Emit the loop.  */
    output_asm_insn ("addk\t%0,%0,r0", operands);
    output_asm_insn ("addik\t%3,%3,-1", operands);
    output_asm_insn ("bneid\t%3,.-4", operands);
    return "addk\t%0,%0,%0";
  }
  [(set_attr "type"    "multi")
  (set_attr "mode"     "SI")
  (set_attr "length"   "28")]
)


;;----------------------------------------------------------------
;; 32-bit right shifts
;;----------------------------------------------------------------
(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=&d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "d")
                     (match_operand:SI 2 "arith_operand" "")))]
  ""
  {
    /* Avoid recursion for trivial cases. */
    if (!((GET_CODE (operands [2]) == CONST_INT) && (INTVAL (operands[2]) == 1)))
      if (microblaze_expand_shift (operands))
        DONE;
  }
)

;; Irrespective of if we have a barrel-shifter or not, we want to match 
;; shifts by 1 with a special pattern. When a barrel shifter is present, 
;; saves a cycle. If not, allows us to annotate the instruction for delay 
;; slot optimization
(define_insn "*ashrsi3_byone"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "d")
                     (match_operand:SI 2 "arith_operand"    "I")))] 
  "(operands[2] == const1_rtx)"
  "sra\t%0,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4")]
)

;; Barrel shift right logical
(define_insn "*ashrsi3_bshift"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "d,d")
                     (match_operand:SI 2 "arith_operand"    "I,d")))]
  "TARGET_BARREL_SHIFT"
  "@
  bsrai\t%0,%1,%2
  bsra\t%0,%1,%2"
  [(set_attr "type"	"bshift,bshift")
  (set_attr "mode"	"SI,SI")
  (set_attr "length"	"4,4")]
)

(define_insn "*ashrsi_inline"
  [(set (match_operand:SI 0 "register_operand" "=&d")
       (ashiftrt:SI (match_operand:SI 1 "register_operand"  "d")
                   (match_operand:SI 2 "immediate_operand" "I")))]
  ""
  {
    int i;
    int nshift = INTVAL (operands[2]);
    if (REGNO (operands[0]) != REGNO (operands[1]))
      output_asm_insn ("addk\t%0,r0,%1", operands);
    output_asm_insn ("sra\t%0,%1", operands);
    for (i = 0; i < (nshift - 2); i++)
      output_asm_insn ("sra\t%0,%0", operands);
    return "sra\t%0,%0";
  }
  [(set_attr "type"    "multi")
  (set_attr "mode"     "SI")
  (set_attr "length"   "124")]
)

(define_insn "*ashrsi_reg"
  [(set (match_operand:SI 0 "register_operand" "=&d")
       (ashiftrt:SI (match_operand:SI 1 "register_operand"  "d")
                   (match_operand:SI 2 "register_operand" "d")))]
  ""
  {
    operands[3] = gen_rtx_REG (SImode, MB_ABI_ASM_TEMP_REGNUM);
    output_asm_insn ("andi\t%3,%2,31", operands);
    if (REGNO (operands[0]) != REGNO (operands[1])) 
      output_asm_insn ("addk\t%0,r0,%1", operands);
    /* Exit the loop if zero shift. */
    output_asm_insn ("beqid\t%3,.+20", operands);
    /* Emit the loop.  */
    output_asm_insn ("addk\t%0,%0,r0", operands);
    output_asm_insn ("addik\t%3,%3,-1", operands);
    output_asm_insn ("bneid\t%3,.-4", operands);
    return "sra\t%0,%0";
  }
  [(set_attr "type"    "multi")
  (set_attr "mode"     "SI")
  (set_attr "length"   "28")]
)

;;----------------------------------------------------------------
;; 32-bit right shifts (logical)
;;----------------------------------------------------------------

(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=&d")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "d")
                     (match_operand:SI 2 "arith_operand" "")))]
  ""
  {
    /* Avoid recursion for trivial cases. */
    if (!((GET_CODE (operands [2]) == CONST_INT) && (INTVAL (operands[2]) == 1)))
      if (microblaze_expand_shift (operands))
        DONE;
  }
)

;; Irrespective of if we have a barrel-shifter or not, we want to match 
;; shifts by 1 with a special pattern. When a barrel shifter is present, 
;; saves a cycle. If not, allows us to annotate the instruction for delay 
;; slot optimization
(define_insn "*lshrsi3_byone"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "d")
                     (match_operand:SI 2 "arith_operand"    "I")))] 
  "(operands[2] == const1_rtx)"
  "srl\t%0,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4")]
)

;; Barrel shift right logical
(define_insn "*lshrsi3_bshift"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "d,d")
                     (match_operand:SI 2 "arith_operand"    "I,d")))]
  "TARGET_BARREL_SHIFT"
  "@
  bsrli\t%0,%1,%2
  bsrl\t%0,%1,%2"
  [(set_attr "type"	"bshift,bshift")
  (set_attr "mode"	"SI,SI")
  (set_attr "length"	"4,4")]
)

(define_insn "*lshrsi_inline"
  [(set (match_operand:SI 0 "register_operand" "=&d")
       (lshiftrt:SI (match_operand:SI 1 "register_operand"  "d")
                   (match_operand:SI 2 "immediate_operand" "I")))]
  ""
  {
    int i;
    int nshift = INTVAL (operands[2]);
    if (REGNO (operands[0]) != REGNO (operands[1]))
      output_asm_insn ("addk\t%0,r0,%1", operands);
    output_asm_insn ("srl\t%0,%1", operands);
    for (i = 0; i < (nshift - 2); i++)
      output_asm_insn ("srl\t%0,%0", operands);
    return "srl\t%0,%0";
  }
  [(set_attr "type"    "multi")
  (set_attr "mode"     "SI")
  (set_attr "length"   "124")]
)

(define_insn "*lshrsi_reg"
  [(set (match_operand:SI 0 "register_operand" "=&d")
       (lshiftrt:SI (match_operand:SI 1 "register_operand"  "d")
                   (match_operand:SI 2 "register_operand" "d")))]
  ""
  {
    operands[3] = gen_rtx_REG (SImode, MB_ABI_ASM_TEMP_REGNUM);
    output_asm_insn ("andi\t%3,%2,31", operands);
    if (REGNO (operands[0]) != REGNO (operands[1])) 
      output_asm_insn ("addk\t%0,r0,%1", operands);
    /* Exit the loop if zero shift. */
    output_asm_insn ("beqid\t%3,.+20", operands);
    /* Emit the loop.  */
    output_asm_insn ("addk\t%0,%0,r0", operands);
    output_asm_insn ("addik\t%3,%3,-1", operands);
    output_asm_insn ("bneid\t%3,.-4", operands);
    return "srl\t%0,%0";
  }
  [(set_attr "type"    "multi")
  (set_attr "mode"     "SI")
  (set_attr "length"   "28")]
)

;;----------------------------------------------------------------
;; Setting a register from an integer comparison. 
;;----------------------------------------------------------------
(define_expand "cstoresi4"
   [(set (match_operand:SI 0 "register_operand")
        (match_operator:SI 1 "ordered_comparison_operator"
	      [(match_operand:SI 2 "register_operand")
	       (match_operand:SI 3 "register_operand")]))]
  "TARGET_PATTERN_COMPARE"
  "if (GET_CODE (operand1) != EQ && GET_CODE (operand1) != NE) 
     FAIL;
  "
)

(define_insn "seq_internal_pat" 
  [(set (match_operand:SI 0 "register_operand" "=d")
	(eq:SI 
	       (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "register_operand" "d")))]
  "TARGET_PATTERN_COMPARE"
  "pcmpeq\t%0,%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4")]
)              

(define_insn "sne_internal_pat" 
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ne:SI 
	       (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "register_operand" "d")))]
  "TARGET_PATTERN_COMPARE"
  "pcmpne\t%0,%1,%2"
  [(set_attr "type"	"arith")
  (set_attr "mode"	"SI")
  (set_attr "length"	"4")]
)              

;;----------------------------------------------------------------
;; Setting a register from an floating point comparison. 
;;----------------------------------------------------------------
(define_insn "cstoresf4"
   [(set (match_operand:SI 0 "register_operand" "=r")
        (match_operator:SI 1 "ordered_comparison_operator"
	      [(match_operand:SF 2 "register_operand" "r")
	       (match_operand:SF 3 "register_operand" "r")]))]
  "TARGET_HARD_FLOAT"
  "fcmp.%C1\t%0,%3,%2"
  [(set_attr "type"     "fcmp")
   (set_attr "mode"      "SF")
   (set_attr "length"    "4")]
)

;;----------------------------------------------------------------
;; Conditional branches
;;----------------------------------------------------------------

(define_expand "cbranchsi4"
  [(set (pc)
        (if_then_else (match_operator 0 "ordered_comparison_operator"
                       [(match_operand:SI 1 "register_operand")
                        (match_operand:SI 2 "arith_operand" "I,i")])
                      (label_ref (match_operand 3 ""))
                      (pc)))]
  ""
{
  microblaze_expand_conditional_branch (SImode, operands);
  DONE;
})

(define_expand "cbranchsi4_reg"
  [(set (pc)
        (if_then_else (match_operator 0 "ordered_comparison_operator"
                       [(match_operand:SI 1 "register_operand")
                        (match_operand:SI 2 "register_operand")])
                      (label_ref (match_operand 3 ""))
                      (pc)))]
  ""
{
  microblaze_expand_conditional_branch_reg (SImode, operands);
  DONE;
})

(define_expand "cbranchsf4"
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(match_operand:SF 1 "register_operand")
		        (match_operand:SF 2 "register_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  "TARGET_HARD_FLOAT"
{
  microblaze_expand_conditional_branch_sf (operands);
  DONE;

})

;; Used to implement comparison instructions
(define_expand "condjump"
  [(set (pc)
	(if_then_else (match_operand 0)
		      (label_ref (match_operand 1))
		      (pc)))])

(define_insn "branch_zero"
  [(set (pc)
	(if_then_else (match_operator:SI 0 "ordered_comparison_operator"
  				 [(match_operand:SI 1 "register_operand" "d")
                                  (const_int 0)])
                      (match_operand:SI 2 "pc_or_label_operand" "")
                      (match_operand:SI 3 "pc_or_label_operand" "")))
  ]
  ""
  {
    if (operands[3] == pc_rtx) 
      return "b%C0i%?\t%z1,%2";
    else 
      return "b%N0i%?\t%z1,%3";
  }
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"4")]
)

(define_insn "branch_compare"
  [(set (pc)
        (if_then_else (match_operator:SI 0 "cmp_op"
                                         [(match_operand:SI 1 "register_operand" "d")
                                          (match_operand:SI 2 "register_operand" "d")
                                         ])
                      (label_ref (match_operand 3))
                      (pc)))
  (clobber(reg:SI R_TMP))]
  ""
  {
    operands[4] = gen_rtx_REG (SImode, MB_ABI_ASM_TEMP_REGNUM);
    enum rtx_code code = GET_CODE (operands[0]);

    if (code == GT || code == LE)
      {
        output_asm_insn ("cmp\tr18,%z1,%z2", operands);
        code = swap_condition (code);
      }
    else if (code == GTU || code == LEU)
      {
        output_asm_insn ("cmpu\tr18,%z1,%z2", operands);
        code = swap_condition (code);
      }
    else if (code == GE || code == LT)
      {
        output_asm_insn ("cmp\tr18,%z2,%z1", operands);
      }
    else if (code == GEU || code == LTU)
      {
        output_asm_insn ("cmpu\tr18,%z2,%z1", operands);
      }

    operands[0] = gen_rtx_fmt_ee (signed_condition (code), SImode, operands[4], const0_rtx);
    return "b%C0i%?\tr18,%3";
  }
  [(set_attr "type"     "branch")
   (set_attr "mode"     "none")
   (set_attr "length"   "12")]
)

;;----------------------------------------------------------------
;; Unconditional branches
;;----------------------------------------------------------------
(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  {
    if (GET_CODE (operands[0]) == REG)
        return "br%?\t%0";
    else	
        return "bri%?\t%l0";
  }
  [(set_attr "type"	"jump")
  (set_attr "mode"	"none")
  (set_attr "length"	"4")])

(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "register_operand" "d"))]
  ""
  {
    rtx dest = operands[0];
    if (GET_CODE (dest) != REG || GET_MODE (dest) != Pmode)
      operands[0] = copy_to_mode_reg (Pmode, dest);

    emit_jump_insn (gen_indirect_jump_internal1 (operands[0]));
    DONE;
  }
)

;; Indirect jumps. Jump to register values. Assuming absolute jumps

(define_insn "indirect_jump_internal1"
  [(set (pc) (match_operand:SI 0 "register_operand" "d"))]
  ""
  "bra%?\t%0"
  [(set_attr "type"	"jump")
  (set_attr "mode"	"none")
  (set_attr "length"	"4")])

(define_expand "tablejump"
  [(set (pc)
	(match_operand 0 "register_operand" "d"))
  (use (label_ref (match_operand 1 "" "")))]
  ""
  {
    gcc_assert (GET_MODE (operands[0]) == Pmode);

    if (!flag_pic || TARGET_PIC_DATA_TEXT_REL)
      emit_jump_insn (gen_tablejump_internal1 (operands[0], operands[1]));
    else
      emit_jump_insn (gen_tablejump_internal3 (operands[0], operands[1]));
    DONE;
  }
)

(define_insn "tablejump_internal1"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "d"))
  (use (label_ref (match_operand 1 "" "")))]
  ""
  "bra%?\t%0 "
  [(set_attr "type"	"jump")
  (set_attr "mode"	"none")
  (set_attr "length"	"4")])

(define_expand "tablejump_internal3"
  [(parallel [(set (pc)
		   (plus:SI (match_operand:SI 0 "register_operand" "d")
			    (label_ref:SI (match_operand:SI 1 "" ""))))
             (use (label_ref:SI (match_dup 1)))])]
  ""
  ""
)

;; need to change for MicroBlaze PIC
(define_insn ""
 [(set (pc)
	(plus:SI (match_operand:SI 0 "register_operand" "d")
		 (label_ref:SI (match_operand 1 "" ""))))
  (use (label_ref:SI (match_dup 1)))]
 "NEXT_INSN (as_a <rtx_insn *> (operands[1])) != 0
  && GET_CODE (PATTERN (NEXT_INSN (as_a <rtx_insn *> (operands[1])))) == ADDR_DIFF_VEC
  && flag_pic"
  {
    output_asm_insn ("addk\t%0,%0,r20",operands);
    return "bra%?\t%0";
}
 [(set_attr "type"	"jump")
  (set_attr "mode"	"none")
  (set_attr "length"	"4")])

(define_expand "tablejump_internal4"
  [(parallel [(set (pc)
		   (plus:DI (match_operand:DI 0 "register_operand" "d")
			    (label_ref:DI (match_operand:SI 1 "" ""))))
             (use (label_ref:DI (match_dup 1)))])]
  ""
  ""
)

;;----------------------------------------------------------------
;; Function prologue/epilogue and stack allocation
;;----------------------------------------------------------------
(define_expand "prologue"
  [(const_int 1)]
  ""
  {
      microblaze_expand_prologue ();
      DONE;
  }
)

(define_expand "epilogue"
  [(use (const_int 0))]
  ""
  {
      microblaze_expand_epilogue ();
      DONE;
  }
)

;; An insn to allocate new stack space for dynamic use (e.g., alloca).
;; We copy the return address, decrement the stack pointer and save the 
;; return address again at the new stack top 

(define_expand "allocate_stack"
  [(set (match_operand 0 "register_operand" "=r")
	(minus (reg 1) (match_operand 1 "register_operand" "")))
   (set (reg 1)
	(minus (reg 1) (match_dup 1)))]
  ""
  { 
    rtx retaddr = gen_rtx_MEM (Pmode, stack_pointer_rtx);
    rtx rtmp    = gen_rtx_REG (SImode, R_TMP);
    rtx neg_op0;

    emit_move_insn (rtmp, retaddr);
    if (GET_CODE (operands[1]) != CONST_INT)
    {
        neg_op0 = gen_reg_rtx (Pmode);
	emit_insn (gen_negsi2 (neg_op0, operands[1]));
    } else
        neg_op0 = GEN_INT (- INTVAL (operands[1]));

    emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, neg_op0));
    emit_move_insn (gen_rtx_MEM (Pmode, stack_pointer_rtx), rtmp);
    emit_move_insn (operands[0], virtual_stack_dynamic_rtx);
    emit_insn (gen_rtx_CLOBBER (SImode, rtmp));
    DONE;
  }
)

(define_expand "save_stack_block"
  [(match_operand 0 "register_operand" "")
   (match_operand 1 "register_operand" "")]
  ""
  {
    emit_move_insn (operands[0], operands[1]);
    DONE;
  }
)

(define_expand "restore_stack_block"
  [(match_operand 0 "register_operand" "")
   (match_operand 1 "register_operand" "")]
  ""
  {
    rtx retaddr = gen_rtx_MEM (Pmode, stack_pointer_rtx);
    rtx rtmp    = gen_rtx_REG (SImode, R_TMP);

    /* Move the retaddr.  */
    emit_move_insn (rtmp, retaddr);
    emit_move_insn (operands[0], operands[1]);
    emit_move_insn (gen_rtx_MEM (Pmode, operands[0]), rtmp);
    DONE;
  }
)

;; Trivial return.  Make it look like a normal return insn as that
;; allows jump optimizations to work better .
(define_expand "return"
  [(simple_return)]
  "microblaze_can_use_return_insn ()"
  {}
)

(define_expand "simple_return"
  [(simple_return)]
  ""
  {}
)

(define_insn "*<optab>"
  [(any_return)]
  ""
  {
    if (microblaze_is_break_handler ())
        return "rtbd\tr16, 8\;%#";
    else if (microblaze_is_interrupt_variant ())
        return "rtid\tr14, 0\;%#";
    else
        return "rtsd\tr15, 8\;%#";
  }
  [(set_attr "type"	"jump")
  (set_attr "mode"	"none")
  (set_attr "length"	"4")]
)

;; Normal return.

(define_insn "<optab>_internal"
  [(any_return)
   (use (match_operand:SI 0 "register_operand" ""))]
  ""
  {
    if (microblaze_is_break_handler ())
        return "rtbd\tr16,8\;%#";
    else if (microblaze_is_interrupt_variant ())
        return "rtid\tr14,0 \;%#";
    else
        return "rtsd\tr15,8 \;%#";
  }
  [(set_attr "type"	"jump")
  (set_attr "mode"	"none")
  (set_attr "length"	"4")])


;; Block any insns from across this point
;; Useful to group sequences together.
(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] 0)]
  ""
  ""
  [(set_attr "type"	"unknown")
  (set_attr "mode"	"none")
  (set_attr "length"	"0")])

  
;;----------------------------------------------------------------
;; Function calls
;;----------------------------------------------------------------

(define_expand "call"
  [(parallel [(call (match_operand 0 "memory_operand" "m")
		    (match_operand 1 "" "i"))
             (clobber (reg:SI R_SR))
             (use (match_operand 2 "" ""))
             (use (match_operand 3 "" ""))])]
  ""
  {
    rtx addr = XEXP (operands[0], 0);

    if (flag_pic == 2 && !TARGET_PIC_DATA_TEXT_REL
    && GET_CODE (addr) == SYMBOL_REF
	&& !SYMBOL_REF_LOCAL_P (addr)) 
      {
        rtx temp = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_PLT);
        XEXP (operands[0], 0) = temp;
      }
    
    if ((GET_CODE (addr) != REG && !CONSTANT_ADDRESS_P (addr))
	|| !call_insn_operand (addr, VOIDmode))
      XEXP (operands[0], 0) = copy_to_mode_reg (Pmode, addr);

    if (GET_CODE (XEXP (operands[0], 0)) == UNSPEC)
      emit_call_insn (gen_call_internal_plt0 (operands[0], operands[1],
                        gen_rtx_REG (SImode, 
				     GP_REG_FIRST + MB_ABI_SUB_RETURN_ADDR_REGNUM),
                               	     pic_offset_table_rtx));
    else
      emit_call_insn (gen_call_internal0 (operands[0], operands[1],
                        gen_rtx_REG (SImode, 
				     GP_REG_FIRST + MB_ABI_SUB_RETURN_ADDR_REGNUM)));

        DONE;
  }
)

(define_expand "call_internal0"
  [(parallel [(call (match_operand 0 "" "")
		    (match_operand 1 "" ""))
             (clobber (match_operand:SI 2 "" ""))])]
  ""
  {
  }
)
 
(define_expand "call_internal_plt0"
  [(parallel [(call (match_operand 0 "" "")
		    (match_operand 1 "" ""))
             (clobber (match_operand:SI 2 "" ""))
             (use (match_operand:SI 3 "" ""))])]
  ""
  {
  }
)
 
(define_insn "call_internal_plt"
  [(call (mem (match_operand:SI 0 "call_insn_plt_operand" ""))
	 (match_operand:SI 1 "" "i"))
  (clobber (reg:SI R_SR))
  (use (reg:SI R_GOT))]
  "flag_pic"
  {
    rtx target2
      = gen_rtx_REG (Pmode, GP_REG_FIRST + MB_ABI_SUB_RETURN_ADDR_REGNUM);
    gen_rtx_CLOBBER (VOIDmode, target2);
    return "brlid\tr15,%0\;%#";
  }
  [(set_attr "type"	"call")
  (set_attr "mode"	"none")
  (set_attr "length"	"4")])

(define_insn "call_internal1"
  [(call (mem (match_operand:VOID 0 "call_insn_simple_operand" "ri"))
	 (match_operand:SI 1 "" "i"))
  (clobber (reg:SI R_SR))]
  ""
  {
    rtx target = operands[0];
    rtx target2
      = gen_rtx_REG (Pmode, GP_REG_FIRST + MB_ABI_SUB_RETURN_ADDR_REGNUM);
    if (GET_CODE (target) == SYMBOL_REF) {
        if (microblaze_break_function_p (SYMBOL_REF_DECL (target))) {
            gen_rtx_CLOBBER (VOIDmode, target2);
            return "brki\tr16,%0\;%#";
        }
        else {
            gen_rtx_CLOBBER (VOIDmode, target2);
            return "brlid\tr15,%0\;%#";
        }
    } else if (GET_CODE (target) == CONST_INT)
        return "la\t%@,r0,%0\;brald\tr15,%@\;%#";
    else if (GET_CODE (target) == REG)
        return "brald\tr15,%0\;%#";	
    else {
        fprintf (stderr,"Unsupported call insn\n");
        return NULL;
    }
  }
  [(set_attr "type"	"call")
  (set_attr "mode"	"none")
  (set_attr "length"	"4")])

;; calls.cc now passes a fourth argument, make saber happy

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "register_operand" "=d")
		   (call (match_operand 1 "memory_operand" "m")
			 (match_operand 2 "" "i")))
             (clobber (reg:SI R_SR))
             (use (match_operand 3 "" ""))])] ;; next_arg_reg
  ""
  {
    rtx addr = XEXP (operands[1], 0);

    if (flag_pic == 2 && !TARGET_PIC_DATA_TEXT_REL
    && GET_CODE (addr) == SYMBOL_REF
	&& !SYMBOL_REF_LOCAL_P (addr)) 
      {
        rtx temp = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_PLT);
        XEXP (operands[1], 0) = temp;
      }

    if ((GET_CODE (addr) != REG && !CONSTANT_ADDRESS_P (addr))
        || !call_insn_operand (addr, VOIDmode))
      XEXP (operands[1], 0) = copy_to_mode_reg (Pmode, addr);

    if (GET_CODE (XEXP (operands[1], 0)) == UNSPEC)
      emit_call_insn (gen_call_value_intern_plt0 (operands[0], operands[1], 
			operands[2],
                        gen_rtx_REG (SImode, 
				     GP_REG_FIRST + MB_ABI_SUB_RETURN_ADDR_REGNUM),
				     pic_offset_table_rtx));
    else
      emit_call_insn (gen_call_value_internal (operands[0], operands[1], 
			operands[2],
                        gen_rtx_REG (SImode, 
				     GP_REG_FIRST + MB_ABI_SUB_RETURN_ADDR_REGNUM)));

    DONE;
  }
)


(define_expand "call_value_internal"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand 1 "" "")
			 (match_operand 2 "" "")))
             (clobber (match_operand:SI 3 "" ""))
             ])]
  ""
  {}
)

(define_expand "call_value_intern_plt0"
  [(parallel[(set (match_operand 0 "" "")
                  (call (match_operand 1 "" "")
                        (match_operand 2 "" "")))
             (clobber (match_operand:SI 3 "" ""))
             (use (match_operand:SI 4 "" ""))])]
  "flag_pic"
  {}
)

(define_insn "call_value_intern_plt"
  [(set (match_operand:VOID 0 "register_operand" "=d")
        (call (mem (match_operand:SI 1 "call_insn_plt_operand" ""))
              (match_operand:SI 2 "" "i")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))
   (use (match_operand:SI 4 "register_operand"))]
  "flag_pic"
  { 
    rtx target2
      = gen_rtx_REG (Pmode,GP_REG_FIRST + MB_ABI_SUB_RETURN_ADDR_REGNUM);

    gen_rtx_CLOBBER (VOIDmode,target2);
    return "brlid\tr15,%1\;%#";
  }
  [(set_attr "type"	"call")
  (set_attr "mode"	"none")
  (set_attr "length"	"4")])

(define_insn "call_value_intern"
  [(set (match_operand:VOID 0 "register_operand" "=d")
        (call (mem (match_operand:VOID 1 "call_insn_operand" "ri"))
              (match_operand:SI 2 "" "i")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  ""
  { 
    rtx target = operands[1];
    rtx target2
      = gen_rtx_REG (Pmode,GP_REG_FIRST + MB_ABI_SUB_RETURN_ADDR_REGNUM);

    if (GET_CODE (target) == SYMBOL_REF)
    {
      gen_rtx_CLOBBER (VOIDmode,target2);
      if (microblaze_break_function_p (SYMBOL_REF_DECL (target)))
        return "brki\tr16,%1\;%#";
      else if (SYMBOL_REF_FLAGS (target) & SYMBOL_FLAG_FUNCTION)
        {
	  return "brlid\tr15,%1\;%#";
        }
      else
        {
	    return "bralid\tr15,%1\;%#";
        }
    }
    else if (GET_CODE (target) == CONST_INT)
        return "la\t%@,r0,%1\;brald\tr15,%@\;%#";
    else if (GET_CODE (target) == REG)
        return "brald\tr15,%1\;%#";	
    else 
        return "Unsupported call insn\n";
  }
  [(set_attr "type"	"call")
  (set_attr "mode"	"none")
  (set_attr "length"	"4")])


;; Call subroutine returning any type.
(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
             (match_operand 1 "" "")
             (match_operand 2 "" "")])]
  ""
  {
    if (operands[0])		/* silence statement not reached warnings */
    {
        int i;

        emit_call_insn (gen_call (operands[0], const0_rtx, NULL, const0_rtx));

        for (i = 0; i < XVECLEN (operands[2], 0); i++)
	{
	    rtx set = XVECEXP (operands[2], 0, i);
	    emit_move_insn (SET_DEST (set), SET_SRC (set));
	}

        emit_insn (gen_blockage ());
        DONE;
      }
  }
)

;;----------------------------------------------------------------
;; Misc.
;;----------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "type"	"nop")
  (set_attr "mode"	"none")
  (set_attr "length"	"4")])

;; Trap instruction pattern for __builtin_trap. Same as the glibc ABORT_INSTRUCTION
(define_insn "trap"
  [(trap_if (const_int 1) (const_int 0))]
  ""
  "bri\t0"
 [(set_attr "type" "trap")]
)

;; The insn to set GOT. The hardcoded number "8" accounts for $pc difference
;; between "mfs" and "addik" instructions.
(define_insn "set_got"
  [(set (match_operand:SI 0 "register_operand" "=r")
    (unspec:SI [(const_int 0)] UNSPEC_SET_GOT))]
  ""
  "mfs\t%0,rpc\n\taddik\t%0,%0,_GLOBAL_OFFSET_TABLE_+8"
  [(set_attr "type" "multi")
   (set_attr "length" "12")])

;; The insn to set TEXT.
;; The hardcoded number "8" accounts for $pc difference
;; between "mfs" and "addik" instructions.
(define_insn "set_text"
  [(set (match_operand:SI 0 "register_operand" "=r")
    (unspec:SI[(const_int 0)] UNSPEC_SET_TEXT))]
  ""
  "mfs\t%0,rpc\n\taddik\t%0,%0,8@TXTPCREL"
  [(set_attr "type" "multi")
   (set_attr "length" "12")])


;; This insn gives the count of leading number of zeros for the second
;; operand and stores the result in first operand.
(define_insn "clzsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (clz:SI (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_HAS_CLZ"
  "clz\t%0,%1"
  [(set_attr "type"     "arith")
  (set_attr "mode"      "SI")
  (set_attr "length"    "4")])

; This is used in compiling the unwind routines.
(define_expand "eh_return"
  [(use (match_operand 0 "general_operand" ""))]
  ""
  "
{
  microblaze_eh_return (operands[0]);
  DONE;
}")

(include "sync.md")
