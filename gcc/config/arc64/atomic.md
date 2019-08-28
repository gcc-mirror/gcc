;; GCC machine description for ARC atomic instructions.
;; Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

;; Operations which can be used with atomic loads and stores.
(define_code_iterator ATOPS [plus minus ior xor and])

;; Operations which are supported by hardware.
(define_code_iterator ATHWOPS [plus ior xor and])

(define_expand "memory_barrier"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] ARC64_UNSPEC_MEMBAR))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*memory_barrier"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] ARC64_UNSPEC_MEMBAR))]
  ""
  {
       return "dmb\\t3";
  }
  [(set_attr "type" "dmb")
   (set_attr "length" "4")])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand" "")	;; bool out
   (match_operand:ALLI 1 "register_operand" "")	;; val out
   (match_operand:ALLI 2 "mem_noofs_operand" "");; memory
   (match_operand:ALLI 3 "register_operand" "")	;; expected
   (match_operand:ALLI 4 "register_operand" "")	;; desired
   (match_operand:SI 5 "const_int_operand")	;; is_weak
   (match_operand:SI 6 "const_int_operand")    	;; mod_s
   (match_operand:SI 7 "const_int_operand")]	;; mod_f
  "ARC64_HAS_ATOMIC_1"
{
  arc64_expand_compare_and_swap (operands);
  DONE;
})

(define_insn_and_split "atomic_compare_and_swap<mode>_1"
  [(set (reg:CC_Z CC_REGNUM)					;; bool out
	(unspec_volatile:CC_Z [(const_int 0)] ARC64_VUNSPEC_CAS))
   (set (match_operand:GPI 0 "register_operand"      "=&r")		;; val out
	(match_operand:GPI 1 "mem_noofs_operand"      "+ATOMC"))	;; memory
   (set (match_dup 1)
	(unspec_volatile
	  [(match_operand:GPI 2 "register_operand"     "r") ;; expect
	   (match_operand:GPI 3 "register_operand"     "r") ;; desired
	   (match_operand:SI 4 "const_int_operand")	;; is_weak
	   (match_operand:SI 5 "const_int_operand")	;; mod_s
	   (match_operand:SI 6 "const_int_operand")]	;; mod_f
	  ARC64_VUNSPEC_CAS))]
  "ARC64_HAS_ATOMIC_1"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    arc64_split_compare_and_swap (operands);
    DONE;
  })

(define_insn "arc_load_exclusive<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(unspec_volatile:GPI
	  [(match_operand:GPI 1 "mem_noofs_operand" "ATOMC")]
	  ARC64_VUNSPEC_LL))]
  "ARC64_HAS_ATOMIC_1"
  "llock<mcctab>\\t%0,%1"
  [(set_attr "type" "llock")
   (set_attr "iscompact" "no")
   (set_attr "predicable" "no")
   (set_attr "length" "*")])

(define_insn "arc_store_exclusive<mode>"
  [(set (match_operand:GPI 0 "mem_noofs_operand"     "=ATOMC")
	(unspec_volatile:GPI[(match_operand:GPI 1 "register_operand" "r")]
			   ARC64_VUNSPEC_SC))
   (clobber (reg:CC_Z CC_REGNUM))]
  "ARC64_HAS_ATOMIC_1"
  "scond<mcctab>\\t%1,%0"
  [(set_attr "type" "scond")
   (set_attr "iscompact" "no")
   (set_attr "predicable" "no")
   (set_attr "length" "*")])

(define_expand "atomic_exchangesi"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "mem_noofs_operand" "")
   (match_operand:SI 2 "register_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  "ARC64_HAS_ATOMIC_1"
{
  enum memmodel model = (enum memmodel) INTVAL (operands[3]);

  if (model == MEMMODEL_SEQ_CST)
    emit_insn (gen_sync ());
  emit_insn (gen_exchangesi (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "exchange<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(unspec_volatile:GPI [(match_operand:GPI 1 "mem_noofs_operand" "+ATOMC")]
			    ARC64_VUNSPEC_EX))
   (set (match_dup 1)
	(match_operand:GPI 2 "register_operand" "0"))]
  ""
  "ex<mcctab>\\t%0,%1"
  [(set_attr "type" "ex")
   (set_attr "iscompact" "no")
   (set_attr "predicable" "no")
   (set_attr "length" "*")])

;; New Atomic options enabled by option 2
(define_insn_and_split "atld_<optab><mode>"
  [(set (match_operand:GPI 0 "register_operand" "=&r,r")
	(match_operand:GPI 1 "mem_noofs_operand" "+ATOMC,ATOMC"))
   (set (match_dup 1)
	(unspec_volatile:GPI
	 [(ATHWOPS:GPI (match_dup 0)
		       (match_operand:GPI 2 "register_operand" "0,r"))
	  (match_operand:SI 3 "const_int_operand")]
	 ARC64_VUNSPEC_ATOOPS))]
  "ARC64_HAS_ATOMIC_2"
  "@
   atld<sfxtab>.<optab>%A3\\t%0,%1
   #"
  "&& reload_completed && !operands_match_p (operands[0], operands[2])"
  [(const_int 0)]
  {
   emit_insn (gen_rtx_SET (operands[0], operands[2]));
   emit_insn (gen_atld_<optab><mode> (operands[0], operands[1], operands[0], operands[3]));
   DONE;
  }
  [(set_attr "type" "atld<sfxtab>op")])

(define_expand "atomic_<optab><mode>"
  [(match_operand:GPI 0 "mem_noofs_operand" "")  ;; memory
   (ATOPS:GPI (match_dup 0)
	      (match_operand:GPI 1 "register_operand" "")) ;; operand
   (match_operand:SI 2 "const_int_operand" "")] ;; model
  "ARC64_HAS_ATOMIC_1"
{
  arc64_expand_atomic_op (<CODE>, operands[0], operands[1],
				NULL_RTX, NULL_RTX, operands[2]);
  DONE;
})

(define_expand "atomic_nandsi"
  [(match_operand:SI 0 "mem_noofs_operand" "")	;; memory
   (match_operand:SI 1 "register_operand" "")	;; operand
   (match_operand:SI 2 "const_int_operand" "")]	;; model
  "ARC64_HAS_ATOMIC_1"
{
 arc64_expand_atomic_op (NOT, operands[0], operands[1],
			    NULL_RTX, NULL_RTX, operands[2]);
 DONE;
})

(define_expand "atomic_fetch_<optab><mode>"
  [(set (match_operand:GPI 0 "register_operand")	;; output
	(match_operand:GPI 1 "mem_noofs_operand"))	;; memory
   (set (match_dup 1)
	(unspec_volatile:GPI
	 [(ATHWOPS:GPI (match_dup 1)
		     (match_operand:GPI 2 "register_operand")) ;; operand
	  (match_operand:SI 3 "const_int_operand")]	;; model
	 ARC64_VUNSPEC_ATOOPS))]
  "ARC64_HAS_ATOMIC_1"
  {
   if (!ARC64_HAS_ATOMIC_2)
     {
       arc64_expand_atomic_op (<CODE>, operands[1], operands[2],
			       operands[0], NULL_RTX, operands[3]);
       DONE;
     }
    if (!ARC64_HAS_ATOMIC_3)
      arc64_pre_atomic_barrier ((enum memmodel) INTVAL (operands[3]));
    emit_insn (gen_atld_<optab><mode> (operands[0], operands[1], operands[2], operands[3]));
    if (!ARC64_HAS_ATOMIC_3)
      arc64_post_atomic_barrier ((enum memmodel) INTVAL (operands[3]));
    DONE;
   })

;; ARCv3 doesn't have a MINUS atomic memory operation.
(define_expand "atomic_fetch_sub<mode>"
  [(set (match_operand:GPI 0 "register_operand")	;; output
	(match_operand:GPI 1 "mem_noofs_operand"))	;; memory
   (set (match_dup 1)
	(unspec_volatile:GPI
	 [(minus:GPI (match_dup 1)
		     (match_operand:GPI 2 "register_operand")) ;; operand
	  (match_operand:SI 3 "const_int_operand")]	;; model
	 ARC64_VUNSPEC_ATOOPS))]
  "ARC64_HAS_ATOMIC_1"
  {
    arc64_expand_atomic_op (MINUS, operands[1], operands[2],
			    operands[0], NULL_RTX, operands[3]);
    DONE;
  })

(define_expand "atomic_fetch_nand<mode>"
  [(match_operand:GPI 0 "register_operand" "")	;; output
   (match_operand:GPI 1 "mem_noofs_operand" "")	;; memory
   (match_operand:GPI 2 "register_operand" "")	;; operand
   (match_operand:SI  3 "const_int_operand" "")]	;; model
  "ARC64_HAS_ATOMIC_1"
{
  arc64_expand_atomic_op (NOT, operands[1], operands[2],
			     operands[0], NULL_RTX, operands[3]);
  DONE;
})

(define_expand "atomic_<optab>_fetch<mode>"
  [(match_operand:GPI 0 "register_operand" "")	;; output
   (match_operand:GPI 1 "mem_noofs_operand" "")	;; memory
   (ATOPS:GPI (match_dup 1)
	      (match_operand:GPI 2 "register_operand" "")) ;; operand
   (match_operand:SI 3 "const_int_operand" "")]	;; model
  "ARC64_HAS_ATOMIC_1"
{
  arc64_expand_atomic_op (<CODE>, operands[1], operands[2],
				NULL_RTX, operands[0], operands[3]);
  DONE;
})

(define_expand "atomic_nand_fetch<mode>"
  [(match_operand:GPI 0 "register_operand" "")		;; output
   (match_operand:GPI 1 "mem_noofs_operand" "")		;; memory
   (match_operand:GPI 2 "register_operand" "")		;; operand
   (match_operand:SI 3 "const_int_operand" "")]	;; model
  "ARC64_HAS_ATOMIC_1"
{
  arc64_expand_atomic_op (NOT, operands[1], operands[2],
			     NULL_RTX, operands[0], operands[3]);
  DONE;
})


;; mode:emacs-lisp
;; comment-start: ";; "
;; eval: (set-syntax-table (caopy-sequence (syntax-table)))
;; eval: (modify-syntax-entry ?[ "(]")
;; eval: (modify-syntax-entry ?] ")[")
;; eval: (modify-syntax-entry ?{ "(}")
;; eval: (modify-syntax-entry ?} "){")
;; eval: (setq indent-tabs-mode t)
;; End:
