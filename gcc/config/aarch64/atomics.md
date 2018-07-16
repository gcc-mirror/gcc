;; Machine description for AArch64 processor synchronization primitives.
;; Copyright (C) 2009-2018 Free Software Foundation, Inc.
;; Contributed by ARM Ltd.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Instruction patterns.

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand" "")			;; bool out
   (match_operand:ALLI 1 "register_operand" "")			;; val out
   (match_operand:ALLI 2 "aarch64_sync_memory_operand" "")	;; memory
   (match_operand:ALLI 3 "general_operand" "")			;; expected
   (match_operand:ALLI 4 "aarch64_reg_or_zero" "")			;; desired
   (match_operand:SI 5 "const_int_operand")			;; is_weak
   (match_operand:SI 6 "const_int_operand")			;; mod_s
   (match_operand:SI 7 "const_int_operand")]			;; mod_f
  ""
  {
    aarch64_expand_compare_and_swap (operands);
    DONE;
  }
)

(define_insn_and_split "aarch64_compare_and_swap<mode>"
  [(set (reg:CC CC_REGNUM)					;; bool out
    (unspec_volatile:CC [(const_int 0)] UNSPECV_ATOMIC_CMPSW))
   (set (match_operand:SI 0 "register_operand" "=&r")	   ;; val out
    (zero_extend:SI
      (match_operand:SHORT 1 "aarch64_sync_memory_operand" "+Q"))) ;; memory
   (set (match_dup 1)
    (unspec_volatile:SHORT
      [(match_operand:SI 2 "aarch64_plus_operand" "rI")	;; expected
       (match_operand:SHORT 3 "aarch64_reg_or_zero" "rZ")	;; desired
       (match_operand:SI 4 "const_int_operand")		;; is_weak
       (match_operand:SI 5 "const_int_operand")		;; mod_s
       (match_operand:SI 6 "const_int_operand")]	;; mod_f
      UNSPECV_ATOMIC_CMPSW))
   (clobber (match_scratch:SI 7 "=&r"))]
  ""
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_split_compare_and_swap (operands);
    DONE;
  }
)

(define_insn_and_split "aarch64_compare_and_swap<mode>"
  [(set (reg:CC CC_REGNUM)					;; bool out
    (unspec_volatile:CC [(const_int 0)] UNSPECV_ATOMIC_CMPSW))
   (set (match_operand:GPI 0 "register_operand" "=&r")		;; val out
    (match_operand:GPI 1 "aarch64_sync_memory_operand" "+Q"))   ;; memory
   (set (match_dup 1)
    (unspec_volatile:GPI
      [(match_operand:GPI 2 "aarch64_plus_operand" "rI")	;; expect
       (match_operand:GPI 3 "aarch64_reg_or_zero" "rZ")		;; desired
       (match_operand:SI 4 "const_int_operand")			;; is_weak
       (match_operand:SI 5 "const_int_operand")			;; mod_s
       (match_operand:SI 6 "const_int_operand")]		;; mod_f
      UNSPECV_ATOMIC_CMPSW))
   (clobber (match_scratch:SI 7 "=&r"))]
  ""
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_split_compare_and_swap (operands);
    DONE;
  }
)

(define_insn_and_split "aarch64_compare_and_swap<mode>_lse"
  [(set (reg:CC CC_REGNUM)					;; bool out
    (unspec_volatile:CC [(const_int 0)] UNSPECV_ATOMIC_CMPSW))
   (set (match_operand:SI 0 "register_operand" "=&r")		;; val out
    (zero_extend:SI
      (match_operand:SHORT 1 "aarch64_sync_memory_operand" "+Q"))) ;; memory
   (set (match_dup 1)
    (unspec_volatile:SHORT
      [(match_operand:SI 2 "aarch64_plus_operand" "rI")	;; expected
       (match_operand:SHORT 3 "aarch64_reg_or_zero" "rZ")	;; desired
       (match_operand:SI 4 "const_int_operand")		;; is_weak
       (match_operand:SI 5 "const_int_operand")		;; mod_s
       (match_operand:SI 6 "const_int_operand")]	;; mod_f
      UNSPECV_ATOMIC_CMPSW))]
  "TARGET_LSE"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_gen_atomic_cas (operands[0], operands[1],
			    operands[2], operands[3],
			    operands[5]);
    DONE;
  }
)

(define_insn_and_split "aarch64_compare_and_swap<mode>_lse"
  [(set (reg:CC CC_REGNUM)					;; bool out
    (unspec_volatile:CC [(const_int 0)] UNSPECV_ATOMIC_CMPSW))
   (set (match_operand:GPI 0 "register_operand" "=&r")		;; val out
    (match_operand:GPI 1 "aarch64_sync_memory_operand" "+Q"))   ;; memory
   (set (match_dup 1)
    (unspec_volatile:GPI
      [(match_operand:GPI 2 "aarch64_plus_operand" "rI")	;; expect
       (match_operand:GPI 3 "aarch64_reg_or_zero" "rZ")		;; desired
       (match_operand:SI 4 "const_int_operand")			;; is_weak
       (match_operand:SI 5 "const_int_operand")			;; mod_s
       (match_operand:SI 6 "const_int_operand")]		;; mod_f
      UNSPECV_ATOMIC_CMPSW))]
  "TARGET_LSE"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_gen_atomic_cas (operands[0], operands[1],
			    operands[2], operands[3],
			    operands[5]);
    DONE;
  }
)

(define_expand "atomic_exchange<mode>"
 [(match_operand:ALLI 0 "register_operand" "")
  (match_operand:ALLI 1 "aarch64_sync_memory_operand" "")
  (match_operand:ALLI 2 "register_operand" "")
  (match_operand:SI 3 "const_int_operand" "")]
  ""
  {
    rtx (*gen) (rtx, rtx, rtx, rtx);

    /* Use an atomic SWP when available.  */
    if (TARGET_LSE)
      gen = gen_aarch64_atomic_exchange<mode>_lse;
    else
      gen = gen_aarch64_atomic_exchange<mode>;

    emit_insn (gen (operands[0], operands[1], operands[2], operands[3]));

    DONE;
  }
)

(define_insn_and_split "aarch64_atomic_exchange<mode>"
  [(set (match_operand:ALLI 0 "register_operand" "=&r")		;; output
    (match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q")) ;; memory
   (set (match_dup 1)
    (unspec_volatile:ALLI
      [(match_operand:ALLI 2 "register_operand" "r")	;; input
       (match_operand:SI 3 "const_int_operand" "")]		;; model
      UNSPECV_ATOMIC_EXCHG))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 4 "=&r"))]
  ""
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (SET, operands[0], NULL, operands[1],
			     operands[2], operands[3], operands[4]);
    DONE;
  }
)

(define_insn_and_split "aarch64_atomic_exchange<mode>_lse"
  [(set (match_operand:ALLI 0 "register_operand" "=&r")
    (match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q"))
   (set (match_dup 1)
    (unspec_volatile:ALLI
      [(match_operand:ALLI 2 "register_operand" "r")
       (match_operand:SI 3 "const_int_operand" "")]
      UNSPECV_ATOMIC_EXCHG))]
  "TARGET_LSE"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_gen_atomic_ldop (SET, operands[0], NULL, operands[1],
			     operands[2], operands[3]);
    DONE;
  }
)

(define_expand "atomic_<atomic_optab><mode>"
 [(match_operand:ALLI 0 "aarch64_sync_memory_operand" "")
  (atomic_op:ALLI
   (match_operand:ALLI 1 "<atomic_op_operand>" "")
   (match_operand:SI 2 "const_int_operand"))]
  ""
  {
    rtx (*gen) (rtx, rtx, rtx);

    /* Use an atomic load-operate instruction when possible.  */
    if (aarch64_atomic_ldop_supported_p (<CODE>))
      gen = gen_aarch64_atomic_<atomic_optab><mode>_lse;
    else
      gen = gen_aarch64_atomic_<atomic_optab><mode>;

    emit_insn (gen (operands[0], operands[1], operands[2]));

    DONE;
  }
)

(define_insn_and_split "aarch64_atomic_<atomic_optab><mode>"
 [(set (match_operand:ALLI 0 "aarch64_sync_memory_operand" "+Q")
   (unspec_volatile:ALLI
    [(atomic_op:ALLI (match_dup 0)
      (match_operand:ALLI 1 "<atomic_op_operand>" "r<const_atomic>"))
     (match_operand:SI 2 "const_int_operand")]
    UNSPECV_ATOMIC_OP))
  (clobber (reg:CC CC_REGNUM))
  (clobber (match_scratch:ALLI 3 "=&r"))
  (clobber (match_scratch:SI 4 "=&r"))]
  ""
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (<CODE>, NULL, operands[3], operands[0],
			     operands[1], operands[2], operands[4]);
    DONE;
  }
)

(define_insn_and_split "aarch64_atomic_<atomic_optab><mode>_lse"
  [(set (match_operand:ALLI 0 "aarch64_sync_memory_operand" "+Q")
    (unspec_volatile:ALLI
      [(atomic_op:ALLI (match_dup 0)
	(match_operand:ALLI 1 "<atomic_op_operand>" "r<const_atomic>"))
       (match_operand:SI 2 "const_int_operand")]
      UNSPECV_ATOMIC_OP))
   (clobber (match_scratch:ALLI 3 "=&r"))]
  "TARGET_LSE"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_gen_atomic_ldop (<CODE>, operands[3], NULL, operands[0],
			     operands[1], operands[2]);
    DONE;
  }
)

(define_insn_and_split "atomic_nand<mode>"
  [(set (match_operand:ALLI 0 "aarch64_sync_memory_operand" "+Q")
    (unspec_volatile:ALLI
      [(not:ALLI
	(and:ALLI (match_dup 0)
	  (match_operand:ALLI 1 "aarch64_logical_operand" "r<lconst_atomic>")))
       (match_operand:SI 2 "const_int_operand")]		;; model
      UNSPECV_ATOMIC_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:ALLI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=&r"))]
  ""
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
     aarch64_split_atomic_op (NOT, NULL, operands[3], operands[0],
			     operands[1], operands[2], operands[4]);
     DONE;
  }
)

;; Load-operate-store, returning the updated memory data.

(define_expand "atomic_fetch_<atomic_optab><mode>"
 [(match_operand:ALLI 0 "register_operand" "")
  (match_operand:ALLI 1 "aarch64_sync_memory_operand" "")
  (atomic_op:ALLI
   (match_operand:ALLI 2 "<atomic_op_operand>" "")
   (match_operand:SI 3 "const_int_operand"))]
 ""
{
  rtx (*gen) (rtx, rtx, rtx, rtx);

  /* Use an atomic load-operate instruction when possible.  */
  if (aarch64_atomic_ldop_supported_p (<CODE>))
    gen = gen_aarch64_atomic_fetch_<atomic_optab><mode>_lse;
  else
    gen = gen_aarch64_atomic_fetch_<atomic_optab><mode>;

  emit_insn (gen (operands[0], operands[1], operands[2], operands[3]));

  DONE;
})

(define_insn_and_split "aarch64_atomic_fetch_<atomic_optab><mode>"
  [(set (match_operand:ALLI 0 "register_operand" "=&r")
    (match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q"))
   (set (match_dup 1)
    (unspec_volatile:ALLI
      [(atomic_op:ALLI (match_dup 1)
	(match_operand:ALLI 2 "<atomic_op_operand>" "r<const_atomic>"))
       (match_operand:SI 3 "const_int_operand")]		;; model
      UNSPECV_ATOMIC_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:ALLI 4 "=&r"))
   (clobber (match_scratch:SI 5 "=&r"))]
  ""
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (<CODE>, operands[0], operands[4], operands[1],
			     operands[2], operands[3], operands[5]);
    DONE;
  }
)

(define_insn_and_split "aarch64_atomic_fetch_<atomic_optab><mode>_lse"
  [(set (match_operand:ALLI 0 "register_operand" "=&r")
    (match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q"))
   (set (match_dup 1)
    (unspec_volatile:ALLI
      [(atomic_op:ALLI (match_dup 1)
	(match_operand:ALLI 2 "<atomic_op_operand>" "r<const_atomic>"))
       (match_operand:SI 3 "const_int_operand")]
      UNSPECV_ATOMIC_LDOP))]
  "TARGET_LSE"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_gen_atomic_ldop (<CODE>, operands[0], NULL, operands[1],
			     operands[2], operands[3]);
    DONE;
  }
)

(define_insn_and_split "atomic_fetch_nand<mode>"
  [(set (match_operand:ALLI 0 "register_operand" "=&r")
    (match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q"))
   (set (match_dup 1)
    (unspec_volatile:ALLI
      [(not:ALLI
	 (and:ALLI (match_dup 1)
	   (match_operand:ALLI 2 "aarch64_logical_operand" "r<lconst_atomic>")))
       (match_operand:SI 3 "const_int_operand")]		;; model
      UNSPECV_ATOMIC_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:ALLI 4 "=&r"))
   (clobber (match_scratch:SI 5 "=&r"))]
  ""
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (NOT, operands[0], operands[4], operands[1],
			    operands[2], operands[3], operands[5]);
    DONE;
  }
)

;; Load-operate-store, returning the original memory data.

(define_expand "atomic_<atomic_optab>_fetch<mode>"
 [(match_operand:ALLI 0 "register_operand" "")
  (atomic_op:ALLI
   (match_operand:ALLI 1 "aarch64_sync_memory_operand" "")
   (match_operand:ALLI 2 "<atomic_op_operand>" ""))
  (match_operand:SI 3 "const_int_operand")]
 ""
{
  rtx (*gen) (rtx, rtx, rtx, rtx);
  rtx value = operands[2];

  /* Use an atomic load-operate instruction when possible.  */
  if (aarch64_atomic_ldop_supported_p (<CODE>))
    gen = gen_aarch64_atomic_<atomic_optab>_fetch<mode>_lse;
  else
    gen = gen_aarch64_atomic_<atomic_optab>_fetch<mode>;

  emit_insn (gen (operands[0], operands[1], value, operands[3]));

  DONE;
})

(define_insn_and_split "aarch64_atomic_<atomic_optab>_fetch<mode>"
  [(set (match_operand:ALLI 0 "register_operand" "=&r")
    (atomic_op:ALLI
      (match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q")
      (match_operand:ALLI 2 "<atomic_op_operand>" "r<const_atomic>")))
   (set (match_dup 1)
    (unspec_volatile:ALLI
      [(match_dup 1) (match_dup 2)
       (match_operand:SI 3 "const_int_operand")]		;; model
      UNSPECV_ATOMIC_OP))
    (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 4 "=&r"))]
  ""
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (<CODE>, NULL, operands[0], operands[1],
			     operands[2], operands[3], operands[4]);
    DONE;
  }
)

(define_insn_and_split "aarch64_atomic_<atomic_optab>_fetch<mode>_lse"
  [(set (match_operand:ALLI 0 "register_operand" "=&r")
    (atomic_op:ALLI
     (match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q")
     (match_operand:ALLI 2 "<atomic_op_operand>" "r<const_atomic>")))
   (set (match_dup 1)
    (unspec_volatile:ALLI
      [(match_dup 1)
       (match_dup 2)
       (match_operand:SI 3 "const_int_operand")]
      UNSPECV_ATOMIC_LDOP))
     (clobber (match_scratch:ALLI 4 "=&r"))]
  "TARGET_LSE"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_gen_atomic_ldop (<CODE>, operands[4], operands[0], operands[1],
			     operands[2], operands[3]);
    DONE;
  }
)

(define_insn_and_split "atomic_nand_fetch<mode>"
  [(set (match_operand:ALLI 0 "register_operand" "=&r")
    (not:ALLI
      (and:ALLI
	(match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q")
	(match_operand:ALLI 2 "aarch64_logical_operand" "r<lconst_atomic>"))))
   (set (match_dup 1)
    (unspec_volatile:ALLI
      [(match_dup 1) (match_dup 2)
       (match_operand:SI 3 "const_int_operand")]		;; model
      UNSPECV_ATOMIC_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 4 "=&r"))]
  ""
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (NOT, NULL, operands[0], operands[1],
			    operands[2], operands[3], operands[4]);
    DONE;
  }
)

(define_insn "atomic_load<mode>"
  [(set (match_operand:ALLI 0 "register_operand" "=r")
    (unspec_volatile:ALLI
      [(match_operand:ALLI 1 "aarch64_sync_memory_operand" "Q")
       (match_operand:SI 2 "const_int_operand")]			;; model
      UNSPECV_LDA))]
  ""
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[2]));
    if (is_mm_relaxed (model) || is_mm_consume (model) || is_mm_release (model))
      return "ldr<atomic_sfx>\t%<w>0, %1";
    else
      return "ldar<atomic_sfx>\t%<w>0, %1";
  }
)

(define_insn "atomic_store<mode>"
  [(set (match_operand:ALLI 0 "aarch64_sync_memory_operand" "=Q")
    (unspec_volatile:ALLI
      [(match_operand:ALLI 1 "general_operand" "rZ")
       (match_operand:SI 2 "const_int_operand")]			;; model
      UNSPECV_STL))]
  ""
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[2]));
    if (is_mm_relaxed (model) || is_mm_consume (model) || is_mm_acquire (model))
      return "str<atomic_sfx>\t%<w>1, %0";
    else
      return "stlr<atomic_sfx>\t%<w>1, %0";
  }
)

(define_insn "aarch64_load_exclusive<mode>"
  [(set (match_operand:SI 0 "register_operand" "=r")
    (zero_extend:SI
      (unspec_volatile:SHORT
	[(match_operand:SHORT 1 "aarch64_sync_memory_operand" "Q")
	 (match_operand:SI 2 "const_int_operand")]
	UNSPECV_LX)))]
  ""
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[2]));
    if (is_mm_relaxed (model) || is_mm_consume (model) || is_mm_release (model))
      return "ldxr<atomic_sfx>\t%w0, %1";
    else
      return "ldaxr<atomic_sfx>\t%w0, %1";
  }
)

(define_insn "aarch64_load_exclusive<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
    (unspec_volatile:GPI
      [(match_operand:GPI 1 "aarch64_sync_memory_operand" "Q")
       (match_operand:SI 2 "const_int_operand")]
      UNSPECV_LX))]
  ""
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[2]));
    if (is_mm_relaxed (model) || is_mm_consume (model) || is_mm_release (model))
      return "ldxr\t%<w>0, %1";
    else
      return "ldaxr\t%<w>0, %1";
  }
)

(define_insn "aarch64_store_exclusive<mode>"
  [(set (match_operand:SI 0 "register_operand" "=&r")
    (unspec_volatile:SI [(const_int 0)] UNSPECV_SX))
   (set (match_operand:ALLI 1 "aarch64_sync_memory_operand" "=Q")
    (unspec_volatile:ALLI
      [(match_operand:ALLI 2 "aarch64_reg_or_zero" "rZ")
       (match_operand:SI 3 "const_int_operand")]
      UNSPECV_SX))]
  ""
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[3]));
    if (is_mm_relaxed (model) || is_mm_consume (model) || is_mm_acquire (model))
      return "stxr<atomic_sfx>\t%w0, %<w>2, %1";
    else
      return "stlxr<atomic_sfx>\t%w0, %<w>2, %1";
  }
)

(define_expand "mem_thread_fence"
  [(match_operand:SI 0 "const_int_operand" "")]
  ""
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[0]));
    if (!(is_mm_relaxed (model) || is_mm_consume (model)))
      emit_insn (gen_dmb (operands[0]));
    DONE;
  }
)

(define_expand "dmb"
  [(set (match_dup 1)
    (unspec:BLK [(match_dup 1) (match_operand:SI 0 "const_int_operand")]
     UNSPEC_MB))]
   ""
   {
    operands[1] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
    MEM_VOLATILE_P (operands[1]) = 1;
  }
)

(define_insn "*dmb"
  [(set (match_operand:BLK 0 "" "")
    (unspec:BLK [(match_dup 0) (match_operand:SI 1 "const_int_operand")]
     UNSPEC_MB))]
  ""
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[1]));
    if (is_mm_acquire (model))
      return "dmb\\tishld";
    else
      return "dmb\\tish";
  }
)

;; ARMv8.1-A LSE instructions.

;; Atomic swap with memory.
(define_insn "aarch64_atomic_swp<mode>"
 [(set (match_operand:ALLI 0 "register_operand" "+&r")
   (match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q"))
  (set (match_dup 1)
   (unspec_volatile:ALLI
    [(match_operand:ALLI 2 "register_operand" "r")
     (match_operand:SI 3 "const_int_operand" "")]
    UNSPECV_ATOMIC_SWP))]
  "TARGET_LSE && reload_completed"
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[3]));
    if (is_mm_relaxed (model))
      return "swp<atomic_sfx>\t%<w>2, %<w>0, %1";
    else if (is_mm_acquire (model) || is_mm_consume (model))
      return "swpa<atomic_sfx>\t%<w>2, %<w>0, %1";
    else if (is_mm_release (model))
      return "swpl<atomic_sfx>\t%<w>2, %<w>0, %1";
    else
      return "swpal<atomic_sfx>\t%<w>2, %<w>0, %1";
  })

;; Atomic compare-and-swap: HI and smaller modes.

(define_insn "aarch64_atomic_cas<mode>"
 [(set (match_operand:SI 0 "register_operand" "+&r")		  ;; out
   (zero_extend:SI
    (match_operand:SHORT 1 "aarch64_sync_memory_operand" "+Q")))  ;; memory.
  (set (match_dup 1)
   (unspec_volatile:SHORT
    [(match_dup 0)
     (match_operand:SHORT 2 "aarch64_reg_or_zero" "rZ")	;; value.
     (match_operand:SI 3 "const_int_operand" "")]	;; model.
    UNSPECV_ATOMIC_CAS))]
 "TARGET_LSE && reload_completed"
{
  enum memmodel model = memmodel_from_int (INTVAL (operands[3]));
  if (is_mm_relaxed (model))
    return "cas<atomic_sfx>\t%<w>0, %<w>2, %1";
  else if (is_mm_acquire (model) || is_mm_consume (model))
    return "casa<atomic_sfx>\t%<w>0, %<w>2, %1";
  else if (is_mm_release (model))
    return "casl<atomic_sfx>\t%<w>0, %<w>2, %1";
  else
    return "casal<atomic_sfx>\t%<w>0, %<w>2, %1";
})

;; Atomic compare-and-swap: SI and larger modes.

(define_insn "aarch64_atomic_cas<mode>"
 [(set (match_operand:GPI 0 "register_operand" "+&r")	      ;; out
   (match_operand:GPI 1 "aarch64_sync_memory_operand" "+Q"))  ;; memory.
  (set (match_dup 1)
   (unspec_volatile:GPI
    [(match_dup 0)
     (match_operand:GPI 2 "aarch64_reg_or_zero" "rZ")	;; value.
     (match_operand:SI 3 "const_int_operand" "")]	;; model.
    UNSPECV_ATOMIC_CAS))]
  "TARGET_LSE && reload_completed"
{
    enum memmodel model = memmodel_from_int (INTVAL (operands[3]));
    if (is_mm_relaxed (model))
      return "cas<atomic_sfx>\t%<w>0, %<w>2, %1";
    else if (is_mm_acquire (model) || is_mm_consume (model))
      return "casa<atomic_sfx>\t%<w>0, %<w>2, %1";
    else if (is_mm_release (model))
      return "casl<atomic_sfx>\t%<w>0, %<w>2, %1";
    else
      return "casal<atomic_sfx>\t%<w>0, %<w>2, %1";
})

;; Atomic load-op: Load data, operate, store result, keep data.

(define_insn "aarch64_atomic_load<atomic_ldop><mode>"
 [(set (match_operand:ALLI 0 "register_operand" "=r")
   (match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q"))
  (set (match_dup 1)
   (unspec_volatile:ALLI
    [(match_dup 1)
     (match_operand:ALLI 2 "register_operand")
     (match_operand:SI 3 "const_int_operand")]
    ATOMIC_LDOP))]
 "TARGET_LSE && reload_completed"
 {
   enum memmodel model = memmodel_from_int (INTVAL (operands[3]));
   if (is_mm_relaxed (model))
     return "ld<atomic_ldop><atomic_sfx>\t%<w>2, %<w>0, %1";
   else if (is_mm_acquire (model) || is_mm_consume (model))
     return "ld<atomic_ldop>a<atomic_sfx>\t%<w>2, %<w>0, %1";
   else if (is_mm_release (model))
     return "ld<atomic_ldop>l<atomic_sfx>\t%<w>2, %<w>0, %1";
   else
     return "ld<atomic_ldop>al<atomic_sfx>\t%<w>2, %<w>0, %1";
 })
