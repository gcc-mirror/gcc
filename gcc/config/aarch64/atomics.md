;; Machine description for AArch64 processor synchronization primitives.
;; Copyright (C) 2009-2013 Free Software Foundation, Inc.
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

(define_c_enum "unspecv"
 [
    UNSPECV_LX				; Represent a load-exclusive.
    UNSPECV_SX				; Represent a store-exclusive.
    UNSPECV_LDA				; Represent an atomic load or load-acquire.
    UNSPECV_STL				; Represent an atomic store or store-release.
    UNSPECV_ATOMIC_CMPSW		; Represent an atomic compare swap.
    UNSPECV_ATOMIC_EXCHG		; Represent an atomic exchange.
    UNSPECV_ATOMIC_OP			; Represent an atomic operation.
])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand" "")			;; bool out
   (match_operand:ALLI 1 "register_operand" "")			;; val out
   (match_operand:ALLI 2 "aarch64_sync_memory_operand" "")	;; memory
   (match_operand:ALLI 3 "general_operand" "")			;; expected
   (match_operand:ALLI 4 "register_operand" "")			;; desired
   (match_operand:SI 5 "const_int_operand")			;; is_weak
   (match_operand:SI 6 "const_int_operand")			;; mod_s
   (match_operand:SI 7 "const_int_operand")]			;; mod_f
  ""
  {
    aarch64_expand_compare_and_swap (operands);
    DONE;
  }
)

(define_insn_and_split "atomic_compare_and_swap<mode>_1"
  [(set (reg:CC CC_REGNUM)					;; bool out
    (unspec_volatile:CC [(const_int 0)] UNSPECV_ATOMIC_CMPSW))
   (set (match_operand:SI 0 "register_operand" "=&r")		;; val out
    (zero_extend:SI
      (match_operand:SHORT 1 "aarch64_sync_memory_operand" "+Q"))) ;; memory
   (set (match_dup 1)
    (unspec_volatile:SHORT
      [(match_operand:SI 2 "aarch64_plus_operand" "rI")	;; expected
       (match_operand:SHORT 3 "register_operand" "r")	;; desired
       (match_operand:SI 4 "const_int_operand")		;; is_weak
       (match_operand:SI 5 "const_int_operand")		;; mod_s
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

(define_insn_and_split "atomic_compare_and_swap<mode>_1"
  [(set (reg:CC CC_REGNUM)					;; bool out
    (unspec_volatile:CC [(const_int 0)] UNSPECV_ATOMIC_CMPSW))
   (set (match_operand:GPI 0 "register_operand" "=&r")		;; val out
    (match_operand:GPI 1 "aarch64_sync_memory_operand" "+Q")) ;; memory
   (set (match_dup 1)
    (unspec_volatile:GPI
      [(match_operand:GPI 2 "aarch64_plus_operand" "rI")	;; expect
       (match_operand:GPI 3 "register_operand" "r")		;; desired
       (match_operand:SI 4 "const_int_operand")		;; is_weak
       (match_operand:SI 5 "const_int_operand")		;; mod_s
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

(define_insn_and_split "atomic_exchange<mode>"
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

(define_insn_and_split "atomic_<atomic_optab><mode>"
  [(set (match_operand:ALLI 0 "aarch64_sync_memory_operand" "+Q")
    (unspec_volatile:ALLI
      [(atomic_op:ALLI (match_dup 0)
	(match_operand:ALLI 1 "<atomic_op_operand>" "r<lconst_atomic>"))
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
    aarch64_split_atomic_op (<CODE>, NULL, operands[3], operands[0],
			    operands[1], operands[2], operands[4]);
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

(define_insn_and_split "atomic_fetch_<atomic_optab><mode>"
  [(set (match_operand:ALLI 0 "register_operand" "=&r")
    (match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q"))
   (set (match_dup 1)
    (unspec_volatile:ALLI
      [(atomic_op:ALLI (match_dup 1)
	(match_operand:ALLI 2 "<atomic_op_operand>" "r<lconst_atomic>"))
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

(define_insn_and_split "atomic_<atomic_optab>_fetch<mode>"
  [(set (match_operand:ALLI 0 "register_operand" "=&r")
    (atomic_op:ALLI
      (match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q")
      (match_operand:ALLI 2 "<atomic_op_operand>" "r<lconst_atomic>")))
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
    enum memmodel model = (enum memmodel) INTVAL (operands[2]);
    if (model == MEMMODEL_RELAXED
	|| model == MEMMODEL_CONSUME
	|| model == MEMMODEL_RELEASE)
      return "ldr<atomic_sfx>\t%<w>0, %1";
    else
      return "ldar<atomic_sfx>\t%<w>0, %1";
  }
)

(define_insn "atomic_store<mode>"
  [(set (match_operand:ALLI 0 "memory_operand" "=Q")
    (unspec_volatile:ALLI
      [(match_operand:ALLI 1 "general_operand" "rZ")
       (match_operand:SI 2 "const_int_operand")]			;; model
      UNSPECV_STL))]
  ""
  {
    enum memmodel model = (enum memmodel) INTVAL (operands[2]);
    if (model == MEMMODEL_RELAXED
	|| model == MEMMODEL_CONSUME
	|| model == MEMMODEL_ACQUIRE)
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
    enum memmodel model = (enum memmodel) INTVAL (operands[2]);
    if (model == MEMMODEL_RELAXED
	|| model == MEMMODEL_CONSUME
	|| model == MEMMODEL_RELEASE)
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
    enum memmodel model = (enum memmodel) INTVAL (operands[2]);
    if (model == MEMMODEL_RELAXED
	|| model == MEMMODEL_CONSUME
	|| model == MEMMODEL_RELEASE)
      return "ldxr\t%<w>0, %1";
    else
      return "ldaxr\t%<w>0, %1";
  }
)

(define_insn "aarch64_store_exclusive<mode>"
  [(set (match_operand:SI 0 "register_operand" "=r")
    (unspec_volatile:SI [(const_int 0)] UNSPECV_SX))
   (set (match_operand:ALLI 1 "aarch64_sync_memory_operand" "=Q")
    (unspec_volatile:ALLI
      [(match_operand:ALLI 2 "register_operand" "r")
       (match_operand:SI 3 "const_int_operand")]
      UNSPECV_SX))]
  ""
  {
    enum memmodel model = (enum memmodel) INTVAL (operands[3]);
    if (model == MEMMODEL_RELAXED
	|| model == MEMMODEL_CONSUME
	|| model == MEMMODEL_ACQUIRE)
      return "stxr<atomic_sfx>\t%w0, %<w>2, %1";
    else
      return "stlxr<atomic_sfx>\t%w0, %<w>2, %1";
  }
)

(define_expand "mem_thread_fence"
  [(match_operand:SI 0 "const_int_operand" "")]
  ""
  {
    enum memmodel model = (enum memmodel) INTVAL (operands[0]);
    if (model != MEMMODEL_RELAXED && model != MEMMODEL_CONSUME)
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
    enum memmodel model = (enum memmodel) INTVAL (operands[1]);
    if (model == MEMMODEL_ACQUIRE)
      return "dmb\\tishld";
    else
      return "dmb\\tish";
  }
)
