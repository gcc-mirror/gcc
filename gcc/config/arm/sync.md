;; Machine description for ARM processor synchronization primitives.
;; Copyright (C) 2010-2013 Free Software Foundation, Inc.
;; Written by Marcus Shawcroft (marcus.shawcroft@arm.com)
;; 64bit Atomics by Dave Gilbert (david.gilbert@linaro.org)
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
;; <http://www.gnu.org/licenses/>.  */

(define_mode_attr sync_predtab
  [(QI "TARGET_HAVE_LDREXBH && TARGET_HAVE_MEMORY_BARRIER")
   (HI "TARGET_HAVE_LDREXBH && TARGET_HAVE_MEMORY_BARRIER")
   (SI "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER")
   (DI "TARGET_HAVE_LDREXD && ARM_DOUBLEWORD_ALIGN
	&& TARGET_HAVE_MEMORY_BARRIER")])

(define_code_iterator syncop [plus minus ior xor and])

(define_code_attr sync_optab
  [(ior "or") (xor "xor") (and "and") (plus "add") (minus "sub")])

(define_mode_attr sync_sfx
  [(QI "b") (HI "h") (SI "") (DI "d")])

(define_expand "memory_barrier"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_MEMORY_BARRIER))]
  "TARGET_HAVE_MEMORY_BARRIER"
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*memory_barrier"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_MEMORY_BARRIER))]
  "TARGET_HAVE_MEMORY_BARRIER"
  {
    if (TARGET_HAVE_DMB)
      {
	/* Note we issue a system level barrier. We should consider issuing
	   a inner shareabilty zone barrier here instead, ie. "DMB ISH".  */
	/* ??? Differentiate based on SEQ_CST vs less strict?  */
	return "dmb\tsy";
      }

    if (TARGET_HAVE_DMB_MCR)
      return "mcr\tp15, 0, r0, c7, c10, 5";

    gcc_unreachable ();
  }
  [(set_attr "length" "4")
   (set_attr "conds" "unconditional")
   (set_attr "predicable" "no")])

;; Note that ldrd and vldr are *not* guaranteed to be single-copy atomic,
;; even for a 64-bit aligned address.  Instead we use a ldrexd unparied
;; with a store.
(define_expand "atomic_loaddi"
  [(match_operand:DI 0 "s_register_operand")		;; val out
   (match_operand:DI 1 "mem_noofs_operand")		;; memory
   (match_operand:SI 2 "const_int_operand")]		;; model
  "TARGET_HAVE_LDREXD && ARM_DOUBLEWORD_ALIGN"
{
  enum memmodel model = (enum memmodel) INTVAL (operands[2]);
  expand_mem_thread_fence (model);
  emit_insn (gen_atomic_loaddi_1 (operands[0], operands[1]));
  if (model == MEMMODEL_SEQ_CST)
    expand_mem_thread_fence (model);
  DONE;
})

(define_insn "atomic_loaddi_1"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "mem_noofs_operand" "Ua")]
		   UNSPEC_LL))]
  "TARGET_HAVE_LDREXD && ARM_DOUBLEWORD_ALIGN"
  "ldrexd%?\t%0, %H0, %C1"
  [(set_attr "predicable" "yes")])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "s_register_operand" "")		;; bool out
   (match_operand:QHSD 1 "s_register_operand" "")	;; val out
   (match_operand:QHSD 2 "mem_noofs_operand" "")	;; memory
   (match_operand:QHSD 3 "general_operand" "")		;; expected
   (match_operand:QHSD 4 "s_register_operand" "")	;; desired
   (match_operand:SI 5 "const_int_operand")		;; is_weak
   (match_operand:SI 6 "const_int_operand")		;; mod_s
   (match_operand:SI 7 "const_int_operand")]		;; mod_f
  "<sync_predtab>"
{
  arm_expand_compare_and_swap (operands);
  DONE;
})

(define_insn_and_split "atomic_compare_and_swap<mode>_1"
  [(set (reg:CC_Z CC_REGNUM)					;; bool out
	(unspec_volatile:CC_Z [(const_int 0)] VUNSPEC_ATOMIC_CAS))
   (set (match_operand:SI 0 "s_register_operand" "=&r")		;; val out
	(zero_extend:SI
	  (match_operand:NARROW 1 "mem_noofs_operand" "+Ua")))	;; memory
   (set (match_dup 1)
	(unspec_volatile:NARROW
	  [(match_operand:SI 2 "arm_add_operand" "rIL")		;; expected
	   (match_operand:NARROW 3 "s_register_operand" "r")	;; desired
	   (match_operand:SI 4 "const_int_operand")		;; is_weak
	   (match_operand:SI 5 "const_int_operand")		;; mod_s
	   (match_operand:SI 6 "const_int_operand")]		;; mod_f
	  VUNSPEC_ATOMIC_CAS))
   (clobber (match_scratch:SI 7 "=&r"))]
  "<sync_predtab>"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    arm_split_compare_and_swap (operands);
    DONE;
  })

(define_mode_attr cas_cmp_operand
  [(SI "arm_add_operand") (DI "cmpdi_operand")])
(define_mode_attr cas_cmp_str
  [(SI "rIL") (DI "rDi")])

(define_insn_and_split "atomic_compare_and_swap<mode>_1"
  [(set (reg:CC_Z CC_REGNUM)					;; bool out
	(unspec_volatile:CC_Z [(const_int 0)] VUNSPEC_ATOMIC_CAS))
   (set (match_operand:SIDI 0 "s_register_operand" "=&r")	;; val out
	(match_operand:SIDI 1 "mem_noofs_operand" "+Ua"))	;; memory
   (set (match_dup 1)
	(unspec_volatile:SIDI
	  [(match_operand:SIDI 2 "<cas_cmp_operand>" "<cas_cmp_str>") ;; expect
	   (match_operand:SIDI 3 "s_register_operand" "r")	;; desired
	   (match_operand:SI 4 "const_int_operand")		;; is_weak
	   (match_operand:SI 5 "const_int_operand")		;; mod_s
	   (match_operand:SI 6 "const_int_operand")]		;; mod_f
	  VUNSPEC_ATOMIC_CAS))
   (clobber (match_scratch:SI 7 "=&r"))]
  "<sync_predtab>"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    arm_split_compare_and_swap (operands);
    DONE;
  })

(define_insn_and_split "atomic_exchange<mode>"
  [(set (match_operand:QHSD 0 "s_register_operand" "=&r")	;; output
	(match_operand:QHSD 1 "mem_noofs_operand" "+Ua"))	;; memory
   (set (match_dup 1)
	(unspec_volatile:QHSD
	  [(match_operand:QHSD 2 "s_register_operand" "r")	;; input
	   (match_operand:SI 3 "const_int_operand" "")]		;; model
	  VUNSPEC_ATOMIC_XCHG))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 4 "=&r"))]
  "<sync_predtab>"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    arm_split_atomic_op (SET, operands[0], NULL, operands[1],
			 operands[2], operands[3], operands[4]);
    DONE;
  })

(define_mode_attr atomic_op_operand
  [(QI "reg_or_int_operand")
   (HI "reg_or_int_operand")
   (SI "reg_or_int_operand")
   (DI "s_register_operand")])

(define_mode_attr atomic_op_str
  [(QI "rn") (HI "rn") (SI "rn") (DI "r")])

(define_insn_and_split "atomic_<sync_optab><mode>"
  [(set (match_operand:QHSD 0 "mem_noofs_operand" "+Ua")
	(unspec_volatile:QHSD
	  [(syncop:QHSD (match_dup 0)
	     (match_operand:QHSD 1 "<atomic_op_operand>" "<atomic_op_str>"))
	   (match_operand:SI 2 "const_int_operand")]		;; model
	  VUNSPEC_ATOMIC_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:QHSD 3 "=&r"))
   (clobber (match_scratch:SI 4 "=&r"))]
  "<sync_predtab>"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    arm_split_atomic_op (<CODE>, NULL, operands[3], operands[0],
			 operands[1], operands[2], operands[4]);
    DONE;
  })

(define_insn_and_split "atomic_nand<mode>"
  [(set (match_operand:QHSD 0 "mem_noofs_operand" "+Ua")
	(unspec_volatile:QHSD
	  [(not:QHSD
	     (and:QHSD (match_dup 0)
	       (match_operand:QHSD 1 "<atomic_op_operand>" "<atomic_op_str>")))
	   (match_operand:SI 2 "const_int_operand")]		;; model
	  VUNSPEC_ATOMIC_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:QHSD 3 "=&r"))
   (clobber (match_scratch:SI 4 "=&r"))]
  "<sync_predtab>"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    arm_split_atomic_op (NOT, NULL, operands[3], operands[0],
			 operands[1], operands[2], operands[4]);
    DONE;
  })

(define_insn_and_split "atomic_fetch_<sync_optab><mode>"
  [(set (match_operand:QHSD 0 "s_register_operand" "=&r")
	(match_operand:QHSD 1 "mem_noofs_operand" "+Ua"))
   (set (match_dup 1)
	(unspec_volatile:QHSD
	  [(syncop:QHSD (match_dup 1)
	     (match_operand:QHSD 2 "<atomic_op_operand>" "<atomic_op_str>"))
	   (match_operand:SI 3 "const_int_operand")]		;; model
	  VUNSPEC_ATOMIC_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:QHSD 4 "=&r"))
   (clobber (match_scratch:SI 5 "=&r"))]
  "<sync_predtab>"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    arm_split_atomic_op (<CODE>, operands[0], operands[4], operands[1],
			 operands[2], operands[3], operands[5]);
    DONE;
  })

(define_insn_and_split "atomic_fetch_nand<mode>"
  [(set (match_operand:QHSD 0 "s_register_operand" "=&r")
	(match_operand:QHSD 1 "mem_noofs_operand" "+Ua"))
   (set (match_dup 1)
	(unspec_volatile:QHSD
	  [(not:QHSD
	     (and:QHSD (match_dup 1)
	       (match_operand:QHSD 2 "<atomic_op_operand>" "<atomic_op_str>")))
	   (match_operand:SI 3 "const_int_operand")]		;; model
	  VUNSPEC_ATOMIC_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:QHSD 4 "=&r"))
   (clobber (match_scratch:SI 5 "=&r"))]
  "<sync_predtab>"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    arm_split_atomic_op (NOT, operands[0], operands[4], operands[1],
			 operands[2], operands[3], operands[5]);
    DONE;
  })

(define_insn_and_split "atomic_<sync_optab>_fetch<mode>"
  [(set (match_operand:QHSD 0 "s_register_operand" "=&r")
	(syncop:QHSD
	  (match_operand:QHSD 1 "mem_noofs_operand" "+Ua")
	  (match_operand:QHSD 2 "<atomic_op_operand>" "<atomic_op_str>")))
   (set (match_dup 1)
	(unspec_volatile:QHSD
	  [(match_dup 1) (match_dup 2)
	   (match_operand:SI 3 "const_int_operand")]		;; model
	  VUNSPEC_ATOMIC_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 4 "=&r"))]
  "<sync_predtab>"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    arm_split_atomic_op (<CODE>, NULL, operands[0], operands[1],
			 operands[2], operands[3], operands[4]);
    DONE;
  })

(define_insn_and_split "atomic_nand_fetch<mode>"
  [(set (match_operand:QHSD 0 "s_register_operand" "=&r")
	(not:QHSD
	  (and:QHSD
	    (match_operand:QHSD 1 "mem_noofs_operand" "+Ua")
	    (match_operand:QHSD 2 "<atomic_op_operand>" "<atomic_op_str>"))))
   (set (match_dup 1)
	(unspec_volatile:QHSD
	  [(match_dup 1) (match_dup 2)
	   (match_operand:SI 3 "const_int_operand")]		;; model
	  VUNSPEC_ATOMIC_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 4 "=&r"))]
  "<sync_predtab>"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    arm_split_atomic_op (NOT, NULL, operands[0], operands[1],
			 operands[2], operands[3], operands[4]);
    DONE;
  })

(define_insn "arm_load_exclusive<mode>"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (zero_extend:SI
	  (unspec_volatile:NARROW
	    [(match_operand:NARROW 1 "mem_noofs_operand" "Ua")]
	    VUNSPEC_LL)))]
  "TARGET_HAVE_LDREXBH"
  "ldrex<sync_sfx>%?\t%0, %C1"
  [(set_attr "predicable" "yes")])

(define_insn "arm_load_exclusivesi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec_volatile:SI
	  [(match_operand:SI 1 "mem_noofs_operand" "Ua")]
	  VUNSPEC_LL))]
  "TARGET_HAVE_LDREX"
  "ldrex%?\t%0, %C1"
  [(set_attr "predicable" "yes")])

(define_insn "arm_load_exclusivedi"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec_volatile:DI
	  [(match_operand:DI 1 "mem_noofs_operand" "Ua")]
	  VUNSPEC_LL))]
  "TARGET_HAVE_LDREXD"
  "ldrexd%?\t%0, %H0, %C1"
  [(set_attr "predicable" "yes")])

(define_insn "arm_store_exclusive<mode>"
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
	(unspec_volatile:SI [(const_int 0)] VUNSPEC_SC))
   (set (match_operand:QHSD 1 "mem_noofs_operand" "=Ua")
	(unspec_volatile:QHSD
	  [(match_operand:QHSD 2 "s_register_operand" "r")]
	  VUNSPEC_SC))]
  "<sync_predtab>"
  {
    if (<MODE>mode == DImode)
      {
	rtx value = operands[2];
	/* The restrictions on target registers in ARM mode are that the two
	   registers are consecutive and the first one is even; Thumb is
	   actually more flexible, but DI should give us this anyway.
	   Note that the 1st register always gets the lowest word in memory.  */
	gcc_assert ((REGNO (value) & 1) == 0 || TARGET_THUMB2);
	operands[3] = gen_rtx_REG (SImode, REGNO (value) + 1);
	return "strexd%?\t%0, %2, %3, %C1";
      }
    return "strex<sync_sfx>%?\t%0, %2, %C1";
  }
  [(set_attr "predicable" "yes")])
