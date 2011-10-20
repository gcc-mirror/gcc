;; Machine description for ARM processor synchronization primitives.
;; Copyright (C) 2010 Free Software Foundation, Inc.
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

;; ARMV6 introduced ldrex and strex instruction. These instruction
;; access SI width data. In order to implement synchronization
;; primitives for the narrower QI and HI modes we insert appropriate
;; AND/OR sequences into the synchronization loop to mask out the
;; relevant component of an SI access.

(define_expand "memory_barrier"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_MEMORY_BARRIER))]
  "TARGET_HAVE_MEMORY_BARRIER"
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})


(define_mode_attr sync_predtab [(SI "TARGET_HAVE_LDREX &&
					TARGET_HAVE_MEMORY_BARRIER")
				(QI "TARGET_HAVE_LDREXBH &&
					TARGET_HAVE_MEMORY_BARRIER")
				(HI "TARGET_HAVE_LDREXBH &&
					TARGET_HAVE_MEMORY_BARRIER")
				(DI "TARGET_HAVE_LDREXD &&
					ARM_DOUBLEWORD_ALIGN &&
					TARGET_HAVE_MEMORY_BARRIER")])

(define_expand "sync_compare_and_swap<mode>"
  [(set (match_operand:QHSD 0 "s_register_operand")
        (unspec_volatile:QHSD [(match_operand:QHSD 1 "memory_operand")
			     (match_operand:QHSD 2 "s_register_operand")
			     (match_operand:QHSD 3 "s_register_operand")]
			     VUNSPEC_SYNC_COMPARE_AND_SWAP))]
  "<sync_predtab>"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omrn;
    generator.u.omrn = gen_arm_sync_compare_and_swap<mode>;
    arm_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
                     operands[2], operands[3]);
    DONE;
  })

(define_expand "sync_lock_test_and_set<mode>"
  [(match_operand:QHSD 0 "s_register_operand")
   (match_operand:QHSD 1 "memory_operand")
   (match_operand:QHSD 2 "s_register_operand")]
  "<sync_predtab>"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_lock_test_and_set<mode>;
    arm_expand_sync (<MODE>mode, &generator, operands[0], operands[1], NULL,
                     operands[2]);
    DONE;
  })

(define_code_iterator syncop [plus minus ior xor and])

(define_code_attr sync_optab [(ior "ior")
			      (xor "xor")
			      (and "and")
			      (plus "add")
			      (minus "sub")])

(define_code_attr sync_clobber [(ior "=&r")
				(and "=&r")
				(xor "X")
				(plus "X")
				(minus "X")])

(define_code_attr sync_t2_reqd [(ior "4")
				(and "4")
				(xor "*")
				(plus "*")
				(minus "*")])

(define_expand "sync_<sync_optab><mode>"
  [(match_operand:QHSD 0 "memory_operand")
   (match_operand:QHSD 1 "s_register_operand")
   (syncop:QHSD (match_dup 0) (match_dup 1))]
  "<sync_predtab>"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_new_<sync_optab><mode>;
    arm_expand_sync (<MODE>mode, &generator, NULL, operands[0], NULL,
		     operands[1]);
    DONE;
  })

(define_expand "sync_nand<mode>"
  [(match_operand:QHSD 0 "memory_operand")
   (match_operand:QHSD 1 "s_register_operand")
   (not:QHSD (and:QHSD (match_dup 0) (match_dup 1)))]
  "<sync_predtab>"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_new_nand<mode>;
    arm_expand_sync (<MODE>mode, &generator, NULL, operands[0], NULL,
                     operands[1]);
    DONE;
  })

(define_expand "sync_new_<sync_optab><mode>"
  [(match_operand:QHSD 0 "s_register_operand")
   (match_operand:QHSD 1 "memory_operand")
   (match_operand:QHSD 2 "s_register_operand")
   (syncop:QHSD (match_dup 1) (match_dup 2))]
  "<sync_predtab>"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_new_<sync_optab><mode>;
    arm_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
		     NULL, operands[2]);
    DONE;
  })

(define_expand "sync_new_nand<mode>"
  [(match_operand:QHSD 0 "s_register_operand")
   (match_operand:QHSD 1 "memory_operand")
   (match_operand:QHSD 2 "s_register_operand")
   (not:QHSD (and:QHSD (match_dup 1) (match_dup 2)))]
  "<sync_predtab>"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_new_nand<mode>;
    arm_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
    		     NULL, operands[2]);
    DONE;
  });

(define_expand "sync_old_<sync_optab><mode>"
  [(match_operand:QHSD 0 "s_register_operand")
   (match_operand:QHSD 1 "memory_operand")
   (match_operand:QHSD 2 "s_register_operand")
   (syncop:QHSD (match_dup 1) (match_dup 2))]
  "<sync_predtab>"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_old_<sync_optab><mode>;
    arm_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
		     NULL, operands[2]);
    DONE;
  })

(define_expand "sync_old_nand<mode>"
  [(match_operand:QHSD 0 "s_register_operand")
   (match_operand:QHSD 1 "memory_operand")
   (match_operand:QHSD 2 "s_register_operand")
   (not:QHSD (and:QHSD (match_dup 1) (match_dup 2)))]
  "<sync_predtab>"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_old_nand<mode>;
    arm_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
                     NULL, operands[2]);
    DONE;
  })

(define_insn "arm_sync_compare_and_swap<mode>"
  [(set (match_operand:SIDI 0 "s_register_operand" "=&r")
        (unspec_volatile:SIDI
	 [(match_operand:SIDI 1 "arm_sync_memory_operand" "+Q")
	  (match_operand:SIDI 2 "s_register_operand" "r")
	  (match_operand:SIDI 3 "s_register_operand" "r")]
	 VUNSPEC_SYNC_COMPARE_AND_SWAP))
   (set (match_dup 1) (unspec_volatile:SIDI [(match_dup 2)]
                                          VUNSPEC_SYNC_COMPARE_AND_SWAP))
   (set (reg:CC CC_REGNUM) (unspec_volatile:CC [(match_dup 1)]
                                                VUNSPEC_SYNC_COMPARE_AND_SWAP))
   ]
  "<sync_predtab>"
  {
    return arm_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_required_value"  "2")
   (set_attr "sync_new_value"       "3")
   (set_attr "sync_t1"              "0")
   (set_attr "conds" "clob")
   (set_attr "predicable" "no")])

(define_insn "arm_sync_compare_and_swap<mode>"
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
        (zero_extend:SI
	  (unspec_volatile:NARROW
	    [(match_operand:NARROW 1 "arm_sync_memory_operand" "+Q")
	     (match_operand:SI 2 "s_register_operand" "r")
	     (match_operand:SI 3 "s_register_operand" "r")]
	    VUNSPEC_SYNC_COMPARE_AND_SWAP)))
   (set (match_dup 1) (unspec_volatile:NARROW [(match_dup 2)]
                                          VUNSPEC_SYNC_COMPARE_AND_SWAP))
   (set (reg:CC CC_REGNUM) (unspec_volatile:CC [(match_dup 1)]
                                                VUNSPEC_SYNC_COMPARE_AND_SWAP))
   ]
  "<sync_predtab>"
  {
    return arm_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_required_value"  "2")
   (set_attr "sync_new_value"       "3")
   (set_attr "sync_t1"              "0")
   (set_attr "conds" "clob")
   (set_attr "predicable" "no")])

(define_insn "arm_sync_lock_test_and_set<mode>"
  [(set (match_operand:SIDI 0 "s_register_operand" "=&r")
	(match_operand:SIDI 1 "arm_sync_memory_operand" "+Q"))
   (set (match_dup 1)
	(unspec_volatile:SIDI [(match_operand:SIDI 2 "s_register_operand" "r")]
	VUNSPEC_SYNC_LOCK))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))]
  "<sync_predtab>"
  {
    return arm_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_release_barrier" "no")
   (set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "3")
   (set_attr "conds" "clob")
   (set_attr "predicable" "no")])

(define_insn "arm_sync_lock_test_and_set<mode>"
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
        (zero_extend:SI (match_operand:NARROW 1 "arm_sync_memory_operand" "+Q")))
   (set (match_dup 1)
        (unspec_volatile:NARROW [(match_operand:SI 2 "s_register_operand" "r")]
				VUNSPEC_SYNC_LOCK))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))]
  "<sync_predtab>"
  {
    return arm_output_sync_insn (insn, operands);
  } 
  [(set_attr "sync_release_barrier" "no")
   (set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "3")
   (set_attr "conds" "clob")
   (set_attr "predicable" "no")])

(define_insn "arm_sync_new_<sync_optab><mode>"
  [(set (match_operand:SIDI 0 "s_register_operand" "=&r")
        (unspec_volatile:SIDI [(syncop:SIDI
			       (match_operand:SIDI 1 "arm_sync_memory_operand" "+Q")
			       (match_operand:SIDI 2 "s_register_operand" "r"))
			    ]
			    VUNSPEC_SYNC_NEW_OP))
   (set (match_dup 1)
	(unspec_volatile:SIDI [(match_dup 1) (match_dup 2)]
			    VUNSPEC_SYNC_NEW_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))]
  "<sync_predtab>"
  {
    return arm_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "3")
   (set_attr "sync_op"              "<sync_optab>")
   (set_attr "conds" "clob")
   (set_attr "predicable" "no")])

(define_insn "arm_sync_new_<sync_optab><mode>"
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
        (unspec_volatile:SI [(syncop:SI
			       (zero_extend:SI
				 (match_operand:NARROW 1 "arm_sync_memory_operand" "+Q"))
			       (match_operand:SI 2 "s_register_operand" "r"))
			    ]
			    VUNSPEC_SYNC_NEW_OP))
   (set (match_dup 1)
	(unspec_volatile:NARROW [(match_dup 1) (match_dup 2)]
				VUNSPEC_SYNC_NEW_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))]
  "<sync_predtab>"
  {
    return arm_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "3")
   (set_attr "sync_op"              "<sync_optab>")
   (set_attr "conds" "clob")
   (set_attr "predicable" "no")])

(define_insn "arm_sync_new_nand<mode>"
  [(set (match_operand:SIDI 0 "s_register_operand" "=&r")
        (unspec_volatile:SIDI [(not:SIDI (and:SIDI
			       (match_operand:SIDI 1 "arm_sync_memory_operand" "+Q")
			       (match_operand:SIDI 2 "s_register_operand" "r")))
			    ]
			    VUNSPEC_SYNC_NEW_OP))
   (set (match_dup 1)
	(unspec_volatile:SIDI [(match_dup 1) (match_dup 2)]
			    VUNSPEC_SYNC_NEW_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))]
  "<sync_predtab>"
  {
    return arm_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "3")
   (set_attr "sync_op"              "nand")
   (set_attr "conds" "clob")
   (set_attr "predicable" "no")])

(define_insn "arm_sync_new_nand<mode>"
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
        (unspec_volatile:SI
	  [(not:SI
	     (and:SI
	       (zero_extend:SI
		 (match_operand:NARROW 1 "arm_sync_memory_operand" "+Q"))
	       (match_operand:SI 2 "s_register_operand" "r")))
	  ] VUNSPEC_SYNC_NEW_OP))
   (set (match_dup 1)
        (unspec_volatile:NARROW [(match_dup 1) (match_dup 2)]
				VUNSPEC_SYNC_NEW_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))]
  "<sync_predtab>"
  {
    return arm_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "3")
   (set_attr "sync_op"              "nand")
   (set_attr "conds" "clob")
   (set_attr "predicable" "no")])

(define_insn "arm_sync_old_<sync_optab><mode>"
  [(set (match_operand:SIDI 0 "s_register_operand" "=&r")
	(unspec_volatile:SIDI [(syncop:SIDI
			       (match_operand:SIDI 1 "arm_sync_memory_operand" "+Q")
			       (match_operand:SIDI 2 "s_register_operand" "r"))
			    ]
			    VUNSPEC_SYNC_OLD_OP))
   (set (match_dup 1)
        (unspec_volatile:SIDI [(match_dup 1) (match_dup 2)]
			      VUNSPEC_SYNC_OLD_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SIDI 3 "=&r"))
   (clobber (match_scratch:SI 4 "<sync_clobber>"))]
  "<sync_predtab>"
  {
    return arm_output_sync_insn (insn, operands);
  } 
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "3")
   (set_attr "sync_t2"              "<sync_t2_reqd>")
   (set_attr "sync_op"              "<sync_optab>")
   (set_attr "conds" "clob")
   (set_attr "predicable" "no")])

(define_insn "arm_sync_old_<sync_optab><mode>"
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
        (unspec_volatile:SI [(syncop:SI
			       (zero_extend:SI
				 (match_operand:NARROW 1 "arm_sync_memory_operand" "+Q"))
			       (match_operand:SI 2 "s_register_operand" "r"))
			    ]
			    VUNSPEC_SYNC_OLD_OP))
   (set (match_dup 1)
	(unspec_volatile:NARROW [(match_dup 1) (match_dup 2)]
			    VUNSPEC_SYNC_OLD_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "<sync_clobber>"))]
  "<sync_predtab>"
  {
    return arm_output_sync_insn (insn, operands);
  } 
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "3")
   (set_attr "sync_t2"              "<sync_t2_reqd>")
   (set_attr "sync_op"              "<sync_optab>")
   (set_attr "conds" 		    "clob")
   (set_attr "predicable" "no")])

(define_insn "arm_sync_old_nand<mode>"
  [(set (match_operand:SIDI 0 "s_register_operand" "=&r")
	(unspec_volatile:SIDI [(not:SIDI (and:SIDI
			       (match_operand:SIDI 1 "arm_sync_memory_operand" "+Q")
			       (match_operand:SIDI 2 "s_register_operand" "r")))
			    ]
			    VUNSPEC_SYNC_OLD_OP))
   (set (match_dup 1)
        (unspec_volatile:SIDI [(match_dup 1) (match_dup 2)]
	                    VUNSPEC_SYNC_OLD_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SIDI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=&r"))]
  "<sync_predtab>"
  {
    return arm_output_sync_insn (insn, operands);
  } 
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "3")
   (set_attr "sync_t2"              "4")
   (set_attr "sync_op"              "nand")
   (set_attr "conds" 		    "clob")
   (set_attr "predicable" "no")])

(define_insn "arm_sync_old_nand<mode>"
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
	(unspec_volatile:SI [(not:SI (and:SI
			       (zero_extend:SI
				 (match_operand:NARROW 1 "arm_sync_memory_operand" "+Q"))
			       (match_operand:SI 2 "s_register_operand" "r")))
			    ]
			    VUNSPEC_SYNC_OLD_OP))
   (set (match_dup 1)
	(unspec_volatile:NARROW [(match_dup 1) (match_dup 2)]
			    VUNSPEC_SYNC_OLD_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=&r"))]
  "<sync_predtab>"
  {
    return arm_output_sync_insn (insn, operands);
  } 
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "3")
   (set_attr "sync_t2"              "4")
   (set_attr "sync_op"              "nand")
   (set_attr "conds"                "clob")
   (set_attr "predicable" "no")])

(define_insn "*memory_barrier"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_MEMORY_BARRIER))]
  "TARGET_HAVE_MEMORY_BARRIER"
  {
    return arm_output_memory_barrier (operands);
  }
  [(set_attr "length" "4")
   (set_attr "conds" "unconditional")
   (set_attr "predicable" "no")])

