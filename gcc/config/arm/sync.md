;; Machine description for ARM processor synchronization primitives.
;; Copyright (C) 2010 Free Software Foundation, Inc.
;; Written by Marcus Shawcroft (marcus.shawcroft@arm.com)
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

(define_expand "sync_compare_and_swapsi"
  [(set (match_operand:SI 0 "s_register_operand")
        (unspec_volatile:SI [(match_operand:SI 1 "memory_operand")
			     (match_operand:SI 2 "s_register_operand")
			     (match_operand:SI 3 "s_register_operand")]
			     VUNSPEC_SYNC_COMPARE_AND_SWAP))]
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omrn;
    generator.u.omrn = gen_arm_sync_compare_and_swapsi;
    arm_expand_sync (SImode, &generator, operands[0], operands[1], operands[2],
                     operands[3]);
    DONE;
  })

(define_mode_iterator NARROW [QI HI])

(define_expand "sync_compare_and_swap<mode>"
  [(set (match_operand:NARROW 0 "s_register_operand")
        (unspec_volatile:NARROW [(match_operand:NARROW 1 "memory_operand")
			     (match_operand:NARROW 2 "s_register_operand")
			     (match_operand:NARROW 3 "s_register_operand")]
			     VUNSPEC_SYNC_COMPARE_AND_SWAP))]
  "TARGET_HAVE_LDREXBHD && TARGET_HAVE_MEMORY_BARRIER"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omrn;
    generator.u.omrn = gen_arm_sync_compare_and_swap<mode>;
    arm_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
                     operands[2], operands[3]);
    DONE;
  })

(define_expand "sync_lock_test_and_setsi"
  [(match_operand:SI 0 "s_register_operand")
   (match_operand:SI 1 "memory_operand")
   (match_operand:SI 2 "s_register_operand")]
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_lock_test_and_setsi;
    arm_expand_sync (SImode, &generator, operands[0], operands[1], NULL,
                     operands[2]);
    DONE;
  })

(define_expand "sync_lock_test_and_set<mode>"
  [(match_operand:NARROW 0 "s_register_operand")
   (match_operand:NARROW 1 "memory_operand")
   (match_operand:NARROW 2 "s_register_operand")]
  "TARGET_HAVE_LDREXBHD && TARGET_HAVE_MEMORY_BARRIER"
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

(define_expand "sync_<sync_optab>si"
  [(match_operand:SI 0 "memory_operand")
   (match_operand:SI 1 "s_register_operand")
   (syncop:SI (match_dup 0) (match_dup 1))]
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_new_<sync_optab>si;
    arm_expand_sync (SImode, &generator, NULL, operands[0], NULL, operands[1]);
    DONE;
  })

(define_expand "sync_nandsi"
  [(match_operand:SI 0 "memory_operand")
   (match_operand:SI 1 "s_register_operand")
   (not:SI (and:SI (match_dup 0) (match_dup 1)))]
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_new_nandsi;
    arm_expand_sync (SImode, &generator, NULL, operands[0], NULL, operands[1]);
    DONE;
  })

(define_expand "sync_<sync_optab><mode>"
  [(match_operand:NARROW 0 "memory_operand")
   (match_operand:NARROW 1 "s_register_operand")
   (syncop:NARROW (match_dup 0) (match_dup 1))]
  "TARGET_HAVE_LDREXBHD && TARGET_HAVE_MEMORY_BARRIER"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_new_<sync_optab><mode>;
    arm_expand_sync (<MODE>mode, &generator, NULL, operands[0], NULL,
    		     operands[1]);
    DONE;
  })

(define_expand "sync_nand<mode>"
  [(match_operand:NARROW 0 "memory_operand")
   (match_operand:NARROW 1 "s_register_operand")
   (not:NARROW (and:NARROW (match_dup 0) (match_dup 1)))]
  "TARGET_HAVE_LDREXBHD && TARGET_HAVE_MEMORY_BARRIER"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_new_nand<mode>;
    arm_expand_sync (<MODE>mode, &generator, NULL, operands[0], NULL,
                     operands[1]);
    DONE;
  })

(define_expand "sync_new_<sync_optab>si"
  [(match_operand:SI 0 "s_register_operand")
   (match_operand:SI 1 "memory_operand")
   (match_operand:SI 2 "s_register_operand")
   (syncop:SI (match_dup 1) (match_dup 2))]
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_new_<sync_optab>si;
    arm_expand_sync (SImode, &generator, operands[0], operands[1], NULL,
                     operands[2]);
    DONE;
  })

(define_expand "sync_new_nandsi"
  [(match_operand:SI 0 "s_register_operand")
   (match_operand:SI 1 "memory_operand")
   (match_operand:SI 2 "s_register_operand")
   (not:SI (and:SI (match_dup 1) (match_dup 2)))]
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_new_nandsi;
    arm_expand_sync (SImode, &generator, operands[0], operands[1], NULL,
    		     operands[2]);
    DONE;
  })

(define_expand "sync_new_<sync_optab><mode>"
  [(match_operand:NARROW 0 "s_register_operand")
   (match_operand:NARROW 1 "memory_operand")
   (match_operand:NARROW 2 "s_register_operand")
   (syncop:NARROW (match_dup 1) (match_dup 2))]
  "TARGET_HAVE_LDREXBHD && TARGET_HAVE_MEMORY_BARRIER"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_new_<sync_optab><mode>;
    arm_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
    		     NULL, operands[2]);
    DONE;
  })

(define_expand "sync_new_nand<mode>"
  [(match_operand:NARROW 0 "s_register_operand")
   (match_operand:NARROW 1 "memory_operand")
   (match_operand:NARROW 2 "s_register_operand")
   (not:NARROW (and:NARROW (match_dup 1) (match_dup 2)))]
  "TARGET_HAVE_LDREXBHD && TARGET_HAVE_MEMORY_BARRIER"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_new_nand<mode>;
    arm_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
    		     NULL, operands[2]);
    DONE;
  });

(define_expand "sync_old_<sync_optab>si"
  [(match_operand:SI 0 "s_register_operand")
   (match_operand:SI 1 "memory_operand")
   (match_operand:SI 2 "s_register_operand")
   (syncop:SI (match_dup 1) (match_dup 2))]
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_old_<sync_optab>si;
    arm_expand_sync (SImode, &generator, operands[0], operands[1], NULL,
                     operands[2]);
    DONE;
  })

(define_expand "sync_old_nandsi"
  [(match_operand:SI 0 "s_register_operand")
   (match_operand:SI 1 "memory_operand")
   (match_operand:SI 2 "s_register_operand")
   (not:SI (and:SI (match_dup 1) (match_dup 2)))]
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_old_nandsi;
    arm_expand_sync (SImode, &generator, operands[0], operands[1], NULL,
                     operands[2]);
    DONE;
  })

(define_expand "sync_old_<sync_optab><mode>"
  [(match_operand:NARROW 0 "s_register_operand")
   (match_operand:NARROW 1 "memory_operand")
   (match_operand:NARROW 2 "s_register_operand")
   (syncop:NARROW (match_dup 1) (match_dup 2))]
  "TARGET_HAVE_LDREXBHD && TARGET_HAVE_MEMORY_BARRIER"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_old_<sync_optab><mode>;
    arm_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
    		     NULL, operands[2]);
    DONE;
  })

(define_expand "sync_old_nand<mode>"
  [(match_operand:NARROW 0 "s_register_operand")
   (match_operand:NARROW 1 "memory_operand")
   (match_operand:NARROW 2 "s_register_operand")
   (not:NARROW (and:NARROW (match_dup 1) (match_dup 2)))]
  "TARGET_HAVE_LDREXBHD && TARGET_HAVE_MEMORY_BARRIER"
  {
    struct arm_sync_generator generator;
    generator.op = arm_sync_generator_omn;
    generator.u.omn = gen_arm_sync_old_nand<mode>;
    arm_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
                     NULL, operands[2]);
    DONE;
  })

(define_insn "arm_sync_compare_and_swapsi"
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
        (unspec_volatile:SI
	  [(match_operand:SI 1 "arm_sync_memory_operand" "+Q")
   	   (match_operand:SI 2 "s_register_operand" "r")
	   (match_operand:SI 3 "s_register_operand" "r")]
	  VUNSPEC_SYNC_COMPARE_AND_SWAP))
   (set (match_dup 1) (unspec_volatile:SI [(match_dup 2)]
                                          VUNSPEC_SYNC_COMPARE_AND_SWAP))
   (clobber:SI (match_scratch:SI 4 "=&r"))
   (set (reg:CC CC_REGNUM) (unspec_volatile:CC [(match_dup 1)]
                                                VUNSPEC_SYNC_COMPARE_AND_SWAP))
   ]
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
  {
    return arm_output_sync_insn (insn, operands);
  } 
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_required_value"  "2")
   (set_attr "sync_new_value"       "3")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "4")
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
   (clobber:SI (match_scratch:SI 4 "=&r"))
   (set (reg:CC CC_REGNUM) (unspec_volatile:CC [(match_dup 1)]
                                                VUNSPEC_SYNC_COMPARE_AND_SWAP))
   ]
  "TARGET_HAVE_LDREXBHD && TARGET_HAVE_MEMORY_BARRIER"
  {
    return arm_output_sync_insn (insn, operands);
  } 
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_required_value"  "2")
   (set_attr "sync_new_value"       "3")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "4")
   (set_attr "conds" "clob")
   (set_attr "predicable" "no")])

(define_insn "arm_sync_lock_test_and_setsi"
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
        (match_operand:SI 1 "arm_sync_memory_operand" "+Q"))
   (set (match_dup 1)
        (unspec_volatile:SI [(match_operand:SI 2 "s_register_operand" "r")]
	                    VUNSPEC_SYNC_LOCK))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))]
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
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
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
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

(define_insn "arm_sync_new_<sync_optab>si"
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
        (unspec_volatile:SI [(syncop:SI
                               (match_operand:SI 1 "arm_sync_memory_operand" "+Q")
                               (match_operand:SI 2 "s_register_operand" "r"))
	                    ]
	                    VUNSPEC_SYNC_NEW_OP))
   (set (match_dup 1)
        (unspec_volatile:SI [(match_dup 1) (match_dup 2)]
	                    VUNSPEC_SYNC_NEW_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))]
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
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

(define_insn "arm_sync_new_nandsi"
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
        (unspec_volatile:SI [(not:SI (and:SI
                               (match_operand:SI 1 "arm_sync_memory_operand" "+Q")
                               (match_operand:SI 2 "s_register_operand" "r")))
	                    ]
	                    VUNSPEC_SYNC_NEW_OP))
   (set (match_dup 1)
        (unspec_volatile:SI [(match_dup 1) (match_dup 2)]
	                    VUNSPEC_SYNC_NEW_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))]
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
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
  "TARGET_HAVE_LDREXBHD && TARGET_HAVE_MEMORY_BARRIER"
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
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
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

(define_insn "arm_sync_old_<sync_optab>si"
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
        (unspec_volatile:SI [(syncop:SI
                               (match_operand:SI 1 "arm_sync_memory_operand" "+Q")
                               (match_operand:SI 2 "s_register_operand" "r"))
	                    ]
	                    VUNSPEC_SYNC_OLD_OP))
   (set (match_dup 1)
        (unspec_volatile:SI [(match_dup 1) (match_dup 2)]
	                    VUNSPEC_SYNC_OLD_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=&r"))]
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
  {
    return arm_output_sync_insn (insn, operands);
  } 
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "3")
   (set_attr "sync_t2"              "4")
   (set_attr "sync_op"              "<sync_optab>")
   (set_attr "conds" "clob")
   (set_attr "predicable" "no")])

(define_insn "arm_sync_old_nandsi"
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
        (unspec_volatile:SI [(not:SI (and:SI
                               (match_operand:SI 1 "arm_sync_memory_operand" "+Q")
                               (match_operand:SI 2 "s_register_operand" "r")))
	                    ]
	                    VUNSPEC_SYNC_OLD_OP))
   (set (match_dup 1)
        (unspec_volatile:SI [(match_dup 1) (match_dup 2)]
	                    VUNSPEC_SYNC_OLD_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=&r"))]
  "TARGET_HAVE_LDREX && TARGET_HAVE_MEMORY_BARRIER"
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
   (clobber (match_scratch:SI 4 "=&r"))]
  "TARGET_HAVE_LDREXBHD && TARGET_HAVE_MEMORY_BARRIER"
  {
    return arm_output_sync_insn (insn, operands);
  } 
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "3")
   (set_attr "sync_t2"              "4")
   (set_attr "sync_op"              "<sync_optab>")
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
  "TARGET_HAVE_LDREXBHD && TARGET_HAVE_MEMORY_BARRIER"
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

