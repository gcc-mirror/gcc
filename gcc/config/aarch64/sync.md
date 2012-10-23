;; Machine description for AArch64 processor synchronization primitives.
;; Copyright (C) 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
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
    UNSPECV_SYNC_COMPARE_AND_SWAP       ; Represent a sync_compare_and_swap.
    UNSPECV_SYNC_LOCK			; Represent a sync_lock_test_and_set.
    UNSPECV_SYNC_LOCK_RELEASE		; Represent a sync_lock_release.
    UNSPECV_SYNC_OP			; Represent a sync_<op>
    UNSPECV_SYNC_NEW_OP			; Represent a sync_new_<op>
    UNSPECV_SYNC_OLD_OP			; Represent a sync_old_<op>
])

(define_expand "sync_compare_and_swap<mode>"
  [(set (match_operand:ALLI 0 "register_operand")
        (unspec_volatile:ALLI [(match_operand:ALLI 1 "memory_operand")
			       (match_operand:ALLI 2 "register_operand")
			       (match_operand:ALLI 3 "register_operand")]
			       UNSPECV_SYNC_COMPARE_AND_SWAP))]
  ""
  {
    struct aarch64_sync_generator generator;
    generator.op = aarch64_sync_generator_omrn;
    generator.u.omrn = gen_aarch64_sync_compare_and_swap<mode>;
    aarch64_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
    			 operands[2], operands[3]);
    DONE;
  })

(define_expand "sync_lock_test_and_set<mode>"
  [(match_operand:ALLI 0 "register_operand")
   (match_operand:ALLI 1 "memory_operand")
   (match_operand:ALLI 2 "register_operand")]
  ""
  {
    struct aarch64_sync_generator generator;
    generator.op = aarch64_sync_generator_omn;
    generator.u.omn = gen_aarch64_sync_lock_test_and_set<mode>;
    aarch64_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
                         NULL, operands[2]);
    DONE;
  })

(define_expand "sync_<optab><mode>"
  [(match_operand:ALLI 0 "memory_operand")
   (match_operand:ALLI 1 "register_operand")
   (syncop:ALLI (match_dup 0) (match_dup 1))]
  ""
  {
    struct aarch64_sync_generator generator;
    generator.op = aarch64_sync_generator_omn;
    generator.u.omn = gen_aarch64_sync_new_<optab><mode>;
    aarch64_expand_sync (<MODE>mode, &generator, NULL, operands[0], NULL,
                         operands[1]);
    DONE;
  })

(define_expand "sync_nand<mode>"
  [(match_operand:ALLI 0 "memory_operand")
   (match_operand:ALLI 1 "register_operand")
   (not:ALLI (and:ALLI (match_dup 0) (match_dup 1)))]
  ""
  {
    struct aarch64_sync_generator generator;
    generator.op = aarch64_sync_generator_omn;
    generator.u.omn = gen_aarch64_sync_new_nand<mode>;
    aarch64_expand_sync (<MODE>mode, &generator, NULL, operands[0], NULL,
                         operands[1]);
    DONE;
  })

(define_expand "sync_new_<optab><mode>"
  [(match_operand:ALLI 0 "register_operand")
   (match_operand:ALLI 1 "memory_operand")
   (match_operand:ALLI 2 "register_operand")
   (syncop:ALLI (match_dup 1) (match_dup 2))]
  ""
  {
    struct aarch64_sync_generator generator;
    generator.op = aarch64_sync_generator_omn;
    generator.u.omn = gen_aarch64_sync_new_<optab><mode>;
    aarch64_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
    		    	 NULL, operands[2]);
    DONE;
  })

(define_expand "sync_new_nand<mode>"
  [(match_operand:ALLI 0 "register_operand")
   (match_operand:ALLI 1 "memory_operand")
   (match_operand:ALLI 2 "register_operand")
   (not:ALLI (and:ALLI (match_dup 1) (match_dup 2)))]
  ""
  {
    struct aarch64_sync_generator generator;
    generator.op = aarch64_sync_generator_omn;
    generator.u.omn = gen_aarch64_sync_new_nand<mode>;
    aarch64_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
    			 NULL, operands[2]);
    DONE;
  });

(define_expand "sync_old_<optab><mode>"
  [(match_operand:ALLI 0 "register_operand")
   (match_operand:ALLI 1 "memory_operand")
   (match_operand:ALLI 2 "register_operand")
   (syncop:ALLI (match_dup 1) (match_dup 2))]
  ""
  {
    struct aarch64_sync_generator generator;
    generator.op = aarch64_sync_generator_omn;
    generator.u.omn = gen_aarch64_sync_old_<optab><mode>;
    aarch64_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
    		         NULL, operands[2]);
    DONE;
  })

(define_expand "sync_old_nand<mode>"
  [(match_operand:ALLI 0 "register_operand")
   (match_operand:ALLI 1 "memory_operand")
   (match_operand:ALLI 2 "register_operand")
   (not:ALLI (and:ALLI (match_dup 1) (match_dup 2)))]
  ""
  {
    struct aarch64_sync_generator generator;
    generator.op = aarch64_sync_generator_omn;
    generator.u.omn = gen_aarch64_sync_old_nand<mode>;
    aarch64_expand_sync (<MODE>mode, &generator, operands[0], operands[1],
                         NULL, operands[2]);
    DONE;
  })

(define_expand "memory_barrier"
  [(set (match_dup 0) (unspec:BLK [(match_dup 0)] UNSPEC_MB))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "aarch64_sync_compare_and_swap<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=&r")
        (unspec_volatile:GPI
	  [(match_operand:GPI 1 "aarch64_sync_memory_operand" "+Q")
   	   (match_operand:GPI 2 "register_operand" "r")
	   (match_operand:GPI 3 "register_operand" "r")]
	  UNSPECV_SYNC_COMPARE_AND_SWAP))
   (set (match_dup 1) (unspec_volatile:GPI [(match_dup 2)]
                                          UNSPECV_SYNC_COMPARE_AND_SWAP))
   (clobber:GPI (match_scratch:GPI 4 "=&r"))
   (set (reg:CC CC_REGNUM) (unspec_volatile:CC [(match_dup 1)]
                                                UNSPECV_SYNC_COMPARE_AND_SWAP))
   ]
  ""
  {
    return aarch64_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_required_value"  "2")
   (set_attr "sync_new_value"       "3")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "4")
   ])

(define_insn "aarch64_sync_compare_and_swap<mode>"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (zero_extend:SI
	  (unspec_volatile:SHORT
	    [(match_operand:SHORT 1 "aarch64_sync_memory_operand" "+Q")
   	     (match_operand:SI 2 "register_operand" "r")
	     (match_operand:SI 3 "register_operand" "r")]
	    UNSPECV_SYNC_COMPARE_AND_SWAP)))
   (set (match_dup 1) (unspec_volatile:SHORT [(match_dup 2)]
                                             UNSPECV_SYNC_COMPARE_AND_SWAP))
   (clobber:SI (match_scratch:SI 4 "=&r"))
   (set (reg:CC CC_REGNUM) (unspec_volatile:CC [(match_dup 1)]
                                                UNSPECV_SYNC_COMPARE_AND_SWAP))
   ]
  ""
  {
    return aarch64_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_required_value"  "2")
   (set_attr "sync_new_value"       "3")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "4")
   ])

(define_insn "aarch64_sync_lock_test_and_set<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=&r")
        (match_operand:GPI 1 "aarch64_sync_memory_operand" "+Q"))
   (set (match_dup 1)
        (unspec_volatile:GPI [(match_operand:GPI 2 "register_operand" "r")]
	                     UNSPECV_SYNC_LOCK))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:GPI 3 "=&r"))]
  ""
  {
    return aarch64_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_release_barrier" "no")
   (set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "3")
   ])

(define_insn "aarch64_sync_lock_test_and_set<mode>"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (zero_extend:SI (match_operand:SHORT 1
	                  "aarch64_sync_memory_operand" "+Q")))
   (set (match_dup 1)
        (unspec_volatile:SHORT [(match_operand:SI 2 "register_operand" "r")]
                               UNSPECV_SYNC_LOCK))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))]
  ""
  {
    return aarch64_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_release_barrier" "no")
   (set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "3")
   ])

(define_insn "aarch64_sync_new_<optab><mode>"
  [(set (match_operand:GPI 0 "register_operand" "=&r")
        (unspec_volatile:GPI
	  [(syncop:GPI
	     (match_operand:GPI 1 "aarch64_sync_memory_operand" "+Q")
             (match_operand:GPI 2 "register_operand" "r"))]
           UNSPECV_SYNC_NEW_OP))
   (set (match_dup 1)
        (unspec_volatile:GPI [(match_dup 1) (match_dup 2)]
	                    UNSPECV_SYNC_NEW_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:GPI 3 "=&r"))]
  ""
  {
    return aarch64_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "3")
   (set_attr "sync_op"              "<optab>")
   ])

(define_insn "aarch64_sync_new_nand<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=&r")
        (unspec_volatile:GPI
	  [(not:GPI (and:GPI
                     (match_operand:GPI 1 "aarch64_sync_memory_operand" "+Q")
                     (match_operand:GPI 2 "register_operand" "r")))]
          UNSPECV_SYNC_NEW_OP))
   (set (match_dup 1)
        (unspec_volatile:GPI [(match_dup 1) (match_dup 2)]
	                    UNSPECV_SYNC_NEW_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:GPI 3 "=&r"))]
  ""
  {
    return aarch64_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "3")
   (set_attr "sync_op"              "nand")
   ])

(define_insn "aarch64_sync_new_<optab><mode>"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (unspec_volatile:SI
	  [(syncop:SI
             (zero_extend:SI
	       (match_operand:SHORT 1 "aarch64_sync_memory_operand" "+Q"))
               (match_operand:SI 2 "register_operand" "r"))]
          UNSPECV_SYNC_NEW_OP))
   (set (match_dup 1)
        (unspec_volatile:SHORT [(match_dup 1) (match_dup 2)]
	                       UNSPECV_SYNC_NEW_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))]
  ""
  {
    return aarch64_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "3")
   (set_attr "sync_op"              "<optab>")
   ])

(define_insn "aarch64_sync_new_nand<mode>"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (unspec_volatile:SI
	  [(not:SI
	     (and:SI
               (zero_extend:SI
	         (match_operand:SHORT 1 "aarch64_sync_memory_operand" "+Q"))
               (match_operand:SI 2 "register_operand" "r")))
	  ] UNSPECV_SYNC_NEW_OP))
   (set (match_dup 1)
        (unspec_volatile:SHORT [(match_dup 1) (match_dup 2)]
	                       UNSPECV_SYNC_NEW_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))]
  ""
  {
    return aarch64_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "0")
   (set_attr "sync_t2"              "3")
   (set_attr "sync_op"              "nand")
   ])

(define_insn "aarch64_sync_old_<optab><mode>"
  [(set (match_operand:GPI 0 "register_operand" "=&r")
        (unspec_volatile:GPI
          [(syncop:GPI
             (match_operand:GPI 1 "aarch64_sync_memory_operand" "+Q")
             (match_operand:GPI 2 "register_operand" "r"))]
          UNSPECV_SYNC_OLD_OP))
   (set (match_dup 1)
        (unspec_volatile:GPI [(match_dup 1) (match_dup 2)]
	                     UNSPECV_SYNC_OLD_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:GPI 3 "=&r"))
   (clobber (match_scratch:GPI 4 "=&r"))]
  ""
  {
    return aarch64_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "3")
   (set_attr "sync_t2"              "4")
   (set_attr "sync_op"              "<optab>")
   ])

(define_insn "aarch64_sync_old_nand<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=&r")
        (unspec_volatile:GPI
	  [(not:GPI (and:GPI
                     (match_operand:GPI 1 "aarch64_sync_memory_operand" "+Q")
                     (match_operand:GPI 2 "register_operand" "r")))]
          UNSPECV_SYNC_OLD_OP))
   (set (match_dup 1)
        (unspec_volatile:GPI [(match_dup 1) (match_dup 2)]
	                     UNSPECV_SYNC_OLD_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:GPI 3 "=&r"))
   (clobber (match_scratch:GPI 4 "=&r"))]
  ""
  {
    return aarch64_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "3")
   (set_attr "sync_t2"              "4")
   (set_attr "sync_op"              "nand")
   ])

(define_insn "aarch64_sync_old_<optab><mode>"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (unspec_volatile:SI
	  [(syncop:SI
             (zero_extend:SI
	       (match_operand:SHORT 1 "aarch64_sync_memory_operand" "+Q"))
               (match_operand:SI 2 "register_operand" "r"))]
           UNSPECV_SYNC_OLD_OP))
   (set (match_dup 1)
        (unspec_volatile:SHORT [(match_dup 1) (match_dup 2)]
	                       UNSPECV_SYNC_OLD_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=&r"))]
  ""
  {
    return aarch64_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "3")
   (set_attr "sync_t2"              "4")
   (set_attr "sync_op"              "<optab>")
   ])

(define_insn "aarch64_sync_old_nand<mode>"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (unspec_volatile:SI
	  [(not:SI
	     (and:SI
               (zero_extend:SI
		 (match_operand:SHORT 1 "aarch64_sync_memory_operand" "+Q"))
                 (match_operand:SI 2 "register_operand" "r")))]
          UNSPECV_SYNC_OLD_OP))
   (set (match_dup 1)
        (unspec_volatile:SHORT [(match_dup 1) (match_dup 2)]
	                       UNSPECV_SYNC_OLD_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=&r"))]
  ""
  {
    return aarch64_output_sync_insn (insn, operands);
  }
  [(set_attr "sync_result"          "0")
   (set_attr "sync_memory"          "1")
   (set_attr "sync_new_value"       "2")
   (set_attr "sync_t1"              "3")
   (set_attr "sync_t2"              "4")
   (set_attr "sync_op"              "nand")
   ])

(define_insn "*memory_barrier"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_MB))]
  ""
  "dmb\\tish"
)

(define_insn "sync_lock_release<mode>"
  [(set (match_operand:ALLI 0 "memory_operand" "+Q")
  	(unspec_volatile:ALLI [(match_operand:ALLI 1 "register_operand" "r")]
	                      UNSPECV_SYNC_LOCK_RELEASE))]

  ""
  {
    return aarch64_output_sync_lock_release (operands[1], operands[0]);
  })

