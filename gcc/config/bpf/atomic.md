;; Machine description for eBPF.
;; Copyright (C) 2023-2025 Free Software Foundation, Inc.

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
;; <http://www.gnu.org/licenses/>.


(define_mode_iterator AMO [SI DI])

;;; Plain atomic modify operations.

;; The BPF instruction set provides non-fetching atomic instructions
;; that could be used to implement the corresponding named insns:
;;
;;  atomic_add -> aadd (aka xadd)
;;  atomic_and -> aand 
;;  atomic_or  -> aor
;;  atomic_xor -> axor
;;
;; However, we are not including insns for these here because the
;; non-fetching BPF atomic instruction imply different memory ordering
;; semantics than the fetching BPF atomic instruction used to
;; implement the atomic_fetch_* insns below (afadd, afand, afor,
;; afxor) and they cannot be used interchangeably, as it is expected
;; by GCC when it uses a non-fetching variant as an optimization of a
;; fetching operation where the returned value is not used.

;;; Feching (read-modify-store) versions of atomic operations.

(define_insn "atomic_fetch_add<AMO:mode>"
  [(set (match_operand:AMO 0 "register_operand" "=r") ; output
        (match_operand:AMO 1 "memory_operand" "+m"))
   (set (match_dup 1)
        (unspec_volatile:AMO
         [(plus:AMO (match_dup 1)
                    (match_operand:AMO 2 "nonmemory_operand" "0")) ; second operand to op
          (match_operand:AMO 3 "const_int_operand")] ;; Memory model
        UNSPEC_AFADD))]
  "bpf_has_v3_atomics"
  "{afadd<msuffix>\t%1,%0|%w0 = atomic_fetch_add((<smop> *)%1, %w0)}")

(define_insn "atomic_fetch_and<AMO:mode>"
  [(set (match_operand:AMO 0 "register_operand" "=r")
        (match_operand:AMO 1 "memory_operand" "+m"))
   (set (match_dup 1)
        (unspec_volatile:AMO
         [(and:AMO (match_dup 1)
                    (match_operand:AMO 2 "nonmemory_operand" "0"))
          (match_operand:AMO 3 "const_int_operand")]
         UNSPEC_AFAND))]
  "bpf_has_v3_atomics"
  "{afand<msuffix>\t%1,%0|%w0 = atomic_fetch_and((<smop> *)%1, %w0)}")

(define_insn "atomic_fetch_or<AMO:mode>"
  [(set (match_operand:AMO 0 "register_operand" "=r")
        (match_operand:AMO 1 "memory_operand" "+m"))
   (set (match_dup 1)
        (unspec_volatile:AMO
         [(ior:AMO (match_dup 1)
                   (match_operand:AMO 2 "nonmemory_operand" "0"))
          (match_operand:AMO 3 "const_int_operand")]
         UNSPEC_AFOR))]
  "bpf_has_v3_atomics"
  "{afor<msuffix>\t%1,%0|%w0 = atomic_fetch_or((<smop> *)%1, %w0)}")

(define_insn "atomic_fetch_xor<AMO:mode>"
  [(set (match_operand:AMO 0 "register_operand" "=r")
        (match_operand:AMO 1 "memory_operand" "+m"))
   (set (match_dup 1)
        (unspec_volatile:AMO
         [(xor:AMO (match_dup 1)
                   (match_operand:AMO 2 "nonmemory_operand" "0"))
          (match_operand:AMO 3 "const_int_operand")]
         UNSPEC_AFXOR))]
  "bpf_has_v3_atomics"
  "{afxor<msuffix>\t%1,%0|%w0 = atomic_fetch_xor((<smop> *)%1, %w0)}")

;; Weird suffixes used in pseudo-c atomic compare-exchange insns.
(define_mode_attr pcaxsuffix [(SI "32_32") (DI "_64")])

(define_insn "atomic_exchange<AMO:mode>"
  [(set (match_operand:AMO 0 "register_operand" "=r")
        (unspec_volatile:AMO
         [(match_operand:AMO 1 "memory_operand" "+m")
          (match_operand:AMO 3 "const_int_operand")]
         UNSPEC_AXCHG))
   (set (match_dup 1)
        (match_operand:AMO 2 "nonmemory_operand" "0"))]
  "bpf_has_v3_atomics"
  "{axchg<msuffix>\t%1,%0|%w0 = xchg<pcaxsuffix>(%M1, %w0)}")

;; The eBPF atomic-compare-and-exchange instruction has the form
;;   acmp [%dst+offset], %src
;; The instruction atomically compares the value addressed by %dst+offset
;; with register R0.  If they match, the value at %dst+offset is overwritten
;; with the value of %src.  Otherwise, no write occurs.  In either case, the
;; original value of %dst+offset is zero-extended and loaded back into R0.

(define_expand "atomic_compare_and_swap<AMO:mode>"
  [(match_operand:SI 0 "register_operand" "=r")    ;; bool success
   (match_operand:AMO 1 "register_operand" "=r")   ;; old value
   (match_operand:AMO 2 "memory_operand" "+m")     ;; memory
   (match_operand:AMO 3 "register_operand")        ;; expected
   (match_operand:AMO 4 "register_operand")        ;; desired
   (match_operand:SI 5 "const_int_operand")        ;; is_weak (unused)
   (match_operand:SI 6 "const_int_operand")        ;; success model (unused)
   (match_operand:SI 7 "const_int_operand")]       ;; failure model (unused)
  "bpf_has_v3_atomics"
{
  /* Load the expected value (into R0 by constraint of below).  */
  emit_move_insn (operands[1], operands[3]);

  /* Emit the acmp.  */
  emit_insn (gen_atomic_compare_and_swap<AMO:mode>_1 (operands[1], operands[2], operands[3], operands[4]));

  /* Assume that the operation was successful.  */
  emit_move_insn (operands[0], const1_rtx);
  rtx_code_label *success_label = gen_label_rtx ();

  /* Compare value that was in memory (now in R0/op[1]) to expected value.
     If they are equal, then the write occurred. Otherwise, indicate fail in output.  */
  emit_cmp_and_jump_insns (operands[1], operands[3], EQ, 0,
                           GET_MODE (operands[1]), 1, success_label);
  emit_move_insn (operands[0], const0_rtx);

  if (success_label)
    {
       emit_label (success_label);
       LABEL_NUSES (success_label) = 1;
    }
  DONE;
})

(define_insn "atomic_compare_and_swap<AMO:mode>_1"
  [(set (match_operand:AMO 0 "register_operand" "+t") ;; R0 is both input (expected value)
        (unspec_volatile:AMO                          ;;       and output (original value)
         [(match_dup 0)                               ;; result depends on R0
          (match_operand:AMO 1 "memory_operand")      ;; memory
          (match_operand:AMO 2 "register_operand")    ;; expected
          (match_operand:AMO 3 "register_operand")]   ;; desired
         UNSPEC_ACMP))]
  "bpf_has_v3_atomics"
  "{acmp<msuffix>\t%1,%3|%w0 = cmpxchg<pcaxsuffix>(%M1, %w0, %w3)}")
