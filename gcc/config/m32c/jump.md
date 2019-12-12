;; Machine Descriptions for R8C/M16C/M32C
;; Copyright (C) 2005-2019 Free Software Foundation, Inc.
;; Contributed by Red Hat.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; jump, conditionals, calls, etc

(define_insn "indirect_jump_16"
  [(set (pc)
       (match_operand:HI 0 "register_operand" "Rhi"))]
  "TARGET_A16"
;  "jmpi.a\t%0"
  ; no 16-bit jmpi in r8c
  "push.b #0 | push.w\t%0 | rts"
  [(set_attr "flags" "x")]
  )

(define_insn "indirect_jump_24"
  [(set (pc)
       (match_operand:PSI 0 "register_operand" "Rpi"))]
  "TARGET_A24"
  "jmpi.a\t%0"
  [(set_attr "flags" "n")]
  )

(define_expand "indirect_jump"
  [(match_operand 0 "register_operand" "")]
  ""
  "if (TARGET_A16)
     emit_jump_insn (gen_indirect_jump_16(operands[0]));
   else
     emit_jump_insn (gen_indirect_jump_24(operands[0]));
   DONE;"
  )

; We can replace this with jmp.s when gas supports relaxing.  m32c
; opcodes are too complicated to try to compute their sizes here, it's
; far easier (and more reliable) to let gas worry about it.
(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "jmp.a\t%l0"
  [(set_attr "flags" "n")]
)

; No 16-bit indirect calls on r8c/m16c.  */
(define_insn "call"
  [(call (match_operand:QI 0 "memory_operand" "Si,SaSb,?Rmm")
	 (match_operand 1 "" ""))
   (use (match_operand 2 "immediate_operand" ""))]
  ""
  "*
switch (which_alternative) {
  case 0:
    {
      HOST_WIDE_INT func_vect_num = 
      current_function_special_page_vector(XEXP (operands[0], 0));
      if (func_vect_num)
        {
          operands[3] = gen_rtx_CONST_INT (VOIDmode, func_vect_num);
          return \"jsrs\t%3\";
        }
      else
        return \"jsr.a\t%0\";
    }
  case 1: return TARGET_A16 ? \"push.w %a0 | jsr.a\tm32c_jsri16\" : \"jsri.a\t%a0\";
  case 2: return \"jsri.a\t%a0\";
  default: gcc_unreachable ();
}"
  [(set_attr "flags" "x")]
  )

(define_insn "call_value"
  [(set (match_operand 0 "m32c_return_operand" "=RdiRmmRpa,RdiRmmRpa,RdiRmmRpa")
	(call (match_operand:QI 1 "memory_operand" "Si,SaSb,?Rmm")
	      (match_operand 2 "" "")))
   (use (match_operand 3 "immediate_operand" ""))]
  ""
  "*
switch (which_alternative) {
  case 0:
    {
      HOST_WIDE_INT func_vect_num = 
      current_function_special_page_vector(XEXP (operands[1], 0));
      if (func_vect_num)
        {
          operands[4] = gen_rtx_CONST_INT (VOIDmode, func_vect_num);
          return \"jsrs\t%4\";
        }
      else
        return \"jsr.a\t%1\";
    }
  case 1: return TARGET_A16 ? \"push.w %a1 | jsr.a\tm32c_jsri16\" : \"jsri.a\t%a1\";
  case 2: return \"jsri.a\t%a1\";
  default: gcc_unreachable ();
}"
  [(set_attr "flags" "x,x,x")]
  )

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
                    (const_int 0))
              (match_operand 1 "" "")
              (match_operand 2 "" "")])]
  ""
  "
{
  int i;

  emit_call_insn (gen_call (operands[0], const0_rtx, const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }
  DONE;
}")
