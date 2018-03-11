;; Intrinsic patterns description of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2018 Free Software Foundation, Inc.
;; Contributed by Andes Technology Corporation.
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

;; ------------------------------------------------------------------------

;; Register Transfer.

(define_insn "unspec_volatile_mfsr"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(match_operand:SI 1 "immediate_operand" "i")] UNSPEC_VOLATILE_MFSR))]
  ""
  "mfsr\t%0, %V1"
  [(set_attr "type"   "misc")
   (set_attr "length"    "4")]
)

(define_insn "unspec_volatile_mfusr"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(match_operand:SI 1 "immediate_operand" "i")] UNSPEC_VOLATILE_MFUSR))]
  ""
  "mfusr\t%0, %V1"
  [(set_attr "type"   "misc")
   (set_attr "length"    "4")]
)

(define_insn "unspec_volatile_mtsr"
  [(unspec_volatile:SI [(match_operand:SI 0 "register_operand" "r")
			(match_operand:SI 1 "immediate_operand" "i")] UNSPEC_VOLATILE_MTSR)]
  ""
  "mtsr\t%0, %V1"
  [(set_attr "type"   "misc")
   (set_attr "length"    "4")]
)

(define_insn "unspec_volatile_mtusr"
  [(unspec_volatile:SI [(match_operand:SI 0 "register_operand" "r")
			(match_operand:SI 1 "immediate_operand" "i")] UNSPEC_VOLATILE_MTUSR)]
  ""
  "mtusr\t%0, %V1"
  [(set_attr "type"   "misc")
   (set_attr "length"    "4")]
)

;; ------------------------------------------------------------------------

;; Interrupt Instructions.

(define_insn "unspec_volatile_setgie_en"
  [(unspec_volatile:SI [(const_int 0)] UNSPEC_VOLATILE_SETGIE_EN)]
  ""
  "setgie.e"
  [(set_attr "type" "misc")]
)

(define_insn "unspec_volatile_setgie_dis"
  [(unspec_volatile:SI [(const_int 0)] UNSPEC_VOLATILE_SETGIE_DIS)]
  ""
  "setgie.d"
  [(set_attr "type" "misc")]
)

;; ------------------------------------------------------------------------

;; Cache Synchronization Instructions

(define_insn "unspec_volatile_isync"
  [(unspec_volatile:SI [(match_operand:SI 0 "register_operand" "r")] UNSPEC_VOLATILE_ISYNC)]
  ""
  "isync\t%0"
  [(set_attr "type" "misc")]
)

(define_insn "unspec_volatile_isb"
  [(unspec_volatile:SI [(const_int 0)] UNSPEC_VOLATILE_ISB)]
  ""
  "isb"
  [(set_attr "type" "misc")]
)

;; String Extension

(define_insn "unspec_ffb"
  [(set (match_operand:SI 0 "register_operand" "=r, r")
	(unspec:SI [(match_operand:SI 1 "register_operand" "r, r")
		    (match_operand:SI 2 "nonmemory_operand" "Iu08, r")] UNSPEC_FFB))]
  ""
  "@
  ffbi\t%0, %1, %2
  ffb\t%0, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")]
)

(define_insn "unspec_ffmism"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "register_operand" "r")
		    (match_operand:SI 2 "register_operand" "r")] UNSPEC_FFMISM))]
  ""
  "ffmism\t%0, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")]
)

(define_insn "unspec_flmism"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "register_operand" "r")
		    (match_operand:SI 2 "register_operand" "r")] UNSPEC_FLMISM))]
  ""
  "flmism\t%0, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")]
)

;;Unaligned Load/Store

(define_expand "unaligned_load_hw"
  [(set (match_operand:HI 0 "register_operand" "")
	(unspec:HI [(mem:HI (match_operand:SI 1 "register_operand" ""))] UNSPEC_UALOAD_HW))]
  ""
{
  operands[0] = simplify_gen_subreg (SImode, operands[0],
				     GET_MODE (operands[0]), 0);
  if (TARGET_ISA_V3M)
    {
      nds32_expand_unaligned_load (operands, HImode);
    }
  else
    {
      emit_insn (gen_unaligned_load_w (operands[0],
				       gen_rtx_MEM (SImode, operands[1])));

      if (WORDS_BIG_ENDIAN)
	emit_insn (gen_lshrsi3 (operands[0], operands[0], GEN_INT(16)));
      else
	emit_insn (gen_andsi3 (operands[0], operands[0], GEN_INT (0xffff)));
    }

  DONE;
})

(define_expand "unaligned_loadsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(mem:SI (match_operand:SI 1 "register_operand" "r"))] UNSPEC_UALOAD_W))]
  ""
{
  if (TARGET_ISA_V3M)
    nds32_expand_unaligned_load (operands, SImode);
  else
    emit_insn (gen_unaligned_load_w (operands[0],
				     gen_rtx_MEM (SImode, (operands[1]))));
  DONE;
})

(define_insn "unaligned_load_w"
  [(set (match_operand:SI 0 "register_operand"                       "=  r")
	(unspec:SI [(match_operand:SI 1 "nds32_lmw_smw_base_operand" " Umw")] UNSPEC_UALOAD_W))]
  ""
{
  return nds32_output_lmw_single_word (operands);
}
  [(set_attr "type"   "load")
   (set_attr "length"    "4")]
)

(define_expand "unaligned_loaddi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(mem:DI (match_operand:SI 1 "register_operand" "r"))] UNSPEC_UALOAD_DW))]
  ""
{
  if (TARGET_ISA_V3M)
    {
      nds32_expand_unaligned_load (operands, DImode);
    }
  else
    emit_insn (gen_unaligned_load_dw (operands[0], operands[1]));
  DONE;
})

(define_insn "unaligned_load_dw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(mem:DI (match_operand:SI 1 "register_operand" "r"))] UNSPEC_UALOAD_DW))]
  ""
{
  rtx otherops[3];
  otherops[0] = gen_rtx_REG (SImode, REGNO (operands[0]));
  otherops[1] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
  otherops[2] = operands[1];

  output_asm_insn ("lmw.bi\t%0, [%2], %1, 0", otherops);
  return "";
}
  [(set_attr "type"   "load")
   (set_attr "length"    "4")]
)

(define_expand "unaligned_store_hw"
  [(set (mem:SI (match_operand:SI 0 "register_operand" ""))
	(unspec:HI [(match_operand:HI 1 "register_operand" "")] UNSPEC_UASTORE_HW))]
  ""
{
  operands[1] = simplify_gen_subreg (SImode, operands[1],
				     GET_MODE (operands[1]), 0);
  nds32_expand_unaligned_store (operands, HImode);
  DONE;
})

(define_expand "unaligned_storesi"
  [(set (mem:SI (match_operand:SI 0 "register_operand" "r"))
	(unspec:SI [(match_operand:SI 1 "register_operand" "r")] UNSPEC_UASTORE_W))]
  ""
{
  if (TARGET_ISA_V3M)
    nds32_expand_unaligned_store (operands, SImode);
  else
    emit_insn (gen_unaligned_store_w (gen_rtx_MEM (SImode, operands[0]),
				      operands[1]));
  DONE;
})

(define_insn "unaligned_store_w"
  [(set (match_operand:SI 0 "nds32_lmw_smw_base_operand"   "=Umw")
	(unspec:SI [(match_operand:SI 1 "register_operand" "   r")] UNSPEC_UASTORE_W))]
  ""
{
  return nds32_output_smw_single_word (operands);
}
  [(set_attr "type"   "store")
   (set_attr "length"     "4")]
)

(define_expand "unaligned_storedi"
  [(set (mem:DI (match_operand:SI 0 "register_operand" "r"))
	(unspec:DI [(match_operand:DI 1 "register_operand" "r")] UNSPEC_UASTORE_DW))]
  ""
{
  if (TARGET_ISA_V3M)
    nds32_expand_unaligned_store (operands, DImode);
  else
    emit_insn (gen_unaligned_store_dw (operands[0], operands[1]));
  DONE;
})

(define_insn "unaligned_store_dw"
  [(set (mem:DI (match_operand:SI 0 "register_operand" "r"))
	(unspec:DI [(match_operand:DI 1 "register_operand" "r")] UNSPEC_UASTORE_DW))]
  ""
{
  rtx otherops[3];
  otherops[0] = gen_rtx_REG (SImode, REGNO (operands[1]));
  otherops[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 1);
  otherops[2] = operands[0];

  output_asm_insn ("smw.bi\t%0, [%2], %1, 0", otherops);
  return "";
}
  [(set_attr "type"   "store")
   (set_attr "length"     "4")]
)

;; ------------------------------------------------------------------------
