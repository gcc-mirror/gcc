;; Copyright (C) 2016-2024 Free Software Foundation, Inc.

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; {{{ Vector iterators
; SV iterators include both scalar and vector modes.

; Vector modes for specific types
(define_mode_iterator V_QI
		      [V2QI V4QI V8QI V16QI V32QI V64QI])
(define_mode_iterator V_HI
		      [V2HI V4HI V8HI V16HI V32HI V64HI])
(define_mode_iterator V_HF
		      [V2HF V4HF V8HF V16HF V32HF V64HF])
(define_mode_iterator V_SI
		      [V2SI V4SI V8SI V16SI V32SI V64SI])
(define_mode_iterator V_SF
		      [V2SF V4SF V8SF V16SF V32SF V64SF])
(define_mode_iterator V_DI
		      [V2DI V4DI V8DI V16DI V32DI V64DI])
(define_mode_iterator V_DF
		      [V2DF V4DF V8DF V16DF V32DF V64DF])

; Vector modes for sub-dword modes
(define_mode_iterator V_QIHI
		      [V2QI V2HI
		       V4QI V4HI
		       V8QI V8HI
		       V16QI V16HI
		       V32QI V32HI
		       V64QI V64HI])

; Vector modes for one vector register
(define_mode_iterator V_1REG
		      [V2QI V2HI V2SI V2HF V2SF
		       V4QI V4HI V4SI V4HF V4SF
		       V8QI V8HI V8SI V8HF V8SF
		       V16QI V16HI V16SI V16HF V16SF
		       V32QI V32HI V32SI V32HF V32SF
		       V64QI V64HI V64SI V64HF V64SF])
(define_mode_iterator V_1REG_ALT
		      [V2QI V2HI V2SI V2HF V2SF
		       V4QI V4HI V4SI V4HF V4SF
		       V8QI V8HI V8SI V8HF V8SF
		       V16QI V16HI V16SI V16HF V16SF
		       V32QI V32HI V32SI V32HF V32SF
		       V64QI V64HI V64SI V64HF V64SF])

(define_mode_iterator V_INT_1REG
		      [V2QI V2HI V2SI
		       V4QI V4HI V4SI
		       V8QI V8HI V8SI
		       V16QI V16HI V16SI
		       V32QI V32HI V32SI
		       V64QI V64HI V64SI])
(define_mode_iterator V_INT_1REG_ALT
		      [V2QI V2HI V2SI
		       V4QI V4HI V4SI
		       V8QI V8HI V8SI
		       V16QI V16HI V16SI
		       V32QI V32HI V32SI
		       V64QI V64HI V64SI])
(define_mode_iterator V_FP_1REG
		      [V2HF V2SF
		       V4HF V4SF
		       V8HF V8SF
		       V16HF V16SF
		       V32HF V32SF
		       V64HF V64SF])

; Vector modes for two vector registers
(define_mode_iterator V_2REG
		      [V2DI V2DF
		       V4DI V4DF
		       V8DI V8DF
		       V16DI V16DF
		       V32DI V32DF
		       V64DI V64DF])
(define_mode_iterator V_2REG_ALT
		      [V2DI V2DF
		       V4DI V4DF
		       V8DI V8DF
		       V16DI V16DF
		       V32DI V32DF
		       V64DI V64DF])

; Vector modes for four vector registers
(define_mode_iterator V_4REG [V2TI V4TI V8TI V16TI V32TI V64TI])
(define_mode_iterator V_4REG_ALT [V2TI V4TI V8TI V16TI V32TI V64TI])

; Vector modes with native support
(define_mode_iterator V_noQI
		      [V2HI V2HF V2SI V2SF V2DI V2DF
		       V4HI V4HF V4SI V4SF V4DI V4DF
		       V8HI V8HF V8SI V8SF V8DI V8DF
		       V16HI V16HF V16SI V16SF V16DI V16DF
		       V32HI V32HF V32SI V32SF V32DI V32DF
		       V64HI V64HF V64SI V64SF V64DI V64DF])
(define_mode_iterator V_noHI
		      [V2HF V2SI V2SF V2DI V2DF
		       V4HF V4SI V4SF V4DI V4DF
		       V8HF V8SI V8SF V8DI V8DF
		       V16HF V16SI V16SF V16DI V16DF
		       V32HF V32SI V32SF V32DI V32DF
		       V64HF V64SI V64SF V64DI V64DF])

(define_mode_iterator V_INT_noQI
		      [V2HI V2SI V2DI
		       V4HI V4SI V4DI
		       V8HI V8SI V8DI
		       V16HI V16SI V16DI
		       V32HI V32SI V32DI
		       V64HI V64SI V64DI])
(define_mode_iterator V_INT_noHI
		      [V2SI V2DI
		       V4SI V4DI
		       V8SI V8DI
		       V16SI V16DI
		       V32SI V32DI
		       V64SI V64DI])

(define_mode_iterator SV_SFDF
		      [SF DF
		       V2SF V2DF
		       V4SF V4DF
		       V8SF V8DF
		       V16SF V16DF
		       V32SF V32DF
		       V64SF V64DF])

; All modes in which we want to do more than just moves.
(define_mode_iterator V_ALL
		      [V2QI V2HI V2HF V2SI V2SF V2DI V2DF
		       V4QI V4HI V4HF V4SI V4SF V4DI V4DF
		       V8QI V8HI V8HF V8SI V8SF V8DI V8DF
		       V16QI V16HI V16HF V16SI V16SF V16DI V16DF
		       V32QI V32HI V32HF V32SI V32SF V32DI V32DF
		       V64QI V64HI V64HF V64SI V64SF V64DI V64DF])
(define_mode_iterator V_ALL_ALT
		      [V2QI V2HI V2HF V2SI V2SF V2DI V2DF
		       V4QI V4HI V4HF V4SI V4SF V4DI V4DF
		       V8QI V8HI V8HF V8SI V8SF V8DI V8DF
		       V16QI V16HI V16HF V16SI V16SF V16DI V16DF
		       V32QI V32HI V32HF V32SI V32SF V32DI V32DF
		       V64QI V64HI V64HF V64SI V64SF V64DI V64DF])

(define_mode_iterator V_INT
		      [V2QI V2HI V2SI V2DI
		       V4QI V4HI V4SI V4DI
		       V8QI V8HI V8SI V8DI
		       V16QI V16HI V16SI V16DI
		       V32QI V32HI V32SI V32DI
		       V64QI V64HI V64SI V64DI])
(define_mode_iterator V_FP
		      [V2HF V2SF V2DF
		       V4HF V4SF V4DF
		       V8HF V8SF V8DF
		       V16HF V16SF V16DF
		       V32HF V32SF V32DF
		       V64HF V64SF V64DF])
(define_mode_iterator SV_FP
		      [HF SF DF
		       V2HF V2SF V2DF
		       V4HF V4SF V4DF
		       V8HF V8SF V8DF
		       V16HF V16SF V16DF
		       V32HF V32SF V32DF
		       V64HF V64SF V64DF])

; All modes that need moves, including those without many insns.
(define_mode_iterator V_MOV
		      [V2QI V2HI V2HF V2SI V2SF V2DI V2DF V2TI
		       V4QI V4HI V4HF V4SI V4SF V4DI V4DF V4TI
		       V8QI V8HI V8HF V8SI V8SF V8DI V8DF V8TI
		       V16QI V16HI V16HF V16SI V16SF V16DI V16DF V16TI
		       V32QI V32HI V32HF V32SI V32SF V32DI V32DF V32TI
		       V64QI V64HI V64HF V64SI V64SF V64DI V64DF V64TI])
(define_mode_iterator V_MOV_ALT
		      [V2QI V2HI V2HF V2SI V2SF V2DI V2DF V2TI
		       V4QI V4HI V4HF V4SI V4SF V4DI V4DF V4TI
		       V8QI V8HI V8HF V8SI V8SF V8DI V8DF V8TI
		       V16QI V16HI V16HF V16SI V16SF V16DI V16DF V16TI
		       V32QI V32HI V32HF V32SI V32SF V32DI V32DF V32TI
		       V64QI V64HI V64HF V64SI V64SF V64DI V64DF V64TI])

(define_mode_attr scalar_mode
  [(QI "qi") (HI "hi") (SI "si") (TI "ti")
   (HF "hf") (SF "sf") (DI "di") (DF "df")
   (V2QI "qi") (V2HI "hi") (V2SI "si") (V2TI "ti")
   (V2HF "hf") (V2SF "sf") (V2DI "di") (V2DF "df")
   (V4QI "qi") (V4HI "hi") (V4SI "si") (V4TI "ti")
   (V4HF "hf") (V4SF "sf") (V4DI "di") (V4DF "df")
   (V8QI "qi") (V8HI "hi") (V8SI "si") (V8TI "ti")
   (V8HF "hf") (V8SF "sf") (V8DI "di") (V8DF "df")
   (V16QI "qi") (V16HI "hi") (V16SI "si") (V16TI "ti")
   (V16HF "hf") (V16SF "sf") (V16DI "di") (V16DF "df")
   (V32QI "qi") (V32HI "hi") (V32SI "si") (V32TI "ti")
   (V32HF "hf") (V32SF "sf") (V32DI "di") (V32DF "df")
   (V64QI "qi") (V64HI "hi") (V64SI "si") (V64TI "ti")
   (V64HF "hf") (V64SF "sf") (V64DI "di") (V64DF "df")])

(define_mode_attr SCALAR_MODE
  [(QI "QI") (HI "HI") (SI "SI") (TI "TI")
   (HF "HF") (SF "SF") (DI "DI") (DF "DF")
   (V2QI "QI") (V2HI "HI") (V2SI "SI") (V2TI "TI")
   (V2HF "HF") (V2SF "SF") (V2DI "DI") (V2DF "DF")
   (V4QI "QI") (V4HI "HI") (V4SI "SI") (V4TI "TI")
   (V4HF "HF") (V4SF "SF") (V4DI "DI") (V4DF "DF")
   (V8QI "QI") (V8HI "HI") (V8SI "SI") (V8TI "TI")
   (V8HF "HF") (V8SF "SF") (V8DI "DI") (V8DF "DF")
   (V16QI "QI") (V16HI "HI") (V16SI "SI") (V16TI "TI")
   (V16HF "HF") (V16SF "SF") (V16DI "DI") (V16DF "DF")
   (V32QI "QI") (V32HI "HI") (V32SI "SI") (V32TI "TI")
   (V32HF "HF") (V32SF "SF") (V32DI "DI") (V32DF "DF")
   (V64QI "QI") (V64HI "HI") (V64SI "SI") (V64TI "TI")
   (V64HF "HF") (V64SF "SF") (V64DI "DI") (V64DF "DF")])

(define_mode_attr vnsi
  [(QI "si") (HI "si") (SI "si") (TI "si")
   (HF "si") (SF "si") (DI "si") (DF "si")
   (V2QI "v2si") (V2HI "v2si") (V2HF "v2si") (V2SI "v2si")
   (V2SF "v2si") (V2DI "v2si") (V2DF "v2si") (V2TI "v2si")
   (V4QI "v4si") (V4HI "v4si") (V4HF "v4si") (V4SI "v4si")
   (V4SF "v4si") (V4DI "v4si") (V4DF "v4si") (V4TI "v4si")
   (V8QI "v8si") (V8HI "v8si") (V8HF "v8si") (V8SI "v8si")
   (V8SF "v8si") (V8DI "v8si") (V8DF "v8si") (V8TI "v8si")
   (V16QI "v16si") (V16HI "v16si") (V16HF "v16si") (V16SI "v16si")
   (V16SF "v16si") (V16DI "v16si") (V16DF "v16si") (V16TI "v16si")
   (V32QI "v32si") (V32HI "v32si") (V32HF "v32si") (V32SI "v32si")
   (V32SF "v32si") (V32DI "v32si") (V32DF "v32si") (V32TI "v32si")
   (V64QI "v64si") (V64HI "v64si") (V64HF "v64si") (V64SI "v64si")
   (V64SF "v64si") (V64DI "v64si") (V64DF "v64si") (V64TI "v64si")])

(define_mode_attr VnSI
  [(QI "SI") (HI "SI") (SI "SI") (TI "SI")
   (HF "SI") (SF "SI") (DI "SI") (DF "SI")
   (V2QI "V2SI") (V2HI "V2SI") (V2HF "V2SI") (V2SI "V2SI")
   (V2SF "V2SI") (V2DI "V2SI") (V2DF "V2SI") (V2TI "V2SI")
   (V4QI "V4SI") (V4HI "V4SI") (V4HF "V4SI") (V4SI "V4SI")
   (V4SF "V4SI") (V4DI "V4SI") (V4DF "V4SI") (V4TI "V4SI")
   (V8QI "V8SI") (V8HI "V8SI") (V8HF "V8SI") (V8SI "V8SI")
   (V8SF "V8SI") (V8DI "V8SI") (V8DF "V8SI") (V8TI "V8SI")
   (V16QI "V16SI") (V16HI "V16SI") (V16HF "V16SI") (V16SI "V16SI")
   (V16SF "V16SI") (V16DI "V16SI") (V16DF "V16SI") (V16TI "V16SI")
   (V32QI "V32SI") (V32HI "V32SI") (V32HF "V32SI") (V32SI "V32SI")
   (V32SF "V32SI") (V32DI "V32SI") (V32DF "V32SI") (V32TI "V32SI")
   (V64QI "V64SI") (V64HI "V64SI") (V64HF "V64SI") (V64SI "V64SI")
   (V64SF "V64SI") (V64DI "V64SI") (V64DF "V64SI") (V64TI "V64SI")])

(define_mode_attr vndi
  [(V2QI "v2di") (V2HI "v2di") (V2HF "v2di") (V2SI "v2di")
   (V2SF "v2di") (V2DI "v2di") (V2DF "v2di") (V2TI "v2di")
   (V4QI "v4di") (V4HI "v4di") (V4HF "v4di") (V4SI "v4di")
   (V4SF "v4di") (V4DI "v4di") (V4DF "v4di") (V4TI "v4di")
   (V8QI "v8di") (V8HI "v8di") (V8HF "v8di") (V8SI "v8di")
   (V8SF "v8di") (V8DI "v8di") (V8DF "v8di") (V8TI "v8di")
   (V16QI "v16di") (V16HI "v16di") (V16HF "v16di") (V16SI "v16di")
   (V16SF "v16di") (V16DI "v16di") (V16DF "v16di") (V16TI "v16di")
   (V32QI "v32di") (V32HI "v32di") (V32HF "v32di") (V32SI "v32di")
   (V32SF "v32di") (V32DI "v32di") (V32DF "v32di") (V32TI "v32di")
   (V64QI "v64di") (V64HI "v64di") (V64HF "v64di") (V64SI "v64di")
   (V64SF "v64di") (V64DI "v64di") (V64DF "v64di") (V64TI "v64di")])

(define_mode_attr VnDI
  [(V2QI "V2DI") (V2HI "V2DI") (V2HF "V2DI") (V2SI "V2DI")
   (V2SF "V2DI") (V2DI "V2DI") (V2DF "V2DI") (V2TI "V2DI")
   (V4QI "V4DI") (V4HI "V4DI") (V4HF "V4DI") (V4SI "V4DI")
   (V4SF "V4DI") (V4DI "V4DI") (V4DF "V4DI") (V4TI "V4DI")
   (V8QI "V8DI") (V8HI "V8DI") (V8HF "V8DI") (V8SI "V8DI")
   (V8SF "V8DI") (V8DI "V8DI") (V8DF "V8DI") (V8TI "V8DI")
   (V16QI "V16DI") (V16HI "V16DI") (V16HF "V16DI") (V16SI "V16DI")
   (V16SF "V16DI") (V16DI "V16DI") (V16DF "V16DI") (V16TI "V16DI")
   (V32QI "V32DI") (V32HI "V32DI") (V32HF "V32DI") (V32SI "V32DI")
   (V32SF "V32DI") (V32DI "V32DI") (V32DF "V32DI") (V32TI "V32DI")
   (V64QI "V64DI") (V64HI "V64DI") (V64HF "V64DI") (V64SI "V64DI")
   (V64SF "V64DI") (V64DI "V64DI") (V64DF "V64DI") (V64TI "V64DI")])

(define_mode_attr sdwa
  [(V2QI "BYTE_0") (V2HI "WORD_0") (V2SI "DWORD")
   (V4QI "BYTE_0") (V4HI "WORD_0") (V4SI "DWORD")
   (V8QI "BYTE_0") (V8HI "WORD_0") (V8SI "DWORD")
   (V16QI "BYTE_0") (V16HI "WORD_0") (V16SI "DWORD")
   (V32QI "BYTE_0") (V32HI "WORD_0") (V32SI "DWORD")
   (V64QI "BYTE_0") (V64HI "WORD_0") (V64SI "DWORD")])

;; }}}
;; {{{ Substitutions

(define_subst_attr "exec" "vec_merge"
		   "" "_exec")
(define_subst_attr "exec_clobber" "vec_merge_with_clobber"
		   "" "_exec")
(define_subst_attr "exec_vcc" "vec_merge_with_vcc"
		   "" "_exec")
(define_subst_attr "exec_scatter" "scatter_store"
		   "" "_exec")

(define_subst "vec_merge"
  [(set (match_operand:V_MOV 0)
	(match_operand:V_MOV 1))]
  ""
  [(set (match_dup 0)
	(vec_merge:V_MOV
	  (match_dup 1)
	  (match_operand:V_MOV 3 "gcn_register_or_unspec_operand" "U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand" "e")))])

(define_subst "vec_merge_with_clobber"
  [(set (match_operand:V_MOV 0)
	(match_operand:V_MOV 1))
   (clobber (match_operand 2))]
  ""
  [(set (match_dup 0)
	(vec_merge:V_MOV
	  (match_dup 1)
	  (match_operand:V_MOV 3 "gcn_register_or_unspec_operand" "U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand" "e")))
   (clobber (match_dup 2))])

(define_subst "vec_merge_with_vcc"
  [(set (match_operand:V_MOV 0)
	(match_operand:V_MOV 1))
   (set (match_operand:DI 2)
	(match_operand:DI 3))]
  ""
  [(parallel
     [(set (match_dup 0)
	   (vec_merge:V_MOV
	     (match_dup 1)
	     (match_operand:V_MOV 4 "gcn_register_or_unspec_operand" "U0")
	     (match_operand:DI 5 "gcn_exec_reg_operand" "e")))
      (set (match_dup 2)
	   (and:DI (match_dup 3)
		   (reg:DI EXEC_REG)))])])

(define_subst "scatter_store"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand 0)
	   (match_operand 1)
	   (match_operand 2)
	   (match_operand 3)]
	  UNSPEC_SCATTER))]
  ""
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_dup 0)
	   (match_dup 1)
	   (match_dup 2)
	   (match_dup 3)
	   (match_operand:DI 4 "gcn_exec_reg_operand" "e")]
	  UNSPEC_SCATTER))])

;; }}}
;; {{{ Vector moves

; This is the entry point for all vector register moves.  Memory accesses can
; come this way also, but will more usually use the reload_in/out,
; gather/scatter, maskload/store, etc.

(define_expand "mov<mode>"
  [(set (match_operand:V_MOV 0 "nonimmediate_operand")
	(match_operand:V_MOV 1 "general_operand"))]
  ""
  {
    /* Bitwise reinterpret casts via SUBREG don't work with GCN vector
       registers, but we can convert the MEM to a mode that does work.  */
    if (MEM_P (operands[0]) && !SUBREG_P (operands[0])
	&& SUBREG_P (operands[1])
	&& GET_MODE_SIZE (GET_MODE (operands[1]))
	   == GET_MODE_SIZE (GET_MODE (SUBREG_REG (operands[1]))))
      {
        rtx src = SUBREG_REG (operands[1]);
        rtx mem = copy_rtx (operands[0]);
	PUT_MODE_RAW (mem, GET_MODE (src));
	emit_move_insn (mem, src);
	DONE;
      }
    if (MEM_P (operands[1]) && !SUBREG_P (operands[1])
	&& SUBREG_P (operands[0])
	&& GET_MODE_SIZE (GET_MODE (operands[0]))
	   == GET_MODE_SIZE (GET_MODE (SUBREG_REG (operands[0]))))
      {
        rtx dest = SUBREG_REG (operands[0]);
        rtx mem = copy_rtx (operands[1]);
	PUT_MODE_RAW (mem, GET_MODE (dest));
	emit_move_insn (dest, mem);
	DONE;
      }

    /* SUBREG of MEM is not supported.  */
    gcc_assert ((!SUBREG_P (operands[0])
		 || !MEM_P (SUBREG_REG (operands[0])))
		&& (!SUBREG_P (operands[1])
		    || !MEM_P (SUBREG_REG (operands[1]))));

    if (MEM_P (operands[0]) && !lra_in_progress && !reload_completed)
      {
	operands[1] = force_reg (<MODE>mode, operands[1]);
	rtx scratch = gen_rtx_SCRATCH (<VnDI>mode);
	rtx a = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[0]));
	rtx v = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[0]));
	rtx expr = gcn_expand_scalar_to_vector_address (<MODE>mode, NULL,
							operands[0],
							scratch);
	emit_insn (gen_scatter<mode>_expr (expr, operands[1], a, v));
	DONE;
      }
    else if (MEM_P (operands[1]) && !lra_in_progress && !reload_completed)
      {
	rtx scratch = gen_rtx_SCRATCH (<VnDI>mode);
	rtx a = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[1]));
	rtx v = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[1]));
	rtx expr = gcn_expand_scalar_to_vector_address (<MODE>mode, NULL,
							operands[1],
							scratch);
	emit_insn (gen_gather<mode>_expr (operands[0], expr, a, v));
	DONE;
      }
    else if ((MEM_P (operands[0]) || MEM_P (operands[1])))
      {
        gcc_assert (!reload_completed);
	rtx scratch = gen_reg_rtx (<VnDI>mode);
	emit_insn (gen_mov<mode>_sgprbase (operands[0], operands[1], scratch));
	DONE;
      }
  })

; A pseudo instruction that helps LRA use the "U0" constraint.

(define_insn "mov<mode>_unspec"
  [(set (match_operand:V_MOV 0 "nonimmediate_operand" "=v")
	(match_operand:V_MOV 1 "gcn_unspec_operand"   " U"))]
  ""
  ""
  [(set_attr "type" "unknown")
   (set_attr "length" "0")])

(define_insn "*mov<mode>"
  [(set (match_operand:V_1REG 0 "nonimmediate_operand")
	(match_operand:V_1REG 1 "general_operand"))]
  ""
  {@ [cons: =0, 1; attrs: type, length, gcn_version]
  [v  ,vA;vop1     ,4,*    ] v_mov_b32\t%0, %1
  [v  ,B ;vop1     ,8,*    ] ^
  [v  ,a ;vop3p_mai,8,*    ] v_accvgpr_read_b32\t%0, %1
  [$a ,v ;vop3p_mai,8,*    ] v_accvgpr_write_b32\t%0, %1
  [a  ,a ;vop1     ,4,cdna2] v_accvgpr_mov_b32\t%0, %1
  })

(define_insn "mov<mode>_exec"
  [(set (match_operand:V_1REG 0 "nonimmediate_operand")
	(vec_merge:V_1REG
	  (match_operand:V_1REG 1 "general_operand")
	  (match_operand:V_1REG 2 "gcn_alu_or_unspec_operand")
	  (match_operand:DI 3 "register_operand")))
   (clobber (match_scratch:<VnDI> 4))]
  "!MEM_P (operands[0]) || REG_P (operands[1])"
  {@ [cons: =0, 1, 2, 3, =4; attrs: type, length]
  [v,vA,U0,e ,X ;vop1 ,4 ] v_mov_b32\t%0, %1
  [v,B ,U0,e ,X ;vop1 ,8 ] v_mov_b32\t%0, %1
  [v,v ,vA,cV,X ;vop2 ,4 ] v_cndmask_b32\t%0, %2, %1, vcc
  [v,vA,vA,Sv,X ;vop3a,8 ] v_cndmask_b32\t%0, %2, %1, %3
  [v,m ,U0,e ,&v;*    ,16] #
  [m,v ,U0,e ,&v;*    ,16] #
  })

; This variant does not accept an unspec, but does permit MEM
; read/modify/write which is necessary for maskstore.

;(define_insn "*mov<mode>_exec_match"
;  [(set (match_operand:V_1REG 0 "nonimmediate_operand" "=v,v, v, m")
;	(vec_merge:V_1REG
;	  (match_operand:V_1REG 1 "general_operand"	"vA,B, m, v")
;	  (match_dup 0)
;	  (match_operand:DI 2 "gcn_exec_reg_operand"	" e,e, e, e")))
;   (clobber (match_scratch:<VnDI> 3			"=X,X,&v,&v"))]
;  "!MEM_P (operands[0]) || REG_P (operands[1])"
;  "@
;  v_mov_b32\t%0, %1
;  v_mov_b32\t%0, %1
;  #
;  #"
;  [(set_attr "type" "vop1,vop1,*,*")
;   (set_attr "length" "4,8,16,16")])

(define_insn "*mov<mode>"
  [(set (match_operand:V_2REG 0 "nonimmediate_operand" "=v, v,$a,a")
	(match_operand:V_2REG 1 "general_operand"      "vDB,a, v,a"))]
  ""
  "@
   * if (!REG_P (operands[1]) || REGNO (operands[0]) <= REGNO (operands[1])) \
       return \"v_mov_b32\t%L0, %L1\;v_mov_b32\t%H0, %H1\"; \
     else \
       return \"v_mov_b32\t%H0, %H1\;v_mov_b32\t%L0, %L1\";
   * if (REGNO (operands[0]) <= REGNO (operands[1])) \
       return \"v_accvgpr_read_b32\t%L0, %L1\;v_accvgpr_read_b32\t%H0, %H1\"; \
     else \
       return \"v_accvgpr_read_b32\t%H0, %H1\;v_accvgpr_read_b32\t%L0, %L1\";
   * if (REGNO (operands[0]) <= REGNO (operands[1])) \
       return \"v_accvgpr_write_b32\t%L0, %L1\;v_accvgpr_write_b32\t%H0, %H1\"; \
     else \
       return \"v_accvgpr_write_b32\t%H0, %H1\;v_accvgpr_write_b32\t%L0, %L1\";
   * if (REGNO (operands[0]) <= REGNO (operands[1])) \
       return \"v_accvgpr_mov_b32\t%L0, %L1\;v_accvgpr_mov_b32\t%H0, %H1\"; \
     else \
       return \"v_accvgpr_mov_b32\t%H0, %H1\;v_accvgpr_mov_b32\t%L0, %L1\";"
  [(set_attr "type" "vmult,vmult,vmult,vmult")
   (set_attr "length" "16,16,16,8")
   (set_attr "gcn_version" "*,*,*,cdna2")])

(define_insn "mov<mode>_exec"
  [(set (match_operand:V_2REG 0 "nonimmediate_operand" "= v,   v,   v, v, m")
	(vec_merge:V_2REG
	  (match_operand:V_2REG 1 "general_operand"    "vDB,  v0,  v0, m, v")
	  (match_operand:V_2REG 2 "gcn_alu_or_unspec_operand"
						       " U0,vDA0,vDA0,U0,U0")
	  (match_operand:DI 3 "register_operand"       "  e,  cV,  Sv, e, e")))
   (clobber (match_scratch:<VnDI> 4		       "= X,   X,   X,&v,&v"))]
  "!MEM_P (operands[0]) || REG_P (operands[1])"
  {
    if (!REG_P (operands[1]) || REGNO (operands[0]) <= REGNO (operands[1]))
      switch (which_alternative)
	{
	case 0:
	  return "v_mov_b32\t%L0, %L1\;v_mov_b32\t%H0, %H1";
	case 1:
	  return "v_cndmask_b32\t%L0, %L2, %L1, vcc\;"
		 "v_cndmask_b32\t%H0, %H2, %H1, vcc";
	case 2:
	  return "v_cndmask_b32\t%L0, %L2, %L1, %3\;"
		 "v_cndmask_b32\t%H0, %H2, %H1, %3";
	}
    else
      switch (which_alternative)
	{
	case 0:
	  return "v_mov_b32\t%H0, %H1\;v_mov_b32\t%L0, %L1";
	case 1:
	  return "v_cndmask_b32\t%H0, %H2, %H1, vcc\;"
		 "v_cndmask_b32\t%L0, %L2, %L1, vcc";
	case 2:
	  return "v_cndmask_b32\t%H0, %H2, %H1, %3\;"
		 "v_cndmask_b32\t%L0, %L2, %L1, %3";
	}

    return "#";
  }
  [(set_attr "type" "vmult,vmult,vmult,*,*")
   (set_attr "length" "16,16,16,16,16")])

(define_insn "*mov<mode>_4reg"
  [(set (match_operand:V_4REG 0 "nonimmediate_operand")
	(match_operand:V_4REG 1 "general_operand"))]
  ""
  {@ [cons: =0, 1; attrs: type, length, gcn_version]
  [v ,vDB;vmult,16,*    ]           v_mov_b32\t%L0, %L1\;          v_mov_b32\t%H0, %H1\;          v_mov_b32\t%J0, %J1\;          v_mov_b32\t%K0, %K1
  [v ,a  ;vmult,32,*    ]  v_accvgpr_read_b32\t%L0, %L1\; v_accvgpr_read_b32\t%H0, %H1\; v_accvgpr_read_b32\t%J0, %J1\; v_accvgpr_read_b32\t%K0, %K1
  [$a,v  ;vmult,32,*    ] v_accvgpr_write_b32\t%L0, %L1\;v_accvgpr_write_b32\t%H0, %H1\;v_accvgpr_write_b32\t%J0, %J1\;v_accvgpr_write_b32\t%K0, %K1
  [a ,a  ;vmult,32,cdna2]   v_accvgpr_mov_b32\t%L0, %L1\;  v_accvgpr_mov_b32\t%H0, %H1\;  v_accvgpr_mov_b32\t%J0, %J1\;  v_accvgpr_mov_b32\t%K0, %K1
  })

(define_insn "mov<mode>_exec"
  [(set (match_operand:V_4REG 0 "nonimmediate_operand" "= v,   v,   v, v, m")
	(vec_merge:V_4REG
	  (match_operand:V_4REG 1 "general_operand"    "vDB,  v0,  v0, m, v")
	  (match_operand:V_4REG 2 "gcn_alu_or_unspec_operand"
						       " U0,vDA0,vDA0,U0,U0")
	  (match_operand:DI 3 "register_operand"       "  e,  cV,  Sv, e, e")))
   (clobber (match_scratch:<VnDI> 4		       "= X,   X,   X,&v,&v"))]
  "!MEM_P (operands[0]) || REG_P (operands[1])"
  {
    if (!REG_P (operands[1]) || REGNO (operands[0]) <= REGNO (operands[1]))
      switch (which_alternative)
	{
	case 0:
	  return "v_mov_b32\t%L0, %L1\;v_mov_b32\t%H0, %H1\;"
                 "v_mov_b32\t%J0, %J1\;v_mov_b32\t%K0, %K1";
	case 1:
	  return "v_cndmask_b32\t%L0, %L2, %L1, vcc\;"
		 "v_cndmask_b32\t%H0, %H2, %H1, vcc\;"
		 "v_cndmask_b32\t%J0, %J2, %J1, vcc\;"
		 "v_cndmask_b32\t%K0, %K2, %K1, vcc";
	case 2:
	  return "v_cndmask_b32\t%L0, %L2, %L1, %3\;"
		 "v_cndmask_b32\t%H0, %H2, %H1, %3\;"
		 "v_cndmask_b32\t%J0, %J2, %J1, %3\;"
		 "v_cndmask_b32\t%K0, %K2, %K1, %3";
	}
    else
      switch (which_alternative)
	{
	case 0:
	  return "v_mov_b32\t%H0, %H1\;v_mov_b32\t%L0, %L1\;"
                 "v_mov_b32\t%J0, %J1\;v_mov_b32\t%K0, %K1";
	case 1:
	  return "v_cndmask_b32\t%H0, %H2, %H1, vcc\;"
		 "v_cndmask_b32\t%L0, %L2, %L1, vcc\;"
		 "v_cndmask_b32\t%J0, %J2, %J1, vcc\;"
		 "v_cndmask_b32\t%K0, %K2, %K1, vcc";
	case 2:
	  return "v_cndmask_b32\t%H0, %H2, %H1, %3\;"
		 "v_cndmask_b32\t%L0, %L2, %L1, %3\;"
		 "v_cndmask_b32\t%J0, %J2, %J1, %3\;"
		 "v_cndmask_b32\t%K0, %K2, %K1, %3";
	}

    return "#";
  }
  [(set_attr "type" "vmult,vmult,vmult,*,*")
   (set_attr "length" "32")])

; This variant does not accept an unspec, but does permit MEM
; read/modify/write which is necessary for maskstore.

;(define_insn "*mov<mode>_exec_match"
;  [(set (match_operand:V_2REG 0 "nonimmediate_operand" "=v, v, m")
;	(vec_merge:V_2REG
;	  (match_operand:V_2REG 1 "general_operand"	"vDB, m, v")
;	  (match_dup 0)
;	  (match_operand:DI 2 "gcn_exec_reg_operand"	" e, e, e")))
;   (clobber (match_scratch:<VnDI> 3			"=X,&v,&v"))]
;  "!MEM_P (operands[0]) || REG_P (operands[1])"
;  "@
;   * if (!REG_P (operands[1]) || REGNO (operands[0]) <= REGNO (operands[1])) \
;       return \"v_mov_b32\t%L0, %L1\;v_mov_b32\t%H0, %H1\"; \
;     else \
;       return \"v_mov_b32\t%H0, %H1\;v_mov_b32\t%L0, %L1\";
;   #
;   #"
;  [(set_attr "type" "vmult,*,*")
;   (set_attr "length" "16,16,16")])

; A SGPR-base load looks like:
;   <load> v, Sv
;
; There's no hardware instruction that corresponds to this, but vector base
; addresses are placed in an SGPR because it is easier to add to a vector.
; We also have a temporary vT, and the vector v1 holding numbered lanes.
;
; Rewrite as:
;   vT = v1 << log2(element-size)
;   vT += Sv
;   flat_load v, vT

(define_insn "@mov<mode>_sgprbase"
  [(set (match_operand:V_1REG 0 "nonimmediate_operand")
	(unspec:V_1REG
	  [(match_operand:V_1REG 1 "general_operand")]
	  UNSPEC_SGPRBASE))
   (clobber (match_operand:<VnDI> 2 "register_operand"))]
  "lra_in_progress || reload_completed"
  {@ [cons: =0, 1, =2; attrs: type, length, gcn_version]
  [v,vA,&v;vop1,4 ,*    ] v_mov_b32\t%0, %1
  [v,vB,&v;vop1,8 ,*    ] ^
  [v,m ,&v;*   ,12,*    ] #
  [m,v ,&v;*   ,12,*    ] #
  [a,m ,&v;*   ,12,cdna2] #
  [m,a ,&v;*   ,12,cdna2] #
  })

(define_insn "@mov<mode>_sgprbase"
  [(set (match_operand:V_2REG 0 "nonimmediate_operand" "= v, v, m, a, m")
	(unspec:V_2REG
	  [(match_operand:V_2REG 1 "general_operand"   "vDB, m, v, m, a")]
	  UNSPEC_SGPRBASE))
   (clobber (match_operand:<VnDI> 2 "register_operand"  "=&v,&v,&v,&v,&v"))]
  "lra_in_progress || reload_completed"
  "@
   * if (!REG_P (operands[1]) || REGNO (operands[0]) <= REGNO (operands[1])) \
       return \"v_mov_b32\t%L0, %L1\;v_mov_b32\t%H0, %H1\"; \
     else \
       return \"v_mov_b32\t%H0, %H1\;v_mov_b32\t%L0, %L1\";
   #
   #
   #
   #"
  [(set_attr "type" "vmult,*,*,*,*")
   (set_attr "length" "8,12,12,12,12")
   (set_attr "gcn_version" "*,*,*,cdna2,cdna2")])

(define_insn "@mov<mode>_sgprbase"
  [(set (match_operand:V_4REG 0 "nonimmediate_operand")
	(unspec:V_4REG
	  [(match_operand:V_4REG 1 "general_operand")]
	  UNSPEC_SGPRBASE))
   (clobber (match_operand:<VnDI> 2 "register_operand"))]
  "lra_in_progress || reload_completed"
  {@ [cons: =0, 1, =2; attrs: type, length]
  [v,vDB,&v;vmult,8 ] v_mov_b32\t%L0, %L1\;v_mov_b32\t%H0, %H1\;v_mov_b32\t%J0, %J1\;v_mov_b32\t%K0, %K1
  [v,m  ,&v;*    ,12] #
  [m,v  ,&v;*    ,12] #
  })

; Expand scalar addresses into gather/scatter patterns

(define_split
  [(set (match_operand:V_MOV 0 "memory_operand")
	(unspec:V_MOV
	  [(match_operand:V_MOV 1 "general_operand")]
	  UNSPEC_SGPRBASE))
   (clobber (match_scratch:<VnDI> 2))]
  ""
  [(set (mem:BLK (scratch))
	(unspec:BLK [(match_dup 5) (match_dup 1) (match_dup 6) (match_dup 7)]
		    UNSPEC_SCATTER))]
  {
    operands[5] = gcn_expand_scalar_to_vector_address (<MODE>mode, NULL,
						       operands[0],
						       operands[2]);
    operands[6] = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[0]));
    operands[7] = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[0]));
  })

(define_split
  [(set (match_operand:V_MOV 0 "memory_operand")
	(vec_merge:V_MOV
	  (match_operand:V_MOV 1 "general_operand")
	  (match_operand:V_MOV 2 "")
	  (match_operand:DI 3 "gcn_exec_reg_operand")))
   (clobber (match_scratch:<VnDI> 4))]
  ""
  [(set (mem:BLK (scratch))
	(unspec:BLK [(match_dup 5) (match_dup 1)
		     (match_dup 6) (match_dup 7) (match_dup 3)]
		    UNSPEC_SCATTER))]
  {
    operands[5] = gcn_expand_scalar_to_vector_address (<MODE>mode,
						       operands[3],
						       operands[0],
						       operands[4]);
    operands[6] = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[0]));
    operands[7] = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[0]));
  })

(define_split
  [(set (match_operand:V_MOV 0 "nonimmediate_operand")
	(unspec:V_MOV
	  [(match_operand:V_MOV 1 "memory_operand")]
	  UNSPEC_SGPRBASE))
   (clobber (match_scratch:<VnDI> 2))]
  ""
  [(set (match_dup 0)
	(unspec:V_MOV [(match_dup 5) (match_dup 6) (match_dup 7)
		       (mem:BLK (scratch))]
		      UNSPEC_GATHER))]
  {
    operands[5] = gcn_expand_scalar_to_vector_address (<MODE>mode, NULL,
						       operands[1],
						       operands[2]);
    operands[6] = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[1]));
    operands[7] = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[1]));
  })

(define_split
  [(set (match_operand:V_MOV 0 "nonimmediate_operand")
	(vec_merge:V_MOV
	  (match_operand:V_MOV 1 "memory_operand")
	  (match_operand:V_MOV 2 "")
	  (match_operand:DI 3 "gcn_exec_reg_operand")))
   (clobber (match_scratch:<VnDI> 4))]
  ""
  [(set (match_dup 0)
	(vec_merge:V_MOV
	  (unspec:V_MOV [(match_dup 5) (match_dup 6) (match_dup 7)
			 (mem:BLK (scratch))]
			 UNSPEC_GATHER)
	  (match_dup 2)
	  (match_dup 3)))]
  {
    operands[5] = gcn_expand_scalar_to_vector_address (<MODE>mode,
						       operands[3],
						       operands[1],
						       operands[4]);
    operands[6] = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[1]));
    operands[7] = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[1]));
  })

; TODO: Add zero/sign extending variants.

;; }}}
;; {{{ Lane moves

; v_writelane and v_readlane work regardless of exec flags.
; We allow source to be scratch.
;
; FIXME these should take A immediates

(define_insn "*vec_set<mode>"
  [(set (match_operand:V_1REG 0 "register_operand"		   "= v")
	(vec_merge:V_1REG
	  (vec_duplicate:V_1REG
	    (match_operand:<SCALAR_MODE> 1 "register_operand"	   " Sv"))
	  (match_operand:V_1REG 3 "gcn_register_or_unspec_operand" " U0")
	  (ashift (const_int 1)
		  (match_operand:SI 2 "gcn_alu_operand"		   "SvB"))))]
  ""
  "v_writelane_b32 %0, %1, %2"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")
   (set_attr "exec" "none")
   (set_attr "laneselect" "yes")])

; FIXME: 64bit operations really should be splitters, but I am not sure how
; to represent vertical subregs.
(define_insn "*vec_set<mode>"
  [(set (match_operand:V_2REG 0 "register_operand"		   "= v")
	(vec_merge:V_2REG
	  (vec_duplicate:V_2REG
	    (match_operand:<SCALAR_MODE> 1 "register_operand"	   " Sv"))
	  (match_operand:V_2REG 3 "gcn_register_or_unspec_operand" " U0")
	  (ashift (const_int 1)
		  (match_operand:SI 2 "gcn_alu_operand"		   "SvB"))))]
  ""
  "v_writelane_b32 %L0, %L1, %2\;v_writelane_b32 %H0, %H1, %2"
  [(set_attr "type" "vmult")
   (set_attr "length" "16")
   (set_attr "exec" "none")
   (set_attr "laneselect" "yes")])

(define_expand "vec_set<mode>"
  [(set (match_operand:V_MOV 0 "register_operand")
	(vec_merge:V_MOV
	  (vec_duplicate:V_MOV
	    (match_operand:<SCALAR_MODE> 1 "register_operand"))
	  (match_dup 0)
	  (ashift (const_int 1) (match_operand:SI 2 "gcn_alu_operand"))))]
  "")

(define_insn "*vec_set<mode>_1"
  [(set (match_operand:V_1REG 0 "register_operand"		   "=v")
	(vec_merge:V_1REG
	  (vec_duplicate:V_1REG
	    (match_operand:<SCALAR_MODE> 1 "register_operand"	   "Sv"))
	  (match_operand:V_1REG 3 "gcn_register_or_unspec_operand" "U0")
	  (match_operand:SI 2 "const_int_operand"		   " i")))]
  "((unsigned) exact_log2 (INTVAL (operands[2])) < GET_MODE_NUNITS (<MODE>mode))"
  {
    operands[2] = GEN_INT (exact_log2 (INTVAL (operands[2])));
    return "v_writelane_b32 %0, %1, %2";
  }
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")
   (set_attr "exec" "none")
   (set_attr "laneselect" "yes")])

(define_insn "*vec_set<mode>_1"
  [(set (match_operand:V_2REG 0 "register_operand"		   "=v")
	(vec_merge:V_2REG
	  (vec_duplicate:V_2REG
	    (match_operand:<SCALAR_MODE> 1 "register_operand"	   "Sv"))
	  (match_operand:V_2REG 3 "gcn_register_or_unspec_operand" "U0")
	  (match_operand:SI 2 "const_int_operand"		   " i")))]
  "((unsigned) exact_log2 (INTVAL (operands[2])) < GET_MODE_NUNITS (<MODE>mode))"
  {
    operands[2] = GEN_INT (exact_log2 (INTVAL (operands[2])));
    return "v_writelane_b32 %L0, %L1, %2\;v_writelane_b32 %H0, %H1, %2";
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "16")
   (set_attr "exec" "none")
   (set_attr "laneselect" "yes")])

(define_insn "vec_duplicate<mode><exec>"
  [(set (match_operand:V_1REG 0 "register_operand"	   "=v")
	(vec_duplicate:V_1REG
	  (match_operand:<SCALAR_MODE> 1 "gcn_alu_operand" "SvB")))]
  ""
  "v_mov_b32\t%0, %1"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "vec_duplicate<mode><exec>"
  [(set (match_operand:V_2REG 0 "register_operand"	   "=  v")
	(vec_duplicate:V_2REG
	  (match_operand:<SCALAR_MODE> 1 "gcn_alu_operand" "SvDB")))]
  ""
  "v_mov_b32\t%L0, %L1\;v_mov_b32\t%H0, %H1"
  [(set_attr "type" "vop3a")
   (set_attr "length" "16")])

(define_insn "vec_duplicate<mode><exec>"
  [(set (match_operand:V_4REG 0 "register_operand"	   "=  v")
	(vec_duplicate:V_4REG
	  (match_operand:<SCALAR_MODE> 1 "gcn_alu_operand" "SvDB")))]
  ""
  "v_mov_b32\t%L0, %L1\;v_mov_b32\t%H0, %H1\;v_mov_b32\t%J0, %J1\;v_mov_b32\t%K0, %K1"
  [(set_attr "type" "mult")
   (set_attr "length" "32")])

(define_insn "vec_extract<mode><scalar_mode>"
  [(set (match_operand:<SCALAR_MODE> 0 "register_operand"  "=Sg")
	(vec_select:<SCALAR_MODE>
	  (match_operand:V_1REG 1 "register_operand"	   "  v")
	  (parallel [(match_operand:SI 2 "gcn_alu_operand" "SvB")])))]
  ""
  "v_readlane_b32 %0, %1, %2"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")
   (set_attr "exec" "none")
   (set_attr "laneselect" "yes")])

(define_insn "vec_extract<mode><scalar_mode>"
  [(set (match_operand:<SCALAR_MODE> 0 "register_operand"  "=&Sg")
	(vec_select:<SCALAR_MODE>
	  (match_operand:V_2REG 1 "register_operand"	   "   v")
	  (parallel [(match_operand:SI 2 "gcn_alu_operand" " SvB")])))]
  ""
  "v_readlane_b32 %L0, %L1, %2\;v_readlane_b32 %H0, %H1, %2"
  [(set_attr "type" "vmult")
   (set_attr "length" "16")
   (set_attr "exec" "none")
   (set_attr "laneselect" "yes")])

(define_insn "vec_extract<mode><scalar_mode>"
  [(set (match_operand:<SCALAR_MODE> 0 "register_operand"  "=&Sg")
	(vec_select:<SCALAR_MODE>
	  (match_operand:V_4REG 1 "register_operand"	   "   v")
	  (parallel [(match_operand:SI 2 "gcn_alu_operand" " SvB")])))]
  ""
  "v_readlane_b32 %L0, %L1, %2\;v_readlane_b32 %H0, %H1, %2\;v_readlane_b32 %J0, %J1, %2\;v_readlane_b32 %K0, %K1, %2"
  [(set_attr "type" "vmult")
   (set_attr "length" "32")
   (set_attr "exec" "none")
   (set_attr "laneselect" "yes")])

(define_insn "vec_extract<V_1REG:mode><V_1REG_ALT:mode>_nop"
  [(set (match_operand:V_1REG_ALT 0 "register_operand" "=v,v")
	(vec_select:V_1REG_ALT
	  (match_operand:V_1REG 1 "register_operand"   " 0,v")
	  (match_operand 2 "ascending_zero_int_parallel" "")))]
  "MODE_VF (<V_1REG_ALT:MODE>mode) < MODE_VF (<V_1REG:MODE>mode)
   && <V_1REG_ALT:SCALAR_MODE>mode == <V_1REG:SCALAR_MODE>mode
   /* This comment silences a warning for operands[2]. */"
  "@
  ; in-place extract %0
  v_mov_b32\t%L0, %L1"
  [(set_attr "type" "vmult")
   (set_attr "length" "0,8")])
  
(define_insn "vec_extract<V_2REG:mode><V_2REG_ALT:mode>_nop"
  [(set (match_operand:V_2REG_ALT 0 "register_operand" "=v,v")
	(vec_select:V_2REG_ALT
	  (match_operand:V_2REG 1 "register_operand"   " 0,v")
	  (match_operand 2 "ascending_zero_int_parallel" "")))]
  "MODE_VF (<V_2REG_ALT:MODE>mode) < MODE_VF (<V_2REG:MODE>mode)
   && <V_2REG_ALT:SCALAR_MODE>mode == <V_2REG:SCALAR_MODE>mode
   /* This comment silences a warning for operands[2]. */"
  "@
  ; in-place extract %0
  v_mov_b32\t%L0, %L1\;v_mov_b32\t%H0, %H1"
  [(set_attr "type" "vmult")
   (set_attr "length" "0,8")])
  
(define_insn "vec_extract<V_4REG:mode><V_4REG_ALT:mode>_nop"
  [(set (match_operand:V_4REG_ALT 0 "register_operand" "=v,v")
	(vec_select:V_4REG_ALT
	  (match_operand:V_4REG 1 "register_operand"   " 0,v")
	  (match_operand 2 "ascending_zero_int_parallel" "")))]
  "MODE_VF (<V_4REG_ALT:MODE>mode) < MODE_VF (<V_4REG:MODE>mode)
   && <V_4REG_ALT:SCALAR_MODE>mode == <V_4REG:SCALAR_MODE>mode"
  "@
  ; in-place extract %0
  v_mov_b32\t%L0, %L1\;v_mov_b32\t%H0, %H1\;v_mov_b32\t%J0, %J1\;v_mov_b32\t%K0, %K1"
  [(set_attr "type" "vmult")
   (set_attr "length" "0,16")])
  
(define_expand "vec_extract<V_MOV:mode><V_MOV_ALT:mode>"
  [(match_operand:V_MOV_ALT 0 "register_operand")
   (match_operand:V_MOV 1 "register_operand")
   (match_operand 2 "immediate_operand")]
  "MODE_VF (<V_MOV_ALT:MODE>mode) < MODE_VF (<V_MOV:MODE>mode)
   && <V_MOV_ALT:SCALAR_MODE>mode == <V_MOV:SCALAR_MODE>mode
   && (!TARGET_RDNA2_PLUS || MODE_VF (<V_MOV:MODE>mode) <= 32)"
  {
    int numlanes = GET_MODE_NUNITS (<V_MOV_ALT:MODE>mode);
    int firstlane = INTVAL (operands[2]) * numlanes;
    rtx tmp;

    if (firstlane == 0)
      {
	rtx parallel = gen_rtx_PARALLEL (<V_MOV:MODE>mode,
					  rtvec_alloc (numlanes));
	for (int i = 0; i < numlanes; i++)
	  XVECEXP (parallel, 0, i) = GEN_INT (i);
	emit_insn (gen_vec_extract<V_MOV:mode><V_MOV_ALT:mode>_nop
		   (operands[0], operands[1], parallel));
      } else {
        /* FIXME: optimize this by using DPP where available.  */

        rtx permutation = gen_reg_rtx (<V_MOV:VnSI>mode);
	emit_insn (gen_vec_series<V_MOV:vnsi> (permutation,
					       GEN_INT (firstlane*4),
					       GEN_INT (4)));

	tmp = gen_reg_rtx (<V_MOV:MODE>mode);
	emit_insn (gen_ds_bpermute<V_MOV:mode> (tmp, permutation, operands[1],
						get_exec (<V_MOV:MODE>mode)));

	emit_move_insn (operands[0],
			gen_rtx_SUBREG (<V_MOV_ALT:MODE>mode, tmp, 0));
      }
    DONE;
  })

(define_expand "extract_last_<mode>"
  [(match_operand:<SCALAR_MODE> 0 "register_operand")
   (match_operand:DI 1 "gcn_alu_operand")
   (match_operand:V_MOV 2 "register_operand")]
  "can_create_pseudo_p ()"
  {
    rtx dst = operands[0];
    rtx mask = operands[1];
    rtx vect = operands[2];
    rtx tmpreg = gen_reg_rtx (SImode);

    emit_insn (gen_clzdi2 (tmpreg, mask));
    emit_insn (gen_subsi3 (tmpreg, GEN_INT (63), tmpreg));
    emit_insn (gen_vec_extract<mode><scalar_mode> (dst, vect, tmpreg));
    DONE;
  })

(define_expand "fold_extract_last_<mode>"
  [(match_operand:<SCALAR_MODE> 0 "register_operand")
   (match_operand:<SCALAR_MODE> 1 "gcn_alu_operand")
   (match_operand:DI 2 "gcn_alu_operand")
   (match_operand:V_MOV 3 "register_operand")]
  "can_create_pseudo_p ()"
  {
    rtx dst = operands[0];
    rtx default_value = operands[1];
    rtx mask = operands[2];
    rtx vect = operands[3];
    rtx else_label = gen_label_rtx ();
    rtx end_label = gen_label_rtx ();

    rtx cond = gen_rtx_EQ (VOIDmode, mask, const0_rtx);
    emit_jump_insn (gen_cbranchdi4 (cond, mask, const0_rtx, else_label));
    emit_insn (gen_extract_last_<mode> (dst, mask, vect));
    emit_jump_insn (gen_jump (end_label));
    emit_barrier ();
    emit_label (else_label);
    emit_move_insn (dst, default_value);
    emit_label (end_label);
    DONE;
  })

(define_expand "vec_init<mode><scalar_mode>"
  [(match_operand:V_MOV 0 "register_operand")
   (match_operand 1)]
  ""
  {
    gcn_expand_vector_init (operands[0], operands[1]);
    DONE;
  })

(define_expand "vec_init<V_MOV:mode><V_MOV_ALT:mode>"
  [(match_operand:V_MOV 0 "register_operand")
   (match_operand:V_MOV_ALT 1)]
  "<V_MOV:SCALAR_MODE>mode == <V_MOV_ALT:SCALAR_MODE>mode
   && MODE_VF (<V_MOV_ALT:MODE>mode) < MODE_VF (<V_MOV:MODE>mode)"
  {
    gcn_expand_vector_init (operands[0], operands[1]);
    DONE;
  })

;; }}}
;; {{{ Scatter / Gather

;; GCN does not have an instruction for loading a vector from contiguous
;; memory so *all* loads and stores are eventually converted to scatter
;; or gather.
;;
;; GCC does not permit MEM to hold vectors of addresses, so we must use an
;; unspec.  The unspec formats are as follows:
;;
;;     (unspec:V??
;;	 [(<address expression>)
;;	  (<addr_space_t>)
;;	  (<use_glc>)
;;	  (mem:BLK (scratch))]
;;	 UNSPEC_GATHER)
;;
;;     (unspec:BLK
;;	  [(<address expression>)
;;	   (<source register>)
;;	   (<addr_space_t>)
;;	   (<use_glc>)
;;	   (<exec>)]
;;	  UNSPEC_SCATTER)
;;
;; - Loads are expected to be wrapped in a vec_merge, so do not need <exec>.
;; - The mem:BLK does not contain any real information, but indicates that an
;;   unknown memory read is taking place.  Stores are expected to use a similar
;;   mem:BLK outside the unspec.
;; - The address space and glc (volatile) fields are there to replace the
;;   fields normally found in a MEM.
;; - Multiple forms of address expression are supported, below.
;;
;; TODO: implement combined gather and zero_extend, but only for -msram-ecc=on

(define_expand "gather_load<mode><vnsi>"
  [(match_operand:V_MOV 0 "register_operand")
   (match_operand:DI 1 "register_operand")
   (match_operand:<VnSI> 2 "register_operand")
   (match_operand 3 "immediate_operand")
   (match_operand:SI 4 "gcn_alu_operand")]
  ""
  {
    rtx addr = gcn_expand_scaled_offsets (DEFAULT_ADDR_SPACE, operands[1],
					  operands[2], operands[4],
					  INTVAL (operands[3]), NULL);

    if (GET_MODE (addr) == <VnDI>mode)
      emit_insn (gen_gather<mode>_insn_1offset (operands[0], addr, const0_rtx,
						const0_rtx, const0_rtx));
    else
      emit_insn (gen_gather<mode>_insn_2offsets (operands[0], operands[1],
						 addr, const0_rtx, const0_rtx,
						 const0_rtx));
    DONE;
  })

; Allow any address expression
(define_expand "gather<mode>_expr<exec>"
  [(set (match_operand:V_MOV 0 "register_operand")
	(unspec:V_MOV
	  [(match_operand 1 "")
	   (match_operand 2 "immediate_operand")
	   (match_operand 3 "immediate_operand")
	   (mem:BLK (scratch))]
	  UNSPEC_GATHER))]
    ""
    {})

(define_insn "gather<mode>_insn_1offset<exec>"
  [(set (match_operand:V_MOV 0 "register_operand"		   "=v,a,&v,&a")
	(unspec:V_MOV
	  [(plus:<VnDI> (match_operand:<VnDI> 1 "register_operand" " v,v, v, v")
			(vec_duplicate:<VnDI>
			  (match_operand 2 "immediate_operand"	   " n,n, n, n")))
	   (match_operand 3 "immediate_operand"			   " n,n, n, n")
	   (match_operand 4 "immediate_operand"			   " n,n, n, n")
	   (mem:BLK (scratch))]
	  UNSPEC_GATHER))]
  "(AS_FLAT_P (INTVAL (operands[3]))
    && ((TARGET_GCN3 && INTVAL(operands[2]) == 0)
	|| ((unsigned HOST_WIDE_INT)INTVAL(operands[2]) < 0x1000)))
    || (AS_GLOBAL_P (INTVAL (operands[3]))
	&& (((unsigned HOST_WIDE_INT)INTVAL(operands[2]) + 0x1000) < 0x2000))"
  {
    addr_space_t as = INTVAL (operands[3]);
    const char *glc = INTVAL (operands[4]) ? " glc" : "";

    static char buf[200];
    if (AS_FLAT_P (as))
      {
	if (TARGET_GCN5_PLUS)
	  sprintf (buf, "flat_load%%o0\t%%0, %%1 offset:%%2%s\;s_waitcnt\t0",
		   glc);
	else
	  sprintf (buf, "flat_load%%o0\t%%0, %%1%s\;s_waitcnt\t0", glc);
      }
    else if (AS_GLOBAL_P (as))
      sprintf (buf, "global_load%%o0\t%%0, %%1, off offset:%%2%s\;"
	       "s_waitcnt\tvmcnt(0)", glc);
    else
      gcc_unreachable ();

    return buf;
  }
  [(set_attr "type" "flat")
   (set_attr "length" "12")
   (set_attr "gcn_version" "*,cdna2,*,cdna2")
   (set_attr "xnack" "off,off,on,on")])

(define_insn "gather<mode>_insn_1offset_ds<exec>"
  [(set (match_operand:V_MOV 0 "register_operand"		   "=v,a")
	(unspec:V_MOV
	  [(plus:<VnSI> (match_operand:<VnSI> 1 "register_operand" " v,v")
			(vec_duplicate:<VnSI>
			  (match_operand 2 "immediate_operand"	   " n,n")))
	   (match_operand 3 "immediate_operand"			   " n,n")
	   (match_operand 4 "immediate_operand"			   " n,n")
	   (mem:BLK (scratch))]
	  UNSPEC_GATHER))]
  "(AS_ANY_DS_P (INTVAL (operands[3]))
    && ((unsigned HOST_WIDE_INT)INTVAL(operands[2]) < 0x10000))"
  {
    addr_space_t as = INTVAL (operands[3]);
    static char buf[200];
    sprintf (buf, "ds_read%%b0\t%%0, %%1 offset:%%2%s\;s_waitcnt\tlgkmcnt(0)",
	     (AS_GDS_P (as) ? " gds" : ""));
    return buf;
  }
  [(set_attr "type" "ds")
   (set_attr "length" "12")
   (set_attr "gcn_version" "*,cdna2")])

(define_insn "gather<mode>_insn_2offsets<exec>"
  [(set (match_operand:V_MOV 0 "register_operand"		"=v,a,&v,&a")
	(unspec:V_MOV
	  [(plus:<VnDI>
	     (plus:<VnDI>
	       (vec_duplicate:<VnDI>
		 (match_operand:DI 1 "register_operand"		"Sv,Sv,Sv,Sv"))
	       (sign_extend:<VnDI>
		 (match_operand:<VnSI> 2 "register_operand"	" v, v, v, v")))
	     (vec_duplicate:<VnDI> (match_operand 3 "immediate_operand"
								" n, n, n, n")))
	   (match_operand 4 "immediate_operand"			" n, n, n, n")
	   (match_operand 5 "immediate_operand"			" n, n, n, n")
	   (mem:BLK (scratch))]
	  UNSPEC_GATHER))]
  "(AS_GLOBAL_P (INTVAL (operands[4]))
    && (((unsigned HOST_WIDE_INT)INTVAL(operands[3]) + 0x1000) < 0x2000))"
  {
    addr_space_t as = INTVAL (operands[4]);
    const char *glc = INTVAL (operands[5]) ? " glc" : "";

    static char buf[200];
    if (AS_GLOBAL_P (as))
      sprintf (buf, "global_load%%o0\t%%0, %%2, %%1 offset:%%3%s\;"
	       "s_waitcnt\tvmcnt(0)", glc);
    else
      gcc_unreachable ();
      
    return buf;
  }
  [(set_attr "type" "flat")
   (set_attr "length" "12")
   (set_attr "gcn_version" "*,cdna2,*,cdna2")
   (set_attr "xnack" "off,off,on,on")])

(define_expand "scatter_store<mode><vnsi>"
  [(match_operand:DI 0 "register_operand")
   (match_operand:<VnSI> 1 "register_operand")
   (match_operand 2 "immediate_operand")
   (match_operand:SI 3 "gcn_alu_operand")
   (match_operand:V_MOV 4 "register_operand")]
  ""
  {
    rtx addr = gcn_expand_scaled_offsets (DEFAULT_ADDR_SPACE, operands[0],
					  operands[1], operands[3],
					  INTVAL (operands[2]), NULL);

    if (GET_MODE (addr) == <VnDI>mode)
      emit_insn (gen_scatter<mode>_insn_1offset (addr, const0_rtx, operands[4],
						 const0_rtx, const0_rtx));
    else
      emit_insn (gen_scatter<mode>_insn_2offsets (operands[0], addr,
						  const0_rtx, operands[4],
						  const0_rtx, const0_rtx));
    DONE;
  })

; Allow any address expression
(define_expand "scatter<mode>_expr<exec_scatter>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:<VnDI> 0 "")
	   (match_operand:V_MOV 1 "register_operand")
	   (match_operand 2 "immediate_operand")
	   (match_operand 3 "immediate_operand")]
	  UNSPEC_SCATTER))]
  ""
  {})

(define_insn "scatter<mode>_insn_1offset<exec_scatter>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(plus:<VnDI> (match_operand:<VnDI> 0 "register_operand" "v,v")
			(vec_duplicate:<VnDI>
			  (match_operand 1 "immediate_operand"	   "n,n")))
	   (match_operand:V_MOV 2 "register_operand"		   "v,a")
	   (match_operand 3 "immediate_operand"			   "n,n")
	   (match_operand 4 "immediate_operand"			   "n,n")]
	  UNSPEC_SCATTER))]
  "(AS_FLAT_P (INTVAL (operands[3]))
    && (INTVAL(operands[1]) == 0
	|| (TARGET_GCN5_PLUS
	    && (unsigned HOST_WIDE_INT)INTVAL(operands[1]) < 0x1000)))
    || (AS_GLOBAL_P (INTVAL (operands[3]))
	&& (((unsigned HOST_WIDE_INT)INTVAL(operands[1]) + 0x1000) < 0x2000))"
  {
    addr_space_t as = INTVAL (operands[3]);
    const char *glc = INTVAL (operands[4]) ? " glc" : "";

    static char buf[200];
    if (AS_FLAT_P (as))
      {
	if (TARGET_GCN5_PLUS)
	  sprintf (buf, "flat_store%%s2\t%%0, %%2 offset:%%1%s", glc);
	else
	  sprintf (buf, "flat_store%%s2\t%%0, %%2%s", glc);
      }
    else if (AS_GLOBAL_P (as))
      sprintf (buf, "global_store%%s2\t%%0, %%2, off offset:%%1%s", glc);
    else
      gcc_unreachable ();

    return buf;
  }
  [(set_attr "type" "flat")
   (set_attr "length" "12")
   (set_attr "gcn_version" "*,cdna2")])

(define_insn "scatter<mode>_insn_1offset_ds<exec_scatter>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(plus:<VnSI> (match_operand:<VnSI> 0 "register_operand" "v,v")
			(vec_duplicate:<VnSI>
			  (match_operand 1 "immediate_operand"	   "n,n")))
	   (match_operand:V_MOV 2 "register_operand"		   "v,a")
	   (match_operand 3 "immediate_operand"			   "n,n")
	   (match_operand 4 "immediate_operand"			   "n,n")]
	  UNSPEC_SCATTER))]
  "(AS_ANY_DS_P (INTVAL (operands[3]))
    && ((unsigned HOST_WIDE_INT)INTVAL(operands[1]) < 0x10000))"
  {
    addr_space_t as = INTVAL (operands[3]);
    static char buf[200];
    sprintf (buf, "ds_write%%b2\t%%0, %%2 offset:%%1%s\;s_waitcnt\tlgkmcnt(0)",
	     (AS_GDS_P (as) ? " gds" : ""));
    return buf;
  }
  [(set_attr "type" "ds")
   (set_attr "length" "12")
   (set_attr "gcn_version" "*,cdna2")])

(define_insn "scatter<mode>_insn_2offsets<exec_scatter>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(plus:<VnDI>
	     (plus:<VnDI>
	       (vec_duplicate:<VnDI>
		 (match_operand:DI 0 "register_operand"		       "Sv,Sv"))
	       (sign_extend:<VnDI>
		 (match_operand:<VnSI> 1 "register_operand"		"v,v")))
	     (vec_duplicate:<VnDI> (match_operand 2 "immediate_operand" "n,n")))
	   (match_operand:V_MOV 3 "register_operand"			"v,a")
	   (match_operand 4 "immediate_operand"				"n,n")
	   (match_operand 5 "immediate_operand"				"n,n")]
	  UNSPEC_SCATTER))]
  "(AS_GLOBAL_P (INTVAL (operands[4]))
    && (((unsigned HOST_WIDE_INT)INTVAL(operands[2]) + 0x1000) < 0x2000))"
  {
    addr_space_t as = INTVAL (operands[4]);
    const char *glc = INTVAL (operands[5]) ? " glc" : "";

    static char buf[200];
    if (AS_GLOBAL_P (as))
      sprintf (buf, "global_store%%s3\t%%1, %%3, %%0 offset:%%2%s", glc);
    else
      gcc_unreachable ();

    return buf;
  }
  [(set_attr "type" "flat")
   (set_attr "length" "12")
   (set_attr "gcn_version" "*,cdna2")])

;; }}}
;; {{{ Permutations

(define_insn "ds_bpermute<mode>"
  [(set (match_operand:V_1REG 0 "register_operand"    "=v")
	(unspec:V_1REG
	  [(match_operand:V_1REG 2 "register_operand" " v")
	   (match_operand:<VnSI> 1 "register_operand" " v")
	   (match_operand:DI 3 "gcn_exec_reg_operand" " e")]
	  UNSPEC_BPERMUTE))]
  ""
  "ds_bpermute_b32\t%0, %1, %2\;s_waitcnt\tlgkmcnt(0)"
  [(set_attr "type" "vop2")
   (set_attr "length" "12")])

(define_insn_and_split "ds_bpermute<mode>"
  [(set (match_operand:V_2REG 0 "register_operand"    "=&v")
	(unspec:V_2REG
	  [(match_operand:V_2REG 2 "register_operand" " v0")
	   (match_operand:<VnSI> 1 "register_operand" "  v")
	   (match_operand:DI 3 "gcn_exec_reg_operand" "  e")]
	  UNSPEC_BPERMUTE))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 4) (unspec:<VnSI>
			[(match_dup 6) (match_dup 1) (match_dup 3)]
			UNSPEC_BPERMUTE))
   (set (match_dup 5) (unspec:<VnSI>
			[(match_dup 7) (match_dup 1) (match_dup 3)]
			UNSPEC_BPERMUTE))]
  {
    operands[4] = gcn_operand_part (<MODE>mode, operands[0], 0);
    operands[5] = gcn_operand_part (<MODE>mode, operands[0], 1);
    operands[6] = gcn_operand_part (<MODE>mode, operands[2], 0);
    operands[7] = gcn_operand_part (<MODE>mode, operands[2], 1);
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "24")])

(define_insn "@dpp_move<mode>"
  [(set (match_operand:V_noHI 0 "register_operand"    "=v")
	(unspec:V_noHI
	  [(match_operand:V_noHI 1 "register_operand" " v")
	   (match_operand:SI 2 "const_int_operand"    " n")]
	  UNSPEC_MOV_DPP_SHR))]
  "!TARGET_RDNA2_PLUS"
  {
    return gcn_expand_dpp_shr_insn (<MODE>mode, "v_mov_b32",
				    UNSPEC_MOV_DPP_SHR, INTVAL (operands[2]));
  }
  [(set_attr "type" "vop_dpp")
   (set_attr "length" "16")])

(define_insn "@dpp_swap_pairs<mode>"
  [(set (match_operand:V_noHI 0 "register_operand"    "=v")
	(unspec:V_noHI
	  [(match_operand:V_noHI 1 "register_operand" " v")]
	  UNSPEC_MOV_DPP_SWAP_PAIRS))]
  ""
  {
    return gcn_expand_dpp_swap_pairs_insn (<MODE>mode, "v_mov_b32",
	                                   UNSPEC_MOV_DPP_SWAP_PAIRS);
  }
  [(set_attr "type" "vop_dpp")
   (set_attr "length" "16")])

(define_insn "@dpp_distribute_even<mode>"
  [(set (match_operand:V_noHI 0 "register_operand"    "=v")
	(unspec:V_noHI
	  [(match_operand:V_noHI 1 "register_operand" " v")]
	  UNSPEC_MOV_DPP_DISTRIBUTE_EVEN))]
  ""
  {
    return gcn_expand_dpp_distribute_even_insn (<MODE>mode, "v_mov_b32",
						UNSPEC_MOV_DPP_DISTRIBUTE_EVEN);
  }
  [(set_attr "type" "vop_dpp")
   (set_attr "length" "16")])

(define_insn "@dpp_distribute_odd<mode>"
  [(set (match_operand:V_noHI 0 "register_operand"    "=v")
	(unspec:V_noHI
	  [(match_operand:V_noHI 1 "register_operand" " v")]
	  UNSPEC_MOV_DPP_DISTRIBUTE_EVEN))]
  ""
  {
    return gcn_expand_dpp_distribute_odd_insn (<MODE>mode, "v_mov_b32",
					       UNSPEC_MOV_DPP_DISTRIBUTE_ODD);
  }
  [(set_attr "type" "vop_dpp")
   (set_attr "length" "16")])

;; }}}
;; {{{ ALU special case: add/sub

(define_insn "add<mode>3<exec_clobber>"
  [(set (match_operand:V_INT_1REG 0 "register_operand")
	(plus:V_INT_1REG
	  (match_operand:V_INT_1REG 1 "register_operand")
	  (match_operand:V_INT_1REG 2 "gcn_alu_operand")))
   (clobber (reg:DI VCC_REG))]
  ""
  {@ [cons: =0, %1, 2; attrs: type, length]
  [v,v,vSvA;vop2,4] v_add%^_u32\t%0, vcc, %2, %1
  [v,v,vSvB;vop2,8] ^
  })

(define_insn "add<mode>3_dup<exec_clobber>"
  [(set (match_operand:V_INT_1REG 0 "register_operand")
	(plus:V_INT_1REG
	  (vec_duplicate:V_INT_1REG
	    (match_operand:<SCALAR_MODE> 2 "gcn_alu_operand"))
	  (match_operand:V_INT_1REG 1 "register_operand")))
   (clobber (reg:DI VCC_REG))]
  ""
  {@ [cons: =0, 1, 2; attrs: type, length]
  [v,v,SvA;vop2,4] v_add%^_u32\t%0, vcc, %2, %1
  [v,v,SvB;vop2,8] ^
  })

(define_insn "add<mode>3_vcc<exec_vcc>"
  [(set (match_operand:V_SI 0 "register_operand")
	(plus:V_SI
	  (match_operand:V_SI 1 "register_operand")
	  (match_operand:V_SI 2 "gcn_alu_operand")))
   (set (match_operand:DI 3 "register_operand")
	(ltu:DI (plus:V_SI (match_dup 1) (match_dup 2))
		(match_dup 1)))]
  ""
  {@ [cons: =0, %1, 2, =3; attrs: type, length]
  [v,v,vSvA,cV;vop2 ,4] v_add%^_u32\t%0, %3, %2, %1
  [v,v,vSvB,cV;vop2 ,8] ^
  [v,v,vSvA,Sg;vop3b,8] ^
  })

; This pattern only changes the VCC bits when the corresponding lane is
; enabled, so the set must be described as an ior.

(define_insn "add<mode>3_vcc_dup<exec_vcc>"
  [(set (match_operand:V_SI 0 "register_operand")
	(plus:V_SI
	  (vec_duplicate:V_SI
	    (match_operand:SI 1 "gcn_alu_operand"))
	  (match_operand:V_SI 2 "register_operand")))
   (set (match_operand:DI 3 "register_operand")
	(ltu:DI (plus:V_SI (vec_duplicate:V_SI (match_dup 2))
			   (match_dup 1))
		(vec_duplicate:V_SI (match_dup 2))))]
  ""
  {@ [cons: =0, 1, 2, =3; attrs: type, length]
  [v,SvA,v,cV;vop2 ,4] v_add%^_u32\t%0, %3, %1, %2
  [v,SvB,v,cV;vop2 ,8] ^
  [v,SvA,v,Sg;vop3b,8] ^
  })

; v_addc does not accept an SGPR because the VCC read already counts as an
; SGPR use and the number of SGPR operands is limited to 1.  It does not
; accept "B" immediate constants due to a related bus conflict.

(define_insn "addc<mode>3<exec_vcc>"
  [(set (match_operand:V_SI 0 "register_operand"     "=v,   v")
	(plus:V_SI
	  (plus:V_SI
	    (vec_merge:V_SI
	      (vec_duplicate:V_SI (const_int 1))
	      (vec_duplicate:V_SI (const_int 0))
	      (match_operand:DI 3 "register_operand" " cV,cVSv"))
	    (match_operand:V_SI 1 "gcn_alu_operand"  "% v,  vA"))
	  (match_operand:V_SI 2 "gcn_alu_operand"    " vA,  vA")))
   (set (match_operand:DI 4 "register_operand"	     "=cV,cVSg")
	(ior:DI (ltu:DI (plus:V_SI
			  (plus:V_SI
			    (vec_merge:V_SI
			      (vec_duplicate:V_SI (const_int 1))
			      (vec_duplicate:V_SI (const_int 0))
			      (match_dup 3))
			    (match_dup 1))
			  (match_dup 2))
			(match_dup 2))
		(ltu:DI (plus:V_SI
			  (vec_merge:V_SI
			    (vec_duplicate:V_SI (const_int 1))
			    (vec_duplicate:V_SI (const_int 0))
			    (match_dup 3))
			  (match_dup 1))
			(match_dup 1))))]
  ""
  "{v_addc%^_u32|v_add_co_ci_u32}\t%0, %4, %2, %1, %3"
  [(set_attr "type" "vop2,vop3b")
   (set_attr "length" "4,8")])

(define_insn "sub<mode>3<exec_clobber>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"  "=  v,   v")
	(minus:V_INT_1REG
	  (match_operand:V_INT_1REG 1 "gcn_alu_operand" "vSvB,   v")
	  (match_operand:V_INT_1REG 2 "gcn_alu_operand" "   v,vSvB")))
   (clobber (reg:DI VCC_REG))]
  ""
  "@
   v_sub%^_u32\t%0, vcc, %1, %2
   v_subrev%^_u32\t%0, vcc, %2, %1"
  [(set_attr "type" "vop2")
   (set_attr "length" "8,8")])

(define_insn "sub<mode>3_vcc<exec_vcc>"
  [(set (match_operand:V_SI 0 "register_operand"  "=  v,   v,   v,   v")
	(minus:V_SI
	  (match_operand:V_SI 1 "gcn_alu_operand" "vSvB,vSvB,   v,   v")
	  (match_operand:V_SI 2 "gcn_alu_operand" "   v,   v,vSvB,vSvB")))
   (set (match_operand:DI 3 "register_operand"	  "= cV,  Sg,  cV,  Sg")
	(gtu:DI (minus:V_SI (match_dup 1) (match_dup 2))
		(match_dup 1)))]
  ""
  "@
   v_sub%^_u32\t%0, %3, %1, %2
   v_sub%^_u32\t%0, %3, %1, %2
   v_subrev%^_u32\t%0, %3, %2, %1
   v_subrev%^_u32\t%0, %3, %2, %1"
  [(set_attr "type" "vop2,vop3b,vop2,vop3b")
   (set_attr "length" "8")])

; v_subb does not accept an SGPR because the VCC read already counts as an
; SGPR use and the number of SGPR operands is limited to 1.  It does not
; accept "B" immediate constants due to a related bus conflict.

(define_insn "subc<mode>3<exec_vcc>"
  [(set (match_operand:V_SI 0 "register_operand"    "= v, v, v, v")
	(minus:V_SI
	  (minus:V_SI
	    (vec_merge:V_SI
	      (vec_duplicate:V_SI (const_int 1))
	      (vec_duplicate:V_SI (const_int 0))
	      (match_operand:DI 3 "gcn_alu_operand" " cV,cVSv,cV,cVSv"))
	    (match_operand:V_SI 1 "gcn_alu_operand" " vA,  vA, v,  vA"))
	  (match_operand:V_SI 2 "gcn_alu_operand"   "  v,  vA,vA,  vA")))
   (set (match_operand:DI 4 "register_operand"	    "=cV,cVSg,cV,cVSg")
	(ior:DI (gtu:DI (minus:V_SI (minus:V_SI
				      (vec_merge:V_SI
					(vec_duplicate:V_SI (const_int 1))
					(vec_duplicate:V_SI (const_int 0))
					(match_dup 3))
				       (match_dup 1))
				     (match_dup 2))
			(match_dup 2))
		(ltu:DI (minus:V_SI (vec_merge:V_SI
				      (vec_duplicate:V_SI (const_int 1))
				      (vec_duplicate:V_SI (const_int 0))
				      (match_dup 3))
				    (match_dup 1))
			(match_dup 1))))]
  ""
  "@
   {v_subb%^_u32|v_sub_co_ci_u32}\t%0, %4, %1, %2, %3
   {v_subb%^_u32|v_sub_co_ci_u32}\t%0, %4, %1, %2, %3
   {v_subbrev%^_u32|v_subrev_co_ci_u32}\t%0, %4, %2, %1, %3
   {v_subbrev%^_u32|v_subrev_co_ci_u32}\t%0, %4, %2, %1, %3"
  [(set_attr "type" "vop2,vop3b,vop2,vop3b")
   (set_attr "length" "4,8,4,8")])

(define_insn_and_split "add<mode>3"
  [(set (match_operand:V_DI 0 "register_operand"   "=  v")
	(plus:V_DI
	  (match_operand:V_DI 1 "register_operand" "%vDb")
	  (match_operand:V_DI 2 "gcn_alu_operand"  " vDb")))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[1])
   && gcn_can_split_p (<MODE>mode, operands[2])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<vnsi>3_vcc
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (<MODE>mode, operands[1], 0),
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 vcc));
    emit_insn (gen_addc<vnsi>3
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[1], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 vcc, vcc));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_insn_and_split "add<mode>3_exec"
  [(set (match_operand:V_DI 0 "register_operand"		 "=  v")
	(vec_merge:V_DI
	  (plus:V_DI
	    (match_operand:V_DI 1 "register_operand"		 "%vDb")
	    (match_operand:V_DI 2 "gcn_alu_operand"		 " vDb"))
	  (match_operand:V_DI 3 "gcn_register_or_unspec_operand" "  U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		 "   e")))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[1])
   && gcn_can_split_p (<MODE>mode, operands[2])
   && gcn_can_split_p (<MODE>mode, operands[4])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<vnsi>3_vcc_exec
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (<MODE>mode, operands[1], 0),
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 0),
		 operands[4]));
    emit_insn (gen_addc<vnsi>3_exec
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[1], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 vcc, vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 1),
		 operands[4]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_insn_and_split "sub<mode>3"
  [(set (match_operand:V_DI 0 "register_operand"  "= v,  v")
	(minus:V_DI                                        
	  (match_operand:V_DI 1 "gcn_alu_operand" "vDb,  v")
	  (match_operand:V_DI 2 "gcn_alu_operand" "  v,vDb")))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[1])
   && gcn_can_split_p (<MODE>mode, operands[2])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_sub<vnsi>3_vcc
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (<MODE>mode, operands[1], 0),
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 vcc));
    emit_insn (gen_subc<vnsi>3
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[1], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 vcc, vcc));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_insn_and_split "sub<mode>3_exec"
  [(set (match_operand:V_DI 0 "register_operand"		 "=  v,   v")
	(vec_merge:V_DI                                                         
	  (minus:V_DI                                                           
	    (match_operand:V_DI 1 "gcn_alu_operand"		 "vSvB,   v")
	    (match_operand:V_DI 2 "gcn_alu_operand"		 "   v,vSvB"))
	  (match_operand:V_DI 3 "gcn_register_or_unspec_operand" " U0,  U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		 "   e,   e")))
   (clobber (reg:DI VCC_REG))]
  "register_operand (operands[1], VOIDmode)
   || register_operand (operands[2], VOIDmode)"
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[1])
   && gcn_can_split_p (<MODE>mode, operands[2])
   && gcn_can_split_p (<MODE>mode, operands[3])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_sub<vnsi>3_vcc_exec
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (<MODE>mode, operands[1], 0),
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 0),
		 operands[4]));
    emit_insn (gen_subc<vnsi>3_exec
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[1], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 vcc, vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 1),
		 operands[4]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_insn_and_split "add<mode>3_zext"
  [(set (match_operand:V_DI 0 "register_operand"      "= v,  v")
	(plus:V_DI
	  (zero_extend:V_DI
	    (match_operand:<VnSI> 1 "gcn_alu_operand" " vA, vB"))
	  (match_operand:V_DI 2 "gcn_alu_operand"     "vDb,vDA")))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[2])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<vnsi>3_vcc
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 operands[1],
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 vcc));
    emit_insn (gen_addc<vnsi>3
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 const0_rtx, vcc, vcc));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_insn_and_split "add<mode>3_zext_exec"
  [(set (match_operand:V_DI 0 "register_operand"		 "= v,  v")
	(vec_merge:V_DI
	  (plus:V_DI
	    (zero_extend:V_DI
	      (match_operand:<VnSI> 1 "gcn_alu_operand"		 " vA, vB"))
	    (match_operand:V_DI 2 "gcn_alu_operand"		 "vDb,vDA"))
	  (match_operand:V_DI 3 "gcn_register_or_unspec_operand" " U0, U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		 "  e,  e")))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[2])
   && gcn_can_split_p (<MODE>mode, operands[3])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<vnsi>3_vcc_exec
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 operands[1],
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 0),
		 operands[4]));
    emit_insn (gen_addc<vnsi>3_exec
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 const0_rtx, vcc, vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 1),
		 operands[4]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_insn_and_split "add<mode>3_vcc_zext_dup"
  [(set (match_operand:V_DI 0 "register_operand")
	(plus:V_DI
	  (zero_extend:V_DI
	    (vec_duplicate:<VnSI>
	      (match_operand:SI 1 "gcn_alu_operand")))
	  (match_operand:V_DI 2 "gcn_alu_operand")))
   (set (match_operand:DI 3 "register_operand")
	(ltu:DI (plus:V_DI 
		  (zero_extend:V_DI (vec_duplicate:<VnSI> (match_dup 1)))
		  (match_dup 2))
		(match_dup 1)))]
  ""
  {@ [cons: =0, 1, 2, =3]
  [v,ASv,v,&Sg] #
  [v,BSv,v,&cV] ^
  }
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[2])"
  [(const_int 0)]
  {
    emit_insn (gen_add<vnsi>3_vcc_dup
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (DImode, operands[1], 0),
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 operands[3]));
    emit_insn (gen_addc<vnsi>3
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 const0_rtx, operands[3], operands[3]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_expand "add<mode>3_zext_dup"
  [(match_operand:V_DI 0 "register_operand")
   (match_operand:SI 1 "gcn_alu_operand")
   (match_operand:V_DI 2 "gcn_alu_operand")]
  ""
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<mode>3_vcc_zext_dup (operands[0], operands[1],
					    operands[2], vcc));
    DONE;
  })

(define_insn_and_split "add<mode>3_vcc_zext_dup_exec"
  [(set (match_operand:V_DI 0 "register_operand")
	(vec_merge:V_DI
	  (plus:V_DI
	    (zero_extend:V_DI
	      (vec_duplicate:<VnSI>
		(match_operand:SI 1 "gcn_alu_operand")))
	    (match_operand:V_DI 2 "gcn_alu_operand"))
	  (match_operand:V_DI 4 "gcn_register_or_unspec_operand")
	  (match_operand:DI 5 "gcn_exec_reg_operand")))
   (set (match_operand:DI 3 "register_operand")
	(and:DI
	  (ltu:DI (plus:V_DI 
		    (zero_extend:V_DI (vec_duplicate:<VnSI> (match_dup 1)))
		    (match_dup 2))
		  (match_dup 1))
	  (match_dup 5)))]
  ""
  {@ [cons: =0, 1, 2, =3, 4, 5]
  [v,ASv,v,&Sg,U0,e] #
  [v,BSv,v,&cV,U0,e] ^
  }
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[2])
   && gcn_can_split_p (<MODE>mode, operands[4])"
  [(const_int 0)]
  {
    emit_insn (gen_add<vnsi>3_vcc_dup_exec
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (DImode, operands[1], 0),
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 operands[3],
		 gcn_operand_part (<MODE>mode, operands[4], 0),
		 operands[5]));
    emit_insn (gen_addc<vnsi>3_exec
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 const0_rtx, operands[3], operands[3],
		 gcn_operand_part (<MODE>mode, operands[4], 1),
		 operands[5]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_expand "add<mode>3_zext_dup_exec"
  [(match_operand:V_DI 0 "register_operand")
   (match_operand:SI 1 "gcn_alu_operand")
   (match_operand:V_DI 2 "gcn_alu_operand")
   (match_operand:V_DI 3 "gcn_register_or_unspec_operand")
   (match_operand:DI 4 "gcn_exec_reg_operand")]
  ""
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<mode>3_vcc_zext_dup_exec (operands[0], operands[1],
						 operands[2], vcc, operands[3],
						 operands[4]));
    DONE;
  })

(define_insn_and_split "add<mode>3_vcc_zext_dup2"
  [(set (match_operand:V_DI 0 "register_operand")
	(plus:V_DI
	  (zero_extend:V_DI (match_operand:<VnSI> 1 "gcn_alu_operand"))
	  (vec_duplicate:V_DI (match_operand:DI 2 "gcn_alu_operand"))))
   (set (match_operand:DI 3 "register_operand")
	(ltu:DI (plus:V_DI 
		  (zero_extend:V_DI (match_dup 1))
		  (vec_duplicate:V_DI (match_dup 2)))
		(match_dup 1)))]
  ""
  {@ [cons: =0, 1, 2, =3]
  [v,v,DbSv,&cV] #
  [v,v,DASv,&Sg] ^
  }
  "gcn_can_split_p (<MODE>mode, operands[0])"
  [(const_int 0)]
  {
    emit_insn (gen_add<vnsi>3_vcc_dup
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (DImode, operands[2], 0),
		 operands[1],
		 operands[3]));
    rtx dsthi = gcn_operand_part (<MODE>mode, operands[0], 1);
    emit_insn (gen_vec_duplicate<vnsi>
		(dsthi, gcn_operand_part (DImode, operands[2], 1)));
    emit_insn (gen_addc<vnsi>3 (dsthi, dsthi, const0_rtx, operands[3],
				operands[3]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_expand "add<mode>3_zext_dup2"
  [(match_operand:V_DI 0 "register_operand")
   (match_operand:<VnSI> 1 "gcn_alu_operand")
   (match_operand:DI 2 "gcn_alu_operand")]
  ""
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<mode>3_vcc_zext_dup2 (operands[0], operands[1],
					     operands[2], vcc));
    DONE;
  })

(define_insn_and_split "add<mode>3_vcc_zext_dup2_exec"
  [(set (match_operand:V_DI 0 "register_operand")
	(vec_merge:V_DI
	  (plus:V_DI
	    (zero_extend:V_DI (match_operand:<VnSI> 1 "gcn_alu_operand"))
	    (vec_duplicate:V_DI (match_operand:DI 2 "gcn_alu_operand")))
	  (match_operand:V_DI 4 "gcn_register_or_unspec_operand")
	  (match_operand:DI 5 "gcn_exec_reg_operand")))
   (set (match_operand:DI 3 "register_operand")
	(and:DI
	  (ltu:DI (plus:V_DI 
		    (zero_extend:V_DI (match_dup 1))
		    (vec_duplicate:V_DI (match_dup 2)))
		  (match_dup 1))
	  (match_dup 5)))]
  ""
  {@ [cons: =0, 1, 2, =3, 4, 5]
  [v,v,ASv,&Sg,U0,e] #
  [v,v,BSv,&cV,U0,e] ^
  }
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[4])"
  [(const_int 0)]
  {
    emit_insn (gen_add<vnsi>3_vcc_dup_exec
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (DImode, operands[2], 0),
		 operands[1],
		 operands[3],
		 gcn_operand_part (<MODE>mode, operands[4], 0),
		 operands[5]));
    rtx dsthi = gcn_operand_part (<MODE>mode, operands[0], 1);
    emit_insn (gen_vec_duplicate<vnsi>_exec
		(dsthi, gcn_operand_part (DImode, operands[2], 1),
		 gcn_operand_part (<MODE>mode, operands[4], 1),
		 operands[5]));
    emit_insn (gen_addc<vnsi>3_exec
		(dsthi, dsthi, const0_rtx, operands[3], operands[3],
		 gcn_operand_part (<MODE>mode, operands[4], 1),
		 operands[5]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_expand "add<mode>3_zext_dup2_exec"
  [(match_operand:V_DI 0 "register_operand")
   (match_operand:<VnSI> 1 "gcn_alu_operand")
   (match_operand:DI 2 "gcn_alu_operand")
   (match_operand:V_DI 3 "gcn_register_or_unspec_operand")
   (match_operand:DI 4 "gcn_exec_reg_operand")]
  ""
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<mode>3_vcc_zext_dup2_exec (operands[0], operands[1],
						  operands[2], vcc,
						  operands[3], operands[4]));
    DONE;
  })

(define_insn_and_split "add<mode>3_sext_dup2"
  [(set (match_operand:V_DI 0 "register_operand"		      "= v")
	(plus:V_DI
	  (sign_extend:V_DI (match_operand:<VnSI> 1 "gcn_alu_operand" " vA"))
	  (vec_duplicate:V_DI (match_operand:DI 2 "gcn_alu_operand"   "BSv"))))
   (clobber (match_scratch:<VnSI> 3				      "=&v"))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_ashr<vnsi>3 (operands[3], operands[1], GEN_INT (31)));
    emit_insn (gen_add<vnsi>3_vcc_dup
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (DImode, operands[2], 0),
		 operands[1],
		 vcc));
    rtx dsthi = gcn_operand_part (<MODE>mode, operands[0], 1);
    emit_insn (gen_vec_duplicate<vnsi>
		(dsthi, gcn_operand_part (DImode, operands[2], 1)));
    emit_insn (gen_addc<vnsi>3 (dsthi, dsthi, operands[3], vcc, vcc));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_insn_and_split "add<mode>3_sext_dup2_exec"
  [(set (match_operand:V_DI 0 "register_operand"		       "= v")
	(vec_merge:V_DI
	  (plus:V_DI
	    (sign_extend:V_DI (match_operand:<VnSI> 1 "gcn_alu_operand" "vA"))
	    (vec_duplicate:V_DI (match_operand:DI 2 "gcn_alu_operand"  "BSv")))
	  (match_operand:V_DI 3 "gcn_register_or_unspec_operand"       " U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		       "  e")))
   (clobber (match_scratch:<VnSI> 5				       "=&v"))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[3])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_ashr<vnsi>3_exec (operands[5], operands[1], GEN_INT (31),
				     gcn_gen_undef (<VnSI>mode), operands[4]));
    emit_insn (gen_add<vnsi>3_vcc_dup_exec
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (DImode, operands[2], 0),
		 operands[1],
		 vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 0),
		 operands[4]));
    rtx dsthi = gcn_operand_part (<MODE>mode, operands[0], 1);
    emit_insn (gen_vec_duplicate<vnsi>_exec
		(dsthi, gcn_operand_part (DImode, operands[2], 1),
		gcn_operand_part (<MODE>mode, operands[3], 1),
		operands[4]));
    emit_insn (gen_addc<vnsi>3_exec
		(dsthi, dsthi, operands[5], vcc, vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 1),
		 operands[4]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

;; }}}
;; {{{ DS memory ALU: add/sub

(define_mode_iterator DS_ARITH_MODE [V64SI V64SF V64DI])
(define_mode_iterator DS_ARITH_SCALAR_MODE [SI SF DI])

;; FIXME: the vector patterns probably need RD expanded to a vector of
;;        addresses.  For now, the only way a vector can get into LDS is
;;        if the user puts it there manually.
;;
;; FIXME: the scalar patterns are probably fine in themselves, but need to be
;;        checked to see if anything can ever use them.

(define_insn "add<mode>3_ds<exec>"
  [(set (match_operand:DS_ARITH_MODE 0 "gcn_ds_memory_operand"	 "=RD")
	(plus:DS_ARITH_MODE
	  (match_operand:DS_ARITH_MODE 1 "gcn_ds_memory_operand" "%RD")
	  (match_operand:DS_ARITH_MODE 2 "register_operand"	 "  v")))]
  "rtx_equal_p (operands[0], operands[1])"
  "ds_add%u0\t%A0, %2%O0"
  [(set_attr "type" "ds")
   (set_attr "length" "8")])

(define_insn "add<mode>3_ds_scalar"
  [(set (match_operand:DS_ARITH_SCALAR_MODE 0 "gcn_ds_memory_operand" "=RD")
	(plus:DS_ARITH_SCALAR_MODE
	  (match_operand:DS_ARITH_SCALAR_MODE 1 "gcn_ds_memory_operand"
								      "%RD")
	  (match_operand:DS_ARITH_SCALAR_MODE 2 "register_operand"    "  v")))]
  "rtx_equal_p (operands[0], operands[1])"
  "ds_add%u0\t%A0, %2%O0"
  [(set_attr "type" "ds")
   (set_attr "length" "8")])

(define_insn "sub<mode>3_ds<exec>"
  [(set (match_operand:DS_ARITH_MODE 0 "gcn_ds_memory_operand"	 "=RD")
	(minus:DS_ARITH_MODE
	  (match_operand:DS_ARITH_MODE 1 "gcn_ds_memory_operand" " RD")
	  (match_operand:DS_ARITH_MODE 2 "register_operand"	 "  v")))]
  "rtx_equal_p (operands[0], operands[1])"
  "ds_sub%u0\t%A0, %2%O0"
  [(set_attr "type" "ds")
   (set_attr "length" "8")])

(define_insn "sub<mode>3_ds_scalar"
  [(set (match_operand:DS_ARITH_SCALAR_MODE 0 "gcn_ds_memory_operand" "=RD")
	(minus:DS_ARITH_SCALAR_MODE
	  (match_operand:DS_ARITH_SCALAR_MODE 1 "gcn_ds_memory_operand"
								      " RD")
	  (match_operand:DS_ARITH_SCALAR_MODE 2 "register_operand"    "  v")))]
  "rtx_equal_p (operands[0], operands[1])"
  "ds_sub%u0\t%A0, %2%O0"
  [(set_attr "type" "ds")
   (set_attr "length" "8")])

(define_insn "subr<mode>3_ds<exec>"
  [(set (match_operand:DS_ARITH_MODE 0 "gcn_ds_memory_operand"	 "=RD")
	(minus:DS_ARITH_MODE
	  (match_operand:DS_ARITH_MODE 2 "register_operand"	 "  v")
	  (match_operand:DS_ARITH_MODE 1 "gcn_ds_memory_operand" " RD")))]
  "rtx_equal_p (operands[0], operands[1])"
  "ds_rsub%u0\t%A0, %2%O0"
  [(set_attr "type" "ds")
   (set_attr "length" "8")])

(define_insn "subr<mode>3_ds_scalar"
  [(set (match_operand:DS_ARITH_SCALAR_MODE 0 "gcn_ds_memory_operand" "=RD")
	(minus:DS_ARITH_SCALAR_MODE
	  (match_operand:DS_ARITH_SCALAR_MODE 2 "register_operand"    "  v")
	  (match_operand:DS_ARITH_SCALAR_MODE 1 "gcn_ds_memory_operand" 
								      " RD")))]
  "rtx_equal_p (operands[0], operands[1])"
  "ds_rsub%u0\t%A0, %2%O0"
  [(set_attr "type" "ds")
   (set_attr "length" "8")])

;; }}}
;; {{{ ALU special case: mult

(define_insn "<su>mul<mode>3_highpart<exec>"
  [(set (match_operand:V_SI 0 "register_operand"        "=  v")
	(truncate:V_SI
	  (lshiftrt:<VnDI>
	    (mult:<VnDI>
	      (any_extend:<VnDI>
		(match_operand:V_SI 1 "gcn_alu_operand" "  %v"))
	      (any_extend:<VnDI>
		(match_operand:V_SI 2 "gcn_alu_operand" "vSvA")))
	    (const_int 32))))]
  ""
  "v_mul_hi<sgnsuffix>0\t%0, %2, %1"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "mul<mode>3<exec>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"  "=   v")
	(mult:V_INT_1REG
	  (match_operand:V_INT_1REG 1 "gcn_alu_operand" "%vSvA")
	  (match_operand:V_INT_1REG 2 "gcn_alu_operand" " vSvA")))]
  ""
  "v_mul_lo_u32\t%0, %1, %2"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "mul<mode>3_dup<exec>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"	     "=   v")
	(mult:V_INT_1REG
	  (match_operand:V_INT_1REG 1 "gcn_alu_operand"	     "%vSvA")
	  (vec_duplicate:V_INT_1REG
	    (match_operand:<SCALAR_MODE> 2 "gcn_alu_operand" "  SvA"))))]
  ""
  "v_mul_lo_u32\t%0, %1, %2"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn_and_split "mul<mode>3"
  [(set (match_operand:V_DI 0 "register_operand"  "=&v")
	(mult:V_DI
	  (match_operand:V_DI 1 "gcn_alu_operand" "% v")
	  (match_operand:V_DI 2 "gcn_alu_operand" "vDA")))
   (clobber (match_scratch:<VnSI> 3		  "=&v"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out_lo = gcn_operand_part (<MODE>mode, operands[0], 0);
    rtx out_hi = gcn_operand_part (<MODE>mode, operands[0], 1);
    rtx left_lo = gcn_operand_part (<MODE>mode, operands[1], 0);
    rtx left_hi = gcn_operand_part (<MODE>mode, operands[1], 1);
    rtx right_lo = gcn_operand_part (<MODE>mode, operands[2], 0);
    rtx right_hi = gcn_operand_part (<MODE>mode, operands[2], 1);
    rtx tmp = operands[3];

    emit_insn (gen_mul<vnsi>3 (out_lo, left_lo, right_lo));
    emit_insn (gen_umul<vnsi>3_highpart (out_hi, left_lo, right_lo));
    emit_insn (gen_mul<vnsi>3 (tmp, left_hi, right_lo));
    emit_insn (gen_add<vnsi>3 (out_hi, out_hi, tmp));
    emit_insn (gen_mul<vnsi>3 (tmp, left_lo, right_hi));
    emit_insn (gen_add<vnsi>3 (out_hi, out_hi, tmp));
    emit_insn (gen_mul<vnsi>3 (tmp, left_hi, right_hi));
    emit_insn (gen_add<vnsi>3 (out_hi, out_hi, tmp));
    DONE;
  })

(define_insn_and_split "mul<mode>3_exec"
  [(set (match_operand:V_DI 0 "register_operand"		 "=&v")
	(vec_merge:V_DI
	  (mult:V_DI
	    (match_operand:V_DI 1 "gcn_alu_operand"		 "% v")
	    (match_operand:V_DI 2 "gcn_alu_operand"		 "vDA"))
	  (match_operand:V_DI 3 "gcn_register_or_unspec_operand" " U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		 "  e")))
   (clobber (match_scratch:<VnSI> 5				 "=&v"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out_lo = gcn_operand_part (<MODE>mode, operands[0], 0);
    rtx out_hi = gcn_operand_part (<MODE>mode, operands[0], 1);
    rtx left_lo = gcn_operand_part (<MODE>mode, operands[1], 0);
    rtx left_hi = gcn_operand_part (<MODE>mode, operands[1], 1);
    rtx right_lo = gcn_operand_part (<MODE>mode, operands[2], 0);
    rtx right_hi = gcn_operand_part (<MODE>mode, operands[2], 1);
    rtx exec = operands[4];
    rtx tmp = operands[5];

    rtx old_lo, old_hi;
    if (GET_CODE (operands[3]) == UNSPEC)
      {
	old_lo = old_hi = gcn_gen_undef (<VnSI>mode);
      }
    else
      {
	old_lo = gcn_operand_part (<MODE>mode, operands[3], 0);
	old_hi = gcn_operand_part (<MODE>mode, operands[3], 1);
      }

    rtx undef = gcn_gen_undef (<VnSI>mode);

    emit_insn (gen_mul<vnsi>3_exec (out_lo, left_lo, right_lo, old_lo, exec));
    emit_insn (gen_umul<vnsi>3_highpart_exec (out_hi, left_lo, right_lo,
					      old_hi, exec));
    emit_insn (gen_mul<vnsi>3_exec (tmp, left_hi, right_lo, undef, exec));
    emit_insn (gen_add<vnsi>3_exec (out_hi, out_hi, tmp, out_hi, exec));
    emit_insn (gen_mul<vnsi>3_exec (tmp, left_lo, right_hi, undef, exec));
    emit_insn (gen_add<vnsi>3_exec (out_hi, out_hi, tmp, out_hi, exec));
    emit_insn (gen_mul<vnsi>3_exec (tmp, left_hi, right_hi, undef, exec));
    emit_insn (gen_add<vnsi>3_exec (out_hi, out_hi, tmp, out_hi, exec));
    DONE;
  })

(define_insn_and_split "mul<mode>3_zext"
  [(set (match_operand:V_DI 0 "register_operand"      "=&v")
	(mult:V_DI
	  (zero_extend:V_DI
	    (match_operand:<VnSI> 1 "gcn_alu_operand" "  v"))
	  (match_operand:V_DI 2 "gcn_alu_operand"     "vDA")))
   (clobber (match_scratch:<VnSI> 3		      "=&v"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out_lo = gcn_operand_part (<MODE>mode, operands[0], 0);
    rtx out_hi = gcn_operand_part (<MODE>mode, operands[0], 1);
    rtx left = operands[1];
    rtx right_lo = gcn_operand_part (<MODE>mode, operands[2], 0);
    rtx right_hi = gcn_operand_part (<MODE>mode, operands[2], 1);
    rtx tmp = operands[3];

    emit_insn (gen_mul<vnsi>3 (out_lo, left, right_lo));
    emit_insn (gen_umul<vnsi>3_highpart (out_hi, left, right_lo));
    emit_insn (gen_mul<vnsi>3 (tmp, left, right_hi));
    emit_insn (gen_add<vnsi>3 (out_hi, out_hi, tmp));
    DONE;
  })

(define_insn_and_split "mul<mode>3_zext_exec"
  [(set (match_operand:V_DI 0 "register_operand"		 "=&v")
	(vec_merge:V_DI
	  (mult:V_DI
	    (zero_extend:V_DI
	      (match_operand:<VnSI> 1 "gcn_alu_operand"		 "  v"))
	    (match_operand:V_DI 2 "gcn_alu_operand"		 "vDA"))
	  (match_operand:V_DI 3 "gcn_register_or_unspec_operand" " U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		 "  e")))
   (clobber (match_scratch:<VnSI> 5				 "=&v"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out_lo = gcn_operand_part (<MODE>mode, operands[0], 0);
    rtx out_hi = gcn_operand_part (<MODE>mode, operands[0], 1);
    rtx left = operands[1];
    rtx right_lo = gcn_operand_part (<MODE>mode, operands[2], 0);
    rtx right_hi = gcn_operand_part (<MODE>mode, operands[2], 1);
    rtx exec = operands[4];
    rtx tmp = operands[5];

    rtx old_lo, old_hi;
    if (GET_CODE (operands[3]) == UNSPEC)
      {
	old_lo = old_hi = gcn_gen_undef (<VnSI>mode);
      }
    else
      {
	old_lo = gcn_operand_part (<MODE>mode, operands[3], 0);
	old_hi = gcn_operand_part (<MODE>mode, operands[3], 1);
      }

    rtx undef = gcn_gen_undef (<VnSI>mode);

    emit_insn (gen_mul<vnsi>3_exec (out_lo, left, right_lo, old_lo, exec));
    emit_insn (gen_umul<vnsi>3_highpart_exec (out_hi, left, right_lo,
					      old_hi, exec));
    emit_insn (gen_mul<vnsi>3_exec (tmp, left, right_hi, undef, exec));
    emit_insn (gen_add<vnsi>3_exec (out_hi, out_hi, tmp, out_hi, exec));
    DONE;
  })

(define_insn_and_split "mul<mode>3_zext_dup2"
  [(set (match_operand:V_DI 0 "register_operand"      "= &v")
	(mult:V_DI
	  (zero_extend:V_DI
	    (match_operand:<VnSI> 1 "gcn_alu_operand" "   v"))
	  (vec_duplicate:V_DI
	    (match_operand:DI 2 "gcn_alu_operand"     "SvDA"))))
   (clobber (match_scratch:<VnSI> 3		      "= &v"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out_lo = gcn_operand_part (<MODE>mode, operands[0], 0);
    rtx out_hi = gcn_operand_part (<MODE>mode, operands[0], 1);
    rtx left = operands[1];
    rtx right_lo = gcn_operand_part (<MODE>mode, operands[2], 0);
    rtx right_hi = gcn_operand_part (<MODE>mode, operands[2], 1);
    rtx tmp = operands[3];

    emit_insn (gen_mul<vnsi>3 (out_lo, left, right_lo));
    emit_insn (gen_umul<vnsi>3_highpart (out_hi, left, right_lo));
    emit_insn (gen_mul<vnsi>3 (tmp, left, right_hi));
    emit_insn (gen_add<vnsi>3 (out_hi, out_hi, tmp));
    DONE;
  })

(define_insn_and_split "mul<mode>3_zext_dup2_exec"
  [(set (match_operand:V_DI 0 "register_operand"		 "= &v")
	(vec_merge:V_DI
	  (mult:V_DI
	    (zero_extend:V_DI
	      (match_operand:<VnSI> 1 "gcn_alu_operand"		 "   v"))
	    (vec_duplicate:V_DI
	      (match_operand:DI 2 "gcn_alu_operand"		 "SvDA")))
	  (match_operand:V_DI 3 "gcn_register_or_unspec_operand" "  U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		 "   e")))
   (clobber (match_scratch:<VnSI> 5				 "= &v"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out_lo = gcn_operand_part (<MODE>mode, operands[0], 0);
    rtx out_hi = gcn_operand_part (<MODE>mode, operands[0], 1);
    rtx left = operands[1];
    rtx right_lo = gcn_operand_part (<MODE>mode, operands[2], 0);
    rtx right_hi = gcn_operand_part (<MODE>mode, operands[2], 1);
    rtx exec = operands[4];
    rtx tmp = operands[5];

    rtx old_lo, old_hi;
    if (GET_CODE (operands[3]) == UNSPEC)
      {
	old_lo = old_hi = gcn_gen_undef (<VnSI>mode);
      }
    else
      {
	old_lo = gcn_operand_part (<MODE>mode, operands[3], 0);
	old_hi = gcn_operand_part (<MODE>mode, operands[3], 1);
      }

    rtx undef = gcn_gen_undef (<VnSI>mode);

    emit_insn (gen_mul<vnsi>3_exec (out_lo, left, right_lo, old_lo, exec));
    emit_insn (gen_umul<vnsi>3_highpart_exec (out_hi, left, right_lo,
					      old_hi, exec));
    emit_insn (gen_mul<vnsi>3_exec (tmp, left, right_hi, undef, exec));
    emit_insn (gen_add<vnsi>3_exec (out_hi, out_hi, tmp, out_hi, exec));
    DONE;
  })

(define_int_iterator UNSPEC_CMUL_OP [UNSPEC_CMUL UNSPEC_CMUL_CONJ])
(define_int_attr conj_op [(UNSPEC_CMUL "") (UNSPEC_CMUL_CONJ "_conj")])
(define_int_attr cmul_subadd [(UNSPEC_CMUL "sub") (UNSPEC_CMUL_CONJ "add")])
(define_int_attr cmul_addsub [(UNSPEC_CMUL "add") (UNSPEC_CMUL_CONJ "sub")])

(define_expand "cmul<conj_op><mode>3"
  [(set (match_operand:V_noHI 0 "register_operand"    "=&v")
        (unspec:V_noHI
	  [(match_operand:V_noHI 1 "register_operand" "v")
	   (match_operand:V_noHI 2 "register_operand" "v")]
	  UNSPEC_CMUL_OP))]
  ""
  {
    // operands[1]                                                  a   b
    // operands[2]                                                  c   d
    rtx t1 = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_mul<mode>3 (t1, operands[1], operands[2]));   // a*c b*d

    rtx s2_perm = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_dpp_swap_pairs<mode> (s2_perm, operands[2])); // d   c

    rtx t2 = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_mul<mode>3 (t2, operands[1], s2_perm));       // a*d b*c

    rtx t1_perm = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_dpp_swap_pairs<mode> (t1_perm, t1));          // b*d a*c

    rtx even = gen_rtx_REG (DImode, EXEC_REG);
    emit_move_insn (even, get_exec (0x5555555555555555UL));
    rtx dest = operands[0];
    emit_insn (gen_<cmul_subadd><mode>3_exec (dest, t1, t1_perm,
                                              gcn_gen_undef (<MODE>mode),
                                              even));            // a*c-b*d 0

    rtx t2_perm = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_dpp_swap_pairs<mode> (t2_perm, t2));          // b*c a*d

    rtx odd = gen_rtx_REG (DImode, EXEC_REG);
    emit_move_insn (odd, get_exec (0xaaaaaaaaaaaaaaaaUL));
    emit_insn (gen_<cmul_addsub><mode>3_exec (dest, t2, t2_perm, dest, odd));
                                                                   // 0 a*d+b*c
    DONE;
  })

(define_code_iterator addsub [plus minus])
(define_code_attr addsub_as [(plus "a") (minus "s")])

(define_expand "cml<addsub_as><mode>4"
  [(set (match_operand:V_FP 0 "register_operand"      "=&v")
	(addsub:V_FP
	  (unspec:V_FP
	    [(match_operand:V_FP 1 "register_operand" "v")
	     (match_operand:V_FP 2 "register_operand" "v")]
	    UNSPEC_CMUL)
	  (match_operand:V_FP 3 "register_operand"    "v")))]
  ""
  {
    rtx a = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_dpp_distribute_even<mode> (a, operands[1]));    // a   a

    rtx t1 = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_fm<addsub_as><mode>4 (t1, a, operands[2], operands[3]));
                                                                   // a*c a*d

    rtx b = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_dpp_distribute_odd<mode> (b, operands[1]));     // b   b

    rtx t2 = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_mul<mode>3 (t2, b, operands[2]));               // b*c b*d

    rtx t2_perm = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_dpp_swap_pairs<mode> (t2_perm, t2));            // b*d b*c

    rtx even = gen_rtx_REG (DImode, EXEC_REG);
    emit_move_insn (even, get_exec (0x5555555555555555UL));
    rtx dest = operands[0];
    emit_insn (gen_sub<mode>3_exec (dest, t1, t2_perm,
                                    gcn_gen_undef (<MODE>mode), even));

    rtx odd = gen_rtx_REG (DImode, EXEC_REG);
    emit_move_insn (odd, get_exec (0xaaaaaaaaaaaaaaaaUL));
    emit_insn (gen_add<mode>3_exec (dest, t1, t2_perm, dest, odd));

    DONE;
  })

(define_expand "vec_addsub<mode>3"
  [(set (match_operand:V_noHI 0 "register_operand"     "=&v")
        (vec_merge:V_noHI
          (minus:V_noHI
            (match_operand:V_noHI 1 "register_operand" "v")
            (match_operand:V_noHI 2 "register_operand" "v"))
          (plus:V_noHI (match_dup 1) (match_dup 2))
          (const_int 6148914691236517205)))]
  ""
  {
    rtx even = gen_rtx_REG (DImode, EXEC_REG);
    emit_move_insn (even, get_exec (0x5555555555555555UL));
    rtx dest = operands[0];
    rtx x = operands[1];
    rtx y = operands[2];
    emit_insn (gen_sub<mode>3_exec (dest, x, y, gcn_gen_undef (<MODE>mode),
                                    even));
    rtx odd = gen_rtx_REG (DImode, EXEC_REG);
    emit_move_insn (odd, get_exec (0xaaaaaaaaaaaaaaaaUL));
    emit_insn (gen_add<mode>3_exec (dest, x, y, dest, odd));

    DONE;
  })

(define_int_iterator CADD [UNSPEC_CADD90 UNSPEC_CADD270])
(define_int_attr rot [(UNSPEC_CADD90 "90") (UNSPEC_CADD270 "270")])
(define_int_attr cadd_subadd [(UNSPEC_CADD90 "sub") (UNSPEC_CADD270 "add")])
(define_int_attr cadd_addsub [(UNSPEC_CADD90 "add") (UNSPEC_CADD270 "sub")])

(define_expand "cadd<rot><mode>3"
  [(set (match_operand:V_noHI 0 "register_operand"                 "=&v")
        (unspec:V_noHI [(match_operand:V_noHI 1 "register_operand" "v")
                        (match_operand:V_noHI 2 "register_operand" "v")]
                        CADD))]
  ""
  {
    rtx dest = operands[0];
    rtx x = operands[1];
    rtx y = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_dpp_swap_pairs<mode> (y, operands[2]));

    rtx even = gen_rtx_REG (DImode, EXEC_REG);
    emit_move_insn (even, get_exec (0x5555555555555555UL));
    emit_insn (gen_<cadd_subadd><mode>3_exec (dest, x, y,
                                              gcn_gen_undef (<MODE>mode),
                                              even));
    rtx odd = gen_rtx_REG (DImode, EXEC_REG);
    emit_move_insn (odd, get_exec (0xaaaaaaaaaaaaaaaaUL));
    emit_insn (gen_<cadd_addsub><mode>3_exec (dest, x, y, dest, odd));

    DONE;
  })

(define_expand "vec_fmaddsub<mode>4"
  [(match_operand:V_noHI 0 "register_operand" "=&v")
   (match_operand:V_noHI 1 "register_operand" "v")
   (match_operand:V_noHI 2 "register_operand" "v")
   (match_operand:V_noHI 3 "register_operand" "v")]
  ""
  {
    rtx t1 = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_mul<mode>3 (t1, operands[1], operands[2]));
    rtx even = gen_rtx_REG (DImode, EXEC_REG);
    emit_move_insn (even, get_exec (0x5555555555555555UL));
    rtx dest = operands[0];
    emit_insn (gen_sub<mode>3_exec (dest, t1, operands[3],
                                    gcn_gen_undef (<MODE>mode), even));
    rtx odd = gen_rtx_REG (DImode, EXEC_REG);
    emit_move_insn (odd, get_exec (0xaaaaaaaaaaaaaaaaUL));
    emit_insn (gen_add<mode>3_exec (dest, t1, operands[3], dest, odd));

    DONE;
  })

(define_expand "vec_fmsubadd<mode>4"
  [(match_operand:V_noHI 0 "register_operand" "=&v")
   (match_operand:V_noHI 1 "register_operand" "v")
   (match_operand:V_noHI 2 "register_operand" "v")
   (match_operand:V_noHI 3 "register_operand" "v")]
  ""
  {
    rtx t1 = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_mul<mode>3 (t1, operands[1], operands[2]));
    rtx even = gen_rtx_REG (DImode, EXEC_REG);
    emit_move_insn (even, get_exec (0x5555555555555555UL));
    rtx dest = operands[0];
    emit_insn (gen_add<mode>3_exec (dest, t1, operands[3],
                                    gcn_gen_undef (<MODE>mode), even));
    rtx odd = gen_rtx_REG (DImode, EXEC_REG);
    emit_move_insn (odd, get_exec (0xaaaaaaaaaaaaaaaaUL));
    emit_insn (gen_sub<mode>3_exec (dest, t1, operands[3], dest, odd));

    DONE;
  })

;; }}}
;; {{{ ALU generic case

(define_code_iterator bitop [and ior xor])
(define_code_iterator shiftop [ashift lshiftrt ashiftrt])
(define_code_iterator minmaxop [smin smax umin umax])

(define_insn "<expander><mode>2<exec>"
  [(set (match_operand:V_INT_1REG 0 "gcn_valu_dst_operand"    "=  v")
	(bitunop:V_INT_1REG
	  (match_operand:V_INT_1REG 1 "gcn_valu_src0_operand" "vSvB")))]
  ""
  "v_<mnemonic>0\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "<expander><mode>3<exec>"
  [(set (match_operand:V_INT_1REG 0 "gcn_valu_dst_operand"	 "=  v,RD")
	(bitop:V_INT_1REG
	  (match_operand:V_INT_1REG 1 "gcn_valu_src0_operand"	 "%  v, 0")
	  (match_operand:V_INT_1REG 2 "gcn_valu_src1com_operand" "vSvB, v")))]
  ""
  "@
   v_<mnemonic>0\t%0, %2, %1
   ds_<mnemonic>0\t%A0, %2%O0"
  [(set_attr "type" "vop2,ds")
   (set_attr "length" "8,8")])

(define_insn_and_split "<expander><mode>3"
  [(set (match_operand:V_DI 0 "gcn_valu_dst_operand"	   "=  v,RD")
	(bitop:V_DI
	  (match_operand:V_DI 1 "gcn_valu_src0_operand"    "%  v,RD")
	  (match_operand:V_DI 2 "gcn_valu_src1com_operand" "vSvB, v")))]
  ""
  "@
   #
   ds_<mnemonic>0\t%A0, %2%O0"
  "(reload_completed && !gcn_ds_memory_operand (operands[0], <MODE>mode))"
  [(set (match_dup 3)
	(bitop:<VnSI> (match_dup 5) (match_dup 7)))
   (set (match_dup 4)
	(bitop:<VnSI> (match_dup 6) (match_dup 8)))]
  {
    operands[3] = gcn_operand_part (<MODE>mode, operands[0], 0);
    operands[4] = gcn_operand_part (<MODE>mode, operands[0], 1);
    operands[5] = gcn_operand_part (<MODE>mode, operands[1], 0);
    operands[6] = gcn_operand_part (<MODE>mode, operands[1], 1);
    operands[7] = gcn_operand_part (<MODE>mode, operands[2], 0);
    operands[8] = gcn_operand_part (<MODE>mode, operands[2], 1);
  }
  [(set_attr "type" "vmult,ds")
   (set_attr "length" "16,8")])

(define_insn_and_split "<expander><mode>3_exec"
  [(set (match_operand:V_DI 0 "gcn_valu_dst_operand"		  "=  v,RD")
	(vec_merge:V_DI
	  (bitop:V_DI
	    (match_operand:V_DI 1 "gcn_valu_src0_operand"	  "%  v,RD")
	    (match_operand:V_DI 2 "gcn_valu_src1com_operand"	  "vSvB, v"))
	  (match_operand:V_DI 3 "gcn_register_ds_or_unspec_operand" "U0,U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		  "   e, e")))]
  "!memory_operand (operands[0], VOIDmode)
   || (rtx_equal_p (operands[0], operands[1])
       && register_operand (operands[2], VOIDmode))"
  "@
   #
   ds_<mnemonic>0\t%A0, %2%O0"
  "(reload_completed && !gcn_ds_memory_operand (operands[0], <MODE>mode))"
  [(set (match_dup 5)
	(vec_merge:<VnSI>
	  (bitop:<VnSI> (match_dup 7) (match_dup 9))
	  (match_dup 11)
	  (match_dup 4)))
   (set (match_dup 6)
	(vec_merge:<VnSI>
	  (bitop:<VnSI> (match_dup 8) (match_dup 10))
	  (match_dup 12)
	  (match_dup 4)))]
  {
    operands[5] = gcn_operand_part (<MODE>mode, operands[0], 0);
    operands[6] = gcn_operand_part (<MODE>mode, operands[0], 1);
    operands[7] = gcn_operand_part (<MODE>mode, operands[1], 0);
    operands[8] = gcn_operand_part (<MODE>mode, operands[1], 1);
    operands[9] = gcn_operand_part (<MODE>mode, operands[2], 0);
    operands[10] = gcn_operand_part (<MODE>mode, operands[2], 1);
    operands[11] = gcn_operand_part (<MODE>mode, operands[3], 0);
    operands[12] = gcn_operand_part (<MODE>mode, operands[3], 1);
  }
  [(set_attr "type" "vmult,ds")
   (set_attr "length" "16,8")])

(define_expand "<expander><mode>3"
  [(set (match_operand:V_QIHI 0 "register_operand"  "= v")
	(shiftop:V_QIHI
	  (match_operand:V_QIHI 1 "gcn_alu_operand" "  v")
	  (vec_duplicate:V_QIHI
	    (match_operand:SI 2 "gcn_alu_operand"   "SvB"))))]
  ""
  {
    enum {ashift, lshiftrt, ashiftrt};
    bool unsignedp = (<code> == lshiftrt);
    rtx insi1 = gen_reg_rtx (<VnSI>mode);
    rtx insi2 = gen_reg_rtx (SImode);
    rtx outsi = gen_reg_rtx (<VnSI>mode);

    convert_move (insi1, operands[1], unsignedp);
    convert_move (insi2, operands[2], unsignedp);
    emit_insn (gen_<expander><vnsi>3 (outsi, insi1, insi2));
    convert_move (operands[0], outsi, unsignedp);
    DONE;
  })

(define_insn "<expander><mode>3<exec>"
  [(set (match_operand:V_INT_noHI 0 "register_operand"  "= v")
	(shiftop:V_INT_noHI
	  (match_operand:V_INT_noHI 1 "gcn_alu_operand" "  v")
	  (vec_duplicate:<VnSI>
	    (match_operand:SI 2 "gcn_alu_operand"  "SvB"))))]
  ""
  "v_<revmnemonic>0\t%0, %2, %1"
  [(set_attr "type" "vop2")
   (set_attr "length" "8")])

(define_expand "v<expander><mode>3"
  [(set (match_operand:V_QIHI 0 "register_operand"  "=v")
	(shiftop:V_QIHI
	  (match_operand:V_QIHI 1 "gcn_alu_operand" " v")
	  (match_operand:V_QIHI 2 "gcn_alu_operand" "vB")))]
  ""
  {
    enum {ashift, lshiftrt, ashiftrt};
    bool unsignedp = (<code> == lshiftrt);
    rtx insi1 = gen_reg_rtx (<VnSI>mode);
    rtx insi2 = gen_reg_rtx (<VnSI>mode);
    rtx outsi = gen_reg_rtx (<VnSI>mode);

    convert_move (insi1, operands[1], unsignedp);
    convert_move (insi2, operands[2], unsignedp);
    emit_insn (gen_v<expander><vnsi>3 (outsi, insi1, insi2));
    convert_move (operands[0], outsi, unsignedp);
    DONE;
  })

(define_insn "v<expander><mode>3<exec>"
  [(set (match_operand:V_INT_noHI 0 "register_operand"  "=v")
	(shiftop:V_INT_noHI
	  (match_operand:V_INT_noHI 1 "gcn_alu_operand" " v")
	  (match_operand:<VnSI> 2 "gcn_alu_operand"	"vB")))]
  ""
  "v_<revmnemonic>0\t%0, %2, %1"
  [(set_attr "type" "vop2")
   (set_attr "length" "8")])

(define_expand "<expander><mode>3"
  [(set (match_operand:V_QIHI 0 "gcn_valu_dst_operand")
	(minmaxop:V_QIHI
	  (match_operand:V_QIHI 1 "gcn_valu_src0_operand")
	  (match_operand:V_QIHI 2 "gcn_valu_src1com_operand")))]
  ""
  {
    enum {smin, umin, smax, umax};
    bool unsignedp = (<code> == umax || <code> == umin);
    rtx insi1 = gen_reg_rtx (<VnSI>mode);
    rtx insi2 = gen_reg_rtx (<VnSI>mode);
    rtx outsi = gen_reg_rtx (<VnSI>mode);

    convert_move (insi1, operands[1], unsignedp);
    convert_move (insi2, operands[2], unsignedp);
    emit_insn (gen_<code><vnsi>3 (outsi, insi1, insi2));
    convert_move (operands[0], outsi, unsignedp);
    DONE;
  })

(define_expand "<expander><mode>3_exec"
  [(set (match_operand:V_QIHI 0 "gcn_valu_dst_operand")
	(vec_merge:V_QIHI
	  (minmaxop:V_QIHI
	    (match_operand:V_QIHI 1 "gcn_valu_src0_operand")
	    (match_operand:V_QIHI 2 "gcn_valu_src1com_operand"))
	  (match_operand:V_QIHI 3 "gcn_register_or_unspec_operand" "U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand" "e")))]
  ""
  {
    enum {smin, umin, smax, umax};
    bool unsignedp = (<code> == umax || <code> == umin);
    rtx insi1 = gen_reg_rtx (<VnSI>mode);
    rtx insi2 = gen_reg_rtx (<VnSI>mode);
    rtx outsi = gen_reg_rtx (<VnSI>mode);
    rtx out = operands[0];
    rtx exec = operands[4];
    rtx tmp = gen_reg_rtx (<MODE>mode);

    convert_move (insi1, operands[1], unsignedp);
    convert_move (insi2, operands[2], unsignedp);
    emit_insn (gen_<code><vnsi>3_exec (outsi, insi1, insi2,
                                       gcn_gen_undef(<VnSI>mode), exec));
    convert_move (tmp, outsi, unsignedp);
    emit_insn (gen_mov<mode>_exec (out, tmp, operands[3], exec));
    DONE;
  })

(define_insn "<expander><vnsi>3<exec>"
  [(set (match_operand:V_SI 0 "gcn_valu_dst_operand"	   "=  v,RD")
	(minmaxop:V_SI
	  (match_operand:V_SI 1 "gcn_valu_src0_operand"    "%  v, 0")
	  (match_operand:V_SI 2 "gcn_valu_src1com_operand" "vSvB, v")))]
  ""
  "@
   v_<mnemonic>0\t%0, %2, %1
   ds_<mnemonic>0\t%A0, %2%O0"
  [(set_attr "type" "vop2,ds")
   (set_attr "length" "8,8")])

(define_insn_and_split "<expander><mode>3"
  [(set (match_operand:V_DI 0 "register_operand"      "=v")
	(minmaxop:V_DI
	  (match_operand:V_DI 1 "gcn_alu_operand"     " v")
	  (match_operand:V_DI 2 "gcn_alu_operand"     " v")))
    (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out = operands[0];
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);

    enum {smin, smax, umin, umax};
    bool minp = (<code> == smin || <code> == umin);
    if (<code> == smin || <code> == smax)
      emit_insn (gen_vec_cmp<mode>di (vcc, minp ? gen_rtx_LT (VOIDmode, 0, 0) :
                                      gen_rtx_GT (VOIDmode, 0, 0), operands[1],
                                      operands[2]));
    else
      emit_insn (gen_vec_cmp<mode>di (vcc, minp ? gen_rtx_LTU (VOIDmode, 0, 0) :
                                      gen_rtx_GTU (VOIDmode, 0, 0), operands[1],
                                      operands[2]));
    emit_insn (gen_vcond_mask_<mode>di (out, operands[1], operands[2], vcc));
  }
  [(set_attr "type" "mult")])

(define_insn_and_split "<expander><mode>3_exec"
  [(set (match_operand:V_DI 0 "register_operand"                 "= v")
	(vec_merge:V_DI
          (minmaxop:V_DI
            (match_operand:V_DI 1 "gcn_alu_operand"              "  v")
            (match_operand:V_DI 2 "gcn_alu_operand"              "  v"))
          (match_operand:V_DI 3 "gcn_register_or_unspec_operand" " U0")
          (match_operand:DI 4 "gcn_exec_reg_operand"  "+e")))
    (clobber (match_scratch:<VnDI> 5		      "= &v"))
    (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out = operands[0];
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    rtx exec = operands[4];
    rtx tmp = operands[5];

    enum {smin, smax, umin, umax};
    bool minp = (<code> == smin || <code> == umin);
    if (<code> == smin || <code> == smax)
      emit_insn (gen_vec_cmp<mode>di_exec (vcc,
                                           minp ? gen_rtx_LT (VOIDmode, 0, 0) :
                                           gen_rtx_GT (VOIDmode, 0, 0),
                                           operands[1], operands[2], exec));
    else
      emit_insn (gen_vec_cmp<mode>di_exec (vcc,
                                           minp ? gen_rtx_LTU (VOIDmode, 0, 0) :
                                           gen_rtx_GTU (VOIDmode, 0, 0),
                                           operands[1], operands[2], exec));
    emit_insn (gen_vcond_mask_<mode>di (tmp, operands[1], operands[2], vcc));
    emit_insn (gen_mov<mode>_exec (out, tmp, operands[3], exec));
  }
  [(set_attr "type" "mult")])

;; }}}
;; {{{ Int unops

(define_expand "neg<mode>2"
  [(match_operand:V_INT 0 "register_operand")
   (match_operand:V_INT 1 "register_operand")]
  ""
  {
    emit_insn (gen_sub<mode>3 (operands[0], gcn_vec_constant (<MODE>mode, 0),
			       operands[1]));
    DONE;
  })

(define_insn_and_split "one_cmpl<mode>2<exec>"
  [(set (match_operand:V_DI 0 "register_operand"  "=   v")
        (not:V_DI
          (match_operand:V_DI 1 "gcn_alu_operand" "vSvDB")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 3) (not:<VnSI> (match_dup 5)))
   (set (match_dup 4) (not:<VnSI> (match_dup 6)))]
  {
    operands[3] = gcn_operand_part (<VnDI>mode, operands[0], 0);
    operands[4] = gcn_operand_part (<VnDI>mode, operands[0], 1);
    operands[5] = gcn_operand_part (<VnDI>mode, operands[1], 0);
    operands[6] = gcn_operand_part (<VnDI>mode, operands[1], 1);
  }
  [(set_attr "type" "mult")])

;; }}}
;; {{{ FP binops - special cases

; GCN does not directly provide a DFmode subtract instruction, so we do it by
; adding the negated second operand to the first.

(define_insn "sub<mode>3<exec>"
  [(set (match_operand:V_DF 0 "register_operand"  "=  v,   v")
	(minus:V_DF
	  (match_operand:V_DF 1 "gcn_alu_operand" "vSvB,   v")
	  (match_operand:V_DF 2 "gcn_alu_operand" "   v,vSvB")))]
  ""
  "@
   v_add_f64\t%0, %1, -%2
   v_add_f64\t%0, -%2, %1"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8,8")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand"  "=  v,   v")
	(minus:DF
	  (match_operand:DF 1 "gcn_alu_operand" "vSvB,   v")
	  (match_operand:DF 2 "gcn_alu_operand" "   v,vSvB")))]
  ""
  "@
   v_add_f64\t%0, %1, -%2
   v_add_f64\t%0, -%2, %1"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8,8")])

;; }}}
;; {{{ FP binops - generic

(define_code_iterator comm_fp [plus mult smin smax])
(define_code_iterator nocomm_fp [minus])
(define_code_iterator all_fp [plus mult minus smin smax])

(define_insn "<expander><mode>3<exec>"
  [(set (match_operand:V_FP 0 "register_operand"  "=  v")
	(comm_fp:V_FP
	  (match_operand:V_FP 1 "gcn_alu_operand" "%  v")
	  (match_operand:V_FP 2 "gcn_alu_operand" "vSvB")))]
  ""
  "v_<mnemonic>0\t%0, %2, %1"
  [(set_attr "type" "vop2")
   (set_attr "length" "8")])

(define_insn "<expander><mode>3"
  [(set (match_operand:FP 0 "gcn_valu_dst_operand"    "=  v,  RL")
	(comm_fp:FP
	  (match_operand:FP 1 "gcn_valu_src0_operand" "%  v,   0")
	  (match_operand:FP 2 "gcn_valu_src1_operand" "vSvB,vSvB")))]
  ""
  "@
  v_<mnemonic>0\t%0, %2, %1
  v_<mnemonic>0\t%0, %1%O0"
  [(set_attr "type" "vop2,ds")
   (set_attr "length" "8")])

(define_insn "<expander><mode>3<exec>"
  [(set (match_operand:V_FP_1REG 0 "register_operand"  "=  v,   v")
	(nocomm_fp:V_FP_1REG
	  (match_operand:V_FP_1REG 1 "gcn_alu_operand" "vSvB,   v")
	  (match_operand:V_FP_1REG 2 "gcn_alu_operand" "   v,vSvB")))]
  ""
  "@
   v_<mnemonic>0\t%0, %1, %2
   v_<revmnemonic>0\t%0, %2, %1"
  [(set_attr "type" "vop2")
   (set_attr "length" "8,8")])

(define_insn "<expander><mode>3"
  [(set (match_operand:FP_1REG 0 "register_operand"  "=  v,   v")
	(nocomm_fp:FP_1REG
	  (match_operand:FP_1REG 1 "gcn_alu_operand" "vSvB,   v")
	  (match_operand:FP_1REG 2 "gcn_alu_operand" "   v,vSvB")))]
  ""
  "@
   v_<mnemonic>0\t%0, %1, %2
   v_<revmnemonic>0\t%0, %2, %1"
  [(set_attr "type" "vop2")
   (set_attr "length" "8,8")])

(define_code_iterator fminmaxop [smin smax])
(define_expand "<fexpander><mode>3"
  [(set (match_operand:FP 0 "gcn_valu_dst_operand")
	(fminmaxop:FP
	  (match_operand:FP 1 "gcn_valu_src0_operand")
	  (match_operand:FP 2 "gcn_valu_src1_operand")))]
  ""
  {})

(define_expand "<fexpander><mode>3<exec>"
  [(set (match_operand:V_FP 0 "gcn_valu_dst_operand")
	(fminmaxop:V_FP
	  (match_operand:V_FP 1 "gcn_valu_src0_operand")
	  (match_operand:V_FP 2 "gcn_valu_src1_operand")))]
  ""
  {})

;; }}}
;; {{{ FP unops

(define_insn "abs<mode>2"
  [(set (match_operand:FP 0 "register_operand"		 "=v")
	(abs:FP (match_operand:FP 1 "register_operand" " v")))]
  ""
  "v_add%i0\t%0, 0, |%1|"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "abs<mode>2<exec>"
  [(set (match_operand:V_FP 0 "register_operand"   "=v")
	(abs:V_FP
	  (match_operand:V_FP 1 "register_operand" " v")))]
  ""
  "v_add%i0\t%0, 0, |%1|"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "neg<mode>2<exec>"
  [(set (match_operand:V_FP 0 "register_operand"   "=v")
	(neg:V_FP
	  (match_operand:V_FP 1 "register_operand" " v")))]
  ""
  "v_add%i0\t%0, 0, -%1"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "sqrt<mode>2<exec>"
  [(set (match_operand:V_FP 0 "register_operand"  "=  v")
	(sqrt:V_FP
	  (match_operand:V_FP 1 "gcn_alu_operand" "vSvB")))]
  "flag_unsafe_math_optimizations"
  "v_sqrt%i0\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "sqrt<mode>2"
  [(set (match_operand:FP 0 "register_operand"  "=  v")
	(sqrt:FP
	  (match_operand:FP 1 "gcn_alu_operand" "vSvB")))]
  "flag_unsafe_math_optimizations"
  "v_sqrt%i0\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

; These FP unops have f64, f32 and f16 versions.
(define_int_iterator MATH_UNOP_1OR2REG
  [UNSPEC_FLOOR UNSPEC_CEIL])

; These FP unops only have f16/f32 versions.
(define_int_iterator MATH_UNOP_1REG
  [UNSPEC_EXP2 UNSPEC_LOG2])

(define_int_iterator MATH_UNOP_TRIG
  [UNSPEC_SIN UNSPEC_COS])

(define_int_attr math_unop
  [(UNSPEC_FLOOR "floor")
   (UNSPEC_CEIL "ceil")
   (UNSPEC_EXP2 "exp2")
   (UNSPEC_LOG2 "log2")
   (UNSPEC_SIN "sin")
   (UNSPEC_COS "cos")])

(define_int_attr math_unop_insn
  [(UNSPEC_FLOOR "floor")
   (UNSPEC_CEIL "ceil")
   (UNSPEC_EXP2 "exp")
   (UNSPEC_LOG2 "log")
   (UNSPEC_SIN "sin")
   (UNSPEC_COS "cos")])

(define_insn "<math_unop><mode>2"
  [(set (match_operand:FP 0 "register_operand"  "=  v")
	(unspec:FP
	  [(match_operand:FP 1 "gcn_alu_operand" "vSvB")]
	  MATH_UNOP_1OR2REG))]
  ""
  "v_<math_unop_insn>%i0\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "<math_unop><mode>2<exec>"
  [(set (match_operand:V_FP 0 "register_operand"  "=  v")
	(unspec:V_FP
	  [(match_operand:V_FP 1 "gcn_alu_operand" "vSvB")]
	  MATH_UNOP_1OR2REG))]
  ""
  "v_<math_unop_insn>%i0\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "<math_unop><mode>2"
  [(set (match_operand:FP_1REG 0 "register_operand"  "=  v")
	(unspec:FP_1REG
	  [(match_operand:FP_1REG 1 "gcn_alu_operand" "vSvB")]
	  MATH_UNOP_1REG))]
  "flag_unsafe_math_optimizations"
  "v_<math_unop_insn>%i0\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "<math_unop><mode>2<exec>"
  [(set (match_operand:V_FP_1REG 0 "register_operand"  "=  v")
	(unspec:V_FP_1REG
	  [(match_operand:V_FP_1REG 1 "gcn_alu_operand" "vSvB")]
	  MATH_UNOP_1REG))]
  "flag_unsafe_math_optimizations"
  "v_<math_unop_insn>%i0\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "*<math_unop><mode>2_insn"
  [(set (match_operand:FP_1REG 0 "register_operand"  "=  v")
	(unspec:FP_1REG
	  [(match_operand:FP_1REG 1 "gcn_alu_operand" "vSvB")]
	  MATH_UNOP_TRIG))]
  "flag_unsafe_math_optimizations"
  "v_<math_unop_insn>%i0\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "*<math_unop><mode>2<exec>_insn"
  [(set (match_operand:V_FP_1REG 0 "register_operand"  "=  v")
	(unspec:V_FP_1REG
	  [(match_operand:V_FP_1REG 1 "gcn_alu_operand" "vSvB")]
	  MATH_UNOP_TRIG))]
  "flag_unsafe_math_optimizations"
  "v_<math_unop_insn>%i0\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

; Trigonometric functions need their input scaled by 1/(2*PI) first.

(define_expand "<math_unop><mode>2"
  [(set (match_dup 2)
	(mult:FP_1REG
	  (match_dup 3)
	  (match_operand:FP_1REG 1 "gcn_alu_operand")))
   (set (match_operand:FP_1REG 0 "register_operand")
	(unspec:FP_1REG
	  [(match_dup 2)]
	  MATH_UNOP_TRIG))]
  "flag_unsafe_math_optimizations"
  {
    operands[2] = gen_reg_rtx (<MODE>mode);
    operands[3] = const_double_from_real_value (gcn_dconst1over2pi (),
						<MODE>mode);
  })

(define_expand "<math_unop><mode>2<exec>"
  [(set (match_dup 2)
	(mult:V_FP_1REG
	  (match_dup 3)
	  (match_operand:V_FP_1REG 1 "gcn_alu_operand")))
   (set (match_operand:V_FP_1REG 0 "register_operand")
	(unspec:V_FP_1REG
	  [(match_dup 2)]
	  MATH_UNOP_TRIG))]
  "flag_unsafe_math_optimizations"
  {
    operands[2] = gen_reg_rtx (<MODE>mode);
    operands[3] =
	gcn_vec_constant (<MODE>mode,
			  const_double_from_real_value (gcn_dconst1over2pi (),
							<SCALAR_MODE>mode));
  })

; Implement ldexp pattern

(define_insn "ldexp<mode>3<exec>"
  [(set (match_operand:SV_FP 0 "register_operand"     "=  v")
	(unspec:SV_FP
	  [(match_operand:SV_FP 1 "gcn_alu_operand"   "  vA")
	   (match_operand:<VnSI> 2 "gcn_alu_operand" "vSvA")]
	  UNSPEC_LDEXP))]
  ""
  "v_ldexp%i0\t%0, %1, %2"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

; Implement frexp patterns

(define_insn "frexp<mode>_exp2"
  [(set (match_operand:SI 0 "register_operand" "=v")
	(unspec:SI
	  [(match_operand:FP 1 "gcn_alu_operand" "vB")]
	  UNSPEC_FREXP_EXP))]
  ""
  "v_frexp_exp_i32%i1\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "frexp<mode>_mant2"
  [(set (match_operand:FP 0 "register_operand" "=v")
	(unspec:FP
	  [(match_operand:FP 1 "gcn_alu_operand" "vB")]
	  UNSPEC_FREXP_MANT))]
  ""
  "v_frexp_mant%i1\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "frexp<mode>_exp2<exec>"
  [(set (match_operand:<VnSI> 0 "register_operand" "=v")
	(unspec:<VnSI>
	  [(match_operand:V_FP 1 "gcn_alu_operand" "vB")]
	  UNSPEC_FREXP_EXP))]
  ""
  "v_frexp_exp_i32%i1\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "frexp<mode>_mant2<exec>"
  [(set (match_operand:V_FP 0 "register_operand" "=v")
	(unspec:V_FP
	  [(match_operand:V_FP 1 "gcn_alu_operand" "vB")]
	  UNSPEC_FREXP_MANT))]
  ""
  "v_frexp_mant%i1\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

;; }}}
;; {{{ FP fused multiply and add

(define_insn "fma<mode>4<exec>"
  [(set (match_operand:V_FP 0 "register_operand"  "=  v,   v")
	(fma:V_FP
	  (match_operand:V_FP 1 "gcn_alu_operand" "% vA,  vA")
	  (match_operand:V_FP 2 "gcn_alu_operand" "  vA,vSvA")
	  (match_operand:V_FP 3 "gcn_alu_operand" "vSvA,  vA")))]
  ""
  "v_fma%i0\t%0, %1, %2, %3"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "fma<mode>4_negop2<exec>"
  [(set (match_operand:V_FP 0 "register_operand"    "=  v,   v,   v")
	(fma:V_FP
	  (match_operand:V_FP 1 "gcn_alu_operand"   "  vA,  vA,vSvA")
	  (neg:V_FP
	    (match_operand:V_FP 2 "gcn_alu_operand" "  vA,vSvA,  vA"))
	  (match_operand:V_FP 3 "gcn_alu_operand"   "vSvA,  vA,  vA")))]
  ""
  "v_fma%i0\t%0, %1, -%2, %3"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "fma<mode>4"
  [(set (match_operand:FP 0 "register_operand"  "=  v,   v")
	(fma:FP
	  (match_operand:FP 1 "gcn_alu_operand" "% vA,  vA")
	  (match_operand:FP 2 "gcn_alu_operand" "  vA,vSvA")
	  (match_operand:FP 3 "gcn_alu_operand" "vSvA,  vA")))]
  ""
  "v_fma%i0\t%0, %1, %2, %3"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "fma<mode>4_negop2"
  [(set (match_operand:FP 0 "register_operand"    "=  v,   v,   v")
	(fma:FP
	  (match_operand:FP 1 "gcn_alu_operand"   "  vA,  vA,vSvA")
	  (neg:FP
	    (match_operand:FP 2 "gcn_alu_operand" "  vA,vSvA,  vA"))
	  (match_operand:FP 3 "gcn_alu_operand"   "vSvA,  vA,  vA")))]
  ""
  "v_fma%i0\t%0, %1, -%2, %3"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "fms<mode>4<exec>"
  [(set (match_operand:V_FP 0 "register_operand"  "=  v,   v")
	(fma:V_FP
	  (match_operand:V_FP 1 "gcn_alu_operand" "% vA,  vA")
	(match_operand:V_FP 2 "gcn_alu_operand"   "  vA,vSvA")
	(neg:V_FP
	  (match_operand:V_FP 3 "gcn_alu_operand" "vSvA,  vA"))))]
  ""
  "v_fma%i0\t%0, %1, %2, -%3"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "fms<mode>4_negop2<exec>"
  [(set (match_operand:V_FP 0 "register_operand"    "=  v,   v,   v")
	(fma:V_FP
	  (match_operand:V_FP 1 "gcn_alu_operand"   "  vA,  vA,vSvA")
	  (neg:V_FP
	    (match_operand:V_FP 2 "gcn_alu_operand" "  vA,vSvA,  vA"))
	  (neg:V_FP
	    (match_operand:V_FP 3 "gcn_alu_operand" "vSvA,  vA,  vA"))))]
  ""
  "v_fma%i0\t%0, %1, -%2, -%3"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "fms<mode>4"
  [(set (match_operand:FP 0 "register_operand"    "=  v,   v")
	(fma:FP
	  (match_operand:FP 1 "gcn_alu_operand"   "% vA,  vA")
	  (match_operand:FP 2 "gcn_alu_operand"   "  vA,vSvA")
	  (neg:FP
	    (match_operand:FP 3 "gcn_alu_operand" "vSvA,  vA"))))]
  ""
  "v_fma%i0\t%0, %1, %2, -%3"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "fms<mode>4_negop2"
  [(set (match_operand:FP 0 "register_operand"    "=  v,   v,   v")
	(fma:FP
	  (match_operand:FP 1 "gcn_alu_operand"   "  vA,  vA,vSvA")
	  (neg:FP
	    (match_operand:FP 2 "gcn_alu_operand" "  vA,vSvA,  vA"))
	  (neg:FP
	    (match_operand:FP 3 "gcn_alu_operand" "vSvA,  vA,  vA"))))]
  ""
  "v_fma%i0\t%0, %1, -%2, -%3"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

;; }}}
;; {{{ FP division

(define_insn "recip<mode>2<exec>"
  [(set (match_operand:SV_FP 0 "register_operand"  "=  v")
	(unspec:SV_FP
	  [(match_operand:SV_FP 1 "gcn_alu_operand" "vSvB")]
	  UNSPEC_RCP))]
  ""
  "v_rcp%i0\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

;; v_div_scale takes a numerator (op2) and denominator (op1) and returns the
;; one that matches op3 adjusted for best results in reciprocal division.
;; It also emits a VCC mask that is intended for input to v_div_fmas.
;; The caller is expected to call this twice, once for each input.  The output
;; VCC is the same in both cases, so the caller may discard one.
(define_insn "div_scale<mode><exec_vcc>"
  [(set (match_operand:SV_SFDF 0 "register_operand"   "=v")
	(unspec:SV_SFDF
	  [(match_operand:SV_SFDF 1 "gcn_alu_operand" "v")
	   (match_operand:SV_SFDF 2 "gcn_alu_operand" "v")
	   (match_operand:SV_SFDF 3 "gcn_alu_operand" "v")]
	  UNSPEC_DIV_SCALE))
   (set (match_operand:DI 4 "register_operand"        "=SvcV")
	(unspec:DI
	  [(match_dup 1) (match_dup 2) (match_dup 3)]
	  UNSPEC_DIV_SCALE))]
  ""
  "v_div_scale%i0\t%0, %4, %3, %1, %2"
  [(set_attr "type" "vop3b")
   (set_attr "length" "8")])

;; v_div_fmas is "FMA and Scale" that uses the VCC output from v_div_scale
;; to conditionally scale the output of the whole division operation.
;; This is necessary to counter the adjustments made by v_div_scale and
;; replaces the last FMA instruction of the Newton Raphson algorithm.
(define_insn "div_fmas<mode><exec>"
  [(set (match_operand:SV_SFDF 0 "register_operand"       "=v")
	(unspec:SV_SFDF
	  [(plus:SV_SFDF
	     (mult:SV_SFDF
	       (match_operand:SV_SFDF 1 "gcn_alu_operand" "v")
	       (match_operand:SV_SFDF 2 "gcn_alu_operand" "v"))
	     (match_operand:SV_SFDF 3 "gcn_alu_operand"   "v"))
	   (match_operand:DI 4 "register_operand"         "cV")]
	  UNSPEC_DIV_FMAS))]
  ""
  "v_div_fmas%i0\t%0, %1, %2, %3; %4"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")
   (set_attr "vccwait" "5")])

;; v_div_fixup takes the inputs and outputs of a division operation already
;; completed and cleans up the floating-point sign bit, infinity, underflow,
;; overflow, and NaN status.  It will also emit any FP exceptions.
;; op1: quotient,  op2: denominator,  op3: numerator
(define_insn "div_fixup<mode><exec>"
  [(set (match_operand:SV_FP 0 "register_operand"    "=v")
	(unspec:SV_FP
	  [(match_operand:SV_FP 1 "register_operand" "v")
	   (match_operand:SV_FP 2 "gcn_alu_operand"  "v")
	   (match_operand:SV_FP 3 "gcn_alu_operand"  "v")]
	  UNSPEC_DIV_FIXUP))]
  ""
  "v_div_fixup%i0\t%0, %1, %2, %3"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_expand "div<mode>3"
  [(match_operand:SV_SFDF 0 "register_operand")
   (match_operand:SV_SFDF 1 "gcn_alu_operand")
   (match_operand:SV_SFDF 2 "gcn_alu_operand")]
  ""
  {
    rtx numerator = operands[1];
    rtx denominator = operands[2];

    /* Scale the inputs if they are close to the FP limits.
       This will be reversed later.  */
    rtx vcc = gen_reg_rtx (DImode);
    rtx discardedvcc = gen_reg_rtx (DImode);
    rtx scaled_numerator = gen_reg_rtx (<MODE>mode);
    rtx scaled_denominator = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_div_scale<mode> (scaled_denominator,
				    denominator, numerator,
				    denominator, discardedvcc));
    emit_insn (gen_div_scale<mode> (scaled_numerator,
				    denominator, numerator,
				    numerator, vcc));

    /* Find the reciprocal of the denominator, and use Newton-Raphson to
       improve the accuracy over the basic hardware instruction.  */
    rtx one = gcn_vec_constant (<MODE>mode,
		  const_double_from_real_value (dconst1, <SCALAR_MODE>mode));
    rtx initrcp = gen_reg_rtx (<MODE>mode);
    rtx fma1 = gen_reg_rtx (<MODE>mode);
    rtx rcp = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_recip<mode>2 (initrcp, scaled_denominator));
    emit_insn (gen_fma<mode>4_negop2 (fma1, initrcp, scaled_denominator, one));
    emit_insn (gen_fma<mode>4 (rcp, fma1, initrcp, initrcp));

    /* Do the division "a/b" via "a*1/b" and use Newton-Raphson to improve
       the accuracy.  The "div_fmas" instruction reverses any scaling
       performed by "div_scale", above.  */
    rtx div_est = gen_reg_rtx (<MODE>mode);
    rtx fma2 = gen_reg_rtx (<MODE>mode);
    rtx fma3 = gen_reg_rtx (<MODE>mode);
    rtx fma4 = gen_reg_rtx (<MODE>mode);
    rtx fmas = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_mul<mode>3 (div_est, scaled_numerator, rcp));
    emit_insn (gen_fma<mode>4_negop2 (fma2, div_est, scaled_denominator,
				      scaled_numerator));
    emit_insn (gen_fma<mode>4 (fma3, fma2, rcp, div_est));
    emit_insn (gen_fma<mode>4_negop2 (fma4, fma3, scaled_denominator,
				      scaled_numerator));
    emit_insn (gen_div_fmas<mode> (fmas, fma4, rcp, fma3, vcc));

    /* Finally, use "div_fixup" to get the details right and find errors.  */
    emit_insn (gen_div_fixup<mode> (operands[0], fmas, denominator,
				    numerator));
    DONE;
  })

;; }}}
;; {{{ Int/FP conversions

(define_mode_iterator CVT_FROM_MODE [HI SI HF SF DF])
(define_mode_iterator CVT_TO_MODE [HI SI HF SF DF])

(define_mode_iterator VCVT_MODE
		      [V2HI V2SI V2HF V2SF V2DF
		       V4HI V4SI V4HF V4SF V4DF
		       V8HI V8SI V8HF V8SF V8DF
		       V16HI V16SI V16HF V16SF V16DF
		       V32HI V32SI V32HF V32SF V32DF
		       V64HI V64SI V64HF V64SF V64DF])
(define_mode_iterator VCVT_FMODE
		      [V2HF V2SF V2DF
		       V4HF V4SF V4DF
		       V8HF V8SF V8DF
		       V16HF V16SF V16DF
		       V32HF V32SF V32DF
		       V64HF V64SF V64DF])
(define_mode_iterator VCVT_IMODE
		      [V2HI V2SI
		       V4HI V4SI
		       V8HI V8SI
		       V16HI V16SI
		       V32HI V32SI
		       V64HI V64SI])

(define_code_iterator cvt_op [fix unsigned_fix
			      float unsigned_float
			      float_extend float_truncate])
(define_code_attr cvt_name [(fix "fix_trunc") (unsigned_fix "fixuns_trunc")
			    (float "float") (unsigned_float "floatuns")
			    (float_extend "extend") (float_truncate "trunc")])
(define_code_attr cvt_operands [(fix "%i0%i1") (unsigned_fix "%u0%i1")
				(float "%i0%i1") (unsigned_float "%i0%u1")
				(float_extend "%i0%i1")
				(float_truncate "%i0%i1")])

(define_insn "<cvt_name><CVT_FROM_MODE:mode><CVT_TO_MODE:mode>2"
  [(set (match_operand:CVT_TO_MODE 0 "register_operand"	   "=  v")
	(cvt_op:CVT_TO_MODE
	  (match_operand:CVT_FROM_MODE 1 "gcn_alu_operand" "vSvB")))]
  "gcn_valid_cvt_p (<CVT_FROM_MODE:MODE>mode, <CVT_TO_MODE:MODE>mode,
		    <cvt_name>_cvt)"
  "v_cvt<cvt_operands>\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "<cvt_name><VCVT_MODE:mode><VCVT_FMODE:mode>2<exec>"
  [(set (match_operand:VCVT_FMODE 0 "register_operand" "=  v")
	(cvt_op:VCVT_FMODE
	  (match_operand:VCVT_MODE 1 "gcn_alu_operand" "vSvB")))]
  "MODE_VF (<VCVT_MODE:MODE>mode) == MODE_VF (<VCVT_FMODE:MODE>mode)
   && gcn_valid_cvt_p (<VCVT_MODE:MODE>mode, <VCVT_FMODE:MODE>mode,
		       <cvt_name>_cvt)"
  "v_cvt<cvt_operands>\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "<cvt_name><VCVT_FMODE:mode><VCVT_IMODE:mode>2<exec>"
  [(set (match_operand:VCVT_IMODE 0 "register_operand"  "=  v")
	(cvt_op:VCVT_IMODE
	  (match_operand:VCVT_FMODE 1 "gcn_alu_operand" "vSvB")))]
  "MODE_VF (<VCVT_IMODE:MODE>mode) == MODE_VF (<VCVT_FMODE:MODE>mode)
   && gcn_valid_cvt_p (<VCVT_FMODE:MODE>mode, <VCVT_IMODE:MODE>mode,
		       <cvt_name>_cvt)"
  "v_cvt<cvt_operands>\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

;; }}}
;; {{{ Int/int conversions

(define_code_iterator all_convert [truncate zero_extend sign_extend])
(define_code_iterator zero_convert [truncate zero_extend])
(define_code_attr convop [
	(sign_extend "extend")
	(zero_extend "zero_extend")
	(truncate "trunc")])

(define_expand "<convop><V_INT_1REG_ALT:mode><V_INT_1REG:mode>2<exec>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"      "=v")
        (all_convert:V_INT_1REG
	  (match_operand:V_INT_1REG_ALT 1 "gcn_alu_operand" " v")))]
  "")

(define_insn "*<convop><V_INT_1REG_ALT:mode><V_INT_1REG:mode>_sdwa<exec>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"      "=v")
        (zero_convert:V_INT_1REG
	  (match_operand:V_INT_1REG_ALT 1 "gcn_alu_operand" " v")))]
  "!TARGET_RDNA3"
  "v_mov_b32_sdwa\t%0, %1 dst_sel:<V_INT_1REG:sdwa> dst_unused:UNUSED_PAD src0_sel:<V_INT_1REG_ALT:sdwa>"
  [(set_attr "type" "vop_sdwa")
   (set_attr "length" "8")])

(define_insn "extend<V_INT_1REG_ALT:mode><V_INT_1REG:mode>_sdwa<exec>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"	    "=v")
        (sign_extend:V_INT_1REG
	  (match_operand:V_INT_1REG_ALT 1 "gcn_alu_operand" " v")))]
  "!TARGET_RDNA3"
  "v_mov_b32_sdwa\t%0, sext(%1) src0_sel:<V_INT_1REG_ALT:sdwa>"
  [(set_attr "type" "vop_sdwa")
   (set_attr "length" "8")])

(define_insn "*<convop><V_INT_1REG_ALT:mode><V_INT_1REG:mode>_shift<exec>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"      "=v")
        (all_convert:V_INT_1REG
	  (match_operand:V_INT_1REG_ALT 1 "gcn_alu_operand" " v")))]
  "TARGET_RDNA3"
  {
    enum {extend, zero_extend, trunc};
    rtx shiftwidth = (<V_INT_1REG_ALT:SCALAR_MODE>mode == QImode
		      || <V_INT_1REG:SCALAR_MODE>mode == QImode
		      ? GEN_INT (24)
		      : <V_INT_1REG_ALT:SCALAR_MODE>mode == HImode
		        || <V_INT_1REG:SCALAR_MODE>mode == HImode
		      ? GEN_INT (16)
		      : NULL);
    operands[2] = shiftwidth;

    if (!shiftwidth)
      return "v_mov_b32 %0, %1";
    else if (<convop> == extend || <convop> == trunc)
      return "v_lshlrev_b32\t%0, %2, %1\;v_ashrrev_i32\t%0, %2, %0";
    else
      return "v_lshlrev_b32\t%0, %2, %1\;v_lshrrev_b32\t%0, %2, %0";
  }
  [(set_attr "type" "mult")
   (set_attr "length" "8")])

;; GCC can already do these for scalar types, but not for vector types.
;; Unfortunately you can't just do SUBREG on a vector to select the low part,
;; so there must be a few tricks here.

(define_insn_and_split "trunc<vndi><mode>2"
  [(set (match_operand:V_INT_1REG 0 "register_operand" "=v")
	(truncate:V_INT_1REG
	  (match_operand:<VnDI> 1 "gcn_alu_operand"     " v")))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx inlo = gcn_operand_part (<VnDI>mode, operands[1], 0);
    rtx out = operands[0];

    if (<MODE>mode != <VnSI>mode)
      emit_insn (gen_trunc<vnsi><mode>2 (out, inlo));
    else
      emit_move_insn (out, inlo);
  }
  [(set_attr "type" "vop2")
   (set_attr "length" "4")])

(define_insn_and_split "trunc<vndi><mode>2_exec"
  [(set (match_operand:V_INT_1REG 0 "register_operand"		  "=v")
	(vec_merge:V_INT_1REG
	  (truncate:V_INT_1REG
	    (match_operand:<VnDI> 1 "gcn_alu_operand"		  " v"))
	  (match_operand:V_INT_1REG 2 "gcn_alu_or_unspec_operand" "U0")
	  (match_operand:DI 3 "gcn_exec_operand"		  " e")))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out = operands[0];
    rtx inlo = gcn_operand_part (<VnDI>mode, operands[1], 0);
    rtx merge = operands[2];
    rtx exec = operands[3];

    if (<MODE>mode != <VnSI>mode)
      emit_insn (gen_trunc<vnsi><mode>2_exec (out, inlo, merge, exec));
    else
      emit_insn (gen_mov<mode>_exec (out, inlo, merge, exec));
  }
  [(set_attr "type" "vop2")
   (set_attr "length" "4")])

(define_insn_and_split "<convop><mode><vndi>2"
  [(set (match_operand:<VnDI> 0 "register_operand"	"=v")
	(any_extend:<VnDI>
	  (match_operand:V_INT_1REG 1 "gcn_alu_operand" " v")))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx outlo = gcn_operand_part (<VnDI>mode, operands[0], 0);
    rtx outhi = gcn_operand_part (<VnDI>mode, operands[0], 1);
    rtx in = operands[1];
      
    if (<MODE>mode != <VnSI>mode)
      emit_insn (gen_<convop><mode><vnsi>2 (outlo, in));
    else
      emit_move_insn (outlo, in);
    if ('<su>' == 's')
      emit_insn (gen_ashr<vnsi>3 (outhi, outlo, GEN_INT (31)));
    else
      emit_insn (gen_vec_duplicate<vnsi> (outhi, const0_rtx));
  }
  [(set_attr "type" "mult")
   (set_attr "length" "12")])

(define_insn_and_split "<convop><mode><vndi>2_exec"
  [(set (match_operand:<VnDI> 0 "register_operand"	     "=v")
	(vec_merge:<VnDI>
	  (any_extend:<VnDI>
	    (match_operand:V_INT_1REG 1 "gcn_alu_operand"    " v"))
	  (match_operand:<VnDI> 2 "gcn_alu_or_unspec_operand" "U0")
	  (match_operand:DI 3 "gcn_exec_operand"	     " e")))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx outlo = gcn_operand_part (<VnDI>mode, operands[0], 0);
    rtx outhi = gcn_operand_part (<VnDI>mode, operands[0], 1);
    rtx in = operands[1];
    rtx mergelo = gcn_operand_part (<VnDI>mode, operands[2], 0);
    rtx mergehi = gcn_operand_part (<VnDI>mode, operands[2], 1);
    rtx exec = operands[3];
      
    if (<MODE>mode != <VnSI>mode)
      emit_insn (gen_<convop><mode><vnsi>2_exec (outlo, in, mergelo, exec));
    else
      emit_insn (gen_mov<mode>_exec (outlo, in, mergelo, exec));
    if ('<su>' == 's')
      emit_insn (gen_ashr<vnsi>3_exec (outhi, outlo, GEN_INT (31), mergehi,
				       exec));
    else
      emit_insn (gen_vec_duplicate<vnsi>_exec (outhi, const0_rtx, mergehi,
					       exec));
  }
  [(set_attr "type" "mult")
   (set_attr "length" "12")])

;; }}}
;; {{{ Vector comparison/merge

(define_insn "vec_cmp<mode>di"
  [(set (match_operand:DI 0 "register_operand"	      "=cV,cV,  e, e,Sg,Sg,  e, e")
	(match_operator:DI 1 "gcn_fp_compare_operator"
	  [(match_operand:V_noQI 2 "gcn_alu_operand"  "vSv, B,vSv, B, v,vA,vSv, B")
	   (match_operand:V_noQI 3 "gcn_vop3_operand" "  v, v,  v, v,vA, v,  v, v")]))
   (clobber (match_scratch:DI 4			      "= X, X, cV,cV, X, X,  X, X"))]
  ""
  "@
   v_cmp%E1\tvcc, %2, %3
   v_cmp%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmp%E1\t%0, %2, %3
   v_cmp%E1\t%0, %2, %3
   v_cmpx%E1\t%2, %3
   v_cmpx%E1\t%2, %3"
  [(set_attr "type" "vopc,vopc,vopc,vopc,vop3a,vop3a,vopc,vopc")
   (set_attr "length" "4,8,4,8,8,8,4,8")
   (set_attr "rdna" "*,*,no,no,*,*,yes,yes")])

(define_expand "vec_cmpu<mode>di"
  [(match_operand:DI 0 "register_operand")
   (match_operator 1 "gcn_compare_operator"
     [(match_operand:V_INT_noQI 2 "gcn_alu_operand")
      (match_operand:V_INT_noQI 3 "gcn_vop3_operand")])]
  ""
  {
    /* Unsigned comparisons use the same patterns as signed comparisons,
       except that they use unsigned operators (e.g. LTU vs LT).
       The '%E1' directive then does the Right Thing.  */
    emit_insn (gen_vec_cmp<mode>di (operands[0], operands[1], operands[2],
				    operands[3]));
    DONE;
  })

; There's no instruction for 8-bit vector comparison, so we need to extend.
(define_expand "vec_cmp<u><mode>di"
  [(match_operand:DI 0 "register_operand")
   (match_operator 1 "gcn_compare_operator"
     [(any_extend:<VnSI> (match_operand:V_QI 2 "gcn_alu_operand"))
      (any_extend:<VnSI> (match_operand:V_QI 3 "gcn_vop3_operand"))])]
  "can_create_pseudo_p ()"
  {
    rtx sitmp1 = gen_reg_rtx (<VnSI>mode);
    rtx sitmp2 = gen_reg_rtx (<VnSI>mode);

    emit_insn (gen_<expander><mode><vnsi>2 (sitmp1, operands[2]));
    emit_insn (gen_<expander><mode><vnsi>2 (sitmp2, operands[3]));
    emit_insn (gen_vec_cmp<vnsi>di (operands[0], operands[1], sitmp1, sitmp2));
    DONE;
  })

(define_insn "vec_cmp<mode>di_exec"
  [(set (match_operand:DI 0 "register_operand"	       "=cV,cV,  e, e,Sg,Sg,  e, e")
	(and:DI
	  (match_operator 1 "gcn_fp_compare_operator"
	    [(match_operand:V_noQI 2 "gcn_alu_operand" "vSv, B,vSv, B, v,vA,vSv, B")
	     (match_operand:V_noQI 3 "gcn_vop3_operand" " v, v,  v, v,vA, v,  v, v")])
	  (match_operand:DI 4 "gcn_exec_reg_operand"   "  e, e,  e, e, e, e,  e, e")))
   (clobber (match_scratch:DI 5			       "= X, X, cV,cV, X, X,  X, X"))]
  ""
  "@
   v_cmp%E1\tvcc, %2, %3
   v_cmp%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmp%E1\t%0, %2, %3
   v_cmp%E1\t%0, %2, %3
   v_cmpx%E1\t%2, %3
   v_cmpx%E1\t%2, %3"
  [(set_attr "type" "vopc,vopc,vopc,vopc,vop3a,vop3a,vopc,vopc")
   (set_attr "length" "4,8,4,8,8,8,4,8")
   (set_attr "rdna" "*,*,no,no,*,*,yes,yes")])

(define_expand "vec_cmpu<mode>di_exec"
  [(match_operand:DI 0 "register_operand")
   (match_operator 1 "gcn_compare_operator"
     [(match_operand:V_INT_noQI 2 "gcn_alu_operand")
      (match_operand:V_INT_noQI 3 "gcn_vop3_operand")])
   (match_operand:DI 4 "gcn_exec_reg_operand")]
  ""
  {
    /* Unsigned comparisons use the same patterns as signed comparisons,
       except that they use unsigned operators (e.g. LTU vs LT).
       The '%E1' directive then does the Right Thing.  */
    emit_insn (gen_vec_cmpu<mode>di_exec (operands[0], operands[1],
					  operands[2], operands[3],
					  operands[4]));
    DONE;
  })

(define_expand "vec_cmp<u><mode>di_exec"
  [(match_operand:DI 0 "register_operand")
   (match_operator 1 "gcn_compare_operator"
     [(any_extend:<VnSI> (match_operand:V_QI 2 "gcn_alu_operand"))
      (any_extend:<VnSI> (match_operand:V_QI 3 "gcn_vop3_operand"))])
   (match_operand:DI 4 "gcn_exec_reg_operand")]
  "can_create_pseudo_p ()"
  {
    rtx sitmp1 = gen_reg_rtx (<VnSI>mode);
    rtx sitmp2 = gen_reg_rtx (<VnSI>mode);

    emit_insn (gen_<expander><mode><vnsi>2_exec (sitmp1, operands[2],
						 operands[2], operands[4]));
    emit_insn (gen_<expander><mode><vnsi>2_exec (sitmp2, operands[3],
						 operands[3], operands[4]));
    emit_insn (gen_vec_cmp<vnsi>di_exec (operands[0], operands[1], sitmp1,
					 sitmp2, operands[4]));
    DONE;
  })

(define_insn "vec_cmp<mode>di_dup"
  [(set (match_operand:DI 0 "register_operand"		   "=cV,cV, e,e,Sg, e,e")
	(match_operator:DI 1 "gcn_fp_compare_operator"
	  [(vec_duplicate:V_noQI
	     (match_operand:<SCALAR_MODE> 2 "gcn_alu_operand"
							   " Sv, B,Sv,B, A,Sv,B"))
	   (match_operand:V_noQI 3 "gcn_vop3_operand"	   "  v, v, v,v, v, v,v")]))
   (clobber (match_scratch:DI 4				   "= X,X,cV,cV, X, X,X"))]
  ""
  "@
   v_cmp%E1\tvcc, %2, %3
   v_cmp%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmp%E1\t%0, %2, %3
   v_cmpx%E1\t%2, %3
   v_cmpx%E1\t%2, %3"
  [(set_attr "type" "vopc,vopc,vopc,vopc,vop3a,vopc,vopc")
   (set_attr "length" "4,8,4,8,8,4,8")
   (set_attr "rdna" "*,*,no,no,*,yes,yes")])

(define_insn "vec_cmp<mode>di_dup_exec"
  [(set (match_operand:DI 0 "register_operand"		    "=cV,cV, e,e,Sg, e,e")
	(and:DI
	  (match_operator 1 "gcn_fp_compare_operator"
	    [(vec_duplicate:V_noQI
	       (match_operand:<SCALAR_MODE> 2 "gcn_alu_operand"
							    " Sv, B,Sv,B, A,Sv,B"))
	     (match_operand:V_noQI 3 "gcn_vop3_operand"	    "  v, v, v,v, v, v,v")])
	  (match_operand:DI 4 "gcn_exec_reg_operand"	    "  e, e, e,e, e, e,e")))
   (clobber (match_scratch:DI 5				    "= X,X,cV,cV, X, X,X"))]
  ""
  "@
   v_cmp%E1\tvcc, %2, %3
   v_cmp%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmp%E1\t%0, %2, %3
   v_cmpx%E1\t%2, %3
   v_cmpx%E1\t%2, %3"
  [(set_attr "type" "vopc,vopc,vopc,vopc,vop3a,vopc,vopc")
   (set_attr "length" "4,8,4,8,8,4,8")
   (set_attr "rdna" "*,*,no,no,*,yes,yes")])

(define_expand "vcond_mask_<mode>di"
  [(parallel
    [(set (match_operand:V_ALL 0   "register_operand" "")
	  (vec_merge:V_ALL
	    (match_operand:V_ALL 1 "gcn_vop3_operand" "")
	    (match_operand:V_ALL 2 "gcn_alu_operand" "")
	    (match_operand:DI 3		     "register_operand" "")))
     (clobber (scratch:<VnDI>))])]
  ""
  "")

(define_expand "vcond<V_ALL:mode><V_ALL_ALT:mode>"
  [(match_operand:V_ALL 0 "register_operand")
   (match_operand:V_ALL 1 "gcn_vop3_operand")
   (match_operand:V_ALL 2 "gcn_alu_operand")
   (match_operator 3 "gcn_fp_compare_operator"
     [(match_operand:V_ALL_ALT 4 "gcn_alu_operand")
      (match_operand:V_ALL_ALT 5 "gcn_vop3_operand")])]
  ""
  {
    rtx tmp = gen_reg_rtx (DImode);
    emit_insn (gen_vec_cmp<V_ALL_ALT:mode>di
	       (tmp, operands[3], operands[4], operands[5]));
    emit_insn (gen_vcond_mask_<V_ALL:mode>di
	       (operands[0], operands[1], operands[2], tmp));
    DONE;
  })

(define_expand "vcond<V_ALL:mode><V_ALL_ALT:mode>_exec"
  [(match_operand:V_ALL 0 "register_operand")
   (match_operand:V_ALL 1 "gcn_vop3_operand")
   (match_operand:V_ALL 2 "gcn_alu_operand")
   (match_operator 3 "gcn_fp_compare_operator"
     [(match_operand:V_ALL_ALT 4 "gcn_alu_operand")
      (match_operand:V_ALL_ALT 5 "gcn_vop3_operand")])
   (match_operand:DI 6 "gcn_exec_reg_operand" "e")]
  ""
  {
    rtx tmp = gen_reg_rtx (DImode);
    emit_insn (gen_vec_cmp<V_ALL_ALT:mode>di_exec
	       (tmp, operands[3], operands[4], operands[5], operands[6]));
    emit_insn (gen_vcond_mask_<V_ALL:mode>di
	       (operands[0], operands[1], operands[2], tmp));
    DONE;
  })

(define_expand "vcondu<V_ALL:mode><V_INT:mode>"
  [(match_operand:V_ALL 0 "register_operand")
   (match_operand:V_ALL 1 "gcn_vop3_operand")
   (match_operand:V_ALL 2 "gcn_alu_operand")
   (match_operator 3 "gcn_fp_compare_operator"
     [(match_operand:V_INT 4 "gcn_alu_operand")
      (match_operand:V_INT 5 "gcn_vop3_operand")])]
  ""
  {
    rtx tmp = gen_reg_rtx (DImode);
    emit_insn (gen_vec_cmpu<V_INT:mode>di
	       (tmp, operands[3], operands[4], operands[5]));
    emit_insn (gen_vcond_mask_<V_ALL:mode>di
	       (operands[0], operands[1], operands[2], tmp));
    DONE;
  })

(define_expand "vcondu<V_ALL:mode><V_INT:mode>_exec"
  [(match_operand:V_ALL 0 "register_operand")
   (match_operand:V_ALL 1 "gcn_vop3_operand")
   (match_operand:V_ALL 2 "gcn_alu_operand")
   (match_operator 3 "gcn_fp_compare_operator"
     [(match_operand:V_INT 4 "gcn_alu_operand")
      (match_operand:V_INT 5 "gcn_vop3_operand")])
   (match_operand:DI 6 "gcn_exec_reg_operand" "e")]
  ""
  {
    rtx tmp = gen_reg_rtx (DImode);
    emit_insn (gen_vec_cmpu<V_INT:mode>di_exec
	       (tmp, operands[3], operands[4], operands[5], operands[6]));
    emit_insn (gen_vcond_mask_<V_ALL:mode>di
	       (operands[0], operands[1], operands[2], tmp));
    DONE;
  })

;; }}}
;; {{{ Fully masked loop support

(define_expand "while_ultsidi"
  [(match_operand:DI 0 "register_operand")
   (match_operand:SI 1 "")
   (match_operand:SI 2 "")
   (match_operand:SI 3 "")]
  ""
  {
    if (GET_CODE (operands[1]) != CONST_INT
	|| GET_CODE (operands[2]) != CONST_INT)
      {
	rtx _0_1_2_3 = gen_rtx_REG (V64SImode, VGPR_REGNO (1));
	rtx tmp = _0_1_2_3;
	if (GET_CODE (operands[1]) != CONST_INT
	    || INTVAL (operands[1]) != 0)
	  {
	    tmp = gen_reg_rtx (V64SImode);
	    emit_insn (gen_addv64si3_dup (tmp, _0_1_2_3, operands[1]));
	  }
	emit_insn (gen_vec_cmpv64sidi_dup (operands[0],
					   gen_rtx_GT (VOIDmode, 0, 0),
					   operands[2], tmp));
      }
    else
      {
	HOST_WIDE_INT diff = INTVAL (operands[2]) - INTVAL (operands[1]);
	HOST_WIDE_INT mask = (diff >= 64 ? -1
			      : ~((unsigned HOST_WIDE_INT)-1 << diff));
	emit_move_insn (operands[0], gen_rtx_CONST_INT (VOIDmode, mask));
      }
    if (INTVAL (operands[3]) < 64)
      emit_insn (gen_anddi3 (operands[0], operands[0],
			     gen_rtx_CONST_INT (VOIDmode,
						~((unsigned HOST_WIDE_INT)-1
						  << INTVAL (operands[3])))));
    DONE;
  })

(define_expand "maskload<mode>di"
  [(match_operand:V_MOV 0 "register_operand")
   (match_operand:V_MOV 1 "memory_operand")
   (match_operand 2 "")]
  ""
  {
    rtx exec = force_reg (DImode, operands[2]);
    rtx addr = gcn_expand_scalar_to_vector_address
		(<MODE>mode, exec, operands[1], gen_rtx_SCRATCH (<VnDI>mode));
    rtx as = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[1]));
    rtx v = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[1]));

    /* Masked lanes are required to hold zero.  */
    emit_move_insn (operands[0], gcn_vec_constant (<MODE>mode, 0));

    emit_insn (gen_gather<mode>_expr_exec (operands[0], addr, as, v,
					   operands[0], exec));
    DONE;
  })

(define_expand "maskstore<mode>di"
  [(match_operand:V_MOV 0 "memory_operand")
   (match_operand:V_MOV 1 "register_operand")
   (match_operand 2 "")]
  ""
  {
    rtx exec = force_reg (DImode, operands[2]);
    rtx addr = gcn_expand_scalar_to_vector_address
		(<MODE>mode, exec, operands[0], gen_rtx_SCRATCH (<VnDI>mode));
    rtx as = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[0]));
    rtx v = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[0]));
    emit_insn (gen_scatter<mode>_expr_exec (addr, operands[1], as, v, exec));
    DONE;
  })

(define_expand "mask_gather_load<mode><vnsi>"
  [(match_operand:V_MOV 0 "register_operand")
   (match_operand:DI 1 "register_operand")
   (match_operand:<VnSI> 2 "register_operand")
   (match_operand 3 "immediate_operand")
   (match_operand:SI 4 "gcn_alu_operand")
   (match_operand:DI 5 "")]
  ""
  {
    rtx exec = force_reg (DImode, operands[5]);

    rtx addr = gcn_expand_scaled_offsets (DEFAULT_ADDR_SPACE, operands[1],
					  operands[2], operands[4],
					  INTVAL (operands[3]), exec);

    /* Masked lanes are required to hold zero.  */
    emit_move_insn (operands[0], gcn_vec_constant (<MODE>mode, 0));

    if (GET_MODE (addr) == <VnDI>mode)
      emit_insn (gen_gather<mode>_insn_1offset_exec (operands[0], addr,
						     const0_rtx, const0_rtx,
						     const0_rtx, operands[0],
						     exec));
    else
      emit_insn (gen_gather<mode>_insn_2offsets_exec (operands[0], operands[1],
						      addr, const0_rtx,
						      const0_rtx, const0_rtx,
						      operands[0], exec));
    DONE;
  })

(define_expand "mask_scatter_store<mode><vnsi>"
  [(match_operand:DI 0 "register_operand")
   (match_operand:<VnSI> 1 "register_operand")
   (match_operand 2 "immediate_operand")
   (match_operand:SI 3 "gcn_alu_operand")
   (match_operand:V_MOV 4 "register_operand")
   (match_operand:DI 5 "")]
  ""
  {
    rtx exec = force_reg (DImode, operands[5]);

    rtx addr = gcn_expand_scaled_offsets (DEFAULT_ADDR_SPACE, operands[0],
					  operands[1], operands[3],
					  INTVAL (operands[2]), exec);

    if (GET_MODE (addr) == <VnDI>mode)
      emit_insn (gen_scatter<mode>_insn_1offset_exec (addr, const0_rtx,
						      operands[4], const0_rtx,
						      const0_rtx,
						      exec));
    else
      emit_insn (gen_scatter<mode>_insn_2offsets_exec (operands[0], addr,
						       const0_rtx, operands[4],
						       const0_rtx, const0_rtx,
						       exec));
    DONE;
  })

(define_code_iterator cond_op [plus minus mult])

(define_expand "cond_<expander><mode>"
  [(match_operand:V_ALL 0 "register_operand")
   (match_operand:DI 1 "register_operand")
   (cond_op:V_ALL
     (match_operand:V_ALL 2 "gcn_alu_operand")
     (match_operand:V_ALL 3 "gcn_alu_operand"))
   (match_operand:V_ALL 4 "register_operand")]
  ""
  {
    operands[1] = force_reg (DImode, operands[1]);
    operands[2] = force_reg (<MODE>mode, operands[2]);

    emit_insn (gen_<expander><mode>3_exec (operands[0], operands[2],
					   operands[3], operands[4],
					   operands[1]));
    DONE;
  })

(define_code_iterator cond_fminmaxop [smin smax])

(define_expand "cond_<fexpander><mode>"
  [(match_operand:V_FP 0 "register_operand")
   (match_operand:DI 1 "register_operand")
   (cond_fminmaxop:V_FP
     (match_operand:V_FP 2 "gcn_alu_operand")
     (match_operand:V_FP 3 "gcn_alu_operand"))
   (match_operand:V_FP 4 "register_operand")]
  ""
  {
    operands[1] = force_reg (DImode, operands[1]);
    operands[2] = force_reg (<MODE>mode, operands[2]);

    emit_insn (gen_<fexpander><mode>3_exec (operands[0], operands[2],
					    operands[3], operands[4],
					    operands[1]));
    DONE;
  })

(define_code_iterator cond_minmaxop [smin smax umin umax])

(define_expand "cond_<expander><mode>"
  [(match_operand:V_INT 0 "register_operand")
   (match_operand:DI 1 "register_operand")
   (cond_minmaxop:V_INT
     (match_operand:V_INT 2 "gcn_alu_operand")
     (match_operand:V_INT 3 "gcn_alu_operand"))
   (match_operand:V_INT 4 "register_operand")]
  ""
  {
    operands[1] = force_reg (DImode, operands[1]);
    operands[2] = force_reg (<MODE>mode, operands[2]);
    rtx tmp = gen_reg_rtx (<MODE>mode);

    emit_insn (gen_<expander><mode>3_exec (tmp, operands[2], operands[3],
                                           gcn_gen_undef(<MODE>mode),
                                           operands[1]));
    emit_insn (gen_vcond_mask_<mode>di (operands[0], tmp, operands[4],
                                        operands[1]));
    DONE;
  })

(define_code_iterator cond_bitop [and ior xor])

(define_expand "cond_<expander><mode>"
  [(match_operand:V_INT 0 "register_operand")
   (match_operand:DI 1 "register_operand")
   (cond_bitop:V_INT
     (match_operand:V_INT 2 "gcn_alu_operand")
     (match_operand:V_INT 3 "gcn_alu_operand"))
   (match_operand:V_INT 4 "register_operand")]
  ""
  {
    operands[1] = force_reg (DImode, operands[1]);
    operands[2] = force_reg (<MODE>mode, operands[2]);

    emit_insn (gen_<expander><mode>3_exec (operands[0], operands[2],
					   operands[3], operands[4],
					   operands[1]));
    DONE;
  })

(define_code_iterator cond_shiftop [ashift lshiftrt ashiftrt])

(define_expand "cond_<expander><mode>"
  [(match_operand:V_INT_noHI 0 "register_operand")
   (match_operand:DI 1 "register_operand")
   (cond_shiftop:V_INT_noHI
     (match_operand:V_INT_noHI 2 "gcn_alu_operand")
     (match_operand:V_INT_noHI 3 "gcn_alu_operand"))
   (match_operand:V_INT_noHI 4 "register_operand")]
  ""
  {
    operands[1] = force_reg (DImode, operands[1]);
    operands[2] = force_reg (<MODE>mode, operands[2]);

    rtx shiftby = gen_reg_rtx (<VnSI>mode);
    convert_move (shiftby, operands[3], 0);

    emit_insn (gen_v<expander><mode>3_exec (operands[0], operands[2],
                                            shiftby, operands[4],
                                            operands[1]));
    DONE;
  })

;; }}}
;; {{{ Vector reductions

(define_int_iterator REDUC_UNSPEC [UNSPEC_SMIN_DPP_SHR UNSPEC_SMAX_DPP_SHR
				   UNSPEC_UMIN_DPP_SHR UNSPEC_UMAX_DPP_SHR
				   UNSPEC_PLUS_DPP_SHR
				   UNSPEC_AND_DPP_SHR
				   UNSPEC_IOR_DPP_SHR UNSPEC_XOR_DPP_SHR])

(define_int_iterator REDUC_2REG_UNSPEC [UNSPEC_PLUS_DPP_SHR
					UNSPEC_AND_DPP_SHR
					UNSPEC_IOR_DPP_SHR UNSPEC_XOR_DPP_SHR])

; FIXME: Isn't there a better way of doing this?
(define_int_attr reduc_unspec [(UNSPEC_SMIN_DPP_SHR "UNSPEC_SMIN_DPP_SHR")
			       (UNSPEC_SMAX_DPP_SHR "UNSPEC_SMAX_DPP_SHR")
			       (UNSPEC_UMIN_DPP_SHR "UNSPEC_UMIN_DPP_SHR")
			       (UNSPEC_UMAX_DPP_SHR "UNSPEC_UMAX_DPP_SHR")
			       (UNSPEC_PLUS_DPP_SHR "UNSPEC_PLUS_DPP_SHR")
			       (UNSPEC_AND_DPP_SHR "UNSPEC_AND_DPP_SHR")
			       (UNSPEC_IOR_DPP_SHR "UNSPEC_IOR_DPP_SHR")
			       (UNSPEC_XOR_DPP_SHR "UNSPEC_XOR_DPP_SHR")])

(define_int_attr reduc_op [(UNSPEC_SMIN_DPP_SHR "smin")
			   (UNSPEC_SMAX_DPP_SHR "smax")
			   (UNSPEC_UMIN_DPP_SHR "umin")
			   (UNSPEC_UMAX_DPP_SHR "umax")
			   (UNSPEC_PLUS_DPP_SHR "plus")
			   (UNSPEC_AND_DPP_SHR "and")
			   (UNSPEC_IOR_DPP_SHR "ior")
			   (UNSPEC_XOR_DPP_SHR "xor")])

(define_int_attr reduc_insn [(UNSPEC_SMIN_DPP_SHR "v_min%i0")
			     (UNSPEC_SMAX_DPP_SHR "v_max%i0")
			     (UNSPEC_UMIN_DPP_SHR "v_min%u0")
			     (UNSPEC_UMAX_DPP_SHR "v_max%u0")
			     (UNSPEC_PLUS_DPP_SHR "v_add%U0")
			     (UNSPEC_AND_DPP_SHR  "v_and%B0")
			     (UNSPEC_IOR_DPP_SHR  "v_or%B0")
			     (UNSPEC_XOR_DPP_SHR  "v_xor%B0")])

(define_expand "reduc_<reduc_op>_scal_<mode>"
  [(set (match_operand:<SCALAR_MODE> 0 "register_operand")
	(unspec:<SCALAR_MODE>
	  [(match_operand:V_ALL 1 "register_operand")]
	  REDUC_UNSPEC))]
  "!TARGET_RDNA2_PLUS"
  {
    rtx tmp = gcn_expand_reduc_scalar (<MODE>mode, operands[1],
				       <reduc_unspec>);

    rtx last_lane = GEN_INT (GET_MODE_NUNITS (<MODE>mode) - 1);
    emit_insn (gen_vec_extract<mode><scalar_mode> (operands[0], tmp,
						   last_lane));

    DONE;
  })

(define_expand "reduc_<fexpander>_scal_<mode>"
  [(match_operand:<SCALAR_MODE> 0 "register_operand")
   (fminmaxop:V_FP
     (match_operand:V_FP 1 "register_operand"))]
  "!TARGET_RDNA2_PLUS"
  {
    /* fmin/fmax are identical to smin/smax.  */
    emit_insn (gen_reduc_<expander>_scal_<mode> (operands[0], operands[1]));
    DONE;
  })

;; Warning: This "-ffast-math" implementation converts in-order reductions
;;          into associative reductions. It's also used where OpenMP or
;;          OpenACC paralellization has already broken the in-order semantics.
(define_expand "fold_left_plus_<mode>"
 [(match_operand:<SCALAR_MODE> 0 "register_operand")
  (match_operand:<SCALAR_MODE> 1 "gcn_alu_operand")
  (match_operand:V_FP 2 "gcn_alu_operand")]
  "!TARGET_RDNA2_PLUS
   && can_create_pseudo_p ()
   && (flag_openacc || flag_openmp
       || flag_associative_math)"
  {
    rtx dest = operands[0];
    rtx scalar = operands[1];
    rtx vector = operands[2];
    rtx tmp = gen_reg_rtx (<SCALAR_MODE>mode);

    emit_insn (gen_reduc_plus_scal_<mode> (tmp, vector));
    emit_insn (gen_add<scalar_mode>3 (dest, scalar, tmp));
     DONE;
   })

(define_insn "*<reduc_op>_dpp_shr_<mode>"
  [(set (match_operand:V_1REG 0 "register_operand"   "=v")
	(unspec:V_1REG
	  [(match_operand:V_1REG 1 "register_operand" "v")
	   (match_operand:V_1REG 2 "register_operand" "v")
	   (match_operand:SI 3 "const_int_operand"        "n")]
	  REDUC_UNSPEC))]
  ; GCN3 requires a carry out, GCN5 not
  "!(TARGET_GCN3 && SCALAR_INT_MODE_P (<SCALAR_MODE>mode)
     && <reduc_unspec> == UNSPEC_PLUS_DPP_SHR)
   && !TARGET_RDNA2_PLUS"
  {
    return gcn_expand_dpp_shr_insn (<MODE>mode, "<reduc_insn>",
				    <reduc_unspec>, INTVAL (operands[3]));
  }
  [(set_attr "type" "vop_dpp")
   (set_attr "length" "8")])

(define_insn_and_split "*<reduc_op>_dpp_shr_<mode>"
  [(set (match_operand:V_DI 0 "register_operand"    "=v")
	(unspec:V_DI
	  [(match_operand:V_DI 1 "register_operand" "v")
	   (match_operand:V_DI 2 "register_operand" "v")
	   (match_operand:SI 3 "const_int_operand"  "n")]
	  REDUC_2REG_UNSPEC))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 4)
	(unspec:<VnSI>
	  [(match_dup 6) (match_dup 8) (match_dup 3)] REDUC_2REG_UNSPEC))
   (set (match_dup 5)
	(unspec:<VnSI>
	  [(match_dup 7) (match_dup 9) (match_dup 3)] REDUC_2REG_UNSPEC))]
  {
    operands[4] = gcn_operand_part (<MODE>mode, operands[0], 0);
    operands[5] = gcn_operand_part (<MODE>mode, operands[0], 1);
    operands[6] = gcn_operand_part (<MODE>mode, operands[1], 0);
    operands[7] = gcn_operand_part (<MODE>mode, operands[1], 1);
    operands[8] = gcn_operand_part (<MODE>mode, operands[2], 0);
    operands[9] = gcn_operand_part (<MODE>mode, operands[2], 1);
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "16")])

; Special cases for addition.

(define_insn "*plus_carry_dpp_shr_<mode>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"   "=v")
	(unspec:V_INT_1REG
	  [(match_operand:V_INT_1REG 1 "register_operand" "v")
	   (match_operand:V_INT_1REG 2 "register_operand" "v")
	   (match_operand:SI 3 "const_int_operand"	  "n")]
	  UNSPEC_PLUS_CARRY_DPP_SHR))
   (clobber (reg:DI VCC_REG))]
  "!TARGET_RDNA2_PLUS"
  {
    return gcn_expand_dpp_shr_insn (<VnSI>mode, "v_add%^_u32",
				    UNSPEC_PLUS_CARRY_DPP_SHR,
				    INTVAL (operands[3]));
  }
  [(set_attr "type" "vop_dpp")
   (set_attr "length" "8")])

(define_insn "*plus_carry_in_dpp_shr_<mode>"
  [(set (match_operand:V_SI 0 "register_operand"    "=v")
	(unspec:V_SI
	  [(match_operand:V_SI 1 "register_operand" "v")
	   (match_operand:V_SI 2 "register_operand" "v")
	   (match_operand:SI 3 "const_int_operand"  "n")
	   (match_operand:DI 4 "register_operand"   "cV")]
	  UNSPEC_PLUS_CARRY_IN_DPP_SHR))
   (clobber (reg:DI VCC_REG))]
  "!TARGET_RDNA2_PLUS"
  {
    return gcn_expand_dpp_shr_insn (<MODE>mode, "v_addc%^_u32",
				    UNSPEC_PLUS_CARRY_IN_DPP_SHR,
				    INTVAL (operands[3]));
  }
  [(set_attr "type" "vop_dpp")
   (set_attr "length" "8")])

(define_insn_and_split "*plus_carry_dpp_shr_<mode>"
  [(set (match_operand:V_DI 0 "register_operand"    "=v")
	(unspec:V_DI
	  [(match_operand:V_DI 1 "register_operand" "v")
	   (match_operand:V_DI 2 "register_operand" "v")
	   (match_operand:SI 3 "const_int_operand"  "n")]
	  UNSPEC_PLUS_CARRY_DPP_SHR))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 4)
		(unspec:<VnSI>
		  [(match_dup 6) (match_dup 8) (match_dup 3)]
		  UNSPEC_PLUS_CARRY_DPP_SHR))
	      (clobber (reg:DI VCC_REG))])
   (parallel [(set (match_dup 5)
		(unspec:<VnSI>
		  [(match_dup 7) (match_dup 9) (match_dup 3) (reg:DI VCC_REG)]
		  UNSPEC_PLUS_CARRY_IN_DPP_SHR))
	      (clobber (reg:DI VCC_REG))])]
  {
    operands[4] = gcn_operand_part (<MODE>mode, operands[0], 0);
    operands[5] = gcn_operand_part (<MODE>mode, operands[0], 1);
    operands[6] = gcn_operand_part (<MODE>mode, operands[1], 0);
    operands[7] = gcn_operand_part (<MODE>mode, operands[1], 1);
    operands[8] = gcn_operand_part (<MODE>mode, operands[2], 0);
    operands[9] = gcn_operand_part (<MODE>mode, operands[2], 1);
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "16")])

;; }}}
;; {{{ Miscellaneous

(define_expand "vec_series<mode>"
  [(match_operand:V_SI 0 "register_operand")
   (match_operand:SI 1 "gcn_alu_operand")
   (match_operand:SI 2 "gcn_alu_operand")]
  ""
  {
    rtx tmp = gen_reg_rtx (<MODE>mode);
    rtx v1 = gen_rtx_REG (<MODE>mode, VGPR_REGNO (1));

    emit_insn (gen_mul<mode>3_dup (tmp, v1, operands[2]));
    emit_insn (gen_add<mode>3_dup (operands[0], tmp, operands[1]));
    DONE;
  })

(define_expand "vec_series<mode>"
  [(match_operand:V_DI 0 "register_operand")
   (match_operand:DI 1 "gcn_alu_operand")
   (match_operand:DI 2 "gcn_alu_operand")]
  ""
  {
    rtx tmp = gen_reg_rtx (<MODE>mode);
    rtx v1 = gen_rtx_REG (<VnSI>mode, VGPR_REGNO (1));
    rtx op1vec = gen_reg_rtx (<MODE>mode);

    emit_insn (gen_mul<mode>3_zext_dup2 (tmp, v1, operands[2]));
    emit_insn (gen_vec_duplicate<mode> (op1vec, operands[1]));
    emit_insn (gen_add<mode>3 (operands[0], tmp, op1vec));
    DONE;
  })

;; }}}
