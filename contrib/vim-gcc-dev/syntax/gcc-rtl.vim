" Syntax highlighting rules for RTL dump files (for Vim).
"
" Copyright (C) 2018-2025 Free Software Foundation, Inc.
"
" This script is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 3, or (at your option)
" any later version


" Do not continue, if syntax is already enabled in current buffer.
if exists("b:current_syntax")
    finish
endif

" General-purpose comments.
syn match   rtlComment          ";;.*$"

syn keyword rtlInstruction      debug_expr insn_list int_list sequence
                                \ address debug_insn insn expr_list
                                \ jump_table_data barrier code_label
                                \ cond_exec parallel asm_input asm_operands
                                \ unspec unspec_volatile addr_vec
                                \ addr_diff_vec prefetch set use clobber
                                \ call return simple_return eh_return
                                \ trap_if scratch strict_low_part concat concatn
                                \ mem label_ref symbol_ref cc0 compare plus minus
                                \ neg mult ss_mult us_mult div ss_div us_div mod
                                \ udiv umod and ior xor not ashift rotate ashiftrt
                                \ lshiftrt rotatert smin smax umin umax pre_dec
                                \ pre_inc post_dec post_inc pre_modify post_modify
                                \ unordered ordered uneq unge ungt unle unlt ltgt sign_extend
                                \ zero_extend truncate float_extend float_truncate
                                \ float fix unsigned_float unsigned_fix fract_convert
                                \ unsigned_fract_convert sat_fract unsigned_sat_fract
                                \ abs sqrt bswap ffs clrsb clz ctz popcount parity
                                \ sign_extract zero_extract high lo_sum vec_merge
                                \ vec_select vec_concat vec_duplicate vec_series ss_plus
                                \ us_plus ss_minus ss_neg us_neg ss_abs ss_ashift
                                \ us_ashift us_minus ss_truncate us_truncate fma
                                \ entry_value exclusion_set presence_set final_presence_set
                                \ absence_set final_absence_set automata_option attr set_attr
                                \ set_attr_alternative eq_attr eq_attr_alt attr_flag cond
syn keyword rtlConditional      call_insn jump_insn if_then_else
                                \ eq ne gt gtu lt ltu ge geu le leu
syn keyword rtlNote             note barrier code_label
syn keyword rtlVariableLoation  var_location
syn keyword rtlPcRegister       pc

syn keyword rtlModes		        VOID BLK BI QI HI SI DI TI SF DF CC QQ HQ SQ
                                \ DQ TQ UQQ UHQ USQ UDQ UTQ HA SA DA TA UHA
                                \ USA UDA UTA SD DD TD

" String literals
syn region  rtlString           start=/\v"/ skip=/\v\\./ end=/\v"/

syn match   rtlNoteInsn         "NOTE_INSN_[A-Z_]*"
syn match   rtlIntegerConstant  "\vconst_int -?\d+"
syn match   rtlFloatConstant    "\vconst_double:[A-Z]+ -?\d*\.\d+(e\+\d+)?"
syn match   rtlRegister         "\vreg(\/[a-z])?:[A-Z0-9]+ \d+ [a-z0-9]+"
syn match   rtlLocation         /\v"[^"]*":\d+/

hi def link rtlInstruction      Statement
hi def link rtlConditional      Conditional
hi def link rtlNote             Debug
hi def link rtlNoteInsn         Debug
hi def link rtlIntegerConstant  Number
hi def link rtlFloatConstant    Number
hi def link rtlRegister         Type
hi def link rtlPcRegister       Type
hi def link rtlModes            Type
hi def link rtlVariableLoation  Debug
hi def link rtlComment          Comment
hi def link rtlLocation         Debug
hi def link rtlString           String

let b:current_syntax = "gcc-rtl"
