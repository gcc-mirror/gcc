" Vim syntax highlighting rules for GCC match-and-simplify language.
"
" Copyright (C) 2018-2023 Free Software Foundation, Inc.
"
" This script is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 3, or (at your option)
" any later version

if exists("b:current_syntax")
    finish
endif

" Some keywords have a question mark, e.g. 'convert?'
setl isk=@,48-57,_,?

syn keyword pdTodo contained TODO FIXME XXX

syn keyword pdCtrl match simplify
syn keyword pdCtrl define_predicates define_operator_list
syn keyword pdCtrl if switch for with

syn keyword pdType type

syn keyword pdOp view_convert view_convert?
               \ convert convert? convert1 convert2 convert1? convert2?
               \ realpart imagpart
               \ cond vec_cond vec_perm
               \ pointer_plus pointer_diff
               \ plus minus mult mult_highpart
               \ trunc_div ceil_div floor_div round_div
               \ trunc_mod ceil_mod floor_mod round_mod
               \ rdiv exact_div
               \ fix_trunc float negate min max abs absu
               \ lshift rshift lrotate rrotate
               \ bit_ior bit_xor bit_and bit_not
               \ truth_andif truth_orif truth_and
               \ truth_or truth_xor truth_not
               \ lt le gt ge eq ne unordered ordered
               \ unlt unle ungt unge uneq ltgt
               \ addr_space_convert fixed_convert
               \ bit_insert complex conj
               \ reduc_max reduc_min reduc_plus
               \ dot_prod widen_sum sad fma
               \ widen_mult widen_mult_plus widen_mult_minus widen_lshift
               \ vec_widen_mult_hi vec_widen_mult_lo
               \ vec_widen_mult_even vec_widen_mult_odd
               \ vec_unpack_hi vec_unpack_lo
               \ vec_unpack_float_hi vec_unpack_float_lo
               \ vec_pack_trunc vec_pack_sat vec_pack_fix_trunc
               \ vec_widen_lshift_hi vec_widen_lshift_lo

" Match commutative/single-use specifiers: :C, :c, :s, :cs, etc.
syn match pdOpSpec  ":[CcSs]\+\>"

syn match pdCapture "@@\?[a-zA-Z0-9_]\+"

syn region pdComment start="/\*" end="\*/" contains=pdTodo

syn region pdPreProc start="^\s*#" skip="\\$" end="$" keepend

hi def link pdCtrl    Statement
hi def link pdType    Identifier
hi def link pdOp      Constant
hi def link pdOpSpec  Operator
hi def link pdCapture Special
hi def link pdComment Comment
hi def link pdTodo    Todo
hi def link pdPreProc PreProc

let b:current_syntax = "gcc-match"
