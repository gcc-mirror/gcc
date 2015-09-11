" Syntax highlighting rules for GIMPLE dump files (for Vim).
"
" Copyright (C) 2015 Free Software Foundation, Inc.
"
" This script is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 3, or (at your option)
" any later version
"
" This Vim script highlights syntax in debug dumps containing GIMPLE
" intermediate representation.  Such dumps are produced by GCC when
" it is invoked with -fdump-tree-* and/or -fdump-ipa-* switches.  Tested
" in Vim 7.4 (but should also work with earlier versions).
"
" INSTALLATION:
" 1. Copy the script into $HOME/.vim/syntax directory
" 2. Create a file gimple.vim in $HOME/.vim/ftdetect directory with the
"    following command in it:
"
" au BufRead,BufNewFile *.[0-2][0-9][0-9][ti].* set filetype=gimple
"
" The pattern in this autocommand corresponds to default file names
" of debug dumps, e.g.:
" filename.cc.123t.pass-name


" Do not continue, if syntax is already enabled in current buffer.
if exists("b:current_syntax")
    finish
endif

" If this variable is set to true, "Unknown tree" in -fdump-tree-original will
" be highlighted as an error.
let s:unknown_tree_is_error=0

" Comments for Phi nodes, value ranges, use/def-chains, etc.
syn match   gimpleAnnotation    "\v#.*$"
            \ contains=gimpleAnnotationOp, gimpleAnnotationMark,
            \ gimpleNumber, gimpleLineNo
syn match   gimpleAnnotationMark    "#" contained
syn keyword gimpleAnnotationOp    PHI VUSE VDEF RANGE PT USE CLB
            \ ALIGN MISALIGN NONZERO contained

" General-purpose comments.
syn match   gimpleComment       ";;.*$"

" Types - mostly borrowed from original Vim syntax file for C
syn keyword     gimpleType  int long short char void
            \ signed unsigned float double
            \ size_t ssize_t off_t wchar_t ptrdiff_t sig_atomic_t fpos_t
            \ clock_t time_t va_list jmp_buf FILE DIR div_t ldiv_t
            \ mbstate_t wctrans_t wint_t wctype_t
            \ _Bool bool _Complex complex _Imaginary imaginary
            \ int8_t int16_t int32_t int64_t
            \ uint8_t uint16_t uint32_t uint64_t
            \ int_least8_t int_least16_t int_least32_t int_least64_t
            \ uint_least8_t uint_least16_t uint_least32_t uint_least64_t
            \ int_fast8_t int_fast16_t int_fast32_t int_fast64_t
            \ uint_fast8_t uint_fast16_t uint_fast32_t uint_fast64_t
            \ intptr_t uintptr_t
            \ intmax_t uintmax_t
            \ __label__ __complex__ __volatile__
            \ char16_t char32_t sizetype __vtbl_ptr_type

" C/C++-like control structures
syn keyword gimpleStatement     goto return
syn keyword gimpleConditional   if else
syn keyword gimpleLoop          while
syn keyword gimpleException     try catch finally

" Special 'values'
syn match   gimpleConstant      "{CLOBBER}"
syn match   gimpleConstant      "{ref-all}"
syn match   gimpleConstant      "{v}"

" Blocks
syn region  gimpleBlock         start="{" end="}" transparent fold

" String literals
syn region  gimpleString        start=/\v"/ skip=/\v\\./ end=/\v"/

" GENERIC AST nodes
syn keyword gimpleASTNode       BIT_FIELD_REF TARGET_EXPR expr_stmt
            \ NON_LVALUE_EXPR
            \ must_not_throw_expr eh_spec_block eh_filter
            \ eh_must_not_throw aggr_init_expr cleanup_point

if s:unknown_tree_is_error
    syn match   gimpleUnknownTree   "\vUnknown tree: \w+"
end

" Numbers
syn match   gimpleNumber        "\v([^.a-zA-Z0-9_])\zs-?\d+B?"
syn match   gimpleFloat         "\v\W\zs-?\d*\.\d+(e\+\d+)?"

" Basic block label
" <bb 123>:
syn match   gimpleLabel         "\v^\s*\zs\<bb \d+\>"
" <D.1234>:
" <L1>:
syn match   gimpleLabel         "\v^\s*\zs\<[DL]\.?\d+\>"
" label: - user-defined label
" bb1L.1:
syn match   gimpleLabel         "\v^\s*[a-zA-Z0-9._]+\ze:\s*$"

" Match label after goto to suppress highlighting of numbers inside
syn match   gimpleGotoLabel     "\v<bb \d+\>[^:]"

" Line numbers, generated with -fdump-tree-*-lineno
syn match   gimpleLineNo        "\v\[[^\]]+:\d+:\d+\]"

" Misc C/C++-like keywords
syn keyword gimpleStructure     struct union enum typedef class
syn keyword gimpleStorageClass  static register auto volatile extern const
            \ template inline __attribute__ _Alignas alignas _Atomic
            \ _Thread_local thread_local _Alignof alignof sizeof

hi def link gimpleType          Type
hi def link gimpleNumber        Number
hi def link gimpleFloat         Float
hi def link gimpleConstant      Constant
hi def link gimpleStructure     Structure
hi def link gimpleStorageClass  StorageClass
hi def link gimpleOperator      Operator
hi def link gimpleASTNode       Operator
hi def link gimpleStatement     Statement
hi def link gimpleConditional   Conditional
hi def link gimpleLoop          Repeat
hi def link gimpleException     Exception
hi def link gimpleComment       Comment
hi def link gimpleLineNo        Comment
hi def link gimpleLabel         Label
hi def link gimpleAnnotationOp  Debug
hi def link gimpleAnnotationMark Debug
hi def link gimpleString        String
hi def link gimpleUnknownTree   Error

let b:current_syntax = "gimple"

