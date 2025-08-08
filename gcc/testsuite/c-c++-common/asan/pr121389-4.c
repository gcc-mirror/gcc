// PR middle-end/121389
// { dg-do compile { target musttail } }
// { dg-options "-fsanitize=address -fdisable-tree-switchlower_O0" }
// { dg-skip-if "" { *-*-* } { "*" } { "-O0" } }

#include "pr121389-3.c"
