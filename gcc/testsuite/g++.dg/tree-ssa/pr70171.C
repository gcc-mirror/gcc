/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct S { int i; };
S struct_ternary (S a, S b, bool select) { return select ? a : b; }

/* { dg-final { scan-tree-dump-not "&\[ab\]" "optimized" } } */
/* { dg-final { scan-assembler-not "\[er\]sp" { target { { i?86-*-* x86_64-*-* } && { ! ia32 } } } } } */
