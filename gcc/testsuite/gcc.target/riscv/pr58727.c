/* { dg-do compile } */
/* { dg-options "-march=rv64gcb -mabi=lp64d" { target { rv64} } } */
/* { dg-options "-march=rv32gcb -mabi=ilp32" { target { rv32} } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

enum masks { CLEAR = 0x400000, SET = 0x02 };
unsigned clear_set(unsigned a) { return (a & ~CLEAR) | SET; }
unsigned set_clear(unsigned a) { return (a | SET) & ~CLEAR; }
unsigned clear(unsigned a) { return a & ~CLEAR; }
unsigned set(unsigned a) { return a | SET; }
__attribute__((flatten)) unsigned clear_set_inline(unsigned a) { return set(clear(a)); }
__attribute__((flatten)) unsigned set_clear_inline(unsigned a) { return clear(set(a)); }

/* { dg-final { scan-assembler-not "\\sand\\s" } } */
/* { dg-final { scan-assembler-not "\\sandi\\s" } } */

