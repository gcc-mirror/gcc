// { dg-do compile }
// { dg-options "-O -g -dA -gno-strict-dwarf" }
// { dg-final { scan-assembler-times " DW_AT_alignment" 1 } }

typedef int __attribute__((__aligned__(64))) i_t;
i_t i;
