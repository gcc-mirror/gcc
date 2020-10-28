// { dg-do compile }
// { dg-options "-O -g -dA -gno-strict-dwarf" }
// { dg-additional-options "-fno-common" { target hppa*-*-hpux* } }
// { dg-final { scan-assembler-times " DW_AT_alignment" 1 } }

struct tt {
  int i;
} __attribute__((__aligned__(64))) t;
