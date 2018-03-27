/* ??? At the moment, lower-subreg.c decomposes the copy of the multiplication
   result to $2, which prevents the register allocators from storing the
   multiplication result in $2.  */
/* This test requires widening_mul */
/* { dg-options "-mips3 -mfix-r4000 -mgp64 -fno-split-wide-types -dp -EL -fexpensive-optimizations" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
typedef long long int64_t;
typedef int int128_t __attribute__((mode(TI)));
NOMIPS16 int128_t foo (int64_t x, int64_t y) { return (int128_t) x * y; }
/* { dg-final { scan-assembler "[concat {\tdmult\t\$[45],\$[45][^\n]+mulditi3_r4000\n\tmflo\t\$2\n\tmfhi\t\$3\n}]" } } */
