/* ??? At the moment, lower-subreg.c decomposes the copy of the multiplication
   result to $2, which prevents the register allocators from storing the
   multiplication result in $2.  */
/* { dg-options "-mips3 -mfix-r4000 -mgp64 -EL -fno-split-wide-types -dp" } */
/* { dg-skip-if "naming registers makes this a code quality test" { *-*-* } { "-O0" } { "" } } */
typedef unsigned long long uint64_t;
typedef unsigned int uint128_t __attribute__((mode(TI)));
NOMIPS16 uint128_t foo (uint64_t x, uint64_t y) { return (uint128_t) x * y; }
/* { dg-final { scan-assembler "[concat {\tdmultu\t\$[45],\$[45][^\n]+umulditi3_r4000\n\tmflo\t\$2\n\tmfhi\t\$3\n}]" } } */
