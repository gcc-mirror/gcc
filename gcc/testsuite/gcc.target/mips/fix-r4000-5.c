/* ??? At the moment, lower-subreg.c decomposes the copy of the multiplication
   result to $2, which prevents the register allocators from storing the
   multiplication result in $2.  */
/* { dg-options "-mips1 -mfix-r4000 -fno-split-wide-types -dp -EL" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;
NOMIPS16 uint64_t foo (uint32_t x, uint32_t y) { return (uint64_t) x * y; }
/* { dg-final { scan-assembler "[concat {\tmultu\t\$[45],\$[45][^\n]+umulsidi3_32bit_r4000\n\tmflo\t\$2\n\tmfhi\t\$3\n}]" } } */
