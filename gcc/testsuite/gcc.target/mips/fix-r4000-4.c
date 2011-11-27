/* ??? At the moment, lower-subreg.c decomposes the copy of the multiplication
   result to $2, which prevents the register allocators from storing the
   multiplication result in $2.  */
/* { dg-options "-mips1 -mfix-r4000 -O2 -fno-split-wide-types -dp -EL" } */
typedef int int32_t;
typedef long long int64_t;
NOMIPS16 int64_t foo (int32_t x, int32_t y) { return (int64_t) x * y; }
/* { dg-final { scan-assembler "[concat {\tmult\t\$[45],\$[45][^\n]+mulsidi3_32bit_r4000[^\n]+\n\tmflo\t\$2\n\tmfhi\t\$3\n}]" } } */
