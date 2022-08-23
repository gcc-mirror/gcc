/* { dg-do compile { target { aarch64_little_endian && aarch64_asm_sve_ok } } } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=128 --save-temps" } */

#include "struct_vect_14.c"

/* { dg-final { scan-assembler-times {\tld2\t{v[0-9]+\.16b - v[0-9]+\.16b}, \[x[0-9]+\], #?32\n} 1 } } */
/* { dg-final { scan-assembler-times {\tld3\t{v[0-9]+\.16b - v[0-9]+\.16b}, \[x[0-9]+\], #?48\n} 1 } } */
/* { dg-final { scan-assembler-times {\tld4\t{v[0-9]+\.16b - v[0-9]+\.16b}, \[x[0-9]+\], #?64\n} 1 } } */
/* { dg-final { scan-assembler-times {\tst2\t{v[0-9]+\.16b - v[0-9]+\.16b}, \[x[0-9]+\], #?32\n} 1 } } */
/* { dg-final { scan-assembler-times {\tst3\t{v[0-9]+\.16b - v[0-9]+\.16b}, \[x[0-9]+\], #?48\n} 1 } } */
/* { dg-final { scan-assembler-times {\tst4\t{v[0-9]+\.16b - v[0-9]+\.16b}, \[x[0-9]+\], #?64\n} 1 } } */

/* { dg-final { scan-assembler-times {\tld2\t{v[0-9]+\.8h - v[0-9]+\.8h}, \[x[0-9]+\], #?32\n} 2 } } */
/* { dg-final { scan-assembler-times {\tld3\t{v[0-9]+\.8h - v[0-9]+\.8h}, \[x[0-9]+\], #?48\n} 2 } } */
/* { dg-final { scan-assembler-times {\tld4\t{v[0-9]+\.8h - v[0-9]+\.8h}, \[x[0-9]+\], #?64\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst2\t{v[0-9]+\.8h - v[0-9]+\.8h}, \[x[0-9]+\], #?32\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst3\t{v[0-9]+\.8h - v[0-9]+\.8h}, \[x[0-9]+\], #?48\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst4\t{v[0-9]+\.8h - v[0-9]+\.8h}, \[x[0-9]+\], #?64\n} 2 } } */

/* { dg-final { scan-assembler-times {\tld2\t{v[0-9]+\.4s - v[0-9]+\.4s}, \[x[0-9]+\], #?32\n} 2 } } */
/* { dg-final { scan-assembler-times {\tld3\t{v[0-9]+\.4s - v[0-9]+\.4s}, \[x[0-9]+\], #?48\n} 2 } } */
/* { dg-final { scan-assembler-times {\tld4\t{v[0-9]+\.4s - v[0-9]+\.4s}, \[x[0-9]+\], #?64\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst2\t{v[0-9]+\.4s - v[0-9]+\.4s}, \[x[0-9]+\], #?32\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst3\t{v[0-9]+\.4s - v[0-9]+\.4s}, \[x[0-9]+\], #?48\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst4\t{v[0-9]+\.4s - v[0-9]+\.4s}, \[x[0-9]+\], #?64\n} 2 } } */

/* { dg-final { scan-assembler-times {\tld2\t{v[0-9]+\.2d - v[0-9]+\.2d}, \[x[0-9]+\], #?32\n} 2 } } */
/* { dg-final { scan-assembler-times {\tld3\t{v[0-9]+\.2d - v[0-9]+\.2d}, \[x[0-9]+\], #?48\n} 2 } } */
/* { dg-final { scan-assembler-times {\tld4\t{v[0-9]+\.2d - v[0-9]+\.2d}, \[x[0-9]+\], #?64\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst2\t{v[0-9]+\.2d - v[0-9]+\.2d}, \[x[0-9]+\], #?32\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst3\t{v[0-9]+\.2d - v[0-9]+\.2d}, \[x[0-9]+\], #?48\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst4\t{v[0-9]+\.2d - v[0-9]+\.2d}, \[x[0-9]+\], #?64\n} 2 } } */
