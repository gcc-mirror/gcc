/* { dg-do compile } */
/* { dg-options "-O0 -mavx -mvzeroupper -dp" } */
/* { dg-additional-options "-mabi=sysv" { target x86_64-*-mingw* } } */

typedef float __m256 __attribute__ ((__vector_size__ (32), __may_alias__));

extern void bar2 (__m256);
extern __m256 y;

void
foo ()
{
  bar2 (y);
}

/* { dg-final { scan-assembler-not "avx_vzeroupper" } } */
