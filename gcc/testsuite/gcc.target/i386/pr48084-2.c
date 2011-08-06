/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef int __m64 __attribute__ ((__vector_size__ (8), __may_alias__));
typedef char __v8qi __attribute__ ((__vector_size__ (8)));
void
_mm_maskmove_si64 (__m64 __A, __m64 __N, char *__P)
{
    __builtin_ia32_maskmovq ((__v8qi)__A, (__v8qi)__N, __P);
}
