/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2 -fPIC" } */
typedef unsigned int size_t;
 char *
__mempcpy_by2 (char *__dest, __const char *__src, size_t __srclen)
{
  register char *__tmp = __dest;
  register unsigned long int __d0, __d1;
  __asm__ __volatile__
    (
     "shrl      $1,%3\n\t"
     "jz        2f\n"
     "1:\n\t"
     "movl      (%2),%0\n\t"
     "leal      4(%2),%2\n\t"
     "movl      %0,(%1)\n\t"
     "leal      4(%1),%1\n\t"
     "decl      %3\n\t"
     "jnz       1b\n"
     "2:\n\t"
     "movw      (%2),%w0\n\t"
     "movw      %w0,(%1)"
     : "=&q" (__d0), "=r" (__tmp), "=&r" (__src), "=&r" (__d1),
       "=m" ( *(struct { __extension__ char __x[__srclen]; } *)__dest)
     : "1" (__tmp), "2" (__src), "3" (__srclen / 2),
       "m" ( *(struct { __extension__ char __x[__srclen]; } *)__src)
     : "cc");
  return __tmp + 2;
}
