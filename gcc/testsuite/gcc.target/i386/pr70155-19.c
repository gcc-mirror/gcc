/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mtune=generic -dp" } */

extern char *src, *dst;

char *
foo1 (void)
{
  return __builtin_mempcpy (dst, src, 16);
}

/* { dg-final { scan-assembler-times "movv1ti_internal" 2 } } */
