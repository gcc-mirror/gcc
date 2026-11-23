/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */

struct __attribute__ ((aligned (64))) S { long a; };
int a[128];

void
foo (void)
{
  struct S *p = (struct S *) ((char *) __builtin_stack_address () - 128);
  p->a = 0;
  for (int i = 0; i < 64; i++)
    a[i] = i;
}
