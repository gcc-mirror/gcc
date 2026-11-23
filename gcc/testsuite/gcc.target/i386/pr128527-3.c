/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */

/* RAX holds a stack address and is then loaded with a pointer argument
   passed on stack.  The load must not be treated as a stack access.  */

struct __attribute__ ((aligned (64))) S { long a; };
int a[128];

void
f (long a1, long a2, long a3, long a4, long a5, long a6, struct S *p)
{
  char *q = (char *) __builtin_stack_address () - 64;
  __asm__ ("" : : "a" (q));
  __asm__ ("" : "+a" (p));
  p->a = 0;
  for (int i = 0; i < 64; i++)
    a[i] = i;
}

/* { dg-final { scan-assembler-not "and\[lq\]?\[^\\n\]*-64,\[^\\n\]*sp" } } */
