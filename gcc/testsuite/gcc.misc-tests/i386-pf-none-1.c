/* Test that data prefetch instructions are not generated for i386 variants
   that do not support those instructions.  */

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target ilp32 } */

extern void exit (int);

char *msg = "howdy there";

void foo (char *p)
{
  __builtin_prefetch (p, 0, 0);
  __builtin_prefetch (p, 0, 1);
  __builtin_prefetch (p, 0, 2);
  __builtin_prefetch (p, 0, 3);
  __builtin_prefetch (p, 1, 0);
  __builtin_prefetch (p, 1, 1);
  __builtin_prefetch (p, 1, 2);
  __builtin_prefetch (p, 1, 3);
}

int main ()
{
  foo (msg);
  exit (0);
}

/* { dg-final { scan-assembler-not "fetch" } } */
