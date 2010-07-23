/* Verify that X86-64 only SSE registers aren't restored on IA-32.  */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -msse" } */
/* { dg-final { scan-assembler-not "xmm8" } } */

extern void abort (void);
extern void exit (int);

void *bar (void *p, void *q)
{
  if (p != (void *) 26 || q != (void *) 35)
    abort ();
  return (void *) 76;
}

void *foo (void **args)
{
  void *argcookie = &args[1];

  __builtin_return (__builtin_apply (args[0], &argcookie,
				     2 * sizeof (void *)));
}

int main (void)
{
  void *args[3];

  args[0] = (void *) bar;
  args[1] = (void *) 26;
  args[2] = (void *) 35;
  if (foo (args) != (void *) 76)
    abort ();
  exit (0);
}
