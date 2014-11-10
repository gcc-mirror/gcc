/* { dg-require-effective-target indirect_jumps } */

void foo (void *) __attribute__ ((noreturn));

void
bar (void *x)
{
  if (__builtin_setjmp (x))
    return;
  foo (x);
}
