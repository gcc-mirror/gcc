/* This testcase is extracted from s390_emit_prologue.  The negation
   of a 64bit value got split incorrectly on 31 bit.  */

/* { dg-do run { target { ! lp64 } } } */
/* { dg-options "-O2 -mesa" } */

extern void abort (void);

long long frame_size = 42;

int __attribute__((noinline))
foo  (int a __attribute__((unused)), long long b)
{
  return (int)b;
}

int
main ()
{
    if (frame_size > 0)
    {
      if (foo (0, -frame_size) != -42)
	abort ();
    }
    return 0;
}
