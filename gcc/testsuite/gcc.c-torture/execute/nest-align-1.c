/* Test for alignment handling when a variable is accessed by nested
   function.  */
/* Origin: Joey Ye <joey.ye@intel.com> */

/* Force bigger stack alignment for PowerPC EABI targets.  */
/* { dg-options "-mno-eabi" { target powerpc-*-eabi* } } */

#include <stddef.h>

typedef int aligned __attribute__((aligned));
extern void abort (void);

void
check (int *i)
{
  *i = 20;
  if ((((ptrdiff_t) i) & (__alignof__(aligned) - 1)) != 0)
    abort ();
}

void
foo (void)
{
  aligned jj;
  void bar ()
    {
      jj = -20;
    }
  jj = 0;
  bar ();
  if (jj != -20)
    abort ();
  check (&jj);
  if (jj != 20)
    abort ();
}

int
main()
{
  foo ();
  return 0;
}
