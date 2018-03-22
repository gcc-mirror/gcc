/* { dg-do compile } */
/* { dg-options "-O -fcf-protection -mcet" } */
/* { dg-final { scan-assembler-times "endbr32" 2 { target ia32 } } } */
/* { dg-final { scan-assembler-times "endbr64" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "call	_?setjmp" 1 } } */
/* { dg-final { scan-assembler-times "call	longjmp" 1 } } */

#include <stdio.h>
#include <setjmp.h>

jmp_buf buf;
static int bar (int);

__attribute__ ((noinline, noclone))
static int
foo (int i)
{
  int j = i * 11;

  if (!setjmp (buf))
    {
      j += 33;
      printf ("After setjmp: j = %d\n", j);
      bar (j);
    }

  return j + i;
}

__attribute__ ((noinline, noclone))
static int
bar (int i)
{
 int j = i;

  j -= 111;
  printf ("In longjmp: j = %d\n", j);
  longjmp (buf, 1);

  return j;
}

int
main ()
{
  foo (10);
  return 0;
}
