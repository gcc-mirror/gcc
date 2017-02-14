/* Check that the stack pointer is decreased only once in a funtion with
   runtime aligned stack variables and -mwarn-dynamicstack does not generate a
   warning.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O2 -mwarn-dynamicstack" } */

extern int bar (char *pl);

int foo (long size)
{
  char __attribute__ ((aligned(16))) l = size;

  return bar (&l);
}

/* { dg-final { scan-assembler-times "%r15,-" 1 } } */
