/* Test dead code strip support.  */
/* Contributed by Devang Patel  <dpatel@apple.com>  */

/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-g -fno-eliminate-unused-debug-symbols" } */

int
main ()
{
  return 0;
}

/* { dg-final { scan-assembler ".stabd.46,0,0" } } */
/* { dg-final { scan-assembler ".stabd.78,0,0" } } */

