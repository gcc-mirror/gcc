/* PR target/98063 */
/* { dg-do run { target *-*-linux* } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fpic -mcmodel=large -fno-plt -save-temps" } */
/* { dg-final { scan-assembler-not "puts@GOTPCREL" } } */

int
main ()
{
  __builtin_puts ("Hello, world!");
  return 0;
}
