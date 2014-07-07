/* Check that fpchg is used to switch precision.  */

/* { dg-do compile } */
/* { dg-final { scan-assembler "fpchg" } } */
/* { dg-final { scan-assembler-not "fpscr" } } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m4a" } } */

extern float c;

void
foo(int j)
{
  while (j--)
    c++;

}

