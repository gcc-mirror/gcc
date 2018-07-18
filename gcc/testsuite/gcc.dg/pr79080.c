/* PR target/79080 */
/* { dg-do compile } */
/* { dg-options "-O1" } */
/* { dg-additional-options "-mcpu=8548" { target { powerpc*-*-* && ilp32 } } } */

int
foo (char x)
{
  int a;

  for (;;)
    {
      x += 59;
      if (x != 0)
        a = 0;
      else
        return 0;
    }
}
