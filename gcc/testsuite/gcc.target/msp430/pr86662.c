/* PR/86662 */

/* { dg-do link } */
/* -nostdlib prevents link errors due to mismatched code models for
   libgloss objects.  */
/* { dg-options "-mlarge -flto -nostdlib" } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=msp430" } } */

int main(void)
{
  __int20 n = 5;
  return 0;
}
