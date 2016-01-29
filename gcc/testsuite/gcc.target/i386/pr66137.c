/* PR target/66137 */
/* { dg-do compile } */
/* { dg-options "-mavx -O3 -ffixed-ebp" } */

void
foo (char *x, char *y, char *z, int a)
{
  int i;
  for (i = a; i > 0; i--)
    *x++ = *y++ = *z++;
}
