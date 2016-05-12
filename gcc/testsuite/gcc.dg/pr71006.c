/* PR target/71006 */
/* { dg-do compile } */
/* { dg-options "-O1 -ftree-vectorize" } */

unsigned char uu, gu, e2;

void
fs (void)
{
  char *nq = (char *)&gu, *k4 = (char *)&gu;
  while (*k4 < 1)
    {
      uu += (*nq != 0 || e2 != 0);
      ++*k4;
    }
}
