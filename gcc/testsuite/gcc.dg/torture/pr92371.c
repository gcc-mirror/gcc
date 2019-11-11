/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

int a, b;
void d()
{
  int c = sizeof(int);
  for (; a; a++)
    c *= sizeof(int);
  c *= sizeof(int);
  b = c;
}
