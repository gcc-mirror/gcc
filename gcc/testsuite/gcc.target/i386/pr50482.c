/* { dg-do compile } */
/* { dg-options "-O3 -msse4" } */

void
test (int code, unsigned int * image, int * colors)
{
  int i;

  for (i = 0; i < code; ++i)
    image[i] = (colors[i] < 0 ? ~(unsigned int) 0 : colors[i]);
}
