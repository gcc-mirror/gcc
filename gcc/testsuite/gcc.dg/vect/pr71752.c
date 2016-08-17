/* { dg-do compile } */

unsigned int q4, yg;

unsigned int
w6 (unsigned int z5, unsigned int jv)
{
  unsigned int *f2 = &jv;

  while (*f2 < 21)
    {
      q4 -= jv;
      z5 -= jv;
      f2 = &yg;
      ++(*f2);
    }
  return z5;
}

