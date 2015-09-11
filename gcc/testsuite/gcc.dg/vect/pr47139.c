/* { dg-do compile } */

int d;

void
foo ()
{
  int a = 0;
  unsigned char b;
  for (b = 1; b; b++)
    {
      d = a;
      a |= b;
    }
}


