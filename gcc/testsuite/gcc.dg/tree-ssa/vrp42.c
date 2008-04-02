/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

int foo(int val)
{
  int tmp;
  if ((val > 5) && (val < 8))
    {
      switch (val)
        {
        case 6:
          tmp = 1;
          break;
        case 7:
          tmp = 2;
          break;
        }
      return tmp; /* { dg-bogus "used uninitialized" } */
    }
  return 0;
}

