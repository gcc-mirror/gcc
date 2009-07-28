/* Testcase for http://gcc.gnu.org/ml/gcc-patches/2003-02/msg01017.html */
/* { dg-do link { target fpic } } */
/* { dg-options "-O -fpic" } */

int *g;

int main (void)
{
  switch (*g)
    {
    case 0:
      {
        switch (*g)
          {
          case 0: *g = 1; break;
          case 1:
          case 2: *g = 1; break;
          case 3:
          case 4: *g = 1; break;
          }
        break;
      }
    case 1:
      {
        switch (*g)
          {
          case 0: *g = 1; break;
          case 1:
          case 2: *g = 1; break;
          case 3:
          case 4: *g = 1; break;
          }
      }
    }
  return 0;
}
