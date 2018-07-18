/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-sources "abitest.S" } */

extern long long abidi (int a, ...);

int main (void)
{
  long long a = 1;
  a = abidi (10, a);

  if (a != 2)
    return 1;
  return 0;
}
