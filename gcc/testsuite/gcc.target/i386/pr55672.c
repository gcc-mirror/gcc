/* { dg-do compile } */
/* { dg-options "-O -fstack-check=generic" } */

int main ()
{
  int x[8];
  if (x[0] != 4)
    __builtin_abort ();
  return 0;
}
