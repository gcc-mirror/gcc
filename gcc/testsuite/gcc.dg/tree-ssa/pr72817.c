/* { dg-do run } */
/* { dg-options "-O3" } */

signed char a;
short b;

int main ()
{
  for (a = 3; a != -1; a -= 5)
    while (b)
      ;
  return 0;
}
