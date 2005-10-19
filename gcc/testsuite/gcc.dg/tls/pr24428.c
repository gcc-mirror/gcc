/* { dg-do run } */
/* { dg-options "-O2" } */

__thread double thrtest[81];
int main ()
{
  int i;
  for (i = 0; i < 81; i++)
    thrtest[i] = 1.0;
  return 0;
}
