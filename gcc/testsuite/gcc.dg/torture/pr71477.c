/* { dg-do compile } */

#define N 6
int a;
void fn1()
{
  int k = 0;
  for (; k < N;)
    for (a = 0; a < N; k++)
      a = k + N;
}
