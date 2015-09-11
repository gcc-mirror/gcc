/* { dg-do compile } */


int f(int **__restrict a, int ** __restrict b)
{
  int i;
  for(i= 0;i<32;i++)
    a[i] = b[i] + 1;
}


