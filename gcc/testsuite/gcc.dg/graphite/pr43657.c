/* { dg-do run } */
/* { dg-options "-O1 -ftree-loop-linear" } */

extern void abort (void);

#define K 32

int cond_array[2*K][K];
int a[K][K];
int out[K];
int check_result[K] = {2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

__attribute__ ((noinline)) void
foo (int c)
{
  int res, i, j, k, next;

  for (k = 0; k < K; k++)
    {
      res = 0;
      for (j = 0; j < K; j++)
        for (i = 0; i < K; i++)
          {
            next = a[i][j];
            res = c > cond_array[i+k][j] ? next : res;
          }

      out[k] = res;
    }
}

int main ()
{
  int i, j, k;

  for  (j = 0; j < K; j++)
    {
      for (i = 0; i < 2*K; i++)
        cond_array[i][j] = i+j;

      for (i = 0; i < K; i++)
        a[i][j] = i+2;
    }

  foo(5);

  for (k = 0; k < K; k++)
    if (out[k] != check_result[k])
      abort ();

  return 0;
}
