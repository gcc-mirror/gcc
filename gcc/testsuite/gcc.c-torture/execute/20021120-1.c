/* Macros to emit "L Nxx R" for each octal number xx between 000 and 037.  */
#define OP1(L, N, R, I, J) L N##I##J R
#define OP2(L, N, R, I) \
    OP1(L, N, R, 0, I), OP1(L, N, R, 1, I), \
    OP1(L, N, R, 2, I), OP1(L, N, R, 3, I)
#define OP(L, N, R) \
    OP2(L, N, R, 0), OP2(L, N, R, 1), OP2(L, N, R, 2), OP2(L, N, R, 3), \
    OP2(L, N, R, 4), OP2(L, N, R, 5), OP2(L, N, R, 6), OP2(L, N, R, 7)

/* Declare 32 unique variables with prefix N.  */
#define DECLARE(N) OP (, N,)

/* Copy 32 variables with prefix N from the array at ADDR.
   Leave ADDR pointing to the end of the array.  */
#define COPYIN(N, ADDR) OP (, N, = *(ADDR++))

/* Likewise, but copy the other way.  */
#define COPYOUT(N, ADDR) OP (*(ADDR++) =, N,)

/* Add the contents of the array at ADDR to 32 variables with prefix N.
   Leave ADDR pointing to the end of the array.  */
#define ADD(N, ADDR) OP (, N, += *(ADDR++))

volatile double gd[32];
volatile float gf[32];

void foo (int n)
{
  double DECLARE(d);
  float DECLARE(f);
  volatile double *pd;
  volatile float *pf;
  int i;

  pd = gd; COPYIN (d, pd);
  for (i = 0; i < n; i++)
    {
      pf = gf; COPYIN (f, pf);
      pd = gd; ADD (d, pd);
      pd = gd; ADD (d, pd);
      pd = gd; ADD (d, pd);
      pf = gf; COPYOUT (f, pf);
    }
  pd = gd; COPYOUT (d, pd);
}

int main ()
{
  int i;

  for (i = 0; i < 32; i++)
    gd[i] = i, gf[i] = i;
  foo (1);
  for (i = 0; i < 32; i++)
    if (gd[i] != i * 4 || gf[i] != i)
      abort ();
  exit (0);
}
