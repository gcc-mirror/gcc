/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-reload-details" } */

#define R1(X, Y) X (Y##0) X (Y##1) X (Y##2) X (Y##3) \
                 X (Y##4) X (Y##5) X (Y##6) X (Y##7)
#define R2(X) R1 (X, 0) R1 (X, 1) R1 (X, 2) R1 (X, 3)

#define DEFINE(N) extern int x##N;
R2 (DEFINE)

void b1 (int *);

void
foo (int n)
{
  for (int i = 0; i < n; ++i)
    {
#define CALL(N) b1 (&x##N);
      R2 (CALL);
      R2 (CALL);
    }
#define INC(N) x##N += 1;
  R2 (INC);
}

/* { dg-final { scan-rtl-dump-not "DI 32 v0" "reload" } } */
