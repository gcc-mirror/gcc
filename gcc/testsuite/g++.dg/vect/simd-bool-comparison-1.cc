// { dg-do compile }
// { dg-additional-options "-mavx512bw -mavx512dq" { target { i?86-*-* x86_64-*-* } } }

#define N 1024

double a[N];
bool b[N];
bool c;

void test ()
{
  int i;

  for (i = 0; i < N; i++)
    if (b[i] != c)
      a[i] = 0.0;
    else
      a[i] = 1.0;
}

// { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { i?86-*-* x86_64-*-* } } } }
