/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=object-size -fno-sanitize-recover=object-size" } */

/* Test valid uses.  */

#define N 20

__attribute__((noinline, noclone)) void
f1 (int i)
{
  volatile int j;
  char *p, *orig;
  orig = p = (char *) __builtin_calloc (N, 1);
  j = *(p + i);
  j = p[i];
  p++;
  j = p[i - 1];
  j = *(p + i - 1);
  __builtin_free (orig);
}

__attribute__((noinline, noclone)) void
f2 (int i)
{
  volatile int j;
  char a[N];
  __builtin_memset (a, 0, N);
  j = *(a + i);
  char *p = a;
  j = *(p + i);
  j = p[i];
  p += 10;
  j = *(p + i - 10);
  j = p[i - 10];
}

__attribute__((noinline, noclone)) void
f3 (int i)
{
  volatile int j;
  int *p = (int *) __builtin_calloc (N, sizeof (*p));
  int *o = &p[i];
  j = *o;
  j = o[0];
  __builtin_free (p);
}

int
main ()
{
  f1 (N - 1);
  f2 (N - 1);
  f3 (N - 1);
  return 0;
}
