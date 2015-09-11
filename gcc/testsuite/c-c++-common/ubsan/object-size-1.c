/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=undefined" } */

/* Sanity-test -fsanitize=object-size.  We use -fsanitize=undefined option
   to check that this feature doesn't clash with -fsanitize=bounds et al.  */

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

/* { dg-output "load of address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */

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

/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */

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

/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */

__attribute__((noinline, noclone)) void
f4 (void)
{
  /* The second argument to __builtin_calloc is intentional.  */
  int *p = (int *) __builtin_calloc (3, 1);
  *p = 42;
  __builtin_free (p);
}

/* { dg-output "\[^\n\r]*store to address \[^\n\r]* with insufficient space for an object of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^" } */

__attribute__((noinline, noclone)) void
f5 (int *p)
{
  /* This is not instrumented.  But don't ICE, etc.  */
  volatile int i = p[N];
}

int
main ()
{
  f1 (N);
  f2 (N);
  f3 (N);
  f4 ();
  int *p = (int *) __builtin_calloc (N, sizeof (*p));
  f5 (p);
  __builtin_free (p);
  return 0;
}
