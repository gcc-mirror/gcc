/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-Wno-stringop-overflow -fsanitize=undefined" } */

/* Test PARM_DECLs and RESULT_DECLs.  */

struct T { char d[8]; int e; };
struct T t = { "abcdefg", 1 };
#ifdef __cplusplus
struct C { C () : d("abcdefg"), e(1) {} C (const C &x) { __builtin_memcpy (d, x.d, 8); e = x.e; } ~C () {} char d[8]; int e; };
#endif
struct U { int a : 5; int b : 19; int c : 8; };
struct S { struct U d[10]; };
struct S s __attribute__ ((aligned(4096)));

int
f1 (struct T x, int i)
{
  char *p = x.d;
  p += i;
  return *p;
}

/* { dg-output "load of address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */

#ifdef __cplusplus
static struct C
f2 (int i)
{
  struct C x;
  x.d[i] = 'z';
  return x;
}

/* { dg-output "\[^\n\r]*index 12 out of bounds for type 'char \\\[8\\\]'\[^\n\r]*(\n|\r\n|\r)" { target { c++ } } } */
/* { dg-output "\[^\n\r]*store to address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" { target { c++ } } } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" { target { c++ } } } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" { target { c++ } } } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" { target { c++ } } } */

static struct C
f3 (int i)
{
  struct C x;
  char *p = x.d;
  p += i;
  *p = 'z';
  return x;
}

/* { dg-output "\[^\n\r]*store to address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" { target { c++ } } } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" { target { c++ } } } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" { target { c++ } } } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" { target { c++ } } } */

#endif

int
f4 (int i)
{
  return s.d[i].b;
}

/* { dg-output "\[^\n\r]*index 12 out of bounds for type 'U \\\[10\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */

int
f5 (int i)
{
  struct U *u = s.d;
  u += i;
  return u->b;
}

/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^" } */

int
main (void)
{
  f1 (t, 12);
#ifdef __cplusplus
  f2 (12);
  f3 (12);
#endif
  f4 (12);
  f5 (12);
#ifdef __cplusplus
  /* Stack may be smashed by f2/f3 above.  */
  __builtin_exit (0);
#endif
  return 0;
}
