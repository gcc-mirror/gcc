/* Make sure that the 3 types of warnings generated from tree-ssa-uninit.cc have
   proper virtual locations and so can be controlled by pragmas when they appear
   in macros.  */

/* { dg-do compile } */
/* { dg-options "-Wuninitialized -Wmaybe-uninitialized" } */

/* 1.  Check maybe_warn_read_write_only().  */
#define DEREF1(p) (*p) /* { dg-warning {may be used uninitialized} } */
__attribute__ ((access (write_only, 1)))
int f1 (int* x) /* { dg-note {accessing argument 1} } */
{
  return DEREF1 (x); /* { dg-note {in expansion of macro 'DEREF1'} } */
}

#define DEREF2(p) (*p) /* { dg-bogus {may be used uninitialized} } */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
__attribute__ ((access (write_only, 1)))
int f2 (int* x) /* { dg-bogus {accessing argument 1} } */
{
  return DEREF2 (x); /* { dg-bogus {in expansion of macro 'DEREF1'} } */
}
#pragma GCC diagnostic pop

/* 2.  Check warn_uninit().  */
int g;
#define SET3(a, b) ((a) = (b)) /* { dg-warning {'x' is used uninitialized} } */
void f3 ()
{
  int x; /* { dg-note {'x' was declared here} } */
  SET3 (g, x); /* { dg-note {in expansion of macro 'SET3'} } */
}

#define SET4(a, b) ((a) = (b)) /* { dg-bogus {'x' is used uninitialized} } */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wuninitialized"
void f4 ()
{
  int x; /* { dg-bogus {'x' was declared here} } */
  SET4 (g, x); /* { dg-bogus {in expansion of macro 'SET3'} } */
}
#pragma GCC diagnostic pop

/* 3.  Check maybe_warn_operand().  */
#define CALL5(func, arg) ((func) (arg)) /* { dg-warning {'c' may be used uninitialized} } */
void f5a (const char *); /* { dg-note {by argument 1} } */
void f5b ()
{
  char c; /* { dg-note {'c' declared here} } */
  CALL5 (f5a, &c); /* { dg-note {in expansion of macro 'CALL5'} } */
}

#define CALL6(func, arg) ((func) (arg)) /* { dg-bogus {'c' may be used uninitialized} } */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
void f6a (const char *); /* { dg-bogus {by argument 1} } */
void f6b ()
{
  char c; /* { dg-bogus {'c' declared here} } */
  CALL6 (f6a, &c); /* { dg-bogus {in expansion of macro 'CALL6'} } */
}
#pragma GCC diagnostic pop
