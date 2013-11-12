/* Test for _Thread_local in C11.  Test of invalid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

_Thread_local void f (void); /* { dg-error "storage class" } */
_Thread_local void g (void) {} /* { dg-error "_Thread_local" } */
typedef _Thread_local int t1; /* { dg-error "_Thread_local" } */
_Thread_local typedef int t2; /* { dg-error "_Thread_local" } */

void
h (void)
{
  _Thread_local auto int a; /* { dg-error "_Thread_local" } */
  _Thread_local register int b; /* { dg-error "_Thread_local" } */
  auto _Thread_local int c; /* { dg-error "_Thread_local" } */
  register _Thread_local int d; /* { dg-error "_Thread_local" } */
  _Thread_local int e; /* { dg-error "_Thread_local" } */
}

_Thread_local int v; /* { dg-message "previous" } */
extern int v; /* { dg-error "thread" } */
int w; /* { dg-message "previous" } */
extern _Thread_local int w; /* { dg-error "thread" } */

_Thread_local int x; /* { dg-message "previous" } */
int y; /* { dg-message "previous" } */

int vv;

void
i (void)
{
  extern int x; /* { dg-error "thread" } */
  extern _Thread_local int y; /* { dg-error "thread" } */
  static _Thread_local int a[vv]; /* { dg-error "storage size" } */
  static _Thread_local int vi = vv; /* { dg-error "not constant" } */
}

static _Thread_local int sv;

inline void
j (void)
{
  static _Thread_local int vj; /* { dg-error "static but declared" } */
  (void) sv; /* { dg-error "static but used in inline" } */
}
