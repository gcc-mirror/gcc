/* Test for _Thread_local in C11.  Test of valid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

_Thread_local int a;
static _Thread_local long b;
extern _Thread_local int c, a;
_Thread_local static int d;
long _Thread_local extern b;
_Thread_local int extern a;
_Thread_local struct s; /* { dg-warning "useless" } */
_Thread_local int a = 1;
extern _Thread_local int c = 2; /* { dg-warning "initialized and" } */
void
f (void)
{
  static _Thread_local int x;
  extern _Thread_local long b;
  _Thread_local extern int a;
}

inline void
fi (void)
{
  static _Thread_local const int v;
  (void) a;
  static _Thread_local int (*const p)[a];
}
