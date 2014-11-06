// Limit this to known non-strict alignment targets.
// { dg-do run { target { i?86-*-linux* x86_64-*-linux* } } }
// { dg-options "-fsanitize=null -Wall -Wno-unused-variable -std=c++11" }

#include <new>

struct U
{
  int a;
  void foo () {}
};
struct V
{
  V () {};
  ~V () {};
  int a;
  void foo () {}
  static void bar () {}
};
struct S { long int l; char buf[1 + sizeof (U) + 2 * sizeof (V)]; } s;

int
main (void)
{
  U *p = 0;
  p->foo ();
  char *q = 0;
  V *u = new (q) V;
  u->~V ();
  V *v = new (q) V;
  v->foo ();
  v->bar ();	// We don't instrument this right now.
  v->~V ();
}

// { dg-output "\.C:26:\[0-9]*:\[\^\n\r]*member call on null pointer of type 'struct U'.*" }
// { dg-output "\.C:29:\[0-9]*:\[\^\n\r]*member call on null pointer of type 'struct V'.*" }
// { dg-output "\.C:31:\[0-9]*:\[\^\n\r]*member call on null pointer of type 'struct V'.*" }
// { dg-output "\.C:33:\[0-9]*:\[\^\n\r]*member call on null pointer of type 'struct V'" }
