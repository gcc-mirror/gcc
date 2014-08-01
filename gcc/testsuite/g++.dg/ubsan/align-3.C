// Limit this to known non-strict alignment targets.
// { dg-do run { target { i?86-*-linux* x86_64-*-linux* } } }
// { dg-options "-fsanitize=alignment -Wall -Wno-unused-variable -std=c++11" }

#include <new>

struct U
{
  int a;
  void foo () {}
};
struct V
{
  V () : a (0) {};
  ~V () { a = 0; };
  int a;
  void foo () {}
  static void bar () {}
};
struct S { long int l; char buf[1 + sizeof (U) + 2 * sizeof (V)]; } s;

int
main (void)
{
  U *p = (U *) &s.buf[1];
  p->foo ();
  char *q = &s.buf[1 + sizeof (U)];
  V *u = new (q) V;
  u->a = 1;
  u->~V ();
  V *v = new (&s.buf[1 + sizeof (U) + sizeof (V)]) V;
  v->foo ();
  v->bar ();	// We don't instrument this right now.
  v->~V ();
}

// { dg-output "\.C:26:\[0-9]*:\[\^\n\r]*member call on misaligned address 0x\[0-9a-fA-F]* for type 'struct U', which requires 4 byte alignment.*" }
// { dg-output "\.C:28:\[0-9]*:\[\^\n\r]*constructor call on misaligned address 0x\[0-9a-fA-F]* for type 'struct V', which requires 4 byte alignment.*" }
// { dg-output "\.C:14:\[0-9]*:\[\^\n\r]*member access within misaligned address 0x\[0-9a-fA-F]* for type 'struct V', which requires 4 byte alignment.*" }
// { dg-output "\.C:29:\[0-9]*:\[\^\n\r]*member access within misaligned address 0x\[0-9a-fA-F]* for type 'struct V', which requires 4 byte alignment.*" }
// { dg-output "\.C:30:\[0-9]*:\[\^\n\r]*member call on misaligned address 0x\[0-9a-fA-F]* for type 'struct V', which requires 4 byte alignment.*" }
// { dg-output "\.C:15:\[0-9]*:\[\^\n\r]*member access within misaligned address 0x\[0-9a-fA-F]* for type 'struct V', which requires 4 byte alignment.*" }
// { dg-output "\.C:31:\[0-9]*:\[\^\n\r]*constructor call on misaligned address 0x\[0-9a-fA-F]* for type 'struct V', which requires 4 byte alignment.*" }
// { dg-output "\.C:32:\[0-9]*:\[\^\n\r]*member call on misaligned address 0x\[0-9a-fA-F]* for type 'struct V', which requires 4 byte alignment.*" }
// { dg-output "\.C:34:\[0-9]*:\[\^\n\r]*member call on misaligned address 0x\[0-9a-fA-F]* for type 'struct V', which requires 4 byte alignment" }
