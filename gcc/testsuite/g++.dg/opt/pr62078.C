/* PR rtl-optimization/62078 */
/* { dg-do compile } */
/* { dg-options "-Og -fdelete-dead-exceptions -fnon-call-exceptions" } */

struct A { virtual ~A (); };
struct B : A {};
struct C : B {};
struct D : C {};
struct E : D {};
struct F : E {};
struct G : F {};
struct H : G {};
struct I : H {};
struct J : I {};
struct K : J {};
struct L : K {};
struct M : L {};
struct N : M {};
struct O : N {};
struct P : O {};
struct Q : P {};
struct R : Q {};
struct S : R {};
struct T : S {};
struct U : T {};
struct V : U {};
struct W : V {};
struct X : W {};
struct Y : X {};
struct Z : Y {};

void
foo ()
{
  Z z;
}
