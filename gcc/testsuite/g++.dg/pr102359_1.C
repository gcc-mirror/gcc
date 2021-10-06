/* PR middle-end/102359 ICE gimplification failed since
   r12-3433-ga25e0b5e6ac8a77a.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero" } */
/* { dg-require-effective-target c++17 } */

struct A {
  double a = 111;
  auto foo() {
    return [*this] { return a; };
  }
};
int X = A{}.foo()();
