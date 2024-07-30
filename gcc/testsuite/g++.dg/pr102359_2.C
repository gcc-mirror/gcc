/* PR middle-end/102359 ICE gimplification failed since
   r12-3433-ga25e0b5e6ac8a77a.  */
/* { dg-do run } */
/* { dg-options "-ftrivial-auto-var-init=zero" } */
/* { dg-require-effective-target c++17 } */

int main()
{
 int i = 42;
 auto l = [=]() mutable { return i; };
 if (l() != i)
   __builtin_abort ();
}
