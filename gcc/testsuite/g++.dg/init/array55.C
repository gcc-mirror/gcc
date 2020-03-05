/* PR c++/90938 - Initializing array with {1} works, but not {0}
   { dg-do compile { target c++11 } } */

struct A
{
  A () = delete;
  A (int) = delete;
};

A a_[] = { 0 };            // { dg-error "use of deleted function 'A::A\\\(int\\\)'" }

A a1[1] = { 0 };           // { dg-error "use of deleted function 'A::A\\\(int\\\)'" }


struct B
{
  B () = delete;
  B (int) = delete;
  B (long);
};

B b_[] = { 0 };            // { dg-error "use of deleted function 'B::B\\\(int\\\)'" }

B b1[1] = { 0 };           // { dg-error "use of deleted function 'B::B\\\(int\\\)'" }

B b2[] = { 0L };
B b3[1] = { 0L };
