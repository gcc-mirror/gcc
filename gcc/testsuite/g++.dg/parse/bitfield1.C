// PR c++/27803

struct A
{
  double i : 8; // { dg-error "10:bit-field .i. with non-integral type .double." }
};

void foo(A& a)
{
  (char)a.i;    // { dg-error "no member" }
}
