// PR c++/27803

struct A
{
  double i : 8; // { dg-error "type" }
};

void foo(A& a)
{
  (char)a.i;    // { dg-error "no member" }
}
