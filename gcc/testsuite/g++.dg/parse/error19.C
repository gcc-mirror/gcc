// PR C++/17867

struct A
{  // { dg-error "candidate" }
  A(int);
};

const A& foo();

void bar()
{
  foo()=A(0); // { dg-error "A" }
}
