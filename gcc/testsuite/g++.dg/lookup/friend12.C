// PR c++/34912

void foo()
{
  extern void bar (int); // not the bar we are looking for
  struct A
  {
    friend void bar();	// { dg-error "without prior local declaration" }
  };
}
