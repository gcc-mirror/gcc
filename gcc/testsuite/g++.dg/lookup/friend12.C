// PR c++/34912

void foo()
{
  extern void bar (int); // not the bar we are looking for
  struct A
  {
    friend void bar();	// { dg-error "17:friend declaration .void bar\\(\\). in local class without prior local declaration" }
  };
}
