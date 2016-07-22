// PR c++/34912

void foo()
{
  struct A
  {
    friend void bar();		// { dg-error "without prior declaration" }
  };
  bar();			// { dg-error "3:'bar' was not declared" }
}
