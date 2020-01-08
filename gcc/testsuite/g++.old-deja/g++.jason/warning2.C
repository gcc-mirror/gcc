// { dg-do assemble  }
// g++ ought to warn about casting a base pointer to a derived reference.

struct A {
  virtual void f () = 0;
};

struct B: public A { void f () { } };

int main()
{
  B* bp;
  A& ar = (A&)bp;		// { dg-warning "11:casting .B\\*. to .A&. does not dereference pointer" } 
}
