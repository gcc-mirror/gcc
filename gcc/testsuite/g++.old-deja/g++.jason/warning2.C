// g++ ought to warn about casting a base pointer to a derived reference.
// Build don't link:

struct A {
  virtual int f () = 0;
};

struct B: public A { int f () { } };

int main()
{
  B* bp;
  A& ar = (A&)bp;		// WARNING - 
}
