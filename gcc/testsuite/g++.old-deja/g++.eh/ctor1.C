// { dg-do assemble  }
struct A
{
  A();
  A(A&);			// { dg-message "A::A|no known conversion" } referenced below
};

int
main ()
{
  try
    {
      throw A();		// { dg-error "rvalue" "" } can't copy
// { dg-error "thrown expression" "expr" { target *-*-* } 13 }
    }
  catch (...) { }
}
