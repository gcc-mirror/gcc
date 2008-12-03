// { dg-do assemble  }
struct A
{
  A();				// { dg-message "" } candidate
  A(A&);			// { dg-message "candidates" } referenced below
};

int
main ()
{
  try
    {
      throw A();		// { dg-error "no matching" "match" } can't copy
// { dg-error "thrown expression" "expr" { target *-*-* } 13 }
    }
  catch (...) { }
}
