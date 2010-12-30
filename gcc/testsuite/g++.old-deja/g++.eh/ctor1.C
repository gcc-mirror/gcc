// { dg-do assemble  }
struct A
{
  A();				// { dg-message "A::A|candidate expects" } candidate
  A(A&);			// { dg-message "A::A|no known conversion" } referenced below
};

int
main ()
{
  try
    {
      throw A();		// { dg-error "no matching" "match" } can't copy
      // { dg-message "candidate" "candidate note" { target *-*-* } 13 }
// { dg-error "thrown expression" "expr" { target *-*-* } 13 }
    }
  catch (...) { }
}
