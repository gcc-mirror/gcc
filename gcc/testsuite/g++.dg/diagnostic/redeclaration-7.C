// PR c++/52953
// { dg-do compile }
// { dg-options "-pedantic-errors" }

volatile int v;

void
baz ()
{
  for (int x = v;			// { dg-message "'int x' previously declared here" }
       int x = v; ++x)			// { dg-error "redeclaration of 'int x'" }
    ;
  for (int x = v;			// { dg-message "'int x' previously declared here" }
       int x = v; ++x)			// { dg-error "redeclaration of 'int x'" }
    {					// { dg-message "'int x' previously declared here" "" { target *-*-* } .-1 }
      int x;				// { dg-error "redeclaration of 'int x'" }
    }
  for (int x = v;			// { dg-message "'int x' previously declared here" }
       int x = v; ++x)			// { dg-error "redeclaration of 'int x'" }
    {					// { dg-message "previous declaration 'int x'" "" { target *-*-* } .-1 }
      extern int x;			// { dg-error "'int x' conflicts with a previous declaration" }
    }
}
