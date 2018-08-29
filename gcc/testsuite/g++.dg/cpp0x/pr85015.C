// PR c++/85015
// { dg-do compile { target c++11 } }
// { dg-options "" }

void
foo ()
{
  int &&c = v + 1;		// { dg-error "was not declared in this scope" }
  struct S {			// { dg-message "declared here" "" { target *-*-* } .-1 }
    void bar () { int a[c]; }	// { dg-error "use of local variable with automatic storage from containing function" }
  } e;
}
