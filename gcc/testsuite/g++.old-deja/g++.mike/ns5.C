// { dg-do assemble  }
namespace A {
  int i = 1;			// { dg-message "A::i" }
}

int j = i;		// { dg-error "" } 
  // { dg-message "suggested alternative" "suggested alternative" { target *-*-* } 6 }
