// { dg-do assemble  }
namespace A {
  int i = 1;
}

int j = i;		// { dg-error "" } 
  // { dg-message "note" "suggested alternative" { target *-*-* } 6 }
