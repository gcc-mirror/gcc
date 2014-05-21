// { dg-do assemble  }
// { dg-options "" }

template <class X> int f (X x, X y) { return 23; } // { dg-message "note" }

int foo () {
  return f (7);	// { dg-error "" } 
}
