// Build don't link: 
// Special g++ Options: 

template <class X> int f (X x, X y) { return 23; }

int foo () {
  return f (7);	// ERROR - 
}
