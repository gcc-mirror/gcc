// Build don't link:
// prms-id: 2793

void f(char&) {			// ERROR - referenced by error below
  f('c');			// ERROR - 
}
