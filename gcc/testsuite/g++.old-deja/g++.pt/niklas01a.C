// Build don't link: 

struct A { // ERROR - forward declaration
  friend struct B : A {		// ERROR - 
    int x;
  };
  int y;
};
