// Build don't link: 

struct A {
  friend struct B : A {		// ERROR - 
    int x;
  };
  int y;
};
