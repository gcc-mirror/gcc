// Build don't link:
// Special g++ Options:

struct S 
{
  S ();
};

union U {
  struct { 
    S s; // ERROR - struct with constructor in union
  };
};

