// { dg-do assemble  }
// { dg-options "" }

struct S 
{
  S ();
};

union U {
  struct { 
    S s; // { dg-error "" } struct with constructor in union
  };
};

