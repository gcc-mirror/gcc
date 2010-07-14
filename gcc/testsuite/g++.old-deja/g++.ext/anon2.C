// { dg-do assemble  }
// { dg-options "" }
// { dg-prune-output "note" }

struct S 
{
  S ();
};

union U {
  struct { 
    S s; // { dg-error "" } struct with constructor in union
  };
};

