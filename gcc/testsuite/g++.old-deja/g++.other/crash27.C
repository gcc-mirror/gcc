// { dg-do assemble  }
// Origin: Jakub Jelinek <jakub@redhat.com>

X(Y(long, Type, CLink)); break; default: break; } } }	// { dg-error "" } 

struct A {
  inline A ();
};

inline A::A ()
{ 
}
