// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

// crash test - XFAIL *-*-*

X(Y(long, Type, CLink)); break; default: break; } } }	// ERROR - 

struct A {
  inline A ();
};

inline A::A ()
{ 
}
