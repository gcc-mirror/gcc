// { dg-do assemble  }
// GROUPS passed
struct A { virtual void f(); }; 
struct B { virtual void f() ; };
struct C : virtual A , virtual B { virtual void f(); };

/* This used to get an error because the DECL_CONTEXT was blown away. */
void g(A *ptr) { ptr->f(); }
