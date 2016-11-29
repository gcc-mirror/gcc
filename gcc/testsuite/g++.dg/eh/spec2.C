// { dg-do compile }

struct S { void f (void); };

typedef void f1 (void) throw (int); // { dg-error "exception" "" { target c++14_down } }
typedef void (*f2) (void) throw (int); // { dg-error "exception" "" { target c++14_down } }
typedef void (S::*f3) (void) throw (int); // { dg-error "exception" "" { target c++14_down } }

void (*f4) (void) throw (int);
void (S::*f5) (void) throw (int);
