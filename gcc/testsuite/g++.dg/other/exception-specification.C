// { dg-do compile }

struct S { void f (void); };

typedef void f1 (void) throw (int); // { dg-error "exception" }
typedef void (*f2) (void) throw (int); // { dg-error "exception" }
typedef void (S::*f3) (void) throw (int); // { dg-error "exception" }

void (*f4) (void) throw (int);
void (S::*f5) (void) throw (int);
