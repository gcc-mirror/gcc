// PR c++/6477
typedef struct A_ *A;	// { dg-error "previous declaration as" }
typedef struct A B;	// { dg-error "conflicting types" }
