// PR c++/6477
typedef struct A_ *A;	// { dg-error "previous declaration" }
typedef struct A B;	// { dg-error "typedef|invalid type" }
