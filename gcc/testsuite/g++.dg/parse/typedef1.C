// PR c++/6477
typedef struct A_ *A;
typedef struct A B;	// { dg-error "" }
