// PR c++/31743
typedef int A[];
A* p = new A;   // { dg-error "invalid use of array with unspecified bounds" }
A* q = new (A); // { dg-error "invalid use of array with unspecified bounds" }


