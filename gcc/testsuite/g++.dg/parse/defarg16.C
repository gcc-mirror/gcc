// PR c++/28262

typedef void (funcptrhack) (int = 10);   // { dg-error "default arguments" }
typedef funcptrhack * funcptr;
