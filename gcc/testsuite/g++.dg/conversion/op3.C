// PR c++/22256

struct node { int* operator int*(); }; // { dg-error "return type specified" }
