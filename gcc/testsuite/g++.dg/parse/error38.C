// PR c++/29003

typedef int operator !(); // { dg-error "declaration" }
