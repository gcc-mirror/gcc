// PR c++/6634
// { dg-do compile }
// { dg-options "" }

long long double x; // { dg-error "long long" }
long double y;
long float z;       // { dg-error "long" }

typedef short void    SV; // { dg-error "short" }
typedef long struct A LA; // { dg-error "long" }
typedef short char    SC; // { dg-error "short" }
