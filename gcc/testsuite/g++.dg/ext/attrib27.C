//PR c++/29980

struct A { typedef int X; };            // { dg-error "previous declaration" }

struct __attribute__((unused)) A::X;    // { dg-error "typedef-name" }
