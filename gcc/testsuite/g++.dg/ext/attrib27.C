//PR c++/29980

struct A { typedef int X; };            // { dg-message "previous declaration" }

struct __attribute__((unused)) A::X;    // { dg-error "typedef-name" }
