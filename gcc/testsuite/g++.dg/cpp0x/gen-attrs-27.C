//PR c++/29980
// { dg-do compile { target c++11 } }

struct A { typedef int X; };            // { dg-message "previous declaration" }

struct [[gnu::unused]] A::X;    // { dg-error "typedef-name" }
