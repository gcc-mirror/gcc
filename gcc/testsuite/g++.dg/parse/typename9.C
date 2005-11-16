struct A { typedef int X; };

int i = typename A::X(); // { dg-error "typename" }
