// check that using a qualified name with a typename does
// not report an error.

struct A { typedef int X; };

int i = typename A::X();
