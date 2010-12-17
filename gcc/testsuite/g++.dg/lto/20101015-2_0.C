// { dg-lto-do link }
// { dg-lto-options { { -flto } { -g -flto } } }
// { dg-extra-ld-options "-r -nostdlib" }

struct Base { ~Base (); };
void fun(void) { struct Deriv : Base { } x; }
