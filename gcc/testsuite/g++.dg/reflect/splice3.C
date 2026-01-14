// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct A { static int x; };
int q = A ().[: ^^x :];  // { dg-error "'x' has not been declared" }
