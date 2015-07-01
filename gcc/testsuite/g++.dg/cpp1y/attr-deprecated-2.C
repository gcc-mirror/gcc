// PR c++/60365
// { dg-do compile { target c++14 } }

void func [[deprecated, deprecated]] (); // { dg-error "at most once" }
