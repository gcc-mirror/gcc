// { dg-do run { target i?86-*-* } }
// { dg-options "-fabi-version=0" }

struct A { virtual void f(); char c1; };
struct B { B(); char c2; };
struct C : public A, public virtual B { };

int main () {
  if (sizeof (C) != 8)
    return 1;
}

