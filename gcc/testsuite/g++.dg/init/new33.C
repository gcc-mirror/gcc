// PR c++/53356
// { dg-do compile }

struct A {};
struct B { operator const A & () const; };
struct C { operator const A & () const; C (); };
struct D { operator const A & () const; D (); ~D (); };

A *foo () { return new A (B ()); }
A *bar () { return new A (C ()); }
A *baz () { return new A (D ()); }
