// { dg-do assemble  }
// { dg-options "-Wno-non-template-friend" }
// Origin: Mark Mitchell <mark@codesourcery.com>

template<int I>
class C {
    friend void f(struct X *);
};

template class C<0>;

class D {
    friend void f(struct X*);
};

