// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>
// Special g++ Options: -Wno-non-template-friend

template<int I>
class C {
    friend void f(struct X *);
};

template class C<0>;

class D {
    friend void f(struct X*);
};

