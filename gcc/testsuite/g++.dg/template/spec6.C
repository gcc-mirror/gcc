template <bool, int> struct X {};

template <bool C>    struct X<C,1> {
    typedef double* type;
    type foo () const;
};

template <bool C>
typename X<C,1>::type
X<C,1>::foo () const {}
