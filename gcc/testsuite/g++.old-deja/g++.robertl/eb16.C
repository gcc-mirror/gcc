// { dg-do assemble  }
template<class T>
struct A {
    typedef T T1;
};

template<class T>
struct B : T::T1 {           // insert `typename' before T::T1
};

struct C { };

B<A<C> > z;

