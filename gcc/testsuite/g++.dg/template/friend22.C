// { dg-do compile }

// Origin: Benoit Hudson <bh@techhouse.brown.edu>

// PR c++/641: Duplicate friend diagnostics

template <class T> class iterator { };
template <class T> class item {
    friend class iterator<T>;
    friend class iterator<const T>;
};

class A { };

item<const A> i;
