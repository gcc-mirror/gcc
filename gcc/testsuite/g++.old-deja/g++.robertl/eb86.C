// { dg-do assemble  }
template <class T> void foo ();

template <class T>
class Q {
    friend void foo<T> ();
};
