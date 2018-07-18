// PR c++/64574

template<class T>
class TraitCheckImpl;

template<class T, class>
class Swappable;
template<class T, class=typename TraitCheckImpl<Swappable<T, void> >::Complete>
class Swappable;

template<class T>
struct TraitCheckImpl<Swappable<T> > {
    typedef void Complete;
};

Swappable<int> s;  // { dg-error "" }
