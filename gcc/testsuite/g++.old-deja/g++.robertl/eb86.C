// Build don't link: 
template <class T>
class Q {
    friend void foo<T> ();
};
