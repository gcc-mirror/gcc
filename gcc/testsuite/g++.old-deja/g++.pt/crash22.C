// Build don't link:

template <class T>
struct S1 {};

template <class T, class U = S1<T> > 
struct S2 {};

template struct S2<100>; // ERROR - type/value mismatch
