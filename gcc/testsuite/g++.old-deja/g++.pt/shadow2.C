// Build don't link:
// Origin: Jason Merrill <jason@cygnus.com>

template <class T> struct A { // ERROR - shadowed parameter
  struct B {
    void T(); // ERROR - shadows template parameter
  };
};
A<int> a;

