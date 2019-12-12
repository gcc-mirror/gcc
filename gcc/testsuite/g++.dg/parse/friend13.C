// PR c++/90572

template <typename T> struct C {
  friend C(T::fn)();
  friend C(T::fn)(int);
  friend C(T::fn)(int, int);
};

struct X { };

template<typename T>
struct B {
  friend X(T::fn)();
  friend X(T::fn)(int);
  friend X(T::fn)(int, int);
};
