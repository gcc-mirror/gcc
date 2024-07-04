template<class T>
struct A {
  template<class U> struct B { };
  template<class U> friend struct B;
};
