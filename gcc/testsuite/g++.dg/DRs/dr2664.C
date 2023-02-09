// CWG 2664
// { dg-do compile { target c++20 } }

template <class S1, class S2> struct C {
  C(...);
};

template<class T1> C(T1) -> C<T1, T1>;
template<class T1, class T2> C(T1, T2) -> C<T1 *, T2>;

template<class V1, class V2> using A = C<V1, V2>;

C c1{""};
A a1{""};

C c2{"", 1};
A a2{"", 1};
