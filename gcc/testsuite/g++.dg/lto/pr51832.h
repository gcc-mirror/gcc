template<class...T> struct A
{
  static int i;
};

inline void f() { A<int>::i = 0; }
