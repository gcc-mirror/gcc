// Build don't link:
// prms-id: 10951

inline int m1(const int& a) {
  return 1;
}

template <class T>
class A {
public:
  typedef int st;

  static int m2t() {
    return m1(1);
  }
};

A<int>::st i;
int m3() {
  return A<int>::m2t();
}
