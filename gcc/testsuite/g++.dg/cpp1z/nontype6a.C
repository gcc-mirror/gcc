// PR c++/101463
// A version of nontype6.C where v and vt are constexpr.
// { dg-do compile { target c++17 } }

int a;

constexpr int& v = a;

template<const int& = v>
void f(int) { }

template<class T, const int& = v>
void g(T) { }

template<class T>
constexpr int& vt = a;

template<class T, const int& = vt<T>>
void h(T) { }

int main() {
  f(0);
  g(0);
  h(0);
}
