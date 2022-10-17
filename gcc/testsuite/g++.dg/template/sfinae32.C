// PR c++/103700
// { dg-do compile { target c++11 } }

template<class T, int N> auto f(T* p) -> decltype(p + N);
template<class T, int N> auto f(T* p) -> decltype(p - N);
template<class T, int N> auto f(T* p) -> decltype(N + p);
template<class T, int N> void f(T* p);

template<class T> auto g(T* p, int n) -> decltype(p + n);
template<class T> auto g(T* p, int n) -> decltype(p - n);
template<class T> auto g(T* p, int n) -> decltype(n + p);
template<class T> void g(T* p, int n);

struct Incomplete;

int main() {
  f<Incomplete,  0>(nullptr);
  f<Incomplete,  1>(nullptr);
  f<Incomplete, -1>(nullptr);
  f<Incomplete,  7>(nullptr);
  f<Incomplete, -7>(nullptr);

  g<Incomplete>(nullptr, 0);
}
