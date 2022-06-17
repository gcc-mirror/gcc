// PR c++/105797
// { dg-do compile { target c++20 } }

template<class T>
concept C = requires { T(); };

template<class T>
void f(T v) requires C<decltype(v)>; // #1

template<class T, class U>
void f(T v) requires C<decltype(v)>; // #2

int main() {
  f<int, int>(0);
  f<int>(0);
}
