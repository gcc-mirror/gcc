// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template<typename T>
  concept bool Type = true;

template<typename T, typename U>
  concept bool Same = __is_same_as(T, U);

template<typename T, typename U>
  concept bool C1 = true;

template<typename T, typename... Args>
  concept bool C2 = true;

template<typename T, typename U>
  concept bool C3 = __is_same_as(T, int) && __is_same_as(U, double);

template<Same<int> T> struct S1 { };
template<typename T, Same<T> U> struct S2 { };

template<Same<int> Q>
void f(Q q) { }
template<Type A, Same<decltype(A{})> B>
void g(A a, B b) { }

template<Same<int> A>
void h0(A* a) { }
template<C1<int> A>
void h1(A* a) { }
template<C2<char, short, int, long> A>
void h2(A* a) { }
template<C3<double> A>
void h3(A* a) { }

int main() {
  S1<int> s1;
  S2<int, int> s2;
  f(0);
  g(0, 1);
  h0((int*)0);
  h1((int*)0);
  // h2((int*)0);
  h3((int*)0);
}
