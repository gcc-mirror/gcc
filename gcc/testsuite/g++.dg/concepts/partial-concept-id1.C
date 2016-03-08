// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
  concept bool Type() { return true; }

template<typename T, typename U>
  concept bool Same() { return __is_same_as(T, U); }

template<typename T, typename U>
  concept bool C1() { return true; }

template<typename T, typename... Args>
  concept bool C2() { return true; }

template<Same<int> T> struct S1 { };
template<typename T, Same<T> U> struct S2 { };

void f(Same<int> q) { }
void g(Type a, Same<decltype(a)> b) { }

void h0(Same<int>* a) { }
void h1(C1<int>* a) { }
void h2(C2<char, short, int, long>* a) { }

int main() {
  S1<int> s1;
  S2<int, int> s2;
  f(0);
  g(0, 1);
  h0((int*)0);
  h1((int*)0);
  h2((int*)0);
}
