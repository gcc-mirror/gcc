// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
concept bool C1 = __is_class(T);

template<typename T>
concept bool C2 = requires (T t) { t; };

void f1(C1, C1) { }

template<typename T>
  requires C2<T>
void f2(T) { }

void f3(C2) { }

struct S1 {};

int main ()
{
  f1(S1(), S1());
  f2(0);
  f3(0);

  return 0;
}
