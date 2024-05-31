// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
concept C1 = __is_class(T);

template<typename T>
concept C2 = requires (T t) { t; };

void f1(C1 auto, C1 auto) { }

template<typename T>
  requires C2<T>
void f2(T) { }

void f3(C2 auto) { }

struct S1 {};

int main ()
{
  f1(S1(), S1());
  f2(0);
  f3(0);

  return 0;
}
