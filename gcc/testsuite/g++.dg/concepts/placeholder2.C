// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template<typename T>
concept bool C1 = sizeof(T) == 0;

template<typename T, typename U>
concept bool C2 = __is_same_as(T, U);


template<typename T>
concept bool D1 = requires (T t) { { t } -> C1; };

template<typename T>
concept bool D2 = requires (T t) { { t } -> C2<void>; };

void f1(auto D1) { } // OK: D1 is declared as a parameter
void f2(auto D2) { } // OK: D2 is declared as a parameter

int main()
{
  f1(0);
  f2(0);
}
