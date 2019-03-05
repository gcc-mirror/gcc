// PR c++/66758
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <class T, class U>
concept bool C = requires (T t, U u) { t + u; };

template <class T, class U>
requires C<T,U>
void f(T t, U u) { t + u; }

int main()
{
  using T = decltype(f(42, 24));
}
