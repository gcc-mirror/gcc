// PR c++/66758
// { dg-do compile { target c++20 } }

template <class T, class U>
concept C = requires (T t, U u) { t + u; };

template <class T, class U>
  requires C<T,U>
void f(T t, U u) { t + u; }

struct non_addable { };

int main()
{
  using T = decltype(f(42, 24));
}
