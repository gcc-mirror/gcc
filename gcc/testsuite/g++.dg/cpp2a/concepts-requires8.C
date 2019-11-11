// { dg-do compile { target c++2a } }

template <class T, class U>
concept C = requires (T t, U u) { t + u; }; // { dg-message "in requirements" }

template <class T, class U>
  requires C<T,U>
void f(T t, U u) { t + u; }

struct non_addable { };

int main()
{
  // FIXME: This diagnostic is being emitted twice, when it should
  // be emitted just once.
  using U = decltype(f(42, non_addable{})); // { dg-error "" }
}
