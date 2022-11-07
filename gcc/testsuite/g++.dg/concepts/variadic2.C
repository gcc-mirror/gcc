// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template <class T> concept bool Copyable = requires (T t) { T(t); };
template <class T> concept bool Constructable = requires { T(); };
template <class T> concept bool Both = Copyable<T> && Constructable<T>;

template <Copyable... Ts> // requires (Copyable<Ts> && ...)
constexpr int f(Ts...) { return 0; } // #1

template <Both... Ts> // requires (Both<Ts> && ...)
constexpr int f(Ts...) { return 1; }     // #2

int main()
{
  static_assert(f(42) == 1); // { dg-error "ambiguous" }
  // The associated constraints of the two functions are incomparable.
}
