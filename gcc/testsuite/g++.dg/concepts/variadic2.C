// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <class T> concept Copyable = requires (T t) { T(t); };
template <class T> concept Constructable = requires { T(); };
template <class T> concept Both = Copyable<T> && Constructable<T>;

template <Copyable... Ts> // requires (Copyable<Ts> && ...)
constexpr int f(Ts...) { return 0; } // #1

template <Both... Ts> // requires (Both<Ts> && ...)
constexpr int f(Ts...) { return 1; }     // #2

int main()
{
  static_assert(f(42) == 1); // { dg-error "ambiguous" }
  // The associated constraints of the two functions are incomparable.
}
