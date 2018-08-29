// PR c++/78489
// { dg-do compile { target c++11 } }

template <bool Pred, class T> struct enable_if { typedef T type; };
template <class T> struct enable_if<false, T> {};

template <int Idx> struct blows_up { static_assert(Idx != Idx, ""); };

template <int Idx,
           // substitution should fail here
          typename enable_if<Idx != Idx, int>::type = 0,
          // GCC evaluates this statement
          class = typename blows_up<Idx>::type 
>
void Foo() {}

// Check the constructor in as SFINAE context
template <int I> constexpr auto test(int) -> decltype((Foo<I>(), true)) { return true; }
template <int>   constexpr bool test(long) { return false; }

static_assert(!test<3>(0), ""); // Blows up
