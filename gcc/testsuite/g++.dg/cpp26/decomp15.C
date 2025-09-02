// P1061R10 - Structured Bindings can introduce a Pack
// { dg-do run { target c++11 } }
// { dg-options "" }

struct S {
  int a; long long b; short c;
  explicit operator bool () const noexcept { return true; }
};
namespace std {
  template <typename T> struct tuple_size;
  template <int, typename> struct tuple_element;
}
struct T {
  short c; int a; long long b;
  template <int I>
  typename std::tuple_element<I, T>::type &get ();
  template <int I>
  typename std::tuple_element<I, const T>::type &get () const;
  explicit operator bool () const noexcept { return false; }
};
template <>
struct std::tuple_size<T> { static constexpr int value = 3; };
template <>
struct std::tuple_element<0, T> { typedef int type; };
template <>
struct std::tuple_element<1, T> { typedef long long type; };
template <>
struct std::tuple_element<2, T> { typedef short type; };
template <>
std::tuple_element<0, T>::type &T::get <0> () { return a; }
template <>
std::tuple_element<1, T>::type &T::get <1> () { return b; }
template <>
std::tuple_element<2, T>::type &T::get <2> () { return c; }
template <>
struct std::tuple_size<const T> { static constexpr int value = 3; };
template <>
struct std::tuple_element<0, const T> { typedef const int type; };
template <>
struct std::tuple_element<1, const T> { typedef const long long type; };
template <>
struct std::tuple_element<2, const T> { typedef const short type; };
template <>
std::tuple_element<0, const T>::type &T::get <0> () const { return a; }
template <>
std::tuple_element<1, const T>::type &T::get <1> () const { return b; }
template <>
std::tuple_element<2, const T>::type &T::get <2> () const { return c; }
template <typename T, typename U>
struct same_type { static const bool value = false; };
template <typename T>
struct same_type<T, T> { static const bool value = true; };

int
sum ()
{
  return 0;
}

template <typename T, typename ...A>
T
sum (T x, A... y)
{
  return x + sum (y...);
}

template <typename T>
T
square (T x)
{
  return x * x;
}

template <typename T>
T &
ref (T &x)
{
  return x;
}

using size_t = decltype (sizeof 0);

template <typename S, typename T, typename U>
size_t
foo ()
{
  S s = S { 1, 2, 3 };
  auto [sa, sb, sc] = S { 1, 2, 3 };	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  static_assert (same_type <decltype (sa), int>::value, "");
  static_assert (same_type <decltype (sb), long long>::value, "");
  static_assert (same_type <decltype (sc), short>::value, "");
  auto [sd, ...se] = S { 1, 2, 3 };	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (se) == 2, "");
  static_assert (same_type <decltype (sd), int>::value, "");
  static_assert (same_type <decltype (se...[0]), long long>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (se...[1]), short>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  const auto & [...sf [[]], sg] = s;	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } .-2 }
  static_assert (sizeof... (sf) == 2, "");
  static_assert (same_type <decltype (sf...[0]), const int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (sf...[1]), const long long>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (sg), const short>::value, "");
  auto [sh, si, sj [[]], ...sk] = S { 1, 2, 3 };// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } .-2 }
  static_assert (sizeof... (sk) == 0, "");
  static_assert (same_type <decltype (sh), int>::value, "");
  static_assert (same_type <decltype (si), long long>::value, "");
  static_assert (same_type <decltype (sj), short>::value, "");
  auto && [sl, ...sm [[maybe_unused]], sn] = s; // { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } .-2 }
  static_assert (sizeof... (sm) == 1, "");
  static_assert (same_type <decltype (sl), int>::value, "");
  static_assert (same_type <decltype (sm...[0]), long long>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (sn), short>::value, "");
  auto [...so] = S { 1, 2, 3 };		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (so) == 3, "");
  static_assert (same_type <decltype (so...[0]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (so...[1]), long long>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (so...[2]), short>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  auto [...sp, sq, sr, ss [[maybe_unused]]] = S { 1, 2, 3 };// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } .-2 }
  static_assert (sizeof... (sp) == 0, "");
  static_assert (same_type <decltype (sq), int>::value, "");
  static_assert (same_type <decltype (sr), long long>::value, "");
  static_assert (same_type <decltype (ss), short>::value, "");
  auto [st, ...su, sv, sw] = S { 1, 2, 3 };// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (su) == 0, "");
  static_assert (same_type <decltype (st), int>::value, "");
  static_assert (same_type <decltype (sv), long long>::value, "");
  static_assert (same_type <decltype (sw), short>::value, "");
  if (sa != 1 || sb != 2 || sc != 3
      || sd != 1 || se...[0] != 2 || se...[1] != 3	// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || sf...[0] != 1 || sf...[1] != 2 || sg != 3	// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || sh != 1 || si != 2 || sj != 3
      || sl != 1 || sm...[0] != 2 || sn != 3		// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || so...[0] != 1 || so...[1] != 2 || so...[2] != 3// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || sq != 1 || sr != 2 || ss != 3
      || st != 1 || sv != 2 || sw != 3
      || sum (se...) != 5
      || sum <decltype (se)...> (se...) != 5
      || sum (square (square (se))...) != 97
      || sum (sf...) != 3
      || sum (sk...) != 0
      || sum (sm...) != 2
      || sum (so...) != 6
      || sum <decltype (so)...> (so...) != 6
      || sum (square (so)...) != 14
      || sum (sp...) != 0
      || sum (su...) != 0
      || (se + ...) != 5		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (... + sf) != 3		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (0 + ... + sk) != 0		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (sm + ... + 0) != 2		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (so + ...) != 6		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (sp + ... + 0) != 0		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (0 + ... + su) != 0)		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
    __builtin_abort ();
  S s2[] = { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 } };
  int i = 0;
  for (auto [sx, ...sy [[]]] : s2)	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
    {					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } .-2 }
      static_assert (sizeof... (sy) == 2, "");
      static_assert (same_type <decltype (sx), int>::value, "");
      static_assert (same_type <decltype (sy...[0]), long long>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (sy...[1]), short>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      if (sx != i * 3 + 1 || sum (sy...) != i * 6 + 5)
	__builtin_abort ();
      auto fn1 = [&] () { if (sum (sy...) != i * 6 + 5) __builtin_abort (); }; // { dg-warning "captured structured bindings are" "" { target c++17_down } }
      fn1 ();
      auto fn2 = [&sy..., &i] () { if (sum (sy...) != i * 6 + 5) __builtin_abort (); }; // { dg-warning "captured structured bindings are" "" { target c++17_down } }
      fn2 ();
      auto fn3 = [&...sy2 = sy, &i] () { if (sum (sy2...) != i * 6 + 5) __builtin_abort (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn3 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
      auto fn4 = [&...sy3 = ref (sy), &i] () { if (sum (sy3...) != i * 6 + 5) __builtin_abort (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn4 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
      auto fn5 = [=] () { if (sum (sy...) != i * 6 + 5) __builtin_abort (); }; // { dg-warning "captured structured bindings are" "" { target c++17_down } }
      fn5 ();
      auto fn6 = [sy..., i] () { if (sum (sy...) != i * 6 + 5) __builtin_abort (); }; // { dg-warning "captured structured bindings are" "" { target c++17_down } }
      fn6 ();
      auto fn7 = [...sy2 = sy, i] () { if (sum (sy2...) != i * 6 + 5) __builtin_abort (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn7 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
      auto fn8 = [...sy3 = square (sy), i] () { if (sum (sy3...) != (i * 3 + 2) * (i * 3 + 2) + (i * 3 + 3) * (i * 3 + 3)) __builtin_abort (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn8 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
      auto fn9 = [&] () { auto fn = [&] () { if (sum (sy...) != i * 6 + 5) __builtin_abort (); }; fn (); }; // { dg-warning "captured structured bindings are" "" { target c++17_down } }
      fn9 ();
      auto fn10 = [&sy..., &i] () { auto fn = [&] () { if (sum (sy...) != i * 6 + 5) __builtin_abort (); }; fn (); }; // { dg-warning "captured structured bindings are" "" { target c++17_down } }
      fn10 ();
      auto fn11 = [&] () { auto fn = [&...sy2 = sy, &i] () { if (sum (sy2...) != i * 6 + 5) __builtin_abort (); }; fn (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn11 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
					// { dg-warning "captured structured bindings are" "" { target c++17_down } .-2 }
      auto fn12 = [&sy..., &i] () { auto fn = [&...sy3 = ref (sy), &i] () { if (sum (sy3...) != i * 6 + 5) __builtin_abort (); }; fn (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn12 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
					// { dg-warning "captured structured bindings are" "" { target c++17_down } .-2 }
      auto fn13 = [=] () { auto fn = [=] () { if (sum (sy...) != i * 6 + 5) __builtin_abort (); }; fn (); }; // { dg-warning "captured structured bindings are" "" { target c++17_down } }
      fn13 ();
      auto fn14 = [sy..., i] () { auto fn = [=] () { if (sum (sy...) != i * 6 + 5) __builtin_abort (); }; fn (); }; // { dg-warning "captured structured bindings are" "" { target c++17_down } }
      fn14 ();
      auto fn15 = [=] () { auto fn = [...sy2 = sy, i] () { if (sum (sy2...) != i * 6 + 5) __builtin_abort (); }; fn (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn15 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
					// { dg-warning "captured structured bindings are" "" { target c++17_down } .-2 }
      auto fn16 = [&sy..., &i] () { auto fn = [...sy3 = square (sy), i] () { if (sum (sy3...) != (i * 3 + 2) * (i * 3 + 2) + (i * 3 + 3) * (i * 3 + 3)) __builtin_abort (); }; fn (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn16 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
					// { dg-warning "captured structured bindings are" "" { target c++17_down } .-2 }
      ++i;
    }
  i = 0;
  for (auto [...sz] : s2)		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
    {					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
      static_assert (sizeof... (sz) == 3, "");
      static_assert (same_type <decltype (sz...[0]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (sz...[1]), long long>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (sz...[2]), short>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      if (sum (sz...) != i * 9 + 6)
	__builtin_abort ();
      auto fn = [=] () { if (sum (sz...) != i * 9 + 6) __builtin_abort (); };	// { dg-warning "captured structured bindings are" "" { target c++17_down } }
      fn ();
      ++i;
    }
  if (auto [...sx, sy] = s)		// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    {
      static_assert (sizeof... (sx) == 2, "");
      static_assert (same_type <decltype (sx...[0]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (sx...[1]), long long>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (sy), short>::value, "");
      if (sum (sx...) != 3 || sy != 3)
	__builtin_abort ();
    }
  else
    __builtin_abort ();
  T t = T { 3, 1, 2 };
  auto [ta, tb, tc] = T { 3, 1, 2 };	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  static_assert (same_type <decltype (ta), int>::value, "");
  static_assert (same_type <decltype (tb), long long>::value, "");
  static_assert (same_type <decltype (tc), short>::value, "");
  auto [td [[maybe_unused]], ...te] = T { 3, 1, 2 }; // { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } .-2 }
  static_assert (sizeof... (te) == 2, "");
  static_assert (same_type <decltype (td), int>::value, "");
  static_assert (same_type <decltype (te...[0]), long long>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (te...[1]), short>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  auto [...tf [[maybe_unused]], tg] = T { 3, 1, 2 }; // { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } .-2 }
  static_assert (sizeof... (tf) == 2, "");
  static_assert (same_type <decltype (tf...[0]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (tf...[1]), long long>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (tg), short>::value, "");
  const auto & [th, ti, tj, ...tk] = t;	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (tk) == 0, "");
  static_assert (same_type <decltype (th), const int>::value, "");
  static_assert (same_type <decltype (ti), const long long>::value, "");
  static_assert (same_type <decltype (tj), const short>::value, "");
  auto [tl [[]], ...tm [[]], tn [[]]] = T { 3, 1, 2 }; // { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } .-2 }
  static_assert (sizeof... (tm) == 1, "");
  static_assert (same_type <decltype (tl), int>::value, "");
  static_assert (same_type <decltype (tm...[0]), long long>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (tn), short>::value, "");
  auto && [...to] = t;			// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (to) == 3, "");
  constexpr size_t tos = sizeof... (to);
  static_assert (same_type <decltype (to...[0]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (to...[1]), long long>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (to...[2]), short>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  auto [...tp, tq [[]], tr, ts] = T { 3, 1, 2 };// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } .-2 }
  static_assert (sizeof... (tp) == 0, "");
  static_assert (same_type <decltype (tq), int>::value, "");
  static_assert (same_type <decltype (tr), long long>::value, "");
  static_assert (same_type <decltype (ts), short>::value, "");
  auto [tt, ...tu [[]], tv, tw] = T { 3, 1, 2 };// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } .-2 }
  static_assert (sizeof... (tu) == 0, "");
  static_assert (same_type <decltype (tt), int>::value, "");
  static_assert (same_type <decltype (tv), long long>::value, "");
  static_assert (same_type <decltype (tw), short>::value, "");
  if (ta != 1 || tb != 2 || tc != 3
      || td != 1 || te...[0] != 2 || te...[1] != 3	// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || tf...[0] != 1 || tf...[1] != 2 || tg != 3	// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || th != 1 || ti != 2 || tj != 3
      || tl != 1 || tm...[0] != 2 || tn != 3		// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || to...[0] != 1 || to...[1] != 2 || to...[2] != 3// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || tq != 1 || tr != 2 || ts != 3
      || tt != 1 || tv != 2 || tw != 3
      || sum (te...) != 5
      || sum <decltype (te)...> (te...) != 5
      || sum (square (square (te))...) != 97
      || sum (tf...) != 3
      || sum (tk...) != 0
      || sum (tm...) != 2
      || sum (to...) != 6
      || sum <decltype (to)...> (to...) != 6
      || sum (square (to)...) != 14
      || sum (tp...) != 0
      || sum (tu...) != 0
      || (te + ...) != 5		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (... + tf) != 3		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (0 + ... + tk) != 0		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (tm + ... + 0) != 2		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (to + ...) != 6		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (tp + ... + 0) != 0		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (0 + ... + tu) != 0)		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
    __builtin_abort ();
  T t2[] = { { 3, 1, 2 }, { 6, 4, 5 }, { 9, 7, 8 } };
  i = 0;
  for (auto [tx, ...ty] : t2)		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
    {					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
      static_assert (sizeof... (ty) == 2, "");
      static_assert (same_type <decltype (tx), int>::value, "");
      static_assert (same_type <decltype (ty...[0]), long long>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (ty...[1]), short>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      if (tx != i * 3 + 1 || sum (ty...) != i * 6 + 5)
	__builtin_abort ();
      auto fn1 = [&] () { if (sum (ty...) != i * 6 + 5) __builtin_abort (); };
      fn1 ();
      auto fn2 = [&ty..., &i] () { if (sum (ty...) != i * 6 + 5) __builtin_abort (); }; // { dg-warning "captured structured bindings are" "" { target c++17_down } }
      fn2 ();
      auto fn3 = [&...ty2 = ty, &i] () { if (sum (ty2...) != i * 6 + 5) __builtin_abort (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn3 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
      auto fn4 = [&...ty3 = ref (ty), &i] () { if (sum (ty3...) != i * 6 + 5) __builtin_abort (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn4 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
      auto fn5 = [=] () { if (sum (ty...) != i * 6 + 5) __builtin_abort (); };
      fn5 ();
      auto fn6 = [ty..., i] () { if (sum (ty...) != i * 6 + 5) __builtin_abort (); }; // { dg-warning "captured structured bindings are" "" { target c++17_down } }
      fn6 ();
      auto fn7 = [...ty2 = ty, i] () { if (sum (ty2...) != i * 6 + 5) __builtin_abort (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn7 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
      auto fn8 = [...ty3 = square (ty), i] () { if (sum (ty3...) != (i * 3 + 2) * (i * 3 + 2) + (i * 3 + 3) * (i * 3 + 3)) __builtin_abort (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn8 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
      auto fn9 = [&] () { auto fn = [&] () { if (sum (ty...) != i * 6 + 5) __builtin_abort (); }; fn (); };
      fn9 ();
      auto fn10 = [&ty..., &i] () { auto fn = [&] () { if (sum (ty...) != i * 6 + 5) __builtin_abort (); }; fn (); }; // { dg-warning "captured structured bindings are" "" { target c++17_down } }
      fn10 ();
      auto fn11 = [&] () { auto fn = [&...ty2 = ty, &i] () { if (sum (ty2...) != i * 6 + 5) __builtin_abort (); }; fn (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn11 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
      auto fn12 = [&ty..., &i] () { auto fn = [&...ty3 = ref (ty), &i] () { if (sum (ty3...) != i * 6 + 5) __builtin_abort (); }; fn (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn12 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
					// { dg-warning "captured structured bindings are" "" { target c++17_down } .-2 }
      auto fn13 = [=] () { auto fn = [=] () { if (sum (ty...) != i * 6 + 5) __builtin_abort (); }; fn (); };
      fn13 ();
      auto fn14 = [ty..., i] () { auto fn = [=] () { if (sum (ty...) != i * 6 + 5) __builtin_abort (); }; fn (); }; // { dg-warning "captured structured bindings are" "" { target c++17_down } }
      fn14 ();
      auto fn15 = [=] () { auto fn = [...ty2 = ty, i] () { if (sum (ty2...) != i * 6 + 5) __builtin_abort (); }; fn (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn15 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
      auto fn16 = [&ty..., &i] () { auto fn = [...ty3 = square (ty), i] () { if (sum (ty3...) != (i * 3 + 2) * (i * 3 + 2) + (i * 3 + 3) * (i * 3 + 3)) __builtin_abort (); }; fn (); }; // { dg-warning "pack init-capture only available with" "" { target c++17_down } }
      fn16 ();				// { dg-warning "lambda capture initializers only available with" "" { target c++11_only } .-1 }
					// { dg-warning "captured structured bindings are" "" { target c++17_down } .-2 }
      ++i;
    }
  i = 0;
  for (auto [...tz] : t2)		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
    {					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
      static_assert (sizeof... (tz) == 3, "");
      static_assert (same_type <decltype (tz...[0]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (tz...[1]), long long>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (tz...[2]), short>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      if (sum (tz...) != i * 9 + 6)
	__builtin_abort ();
      auto fn = [=] () { if (sum (tz...) != i * 9 + 6) __builtin_abort (); };
      fn ();
      ++i;
    }
  if (auto [...tx [[maybe_unused]], ty] = t) // { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    __builtin_abort ();
  else
    {
      static_assert (sizeof... (tx) == 2, "");
      static_assert (same_type <decltype (tx...[0]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (tx...[1]), long long>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (ty), short>::value, "");
      if (sum (tx...) != 3 || ty != 3)
	__builtin_abort ();
    }
  U a[3] = { 1, 2, 3 };
  auto [aa, ab, ac] = a;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  static_assert (same_type <decltype (aa), int>::value, "");
  static_assert (same_type <decltype (ab), int>::value, "");
  static_assert (same_type <decltype (ac), int>::value, "");
  auto [ad [[maybe_unused]], ...ae [[maybe_unused]]] = a; // { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
				// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } .-2 }
  static_assert (sizeof... (ae) == 2, "");
  static_assert (same_type <decltype (ad), int>::value, "");
  static_assert (same_type <decltype (ae...[0]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (ae...[1]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  auto [...af, ag] = a;		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (af) == 2, "");
  static_assert (same_type <decltype (af...[0]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (af...[1]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (ag), int>::value, "");
  auto [ah, ai [[]], aj, ...ak [[]]] = a; // { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
				// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } .-2 }
  static_assert (sizeof... (ak) == 0, "");
  static_assert (same_type <decltype (ah), int>::value, "");
  static_assert (same_type <decltype (ai), int>::value, "");
  static_assert (same_type <decltype (aj), int>::value, "");
  auto [al, ...am [[]], an] = a;// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
				// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } .-2 }
  static_assert (sizeof... (am) == 1, "");
  static_assert (same_type <decltype (al), int>::value, "");
  static_assert (same_type <decltype (am...[0]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (an), int>::value, "");
  const auto &[...ao] = a;	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (ao) == 3, "");
  static_assert (same_type <decltype (ao...[0]), const int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (ao...[1]), const int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (ao...[2]), const int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  auto &&[...ap, aq, ar [[]], as] = a;// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
				// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } .-2 }
  static_assert (sizeof... (ap) == 0, "");
  static_assert (same_type <decltype (aq), int>::value, "");
  static_assert (same_type <decltype (ar), int>::value, "");
  static_assert (same_type <decltype (as), int>::value, "");
  auto [at, ...au, av, aw] = a;	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (au) == 0, "");
  static_assert (same_type <decltype (at), int>::value, "");
  static_assert (same_type <decltype (av), int>::value, "");
  static_assert (same_type <decltype (aw), int>::value, "");
  if (aa != 1 || ab != 2 || ac != 3
      || ad != 1 || ae...[0] != 2 || ae...[1] != 3	// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || af...[0] != 1 || af...[1] != 2 || ag != 3	// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || ah != 1 || ai != 2 || aj != 3
      || al != 1 || am...[0] != 2 || an != 3		// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || ao...[0] != 1 || ao...[1] != 2 || ao...[2] != 3// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || aq != 1 || ar != 2 || as != 3
      || at != 1 || av != 2 || aw != 3
      || sum (ae...) != 5
      || sum <decltype (ae)...> (ae...) != 5
      || sum (square (square (ae))...) != 97
      || sum (af...) != 3
      || sum (ak...) != 0
      || sum (am...) != 2
      || sum (ao...) != 6
      || sum <decltype (ao)...> (ao...) != 6
      || sum (square (ao)...) != 14
      || sum (ap...) != 0
      || sum (au...) != 0
      || (ae + ...) != 5		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (... + af) != 3		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (0 + ... + ak) != 0		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (am + ... + 0) != 2		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (ao + ...) != 6		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (ap + ... + 0) != 0		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (0 + ... + au) != 0)		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
    __builtin_abort ();
  return tos;
}

int
main ()
{
  if (foo <S, T, int> () != 3)
    __builtin_abort ();
}
