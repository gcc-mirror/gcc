// P1061R10 - Structured Bindings can introduce a Pack
// { dg-do run { target c++11 } }
// { dg-options "" }

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

template <typename T, typename U>
struct same_type { static const bool value = false; };

template <typename T>
struct same_type<T, T> { static const bool value = true; };

typedef int V __attribute__((vector_size (16 * sizeof (int))));

template <int N>
void
foo ()
{
  V v = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
  auto [...va] = v;			// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (va) == 16, "");
  static_assert (same_type <decltype (va...[5]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (va...[13]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  auto [vb, ...vc, vd, ve] = v;		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (vc) == 13, "");
  static_assert (same_type <decltype (vb), int>::value, "");
  static_assert (same_type <decltype (vc...[0]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (vc...[12]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (vd), int>::value, "");
  static_assert (same_type <decltype (ve), int>::value, "");
  auto [vf, vg, vh, vi, ...vj] = v;	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (vj) == 12, "");
  static_assert (same_type <decltype (vf), int>::value, "");
  static_assert (same_type <decltype (vg), int>::value, "");
  static_assert (same_type <decltype (vh), int>::value, "");
  static_assert (same_type <decltype (vi), int>::value, "");
  static_assert (same_type <decltype (vj...[2]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (vj...[10]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  if (va...[13] != 14			// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || sum (va...) != 8 * 17
      || sum (square (va)...) != 1496
      || vb != 1
      || vc...[10] != 12		// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || sum (vc...) != 15 * 7 - 1
      || sum <decltype (vc)...> (vc...) != 15 * 7 - 1
      || vd != 15 || ve != 16
      || vf != 1 || vg != 2 || vh != 3 || vi != 4
      || sum (vj...) != 8 * 17 - 10
      || (va + ...) != 8 * 17		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (... + vc) != 15 * 7 - 1	// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (0 + ... + vj) != 8 * 17 - 10)	// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
    __builtin_abort ();
  V v2[3] = { v, v + 1, v + 2 };
  int i = 0;
  for (auto [vk, ...vl, vm] : v2)	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
    {					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
      static_assert (sizeof... (vl) == 14, "");
      static_assert (same_type <decltype (vk), int>::value, "");
      static_assert (same_type <decltype (vl...[1]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (vl...[9]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (vm), int>::value, "");
      if (vk != i + 1 || sum (vl...) != i * 14 + 15 * 8 - 1 || vm != i + 16)
	__builtin_abort ();
      ++i;
    }
  _Complex double c = 1.0 + 2.0i;
  auto [...ca] = c;			// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (ca) == 2, "");
  static_assert (same_type <decltype (ca...[0]), double>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (ca...[1]), double>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  auto [cb, ...cc, cd] = c;		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (cc) == 0, "");
  static_assert (same_type <decltype (cb), double>::value, "");
  static_assert (same_type <decltype (cd), double>::value, "");
  auto [ce, ...cf] = c;			// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (cf) == 1, "");
  static_assert (same_type <decltype (ce), double>::value, "");
  static_assert (same_type <decltype (cf...[0]), double>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  auto [...cg, ch] = c;			// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (cg) == 1, "");
  static_assert (same_type <decltype (cg...[0]), double>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (ch), double>::value, "");
  if (ca...[0] != 1.0 || ca...[1] != 2.0// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || sum (ca...) != 3.0
      || sum <decltype (ca)...> (ca...) != 3.0
      || sum (square (square (square (ca)))...) != 257.0
      || cb != 1.0 || cd != 2.0
      || ce != 1.0 || cf...[0] != 2.0	// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || sum (cf...) != 2.0
      || cg...[0] != 1.0 || ch != 2.0	// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || sum (cg...) != 1.0
      || (ca + ...) != 3.0		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (0.0 + ... + cc) != 0.0	// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (... + cf) != 2.0		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (cg + ... + 0.0) != 1.0)	// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
    __builtin_abort ();
  _Complex float c2[3] = { 1.0f + 2.0fi, 2.0f + 3.0fi, 3.0f + 4.0fi };
  i = 0;
  for (auto [...ci] : c2)		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
    {					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
      static_assert (sizeof... (ci) == 2, "");
      static_assert (same_type <decltype (ci...[0]), float>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (ci...[1]), float>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      if (sum (ci...) != i * 2 + 3.0f)
	__builtin_abort ();
      ++i;
    }
}

template <typename V, typename C, typename D>
void
bar ()
{
  V v = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
  auto [...va] = v;			// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (va) == 16, "");
  static_assert (same_type <decltype (va...[5]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (va...[13]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  auto [vb, ...vc, vd, ve] = v;		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (vc) == 13, "");
  static_assert (same_type <decltype (vb), int>::value, "");
  static_assert (same_type <decltype (vc...[0]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (vc...[12]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (vd), int>::value, "");
  static_assert (same_type <decltype (ve), int>::value, "");
  auto [vf, vg, vh, vi, ...vj] = v;	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (vj) == 12, "");
  static_assert (same_type <decltype (vf), int>::value, "");
  static_assert (same_type <decltype (vg), int>::value, "");
  static_assert (same_type <decltype (vh), int>::value, "");
  static_assert (same_type <decltype (vi), int>::value, "");
  static_assert (same_type <decltype (vj...[2]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (vj...[10]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  if (va...[13] != 14			// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || sum (va...) != 8 * 17
      || vb != 1
      || vc...[10] != 12		// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || sum (vc...) != 15 * 7 - 1
      || sum <decltype (vc)...> (vc...) != 15 * 7 - 1
      || vd != 15 || ve != 16
      || vf != 1 || vg != 2 || vh != 3 || vi != 4
      || sum (vj...) != 8 * 17 - 10
      || (va + ...) != 8 * 17		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (... + vc) != 15 * 7 - 1	// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (0 + ... + vj) != 8 * 17 - 10)	// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
    __builtin_abort ();
  V v2[3] = { v, v + 1, v + 2 };
  int i = 0;
  for (auto [vk, ...vl, vm] : v2)	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
    {					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
      static_assert (sizeof... (vl) == 14, "");
      static_assert (same_type <decltype (vk), int>::value, "");
      static_assert (same_type <decltype (vl...[1]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (vl...[9]), int>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (vm), int>::value, "");
      if (vk != i + 1 || sum (vl...) != i * 14 + 15 * 8 - 1 || vm != i + 16)
	__builtin_abort ();
      ++i;
    }
  C c = 1.0 + 2.0i;
  auto [...ca] = c;			// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (ca) == 2, "");
  static_assert (same_type <decltype (ca...[0]), double>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (ca...[1]), double>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  auto [cb, ...cc, cd] = c;		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (cc) == 0, "");
  static_assert (same_type <decltype (cb), double>::value, "");
  static_assert (same_type <decltype (cd), double>::value, "");
  auto [ce, ...cf] = c;			// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (cf) == 1, "");
  static_assert (same_type <decltype (ce), double>::value, "");
  static_assert (same_type <decltype (cf...[0]), double>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  auto [...cg, ch] = c;			// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (cg) == 1, "");
  static_assert (same_type <decltype (cg...[0]), double>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  static_assert (same_type <decltype (ch), double>::value, "");
  if (ca...[0] != 1.0 || ca...[1] != 2.0// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || sum (ca...) != 3.0
      || sum <decltype (ca)...> (ca...) != 3.0
      || cb != 1.0 || cd != 2.0
      || ce != 1.0 || cf...[0] != 2.0	// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || sum (cf...) != 2.0
      || cg...[0] != 1.0 || ch != 2.0	// { dg-warning "pack indexing only available with" "" { target c++23_down } }
      || sum (cg...) != 1.0
      || (ca + ...) != 3.0		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (0.0 + ... + cc) != 0.0	// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (... + cf) != 2.0		// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
      || (cg + ... + 0.0) != 1.0)	// { dg-warning "fold-expressions only available with" "" { target c++14_down } }
    __builtin_abort ();
  D c2[3] = { 1.0f + 2.0fi, 2.0f + 3.0fi, 3.0f + 4.0fi };
  i = 0;
  for (auto [...ci] : c2)		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
    {					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
      static_assert (sizeof... (ci) == 2, "");
      static_assert (same_type <decltype (ci...[0]), float>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      static_assert (same_type <decltype (ci...[1]), float>::value, ""); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
      if (sum (ci...) != i * 2 + 3.0f)
	__builtin_abort ();
      ++i;
    }
}

int
main ()
{
  foo <0> ();
  bar <V, _Complex double, _Complex float> ();
}
