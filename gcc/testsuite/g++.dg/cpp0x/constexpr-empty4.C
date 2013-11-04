// { dg-options -std=c++11 }

typedef decltype(sizeof(char)) size_type;

template<class T, size_type N, class Pred>
constexpr size_type do_find_if_or_stop(T (&x)[N], size_type i, Pred p);

template<class T, size_type N, class Pred>
constexpr size_type do_find_if(T (&x)[N], size_type i, Pred p) {
  return p(x[i]) ? i : do_find_if_or_stop(x, i + 1, p);  // line 8
}

template<class T, size_type N, class Pred>
constexpr size_type do_find_if_or_stop(T (&x)[N], size_type i, Pred p) {
  return i == N ? N : do_find_if(x, i, p);
} // Line 14

template<class T, size_type N, class Pred>
constexpr size_type find_if(T (&x)[N], Pred p) {
	return do_find_if(x, 0, p); // Line 18
}

constexpr long items_long[] = {1, 2, 3, 4, -5, 6, -7, 8};

template<class T>
struct IsNegative {
	constexpr bool operator()(const T& x) {
		return x < T(0);
	}
};

constexpr auto pos1 = find_if(items_long, IsNegative<long>{}); // Line 30

static_assert(pos1 == 4, "find_if failure");
