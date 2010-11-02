// { dg-options -std=c++0x }
// { dg-do run }

namespace xstd {

typedef decltype(sizeof(char)) size_t;

template<class E>
class initializer_list {
private:
	size_t sz;
	const E* start;

public:
	typedef E value_type;
  typedef const E& reference;
  typedef const E& const_reference;
  typedef size_t size_type;
  typedef const E* iterator;
  typedef const E* const_iterator;

  constexpr initializer_list() : sz(), start(nullptr) {}

  template<size_t N>
  constexpr initializer_list(const E(&array)[N]) : sz(N), start(array) {}

  constexpr size_t size() { return sz; }

  constexpr const E* begin() { return start; }

  constexpr const E* end() { return start + sz; }
};

template<class E, size_t N>
constexpr initializer_list<E> make_list(const E(&array)[N]) {
	return initializer_list<E>(array);
}

template<class E>
E min(initializer_list<E> list)
{
  //	static_assert(list.size() > 0, "Invalid list");
	auto it = list.begin();
	E result = *it;
	for (++it; it != list.end(); ++it) {
		if (*it < result) {
			result = *it;
		}
	}
	return result;
}

}

constexpr int global_i[] = {2, 4, -5, 6, 10};
constexpr xstd::initializer_list<int> list(global_i);
#define SA(X) static_assert(X, #X)
SA(list.size() == 5);
SA(list.begin()[2] == -5);
SA(list.end()[-1] == 10);

int main() {
  if (xstd::min(xstd::make_list(global_i)) != -5)
    return 1;
}
