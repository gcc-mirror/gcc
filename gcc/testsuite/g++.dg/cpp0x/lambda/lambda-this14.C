// PR c++/52014
// { dg-require-effective-target c++11 }

template <class Iterator, class Func>
void for_each(const Iterator first, const Iterator last, Func func)
{
  for (Iterator it = first; it != last; ++it) {
    func(*it);
  }
}

template <class T>
struct helper
{
  typedef typename T::size_type type;
};

template <class T>
struct helper<T&>
{
  typedef typename T::size_type type;
};

template <class T>
struct helper<T*>
{
  typedef typename T::size_type type;
};

struct bar
{
  struct foo
  {
    typedef int size_type;
  } foo_;

  void test()
  {
    int arr[] = { 1, 2, 3 };
    for_each(arr, arr + 3, [&](helper<foo>::type i) {
	for_each(arr, arr + 3, [&](helper<decltype(foo_)>::type j) { });
      });
  }
};

int main()
{
  return 0;
}
