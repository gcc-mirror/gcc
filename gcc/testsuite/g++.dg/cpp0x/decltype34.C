// PR c++/50870
// { dg-do compile { target c++11 } }

struct impl
{
  template <class T> static T create();
};

template<class T, class U,
	 class = decltype(impl::create<T>()->impl::create<U>())>
struct tester{};

tester<impl*, int> ti;

template<class T, class U,
	 class = decltype(impl::create<T>()->impl::create<U>())>
int test() { return 0; }

int i = test<impl*, int>();
