// { dg-options -std=c++0x }

template<class T>
T&& create();

template <class T, class U,
	  class = decltype(create<T>() = create<U>())
	  >
char test(int);

template <class, class>
double test(...);

int main() {
  test<int[], int[]>(0); // #1
}
