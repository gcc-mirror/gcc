// PR c++/48453, DR 1287
// { dg-do compile { target c++11 } }

template<class T>
T&& create();

template<class T, class Arg>
void test() {
  T t(create<Arg>());
  (void) t;
}

template<class T>
struct To {
  explicit operator T();
};

int main()
{
  test<int&, To<int&>>();
  test<int&&, To<int&&>>();
}
