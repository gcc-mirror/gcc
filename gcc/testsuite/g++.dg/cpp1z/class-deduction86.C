// PR c++/87709
// { dg-do compile { target c++17 } }

template <class T>
struct lit {
  lit(T) { }
};

template <class T>
int operator+(lit<T>, lit<T>) {
  return 0;
}

auto r2 = (lit(0)) + lit(0);

static_assert(sizeof(lit(0)));
