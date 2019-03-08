// PR c++/63540
// { dg-do compile { target c++11 } }

template <typename T,
	  typename = decltype(*static_cast<T*>(0) = 0)> int break_it();
template <typename> int break_it();

struct Base {
  Base(const Base &);
  void operator=(Base &&);
};

struct Derived : Base {
  using Base::operator=;
};

int a = break_it<Derived>();
Derived v(v);
