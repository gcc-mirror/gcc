// PR c++/79488
// { dg-do compile { target c++11 } }

int f();
static int g __attribute__((__weakref__("f")));

template <typename Fn> struct res {
  static Fn val();
  using type = decltype(val()()); // { dg-error "no match for call" }
};

template <typename Fn> struct A {
  template <typename T> void set_result(T) {}

  virtual void run() {
    auto boundfn = []() -> typename res<Fn>::type{};
    set_result(boundfn);
  }
};

struct F {
  void operator()() &;
};

A<F> t;
