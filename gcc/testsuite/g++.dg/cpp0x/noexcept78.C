// PR c++/109761
// { dg-do compile { target c++11 } }

struct base {
  virtual void foo() noexcept { }
  virtual ~base() { }
};

struct outer : base {
  struct nested {
    void foo() noexcept(noexcept(g())); // { dg-bogus "looser" }
    ~nested() noexcept(noexcept(g()));  // { dg-bogus "looser" }
  };
  static void g();
};

