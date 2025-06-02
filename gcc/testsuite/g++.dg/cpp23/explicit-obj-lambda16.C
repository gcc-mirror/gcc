// PR c++/113563
// { dg-do compile { target c++23 } }

struct S {
  int x_;
  void f() {
    [this](this auto) {
      this->x_ = 42;
      return this;
    }();
  }
};

struct R {
  int x;

  auto foo() {
    return [*this](this auto &self) {
      this->x = 4;
    };
  }
};


struct A
{
    int n;
    void fun()
    {
        auto _ = [&](this auto self) { return n; };
    }
};

struct B {
  int i = 42;
  int foo() {
    return [this](this auto &&self) { auto p = &i; return *p; }();
  }
};
