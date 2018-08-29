// PR c++/85761
// { dg-do compile { target c++11 } }

template <typename T>
void out(const T& value);

struct foo {
  void bar();
};

void foo::bar()
{
  constexpr int COUNT = 10000;
  auto run = []() {
    out(COUNT);		// { dg-error "9:not captured" }
  };

  run();
}
