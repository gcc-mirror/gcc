// { dg-do compile { target c++14 } }
// { dg-options "-Wunused" }

template <typename F>
void f (F &&d)
{
  static unsigned context;
  d(context);
}

void g ()
{
  static int b;
  f([](auto c) { return c <= b; });
}

void h ()
{
  static int b = 0; // { dg-warning "unused variable" }
  f([](auto c) { return c <= 0; });
}

void i ()
{
  static int b = 0;
  [](auto c) { return c <= b; };
}

void j ()
{
  static int b = 0; // { dg-warning "unused variable" }
  [](auto c) { return c <= 0; };
}
