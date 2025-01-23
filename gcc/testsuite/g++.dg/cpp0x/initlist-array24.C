// PR c++/117985
// { dg-do compile { target c++20 } }

struct _Vector_impl {
  constexpr _Vector_impl() {}
};
struct _Vector_base {
  constexpr ~_Vector_base() {}
  _Vector_impl _M_impl;
};
struct vector : private _Vector_base {};
struct string {
  string();
};
struct VEC {
  vector pane{};
};
struct FOO {
  VEC screen[1]{};
  string debug_name;
};

int
main ()
{
  FOO{};
}
