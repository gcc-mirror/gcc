// PR c++/117501
// { dg-do run { target c++20 } }

constexpr unsigned
length ()
{
  bool __trans_tmp_1 = __builtin_is_constant_evaluated();
  if (__trans_tmp_1)
    return 42;
  return 1;
}
struct basic_string_view {
  constexpr basic_string_view(const char *) : _M_len{length()}, _M_str{} {}
  long _M_len;
  char _M_str;
};
struct QQQ {
  consteval QQQ(basic_string_view d) : data(d) {}
  basic_string_view data;
};
int
main ()
{
  QQQ q("");
  if (q.data._M_len != 42)
    __builtin_abort ();
}
