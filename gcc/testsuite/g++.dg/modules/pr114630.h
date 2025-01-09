template <typename _CharT>
void _M_do_parse() {
  struct A {};
  struct B {};
  int x;
}

template <typename> struct formatter;
template <> struct formatter<int> {
  void parse() { _M_do_parse<int>(); }
};
