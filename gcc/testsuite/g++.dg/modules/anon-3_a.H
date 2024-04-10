// PR c++/112580
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

template <typename _Out>
struct _Formatting_scanner {
  union {
    int _M_values = 42;
  };
  virtual int _M_format_arg() { return _M_values; }
};

template <typename _Tp>
auto __do_vformat_to() {
  _Formatting_scanner<int> s;
  return s;
}

inline auto vformat() {
  return __do_vformat_to<int>();
}
