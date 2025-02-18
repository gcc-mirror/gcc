// PR c++/114913
// { dg-do compile { target c++11 } }

struct strt {
  char *_M_dataplus;
  char _M_local_buf = 0;
  constexpr strt()
    : _M_dataplus(&_M_local_buf) {}
  constexpr strt(const strt &)
    : _M_dataplus(&_M_local_buf) {}
};

constexpr strt
f ()
{
  return {};
}
constexpr strt HelloWorld = f();
const char *a() { return HelloWorld._M_dataplus; }
