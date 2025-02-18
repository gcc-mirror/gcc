// PR c++/110822
// { dg-do compile { target c++11 } }

void __ostream_insert(const char*);
struct basic_string {
  const char* _M_p;
  char _M_local_buf[16] = {};
  constexpr basic_string() : _M_p(_M_local_buf) {}
  const char *data() const { return _M_p; }
};
constexpr basic_string f() { return {}; }
constexpr basic_string text = f();
int main() {
  __ostream_insert(text._M_p);
}
