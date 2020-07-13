template<typename T> struct foo { virtual void f() = 0; };
extern foo<__Int8x8_t> &x;
void f() { x.f(); }
