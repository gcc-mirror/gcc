// PR 84497 mismatch with thread constructor fn weakness
// { dg-do compile { target c++11 } }
// { dg-require-weak "" }
// { dg-require-alias "" }
// { dg-skip-if "No .weak" { { hppa*-*-hpux* } && { ! lp64 } } }

struct Base
{
  int m;

  Base() noexcept = default;  // trivial but not constexpr
  ~Base() noexcept = default;
};

struct Derived : Base {};
struct Container {
  Base m;
};

#ifdef DEF
// This bit for exposition only.
// All items placed in .tbss
// __tls_init simply sets __tls_guard
// no aliases to __tls_init generated
thread_local Base base_obj;
thread_local Derived derived_obj;
thread_local Container container_obj;
#else
// Erroneously created strong undef refs to
// _ZTH11derived_obj, _ZTH13container_obj, _ZTH8base_obj
extern thread_local Base base_obj;
extern thread_local Derived derived_obj;
extern thread_local Container container_obj;
int main() { return !(&base_obj && &derived_obj && &container_obj);}
#endif

// { dg-final { scan-assembler ".weak\[ \t\]*_ZTH8base_obj" } }
// { dg-final { scan-assembler ".weak\[ \t\]*_ZTH11derived_obj" } }
// { dg-final { scan-assembler ".weak\[ \t\]*_ZTH13container_obj" } }
