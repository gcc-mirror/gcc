// { dg-lto-do link }
// Explicit { dg-require-effective-target exceptions_enabled } so that dependent tests don't turn UNRESOLVED for '-fno-exceptions'.
// { dg-require-effective-target fpic }
// { dg-require-effective-target shared }
// { dg-lto-options { { -O -fPIC -flto } } }
// { dg-extra-ld-options "-shared -nostdlib" }
// { dg-extra-ld-options "-shared -nostdlib -Wl,-undefined,dynamic_lookup -lSystem" { target *-*-darwin[12][0-9]* } }
// { dg-extra-ld-options "-shared -nostdlib -Wl,-undefined,dynamic_lookup -ldylib1.o -lSystem" { target *-*-darwin[89]* } }
namespace com {
namespace moon {
namespace star {}
} // namespace moon
} // namespace com
namespace a = com::moon::star;
namespace com {
namespace moon {
namespace star {
namespace uno {
class a {
public:
  ~a();
};

class b {
public:
  ~b();
  a c;
};
class c {
  b e;
};
class RuntimeException : b {};
} // namespace uno
} // namespace star
} // namespace moon
} // namespace com
template <typename> void d(int) { throw a::uno::RuntimeException(); }
int f;
void g() { d<a::uno::b>(f); }
