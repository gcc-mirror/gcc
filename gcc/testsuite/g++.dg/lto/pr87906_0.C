// { dg-lto-do link }
// { dg-require-effective-target fpic }
// { dg-lto-options { { -O -fPIC -flto } } }
// { dg-extra-ld-options "-shared -nostdlib" }

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
