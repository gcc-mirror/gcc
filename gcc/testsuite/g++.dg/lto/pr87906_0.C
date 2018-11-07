// { dg-lto-do link }
// { dg-lto-options { { -O -fPIC -flto } } }
// { dg-extra-ld-options "-shared -nostdlib" }

namespace com {
namespace sun {
namespace star {}
} // namespace sun
} // namespace com
namespace a = com::sun::star;
namespace com {
namespace sun {
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
} // namespace sun
} // namespace com
template <typename> void d(int) { throw a::uno::RuntimeException(); }
int f;
void g() { d<a::uno::b>(f); }
