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
class RuntimeException : b {};
} // namespace uno
class C : uno::RuntimeException {};
} // namespace star
} // namespace sun
} // namespace com
using com::sun::star::C;
using com::sun::star::uno::RuntimeException;
void d() { throw RuntimeException(); }
void e() { C(); }
