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
class RuntimeException : b {};
} // namespace uno
class C : uno::RuntimeException {};
} // namespace star
} // namespace moon
} // namespace com
using com::moon::star::C;
using com::moon::star::uno::RuntimeException;
void d() { throw RuntimeException(); }
void e() { C(); }
