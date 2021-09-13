// PR 99170, pending instantiation snafu
// { dg-additional-options {-fmodule-header} }
// { dg-module-cmi {} }
namespace STD {
class string {
public:
  template <typename T>
  string (const T *);
};
}
