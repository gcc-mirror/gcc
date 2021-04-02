// { dg-additional-options "-fmodules-ts" }

export module foo;
// { dg-module-cmi foo }

export namespace detail {
  using bob = int;
}

namespace elsewhere {
export namespace det = ::detail;
namespace ail = ::detail;

void frob (det::bob);

}
