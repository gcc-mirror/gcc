// { dg-additional-options -fmodules-ts }

export module Foo;
// { dg-module-cmi Foo }
import RenameString;

namespace std {
template <typename T> struct char_traits {};
} // namespace std

// use Sb mangling, not Ss as this is not global-module std::char_traits.
// { dg-final { scan-assembler {_ZW3Foo1fRSbIcStS_11char_traitsIcESaIcEE:} } }
void f(str<char, std::char_traits<char>> &s) {
}
