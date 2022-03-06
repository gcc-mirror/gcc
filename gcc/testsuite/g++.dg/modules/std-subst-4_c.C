// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
module;
# 5 __FILE__ 1
namespace std {
template <typename A> struct char_traits {};
} // namespace std
# 9 "" 2
export module Bar;
// { dg-module-cmi Bar }
import RenameString;

// Use Ss as this is global-module std::char_traits
void g(str<char, std::char_traits<char>> &s) {
}

// { dg-final { scan-assembler {_ZW3Bar1gRSs:} } }
