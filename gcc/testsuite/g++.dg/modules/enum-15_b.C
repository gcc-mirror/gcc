// PR c++/120824
// { dg-additional-options "-fmodules -Wno-global-module -std=c++20" }
// { dg-module-cmi !bad }

module;
enum { E };
namespace {
  enum { G };  // { dg-message "internal" }
}
export module bad;
import M;
inline void ok() {
  auto a = E;
  auto b = F;
}
inline void err() {  // { dg-error "TU-local" }
  auto c = G;
}
