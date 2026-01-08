// PR c++123393
// { dg-additional-options "-fmodules" }
// { dg-module-cmi m }

export module m;
import fmt;
inline void use() {
  fmt::format("");
}
