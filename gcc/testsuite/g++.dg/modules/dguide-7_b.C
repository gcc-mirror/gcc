// PR c++/120023
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M.D }

export module M.D;
import M.S;

namespace ns {
  S(int) -> S<int>;
}
