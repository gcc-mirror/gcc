// PR c++/120023
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M.S }

export module M.S;

namespace ns {
  export template <typename T> struct S;
}
