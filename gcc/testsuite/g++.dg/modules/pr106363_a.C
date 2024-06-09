// PR c++/106363
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi pr106363.a }

export module pr106363.a;

namespace ns {
  export int x;
}
