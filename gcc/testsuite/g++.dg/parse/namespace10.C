// PR c++/16489

namespace m {} // { dg-error "" }

namespace n {
  namespace m {}
}

namespace m = n::m; // { dg-error "" }
