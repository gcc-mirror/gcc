// PR c++/16529

namespace m {} // { dg-error "" }

namespace n {
  namespace m {}
}

namespace m = n::m; // { dg-error "" }
