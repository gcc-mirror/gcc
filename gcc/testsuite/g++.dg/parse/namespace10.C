// PR c++/16529

namespace m {} // { dg-message "" }

namespace n {
  namespace m {}
}

namespace m = n::m; // { dg-error "" }
