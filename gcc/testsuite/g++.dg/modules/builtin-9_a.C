// Test that the built-in clog doesn't interfere with redeclaring the import.

// { dg-additional-options "-fmodules -Wno-global-module" }

module;

namespace std {
  class ostream;
  extern ostream clog;
}

export module M;

namespace std {
  export using std::clog;
}
