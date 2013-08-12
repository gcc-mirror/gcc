// PR c++/16971

namespace N {
  int i; // { dg-message "" }
  // By checking for an explicit keyword on the next line we avoid
  // matching an ICE message.
  int i; // { dg-error "redefinition" }
}
