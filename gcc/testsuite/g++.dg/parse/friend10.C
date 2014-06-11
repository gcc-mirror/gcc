// PR c++/19200

namespace N {
  void S();
}

struct S {
  friend void N::S();
};
