// { dg-do compile }

// Origin: Jay Cox <jaycox@gimp.org>

// PR c++/1016: Name lookup for injected friend class

class B;

namespace N {
  class A {
    friend class B;
    B* b;
  };
}
