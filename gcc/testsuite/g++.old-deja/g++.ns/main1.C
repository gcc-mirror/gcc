// Build don't link:

// main is only reserved in the global namespace [basic.start.main]/3

// submitted by Gerald Gutierrez <gutier@intergate.bc.ca>

// excess errors test - XFAIL *-*-*

namespace A { void main () { } }
namespace B { void main () { } }
namespace C {
  void main () { }
  namespace D {
    void main () { }
  }
}

