// { dg-do assemble  }

namespace Jazz {
  int horn( int h ) { return 1; }
}

using Jazz::horn;

namespace Jazz {
  int horn ( char c ) { return 0; }
}
