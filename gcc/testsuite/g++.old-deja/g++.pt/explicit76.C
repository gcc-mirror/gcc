// Build don't link:

// Based on bug report by Simon A. Crase <s.crase@ieee.org>

// crash test - XFAIL *-*-*

struct foo {
  template <class T> void bar();
};

template void foo::bar<void>(); // gets bogus error - ICE - XFAIL *-*-*
