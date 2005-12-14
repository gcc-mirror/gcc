// { dg-do assemble }

// Based on bug report by Simon A. Crase <s.crase@ieee.org>


struct foo {
  template <class T> void bar();
};

template void foo::bar<void>(); // { dg-bogus "" "" { xfail *-*-* } }  - ICE - 
