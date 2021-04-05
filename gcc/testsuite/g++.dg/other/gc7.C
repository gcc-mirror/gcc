// PR c++/91416 - GC during late parsing collects live data.
// { dg-do compile }
// { dg-options "--param ggc-min-heapsize=0 --param ggc-min-expand=0" }

__attribute__ ((unused)) struct S {
  S() { }
} s;

__attribute__ ((unused)) struct X {
  void fn ()
  {
    __attribute__ ((unused)) struct N {
	N() { }
    } n;
  }
} x;
