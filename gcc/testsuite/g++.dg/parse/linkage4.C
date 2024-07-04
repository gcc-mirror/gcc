// PR c++/114917
// { dg-do compile }

extern "C++" namespace ns {
  struct Incomplete;
  Incomplete foo;  // { dg-error "incomplete type" }
}

extern "C" extern "C" {
  static int bar;  // { dg-bogus "invalid" }
}
