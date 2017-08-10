// PR c++/17595

// Ideally, the #pragma error would come one line further down, but it
// does not.
int f(int x,
#pragma interface  // { dg-error "not allowed here" }
      // { dg-bogus "expected identifier" "" { xfail *-*-* } .-1 }
      int y);
