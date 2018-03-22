// PR c++/17595

// Ideally, the #pragma error would come one line further down, but it
// does not.
int f(int x,
#pragma interface  // { dg-error "not allowed here" }
      // The parser gets confused and issues an error on the next line.
      int y); // { dg-bogus "" "" { xfail *-*-* } } 
