// PR c++/17595

// Ideally, the #pragma error would come one line further down, but it
// does not.
int f(int x, // { dg-error "not allowed here" }
#pragma interface 
      // The parser gets confused and issues an error on the next line.
      int y); // { dg-bogus "" "" { xfail *-*-* } } 
