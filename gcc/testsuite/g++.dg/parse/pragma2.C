// PR c++/17595

// Ideally, the #pragma error would come one line further down, but it
// does not.
int f(int x,
#pragma interface  // { dg-error "not allowed here" }
// { dg-bogus "two or more" "" { xfail *-*-* } .-1 }      
      // The parser gets confused and issues an error on the next line.
      int y);
