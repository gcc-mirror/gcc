// PR c++/12827
// { dg-options "-fshow-column" }

void f(int x
       int y);

// { dg-error "8:expected ',' or '...' before 'int'" "" { target *-*-* } 5 }
