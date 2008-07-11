// PR c++/10603
// { dg-options "-fshow-column" }

int f(int not) {
  return 1-not;
} 

// { dg-error "11: error: expected ',' or '...' before '!' token" "" { target *-*-* } { 4 } }

// { dg-error "15: error: expected primary\\-expression before ';' token" "" { target *-*-* }  { 5 } }

