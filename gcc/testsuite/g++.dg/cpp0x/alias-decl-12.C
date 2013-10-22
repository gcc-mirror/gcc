// Origin: PR c++/51027
// { dg-options "-std=c++11" }

using INT = int // { dg-error "expected|;|at end of input" }
