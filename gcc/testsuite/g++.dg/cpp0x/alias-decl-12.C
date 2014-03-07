// Origin: PR c++/51027
// { dg-do compile { target c++11 } }

using INT = int // { dg-error "expected|;|at end of input" }
