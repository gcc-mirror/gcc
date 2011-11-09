// Origin: PR c++/51027
// { dg-options "-std=c++0x" }

using INT = int // { dg-error "expected|;|at end of input" }
