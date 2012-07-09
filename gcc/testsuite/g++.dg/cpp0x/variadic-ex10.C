// { dg-options "-std=gnu++0x" }
template<typename... Types> struct Tuple { };

Tuple<> t0; // Types contains no arguments
Tuple<int> t1; // Types contains one argument: int
Tuple<int, float> t2; // Types contains two arguments: int and float
Tuple<0> error; // { dg-error "mismatch" "mismatch" }
// { dg-error "expected a type" "expected a type" { target *-*-* } 7 }
// { dg-error "in declaration" "in declaration" { target *-*-* } 7 }
