// C++26 P3598R0 - CWG 3158 - const-ification of Splice Expressions
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <map>
#include <meta>

template <typename T1, typename T2>
auto
foo (const T1 &t1, const T2 &t2)
  pre([: is_integral_type (^^T1) ? ^^t2 : ^^t1 :]
      [ [: is_integral_type (^^T1) ? ^^t1 : ^^t2 :] ] >= 0)	// { dg-error "no match for 'operator\\\[\\\]' in 't1\\\[t2\\\]' \\\(operand types are 'const std::map<int, int>' and 'const int'\\\)" }
{								// { dg-error "no match for 'operator\\\[\\\]' in 't2\\\[t1\\\]' \\\(operand types are 'const std::map<int, int>' and 'const int'\\\)" "" { target *-*-* } .-1 }
  constexpr auto key = is_integral_type (^^T1) ? ^^t1 : ^^t2;
  constexpr auto container = is_integral_type (^^T1) ? ^^t2 : ^^t1;
  return [: container :][ [: key :] ];				// { dg-error "no match for 'operator\\\[\\\]' in 't1\\\[t2\\\]' \\\(operand types are 'const std::map<int, int>' and 'const int'\\\)" }
}								// { dg-error "no match for 'operator\\\[\\\]' in 't2\\\[t1\\\]' \\\(operand types are 'const std::map<int, int>' and 'const int'\\\)" "" { target *-*-* } .-1 }

void
bar (std::map <int, int> &m)
{
  foo (m, 5);
  foo (5, m);
}

// { dg-error "cannot bind rvalue reference of type 'std::map<int, int>::key_type\\\&\\\&' {aka 'int\\\&\\\&'} to lvalue of type 'const int'" "" { target *-*-* } 0 }
