// Test of std::basic_ostream<char, std::char_traits<char> > mangling

// { dg-do compile }
// { dg-options "-fno-inline" }

namespace std {
  template<typename> struct char_traits;

  template<typename, typename> struct basic_ostream { basic_ostream(){} };
}

std::basic_ostream<char,std::char_traits<char> > s1;

// { dg-final { scan-assembler "\n_?_ZNSoC1Ev\[: \t\n\]" } }
