// Test of std::basic_istream<char, std::char_traits<char> > mangling

// { dg-do compile }
// { dg-options "-fno-inline" }

namespace std {
  template<typename> struct char_traits;

  template<typename, typename> struct basic_istream { basic_istream(){} };
}

std::basic_istream<char,std::char_traits<char> > s1;

// { dg-final { scan-assembler "\n_?_ZNSiC1Ev\[: \t\n\]" } }
