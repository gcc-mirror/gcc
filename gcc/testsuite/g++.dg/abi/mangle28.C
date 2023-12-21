// Test of std::basic_istream<char, std::char_traits<char> > mangling

// { dg-do compile }
// { dg-options "-fno-inline -fabi-compat-version=0" }

namespace std {
  template<typename> struct char_traits;

  template<typename, typename> struct basic_istream { basic_istream(){} };
}

std::basic_istream<char,std::char_traits<char> > s1;

// { dg-final { scan-assembler "\n_?_ZNSiC\[12\]Ev\[: \t\n\]" } }
