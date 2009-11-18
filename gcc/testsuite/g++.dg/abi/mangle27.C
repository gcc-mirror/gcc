// Test of std::basic_iostream<char, std::char_traits<char> > mangling

// { dg-do compile }
// { dg-options "-fno-inline" }

namespace std {
  template<typename> struct char_traits;

  template<typename, typename> struct basic_iostream { basic_iostream(){} };
}

std::basic_iostream<char,std::char_traits<char> > s1;

// { dg-final { scan-assembler "\n_?_ZNSdC\[12\]Ev\[: \t\n\]" } }
