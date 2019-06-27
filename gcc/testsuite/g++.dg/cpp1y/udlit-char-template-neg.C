// { dg-do compile { target { c++11 && { ! c++14 } } } }

template<typename CharT, CharT... String>
  int
  operator"" _script() // { dg-error "3:literal operator template" }
  { return 42; }

int i = "hi!"_script;
