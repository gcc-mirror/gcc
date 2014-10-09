// { dg-do compile { target { c++11 && { ! c++14 } } } }

template<typename CharT, CharT... String>
  int
  operator"" _script()
  { return 42; } // { dg-error "literal operator template|has invalid parameter list" }

int i = "hi!"_script;
