// { dg-do compile { target { c++11 && { ! c++1y } } } }

template<typename CharT, CharT... String>
  int
  operator"" _script()
  { return 42; } // { dg-error "literal operator template|has invalid parameter list" }

int i = "hi!"_script;
