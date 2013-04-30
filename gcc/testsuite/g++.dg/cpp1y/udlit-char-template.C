// { dg-options -std=c++1y }

template<typename CharT, CharT... String>
  int
  operator"" _script()
  { return 42; }

int i = "hi!"_script;
int i8 = u8"hi!"_script;
int iw = L"hi!"_script;
int i16 = u"hi!"_script;
int i32 = U"hi!"_script;
