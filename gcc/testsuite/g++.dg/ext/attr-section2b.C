// PR c++/88061
// { dg-do compile { target { c++11 && named_sections } } }

template<class T>
int* fun() {
  [[gnu::section(".foo")]] static int var;
  return &var;
};

template int* fun<int>();

// { dg-final { scan-assembler {\.(section|csect)[ \t]+"?\.foo} } }
