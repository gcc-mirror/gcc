// PR c++/88061
// { dg-do compile { target { c++14 && named_sections } } }

template<class T>
[[gnu::section(".foo")]] int var = 42;

template int var<int>;

// { dg-final { scan-assembler {\.(section|csect)[ \t]+"?\.foo} } }
