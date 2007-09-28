// PR c++/10179

struct __attribute((aligned(__alignof(double)))) A
{ /* empty */ };

struct T : public A
{
  char c;
};

template<bool> struct StaticAssert;
template<> struct StaticAssert<true> {};

StaticAssert<__alignof(T) == __alignof(double)> d;
