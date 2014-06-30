// PR c++/9278
// { dg-do compile }

typedef void VOID;

int foo(void);
int bar(VOID);

template<int> int foo(void);
template<int> int bar(VOID);

struct A
{
  int foo(void);
  int bar(VOID);
};
