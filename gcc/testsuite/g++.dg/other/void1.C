// PR c++/9278
// { dg-do compile }

typedef void VOID;

int foo(void);
int bar(VOID);                // { dg-error "type|invalid use" }

template<int> int foo(void);
template<int> int bar(VOID);  // { dg-error "type|invalid use" }

struct A
{
  int foo(void);
  int bar(VOID);              // { dg-error "type|invalid use" }
};
