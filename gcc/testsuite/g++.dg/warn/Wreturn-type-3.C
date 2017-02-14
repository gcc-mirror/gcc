// PR middle-end/19583
// { dg-options "-Wreturn-type -O" }

struct E{};

inline int bar()
#if __cplusplus <= 201402L
throw(E)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
{
  return 0;
}

void foo ()
{
  bar();
}
