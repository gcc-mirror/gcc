// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  ~S();
};

inline void f()
{
  static S s;
  atexit (0); // { dg-error "3:'atexit' was not declared" } implicit declaration
}



