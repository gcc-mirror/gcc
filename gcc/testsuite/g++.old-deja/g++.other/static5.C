// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  ~S();
};

inline void f()
{
  static S s;
  atexit (0); // { dg-error "" } implicit declaration
}



