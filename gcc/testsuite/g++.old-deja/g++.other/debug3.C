// { dg-do assemble  }
// { dg-options "-O2" }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  ~S();
};

inline void f()
{
  static S s;
}

typedef void (*fn_t)();

fn_t g()
{
  return &f;
}


