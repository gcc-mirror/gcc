// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>
// Special g++ Options: -O2

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


