// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  ~S();
};

inline void f()
{
  static S s;
  atexit (0); // ERROR - implicit declaration
}



