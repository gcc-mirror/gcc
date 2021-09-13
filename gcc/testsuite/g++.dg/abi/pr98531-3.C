// { dg-do compile { target c++11 } }
// { dg-additional-options -fno-use-cxa-atexit }
// PR 98531  Making __cxa_atexit (or atexit) more visible means it
// must be consistent with the std library's declarations

extern "C" int atexit (void (*) (void));

struct C
{
  ~C () noexcept;
  C () noexcept;
};

C &frob ()
{
  static C c; // Requires atexit functionality

  return c;
}


