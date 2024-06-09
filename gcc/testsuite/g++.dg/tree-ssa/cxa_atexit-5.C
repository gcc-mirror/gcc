/* { dg-do compile { target c++20 } } */
/* { dg-options "-O2 -fdump-tree-dce2-details -fdump-tree-optimized" } */
// { dg-require-effective-target cxa_atexit }
/* PR tree-optimization/19661 */

/* The call to axexit should be removed as constant_init::~constant_init is a pure/const function call
   and there is no visible effect if constant_init::~constant_init() call does not happen.  */
/* This takes until DCE2 as constant_init::~constant_init is not figured out being pure/const until late. */

extern "C" int puts(const char*);

struct A
{
  constexpr A()  { }
  ~A() { puts("bye"); }
};

namespace
{
  struct constant_init
  {
    union {
      A obj;
    };
    constexpr constant_init() : obj() { }

    ~constant_init() { /* do nothing, union member is not destroyed */ }
  };


  // Single-threaded fallback buffer.
  constinit constant_init global;
}

extern "C" A* get() { return &global.obj; }

/* { dg-final { scan-tree-dump-times "Deleting : (?:__cxxabiv1::__cxa_atexit|__aeabiv1::__aeabi_atexit)" 1 "dce2" } } */
/* { dg-final { scan-tree-dump-not "(?:__cxa_atexit|__aeabi_atexit)" "optimized" } } */

