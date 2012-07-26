__attribute__ ((__noreturn__)) extern void fail (void);

char x;

/* This used to get stuck in an infinite loop in find_comparison_args
   when compiling this function for MIPS at -O2.  */

void foo (const unsigned char y)
{
   ((void) (__builtin_expect((!! y == y), 1) ? 0 : (fail (), 0)));
   x = ! y;
}

/* This used to similarly get stuck when compiling for PowerPC at -O2.  */

int foo2 (int arg)
{
  if (arg != !arg)
    fail ();
  if (arg)
    fail ();
}
