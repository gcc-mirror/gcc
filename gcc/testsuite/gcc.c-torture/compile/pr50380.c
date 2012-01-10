/* This test used to get stuck in an infinite loop in find_comparison_args
   when compiling for MIPS at -O2.  */

__attribute__ ((__noreturn__)) extern void fail (void);

char x;

void foo (const unsigned char y)
{
   ((void) (__builtin_expect((!! y == y), 1) ? 0 : (fail (), 0)));
   x = ! y;
}
