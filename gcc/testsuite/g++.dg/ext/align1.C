// Test that __attribute__ ((aligned)) is preserved.
// The alignment used to be 64 but Cygwin does not
// support an alignment greater than 16 and COFF 
// not support an alignment greater than 4.

extern "C" int printf (const char *, ...);

typedef float at[4][4] __attribute__ ((aligned));

float dummy[4][4][15];

static volatile at a1[15];

float f1 __attribute__ ((aligned));

int
main (void)
{
  printf ("%d %d\n", __alignof (a1), __alignof (f1));
  return (__alignof (a1) < __alignof (f1));
}
