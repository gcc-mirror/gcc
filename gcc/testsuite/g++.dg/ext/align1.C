// Test that __attribute__ ((aligned)) is preserved.
// Cygwin does not support alignment greater than 16

extern "C" int printf (const char *, ...);

typedef float at[4][4] __attribute__ ((aligned (64)));

float dummy[4][4][15];

static volatile at a1[15];
// { dg-bogus "object file alignment" "" { xfail i?86-pc-cygwin } 10 }

float f1 __attribute__ ((aligned (64)));
// { dg-bogus "object file alignment" "" { xfail i?86-pc-cygwin } 13 }

int main()
{
  printf ("%d %d\n", __alignof (a1), __alignof (f1));
  return (__alignof (a1) < __alignof (f1));
}
