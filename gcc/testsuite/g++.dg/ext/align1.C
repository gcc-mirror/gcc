// Test that __attribute__ ((aligned)) is preserved.

extern "C" int printf (const char *, ...);

typedef float at[4][4] __attribute__ ((aligned (64)));

float dummy[4][4][15];

static volatile at a1[15];

float f1 __attribute__ ((aligned (64)));

int main()
{
  printf ("%d %d\n", __alignof (a1), __alignof (f1));
  return (__alignof (a1) < __alignof (f1));
}
