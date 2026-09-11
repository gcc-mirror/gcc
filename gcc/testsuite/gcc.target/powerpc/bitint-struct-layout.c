/* { dg-do run { target bitint } } */
/* { dg-additional-options "-std=c23" } */

#include <stddef.h>

struct S1
{
  char c;
  _BitInt(65) x;
};

struct S2
{
  _BitInt(65) x;
  char c;
};

struct S3
{
  int i;
  _BitInt(128) x;
};

struct S4
{
  _BitInt(128) x;
  int i;
};

static void
check_s1 (void)
{
  if (__alignof__ (struct S1) != __alignof__ (_BitInt(65)))
    __builtin_abort ();

  if (offsetof (struct S1, x) % __alignof__ (_BitInt(65)) != 0)
    __builtin_abort ();

  if (sizeof (struct S1) < offsetof (struct S1, x) + sizeof (_BitInt(65)))
    __builtin_abort ();
}

static void
check_s2 (void)
{
  if (__alignof__ (struct S2) != __alignof__ (_BitInt(65)))
    __builtin_abort ();

  if (offsetof (struct S2, x) != 0)
    __builtin_abort ();
}

static void
check_s3 (void)
{
  if (__alignof__ (struct S3) != __alignof__ (_BitInt(128)))
    __builtin_abort ();

  if (offsetof (struct S3, x) % __alignof__ (_BitInt(128)) != 0)
    __builtin_abort ();
}

static void
check_s4 (void)
{
  if (offsetof (struct S4, x) != 0)
    __builtin_abort ();
}

int
main (void)
{
  check_s1 ();
  check_s2 ();
  check_s3 ();
  check_s4 ();
  return 0;
}

