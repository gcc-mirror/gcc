/* { dg-do run } */

/* Check bitfields and non-bitfields are aligned & sized similarly.

   Copyright (C) 2002 Free Software Foundation Inc
   Contributed by Nathan Sidwell <nathan@codesourcery.com>
*/

#include <limits.h>
#include <stdio.h>

static int fail;

#define CHECK1(N, T) do { \
  typedef struct Field_##N { char c; T f; } Field_##N; \
  typedef struct BitField_##N { char c; T f : sizeof (T) * CHAR_BIT; } BitField_##N; \
  if (sizeof (Field_##N) != sizeof (BitField_##N)) { \
    fail = 1; printf ("sizeof %s failed\n", #T); \
  } \
  if (__alignof__ (Field_##N) != __alignof__ (BitField_##N)) { \
    fail = 1; printf ("__alignof__ %s failed\n", #T); \
  } \
} while (0)

#define CHECK(N, T) do { \
  CHECK1(N, T); \
  CHECK1 (s##N, signed T); \
  CHECK1 (u##N, unsigned T); \
} while (0)
 
int main ()
{
  
  CHECK (c, char);
  CHECK (s, short);
  CHECK (i, int);
  CHECK (l, long);
  CHECK (ll, long long);
  
  return fail;
}
