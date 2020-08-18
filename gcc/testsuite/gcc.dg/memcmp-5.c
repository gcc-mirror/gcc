/* PR middle-end/78257 - missing memcmp optimization with constant arrays
   { dg-do compile }
   { dg-options "-O -Wall -fdump-tree-optimized" } */

#define A "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" \
          "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" \
          "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" \
          "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" \
          "0"

const char a257[sizeof A - 1] = A;
const char a258[sizeof A] = A;

_Static_assert (sizeof A == 258);
_Static_assert (sizeof a257 == 257);

/* Verify that initializers longer than 256 characters (an internal limit
   on the size of a buffer used to store representations in) are handled.  */

void eq_256plus (void)
{
  int n = 0;

  n += __builtin_memcmp (a257,       A,       sizeof a257);
  n += __builtin_memcmp (a257 +   1, A +   1, sizeof a257 - 1);
  n += __builtin_memcmp (a257 +   2, A +   2, sizeof a257 - 2);
  n += __builtin_memcmp (a257 + 127, A + 127, sizeof a257 - 127);
  n += __builtin_memcmp (a257 + 128, A + 128, sizeof a257 - 128);
  n += __builtin_memcmp (a257 + 255, A + 255, 2);
  n += __builtin_memcmp (a257 + 256, A + 256, 1);

  n += __builtin_memcmp (a258,       A,       sizeof a257);
  n += __builtin_memcmp (a258 +   1, A +   1, sizeof a257 - 1);
  n += __builtin_memcmp (a258 +   2, A +   2, sizeof a257 - 2);
  n += __builtin_memcmp (a258 + 127, A + 127, sizeof a257 - 127);
  n += __builtin_memcmp (a258 + 128, A + 128, sizeof a257 - 128);
  n += __builtin_memcmp (a258 + 256, A + 256, 2);
  n += __builtin_memcmp (a258 + 257, A + 257, 1);

  if (n)
    __builtin_abort ();
}

#define X "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" \
          "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" \
          "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" \
          "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" \
          "1"

void lt_256plus (void)
{
  int n = 0;

  n += 0 >  __builtin_memcmp (a257,       X,       sizeof a257);
  n += 0 >  __builtin_memcmp (a257 +   1, X +   1, sizeof a257 - 1);
  n += 0 >  __builtin_memcmp (a257 +   2, X +   2, sizeof a257 - 2);
  n += 0 >  __builtin_memcmp (a257 + 127, X + 127, sizeof a257 - 127);
  n += 0 >  __builtin_memcmp (a257 + 128, X + 128, sizeof a257 - 128);
  n += 0 >  __builtin_memcmp (a257 + 255, X + 255, 2);
  n += 0 >  __builtin_memcmp (a257 + 256, X + 256, 1);

  n += 0 >  __builtin_memcmp (a258,       X,       sizeof a258);
  n += 0 >  __builtin_memcmp (a258 +   1, X +   1, sizeof a258 - 1);
  n += 0 >  __builtin_memcmp (a258 +   2, X +   2, sizeof a258 - 2);
  n += 0 >  __builtin_memcmp (a258 + 127, X + 127, sizeof a257 - 127);
  n += 0 >  __builtin_memcmp (a258 + 128, X + 128, sizeof a257 - 128);
  n += 0 >  __builtin_memcmp (a258 + 256, X + 256, 2);
  n += 0 == __builtin_memcmp (a258 + 257, X + 257, 1);

  if (n != 14)
    __builtin_abort ();
}
