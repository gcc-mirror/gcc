/* PR tree-optimization/91294 - strlen result of a conditional with
   an offset
   { dg-do run }
   { dg-options "-O2 -Wall" } */

#include "strlenopt.h"

#define NOIPA __attribute__ ((noclone, noinline, noipa))

#define assert(expr)						\
  ((expr)                                                       \
   ? (void)0                                                    \
   : (__builtin_printf ("line %i %s: assertion failed: %s\n",	\
                        __LINE__, __func__, #expr),		\
      __builtin_abort ()))

int i = 0;

const char s[] = "1234567";

char a[32];

NOIPA void lower_bound_assign_into_empty (void)
{
  a[0] = '1';
  a[1] = '2';
  a[2] = '3';
  assert (strlen (a) == 3);
}

NOIPA void lower_bound_assign_into_longest (void)
{
  a[0] = '1';
  a[1] = '2';
  a[2] = '3';
  assert (strlen (a) == 31);
}


NOIPA void lower_bound_assign_into_empty_idx_3 (int idx)
{
  a[0] = '1';
  a[1] = '2';
  a[2] = '3';
  a[idx] = 'x';
  assert (strlen (a) == 4);
}

NOIPA void lower_bound_assign_into_longest_idx_2 (int idx)
{
  a[0] = '1';
  a[1] = '2';
  a[2] = '3';
  a[idx] = '\0';
  assert (strlen (a) == 2);
}


NOIPA void lower_bound_memcpy_into_empty (void)
{
  memcpy (a, "123", 3);
  assert (strlen (a) == 3);
}

NOIPA void lower_bound_memcpy_into_longest (void)
{
  memcpy (a, "123", 3);
  assert (strlen (a) == 31);
}


NOIPA void lower_bound_memcpy_memcpy_into_empty (void)
{
  memcpy (a, "123", 3);
  memcpy (a + 2, "345", 3);
  assert (strlen (a) == 5);
}

NOIPA void lower_bound_memcpy_memcpy_into_longest (void)
{
  memcpy (a, "123", 3);
  memcpy (a + 2, "345", 3);
  assert (strlen (a) == 31);
}


NOIPA void memove_forward_strlen (void)
{
  char a[] = "123456";

  memmove (a, a + 1, sizeof a - 1);

  assert (strlen (a) == 5);
}

NOIPA void memove_backward_into_empty_strlen (void)
{
  strcpy (a, "123456");

  memmove (a + 1, a, 6);

  assert (strlen (a) == 7);
}

NOIPA void memove_backward_into_longest_strlen (void)
{
  memcpy (a, "123456", 6);

  memmove (a + 1, a, 6);

  assert (strlen (a) == 31);
}

NOIPA void memove_strcmp (void)
{
  /* Test derived from libstdc++-v3's
     20_util/specialized_algorithms/memory_management_tools/1.cc  */

  char a[] = "123456";
  char b[] = "000000";

  memmove (b, a, sizeof a);

  assert (strlen (a) == 6);
  assert (strlen (b) == 6);
  assert (strcmp (a, b) == 0);
}


int main (void)
{
  memset (a, '\0', sizeof a);
  lower_bound_assign_into_empty ();

  memset (a, 'x', sizeof a - 1);
  a[sizeof a - 1] = '\0';
  lower_bound_assign_into_longest ();

  memset (a, '\0', sizeof a);
  lower_bound_assign_into_empty_idx_3 (3);

  memset (a, 'x', sizeof a - 1);
  a[sizeof a - 1] = '\0';
  lower_bound_assign_into_longest_idx_2 (2);

  memset (a, '\0', sizeof a);
  lower_bound_memcpy_into_empty ();

  memset (a, 'x', sizeof a - 1);
  a[sizeof a - 1] = '\0';
  lower_bound_memcpy_into_longest ();

  memset (a, 'x', sizeof a - 1);
  a[sizeof a - 1] = '\0';
  lower_bound_memcpy_into_longest ();

  memset (a, '\0', sizeof a);
  lower_bound_memcpy_memcpy_into_empty ();

  memset (a, 'x', sizeof a - 1);
  a[sizeof a - 1] = '\0';
  lower_bound_memcpy_memcpy_into_longest ();

  memove_forward_strlen ();

  memset (a, '\0', sizeof a);
  memove_backward_into_empty_strlen ();

  memset (a, 'x', sizeof a - 1);
  a[sizeof a - 1] = '\0';
  memove_backward_into_longest_strlen ();

  memove_strcmp ();
}
