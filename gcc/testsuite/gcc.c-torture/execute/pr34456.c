/* { dg-skip-if "requires qsort" { freestanding } }  */

#include <stdlib.h>

int __attribute__ ((noinline)) debug (void) { return 1; }
int errors;

struct s { int elt; int (*compare) (int); };

static int
compare (const void *x, const void *y)
{
  const struct s *s1 = x, *s2 = y;
  int (*compare1) (int);
  int elt2;

  compare1 = s1->compare;
  elt2 = s2->elt;
  if (elt2 != 0 && debug () && compare1 (s1->elt) != 0)
    errors++;
  return compare1 (elt2);
}

int bad_compare (int x) { return -x; }
struct s array[2] = { { 1, bad_compare }, { -1, bad_compare } };

int
main (void)
{
  qsort (array, 2, sizeof (struct s), compare);
  return errors == 0;
}
