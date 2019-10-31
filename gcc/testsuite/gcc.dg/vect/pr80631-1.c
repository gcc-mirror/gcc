/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* PR tree-optimization/80631 */

#include "tree-vect.h"

int v[8] = { 77, 1, 79, 3, 4, 3, 6, 7 };

__attribute__((noipa)) void
f1 (void)
{
  int k, r = -1;
  for (k = 0; k < 8; k++)
    if (v[k] == 77)
      r = k;
  if (r != 0)
    abort ();
}

__attribute__((noipa)) void
f2 (void)
{
  int k, r = 4;
  for (k = 0; k < 8; k++)
    if (v[k] == 79)
      r = k;
  if (r != 2)
    abort ();
}

__attribute__((noipa)) void
f3 (void)
{
  int k, r = -17;
  for (k = 0; k < 8; k++)
    if (v[k] == 78)
      r = k;
  if (r != -17)
    abort ();
}

__attribute__((noipa)) void
f4 (void)
{
  int k, r = 7;
  for (k = 0; k < 8; k++)
    if (v[k] == 78)
      r = k;
  if (r != 7)
    abort ();
}

__attribute__((noipa)) void
f5 (void)
{
  int k, r = -1;
  for (k = 0; k < 8; k++)
    if (v[k] == 3)
      r = k;
  if (r != 5)
    abort ();
}

int
main ()
{
  check_vect ();
  f1 ();
  f2 ();
  f3 ();
  f4 ();
  f5 ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 5 "vect" { target vect_condition } } } */
/* { dg-final { scan-tree-dump-times "optimizing condition reduction with FOLD_EXTRACT_LAST" 5 "vect" { target vect_fold_extract_last } } } */
/* { dg-final { scan-tree-dump-times "condition expression based on integer induction." 5 "vect" { target { { ! vect_fold_extract_last } && vect_condition } } } } */
