/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

typedef long vec __attribute__ ((vector_size (2 * sizeof (long))));

long f (long d, long e)
{
  vec x = { d, e };
  vec m = { 1, 0 };
  return __builtin_shuffle (x, m) [1];
}


/* { dg-final { scan-tree-dump-not "BIT_FIELD_REF" "forwprop1" } } */
