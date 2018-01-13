/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_load_lanes } */

void
f (int *__restrict a, int *__restrict b)
{
  for (int i = 0; i < 96; ++i)
    a[i] = b[i * 3] + b[i * 3 + 1] + b[i * 3 + 2];
}

/* { dg-final { scan-tree-dump-not "Data access with gaps" "vect" } } */
/* { dg-final { scan-tree-dump-not "epilog loop required" "vect" } } */
