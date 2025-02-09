/* { dg-do compile } */
/* { dg-additional-options "-foffload=disable -fdump-tree-gimple" } */

/* Check that variants within a "begin declare variant" directive 
   are attached to the correct overloaded function.  */

int f (int x) { return x; }

#pragma omp begin declare variant match (implementation={vendor("gnu")})
int f (int x) { return -1; }
#pragma omp end declare variant

int f (int x, int y) { return x * y; }

#pragma omp begin declare variant match (construct={target})
int f (int x, int y) { return -2; }
#pragma omp end declare variant

int f (int x, int y, int z) { return x * y * z; }

#pragma omp begin declare variant match (device={kind("host")})
int f (int x, int y, int z) { return -3; }
#pragma omp end declare variant

int main (void)
{
  if (f (10) != -1) __builtin_abort ();
  if (f (10, 20) != 200) __builtin_abort ();   /* no match on this one */
  if (f (10, 20, 30) != -3) __builtin_abort ();
}

/* { dg-final { scan-tree-dump "f\\.ompvariant. \\(10\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "f \\(10, 20\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "f\\.ompvariant. \\(10, 20, 30\\)" "gimple" } } */





