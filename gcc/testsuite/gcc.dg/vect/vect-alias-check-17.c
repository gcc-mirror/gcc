/* { dg-do compile } */
/* { dg-require-effective-target vect_load_lanes } */

struct s { int x[100]; };

void
f (struct s *s1, int a, int b)
{
  for (int i = 0; i < 32; ++i)
    s1->x[a + i] = s1->x[b + i * 2] + s1->x[b + i * 3];
}

/* { dg-final { scan-tree-dump {flags: *[^\n]*MIXED_STEPS} "vect" } } */
/* { dg-final { scan-tree-dump "using an address-based overlap test" "vect" } } */
/* { dg-final { scan-tree-dump-not "using an index-based" "vect" } } */
