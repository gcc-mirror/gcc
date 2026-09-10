/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void link_error ();

/* The transitive table was incorrect for x == y, y != z. */
void test1 (int x, int y, int z)
{
  if (x == y)
    if (y != z)
      if (x == z)
        link_error ();
}

/* The transitive table was incorrect for x != y, y == z. */
void test2 (int x, int y, int z)
{
  if (x != y)
    if (y == z)
      if (x == z)
        link_error ();
}
/* { dg-final { scan-tree-dump-not "link_error" "evrp" } } */
