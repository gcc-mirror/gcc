/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

int main(int argc, char **argv)
{
  switch (argc)
    {
    case 1:
      return 1;
    case 2:
      return 2;
    case 3:
      __builtin_abort();
    case 4:
      __builtin_abort();
    default:
      return 5;
    }

  return 10;
}

/* { dg-final { scan-tree-dump-times "combined heuristics of edge\[^:\]*: 33.3%" 3 "profile_estimate"} } */
/* { dg-final { scan-tree-dump-times "combined heuristics of edge\[^:\]*: 0.1%" 2 "profile_estimate"} } */
