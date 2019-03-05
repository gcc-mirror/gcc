/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

void exit(int);

int main(int argc, char **argv)
{
  switch (argc)
    {
    case 1:
      return 1;
    case 2:
      return 2;
    case 3:
      exit(1);
    case 4:
      exit(2);
    default:
      return 5;
    }

  return 10;
}

/* { dg-final { scan-tree-dump-times "combined heuristics of edge\[^:\]*: 33.30%" 3 "profile_estimate"} } */
/* { dg-final { scan-tree-dump-times "combined heuristics of edge\[^:\]*: 0.05%" 2 "profile_estimate"} } */
