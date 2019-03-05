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

/* { dg-final { scan-tree-dump-times "33.33%" 3 "profile_estimate"} } */
/* { dg-final { scan-tree-dump-times "0.00%" 3 "profile_estimate"} } */
