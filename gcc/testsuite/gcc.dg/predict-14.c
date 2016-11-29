/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

int main(int argc, char **argv)
{
  switch (argc)
    {
    case 1:
      __builtin_unreachable();
    case 4:
      __builtin_unreachable();
    default:
      __builtin_unreachable();
    }

  return 10;
}

/* { dg-final { scan-tree-dump-times "predicted to even probabilities" 4 "profile_estimate"} } */
