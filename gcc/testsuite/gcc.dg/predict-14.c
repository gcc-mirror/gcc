/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

int main(int argc, char **argv)
{
  switch (argc)
    {
    case 1:
      __builtin_abort();
    case 4:
      __builtin_abort();
    default:
      __builtin_abort();
    }

  return 10;
}

/* { dg-final { scan-tree-dump-times "predicted to even probabilities" 4 "profile_estimate"} } */
