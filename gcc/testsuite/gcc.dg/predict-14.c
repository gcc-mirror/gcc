/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

void test(void) __attribute__ ((noreturn));

int main(int argc, char **argv)
{
  switch (argc)
    {
    case 1:
      test();
    case 4:
      test();
    default:
      test();
    }

  return 10;
}

/* { dg-final { scan-tree-dump-times "predicted to even probabilities" 4 "profile_estimate"} } */
