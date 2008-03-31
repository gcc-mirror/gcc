/* { dg-do compile } */
/* { dg-options "-O -fprofile-generate" } */
/* { dg-final { scan-assembler "/profile-dir-2.gcda" } } */

int
main(void)
{
  return 0;
}

/* { dg-final { cleanup-coverage-files } } */
