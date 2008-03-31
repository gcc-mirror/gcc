/* { dg-do compile } */
/* { dg-options "-O -fprofile-generate -fprofile-dir=." } */
/* { dg-final { scan-assembler "\"./profile-dir-3.gcda\"" } } */

int
main(void)
{
  return 0;
}

/* { dg-final { cleanup-coverage-files } } */
