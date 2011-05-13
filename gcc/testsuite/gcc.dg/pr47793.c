/* Bug pr47793: Allow relative paths in profile-generate.  */
/* { dg-do run } */
/* { dg-options "-O -fprofile-generate=./" } */
/* { dg-final { scan-file pr47793.gcda "."} } */

int
main(void)
{
  return 0;
}

/* { dg-final { cleanup-coverage-files } } */
