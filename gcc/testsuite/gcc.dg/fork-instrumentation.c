/* { dg-do compile } */
/* { dg-options "-O0 -fprofile-generate" } */
int fork(void);
t()
{
  fork ();
}
/* { dg-final { scan-assembler "gcov_fork" } } */
