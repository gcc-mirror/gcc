/* { dg-do compile } */
/* { dg-additional-options "-Wuninitialized" } */

void
foo (void)
{
  int i;

#pragma acc kernels
  /* { dg-warning "'i' is used uninitialized in this function" "" { target *-*-* } .-1 } */
  /*TODO With the 'copy' -> 'firstprivate' optimization, the original implicit 'copy(i)' clause gets optimized into a 'firstprivate(i)' clause -- and the expected (?) warning diagnostic appears.
    Have to read up the history behind these test cases.
    Should this test remain here in this file even if now testing 'firstprivate'?
    Or, should the optimization be disabled for such testing?
    Or, the testing be duplicated for both variants?  */
  {
    i = 1;
  }

}

void
foo2 (void)
{
  int i;

#pragma acc kernels copy (i)
  {
    i = 1;
  }

}

void
foo3 (void)
{
  int i;

#pragma acc kernels copyin(i)
  {
    i = 1;
  }

}
