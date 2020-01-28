/* { dg-do preprocess } */
/* { dg-additional-options "-fdirectives-only" } */

int main ()
{
  return 0;
}

/* A regexp that doesn't match itself!  */
/* { dg-final { scan-file-not pr93452-1.i {_[_]has_include} } } */
