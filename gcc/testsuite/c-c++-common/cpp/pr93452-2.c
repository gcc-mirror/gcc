/* { dg-do preprocess } */
/* { dg-additional-options "-dD" } */

#if __has_include ("who cares" )
#endif
int main ()
{
  return 0;
}

/* { dg-final { scan-file-not pr93452-2.i {__has_include} } } */
