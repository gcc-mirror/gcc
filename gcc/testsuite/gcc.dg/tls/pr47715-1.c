/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fPIC" } */
/* { dg-require-effective-target tls } */

extern __thread int h_errno;
int *
__h_errno_location (void)
{
  return &h_errno;
}
