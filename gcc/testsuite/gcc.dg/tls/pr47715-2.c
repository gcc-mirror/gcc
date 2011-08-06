/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fPIC" } */
/* { dg-require-effective-target tls } */

extern __thread int *__libc_resp;
int
__res_init(void) {
  return *__libc_resp;
}
