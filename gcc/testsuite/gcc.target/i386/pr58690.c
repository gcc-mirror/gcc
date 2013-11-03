/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -mx32 -maddress-mode=short" } */

struct gomp_thread
{
  char foo[41];
};
extern __thread struct gomp_thread gomp_tls_data;
void
foo (void)
{
  __builtin_memset (&gomp_tls_data, '\0', sizeof (gomp_tls_data));
}
