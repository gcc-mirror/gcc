/* Regression test for bug with macro expansion on #if lines -
   exposed by glibc.  */
/* { dg-do compile } */

# define SHLIB_COMPAT(lib, introduced, obsoleted) \
  (!(ABI_##lib##_##obsoleted - 0) \
   || ((ABI_##lib##_##introduced - 0) < (ABI_##lib##_##obsoleted - 0)))

#if 0
bad
#elif SHLIB_COMPAT (libc, GLIBC_2_0, GLIBC_2_1)
int
#endif
x;
