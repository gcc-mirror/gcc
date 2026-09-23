/* Verify -Wuse-after-free is an alias for -Wuse-after-free=1. */
/* { dg-do compile } */
/* { dg-options "-O0 -Wuse-after-free" } */

#if __cplusplus
#  define EXTERN_C extern "C"
#else
#  define EXTERN_C extern
#endif

EXTERN_C void free (void *);

void sink (void *);

void warn_call_after_free (void *p)
{
  free (p);
  sink (p);         // { dg-warning "pointer 'p' used" }
}

void warn_cond_call_after_free (void *p, int c)
{
  free (p);
  if (c)
    sink (p);
}
