/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fnon-call-exceptions" } */

#include <arm_sve.h>

svint8_t
foo (svbool_t pg, svint8_t a, svint8_t b)
{
  try
    {
      a = svadd_m (pg, a, b);
    }
  catch (...)
    {
      a = b;
    }
  return a;
}

/* { dg-final { scan-tree-dump-not {__cxa_begin_catch} "optimized" } } */
