/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch" } */

typedef int (*fn_t)(void);

fn_t hp1(void)
{
  __attribute__((hotpatch(0,0)))
  int nested1(void)
  { return 1; }

  return nested1;
}

fn_t hp2(void)
{
  __attribute__ ((hotpatch(1,2)))
  int nested2(void)
  { return 2; }

  return nested2;
}
