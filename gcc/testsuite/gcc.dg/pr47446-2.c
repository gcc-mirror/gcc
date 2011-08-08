/* { dg-do assemble } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fpic" } */

extern void abort (void);
enum
{
  __GCONV_OK = 0,
  __GCONV_NOCONV,
  __GCONV_NODB,
  __GCONV_NOMEM,
  __GCONV_EMPTY_INPUT,
  __GCONV_FULL_OUTPUT,
  __GCONV_ILLEGAL_INPUT,
  __GCONV_INCOMPLETE_INPUT,
  __GCONV_ILLEGAL_DESCRIPTOR,
  __GCONV_INTERNAL_ERROR
};
int
foo (int result)
{
  int irreversible = 0;
  switch (result)
    {
    case __GCONV_ILLEGAL_INPUT:
      irreversible = -1L;
      break;
    case __GCONV_FULL_OUTPUT:
      irreversible = -2L;
      break;
    case __GCONV_INCOMPLETE_INPUT:
      irreversible = -3L;
      break;
    case __GCONV_EMPTY_INPUT:
    case __GCONV_OK:
      break;
    default:
      abort ();
    }
  return irreversible;
}
