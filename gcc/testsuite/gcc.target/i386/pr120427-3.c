/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef int SItype __attribute__ ((mode (SI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));
typedef UDItype __attribute__ ((__may_alias__)) bar_t;

static inline __attribute__((__always_inline__)) SItype
bar (const bar_t **p, SItype prec)
{
  bar_t mslimb = 0;
  SItype i = 20;
  SItype n = ((USItype) prec) % 4;
  if (n)
    {
      prec -= n;
      if (prec == 0)
	return 1;
      mslimb = (*p)[i];
    }
  while (mslimb == 0)
    {
      prec -= 4;
      if (prec == 0)
	return 1;
      --i;
      mslimb = (*p)[i];
    }
  return prec;
}
UDItype
foo (const bar_t *i, SItype iprec)
{
  iprec = bar (&i, iprec);
  USItype aiprec = iprec < 0 ? -iprec : iprec;
  bar_t msb = *i;
  UDItype mantissa = 0;
  if (aiprec % 4)
    msb &= ((bar_t) 1 << aiprec) - 1;
  if (aiprec >= 54)
    mantissa = (UDItype) msb << 32;

  return (mantissa ^ (UDItype) 0x20000000000000);
}
