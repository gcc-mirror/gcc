/* { dg-do compile } */
/* { dg-options "-mmcu=avrtiny -gdwarf -Os" } */

/* This is a stripped down piece of libgcc2.c that triggerd an ICE for avr with
   "-mmcu=avrtiny -g -Os"; replace_reg_with_saved_mem would generate:
   (concatn:SI [
                    (reg:SI 18 r18)
                    (reg:SI 19 r19)
                    (mem/c:QI (plus:HI (reg/f:HI 28 r28)
                            (const_int 43 [0x2b])) [6  S1 A8])
                    (mem/c:QI (plus:HI (reg/f:HI 28 r28)
                            (const_int 44 [0x2c])) [6  S1 A8])
                ]) */

typedef int SItype __attribute__ ((mode (SI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));
struct DWstruct
{
  SItype low, high;
};
typedef union
{
  struct DWstruct s;
  DItype ll;
} DWunion;

UDItype
__udivmoddi4 (UDItype n, UDItype d)
{
  const DWunion nn = {.ll = n };
  const DWunion dd = {.ll = d };
  USItype d0, d1, n2;
  USItype q0;

  d0 = dd.s.low;
  d1 = dd.s.high;
  n2 = nn.s.high;

      USItype m0;

      do
	{
	  USItype __d1, __d0, __q1, __q0;
	  USItype __r1, __m;
	  __d1 = ((USItype) (d1) >> 16);
	  __d0 = ((USItype) (d1) & (((USItype) 1 << 16) - 1));
	  __r1 = (n2) % __d1;
	  __q1 = (n2) / __d1;
	  __m = (USItype) __q1 *__d0;
	  __r1 -= __m;
	  __q0 = __r1 / __d1;
	  (q0) = (USItype) __q1 *((USItype) 1 << 16) | __q0;
	}
      while (0);
      do
	{
	  USItype __x0, __x1, __x2;
	  USItype __ul, __vl, __uh, __vh;
	  __ul = ((USItype) (q0) & (((USItype) 1 << 16) - 1));
	  __uh = ((USItype) (q0) >> 16);
	  __vl = ((USItype) (d0) & (((USItype) 1 << 16) - 1));
	  __vh = ((USItype) (d0) >> 16);
	  __x0 = (USItype) __ul *__vl;
	  __x1 = (USItype) __ul *__vh;
	  __x2 = (USItype) __uh *__vl;
	  __x1 += ((USItype) (__x0) >> 16);
	  __x1 += __x2;
	  (m0) =
	    ((USItype) (__x1) & (((USItype) 1 << 16) - 1)) *
	    ((USItype) 1 << 16) +
	    ((USItype) (__x0) & (((USItype) 1 << 16) - 1));
	}
      while (0);

return m0;
}
