/* { dg-do run { target m68k-*-* sparc-*-* } } */
/* { dg-options { -m68000 -O2 } { target m68k-*-* } } */
/* { dg-options { -O2 } { target sparc-*-* } } */

typedef SFtype __attribute__ ((mode (SF)));
typedef DFtype __attribute__ ((mode (DF)));

typedef int HItype __attribute__ ((mode (HI)));
typedef int SItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));

typedef unsigned int UHItype __attribute__ ((mode (HI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));

typedef UDItype fractype;
typedef USItype halffractype;
typedef DFtype FLO_type;
typedef DItype intfrac;


typedef union
{
  long long foo;
  FLO_type value;
  struct
    {
      fractype fraction:52 __attribute__ ((packed));
      unsigned int exp:11 __attribute__ ((packed));
      unsigned int sign:1 __attribute__ ((packed));
    }
  bits;
} FLO_union_type;

void foo (long long a);
long long x; 

void
pack_d ()
{
  FLO_union_type dst = { 0x0123456789abcdefLL };

  x = dst.bits.fraction;
}

main ()
{
  pack_d ();
  foo (x);
  return 0;
}

void
foo (long long a)
{
  if (a != 0x0123456789abcLL)
    abort ();
}
