/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

/* C99 Logical AND operator.
   C99 Logical OR operator.
   Test with fixed-point operands.
   Based on the test from ../dfp/.  */

extern void abort (void);

#define OPERATE(OPRD1,OPRT,OPRD2,RLT)	\
do					\
{					\
  if (( (OPRD1) OPRT (OPRD2) )!= RLT)	\
    abort ();				\
} while (0)

#define FIXED_POINT_LOGICAL(OPRD)	\
do					\
{					\
  OPRD = 0.1;				\
  OPERATE(1,||,OPRD,1);			\
  OPERATE(0,||,OPRD,1);			\
  OPERATE(OPRD,||,1,1);			\
  OPERATE(OPRD,||,0,1);			\
  OPRD = 0;				\
  OPERATE(1,||,OPRD,1);			\
  OPERATE(0,||,OPRD,0);			\
  OPERATE(OPRD,||,1,1);			\
  OPERATE(OPRD,||,0,0);			\
  OPRD = 0.1;				\
  OPERATE(1,&&,OPRD,1);			\
  OPERATE(0,&&,OPRD,0);			\
  OPERATE(OPRD,&&,1,1);			\
  OPERATE(OPRD,&&,0,0);			\
  OPRD = 0;				\
  OPERATE(1,&&,OPRD,0);			\
  OPERATE(0,&&,OPRD,0);			\
  OPERATE(OPRD,&&,1,0);			\
  OPERATE(OPRD,&&,0,0);			\
} while (0)

int
main ()
{
  short _Fract sf;
  _Fract f;
  long _Fract lf;
  long long _Fract llf;
  unsigned short _Fract usf;
  unsigned _Fract uf;
  unsigned long _Fract ulf;
  unsigned long long _Fract ullf;
  _Sat short _Fract Ssf;
  _Sat _Fract Sf;
  _Sat long _Fract Slf;
  _Sat long long _Fract Sllf;
  _Sat unsigned short _Fract Susf;
  _Sat unsigned _Fract Suf;
  _Sat unsigned long _Fract Sulf;
  _Sat unsigned long long _Fract Sullf;
  short _Accum sa;
  _Accum a;
  long _Accum la;
  long long _Accum lla;
  unsigned short _Accum usa;
  unsigned _Accum ua;
  unsigned long _Accum ula;
  unsigned long long _Accum ulla;
  _Sat short _Accum Ssa;
  _Sat _Accum Sa;
  _Sat long _Accum Sla;
  _Sat long long _Accum Slla;
  _Sat unsigned short _Accum Susa;
  _Sat unsigned _Accum Sua;
  _Sat unsigned long _Accum Sula;
  _Sat unsigned long long _Accum Sulla;

  /* C99 Section 6.5.{13,14} Logical operator.  Constraints Each of the
     operands shall have scalar type.  Fixed-point types would obey this.  */
  FIXED_POINT_LOGICAL (sf);
  FIXED_POINT_LOGICAL (f);
  FIXED_POINT_LOGICAL (lf);
  FIXED_POINT_LOGICAL (llf);
  FIXED_POINT_LOGICAL (usf);
  FIXED_POINT_LOGICAL (uf);
  FIXED_POINT_LOGICAL (ulf);
  FIXED_POINT_LOGICAL (ullf);
  FIXED_POINT_LOGICAL (Ssf);
  FIXED_POINT_LOGICAL (Sf);
  FIXED_POINT_LOGICAL (Slf);
  FIXED_POINT_LOGICAL (Sllf);
  FIXED_POINT_LOGICAL (Susf);
  FIXED_POINT_LOGICAL (Suf);
  FIXED_POINT_LOGICAL (Sulf);
  FIXED_POINT_LOGICAL (Sullf);
  FIXED_POINT_LOGICAL (sa);
  FIXED_POINT_LOGICAL (a);
  FIXED_POINT_LOGICAL (la);
  FIXED_POINT_LOGICAL (lla);
  FIXED_POINT_LOGICAL (usa);
  FIXED_POINT_LOGICAL (ua);
  FIXED_POINT_LOGICAL (ula);
  FIXED_POINT_LOGICAL (ulla);
  FIXED_POINT_LOGICAL (Ssa);
  FIXED_POINT_LOGICAL (Sa);
  FIXED_POINT_LOGICAL (Sla);
  FIXED_POINT_LOGICAL (Slla);
  FIXED_POINT_LOGICAL (Susa);
  FIXED_POINT_LOGICAL (Sua);
  FIXED_POINT_LOGICAL (Sula);
  FIXED_POINT_LOGICAL (Sulla);

  return 0;
}
