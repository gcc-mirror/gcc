/* { dg-options "-std=gnu99" } */

/* C99 6.5.16 Assignment operators.
   Verify the compound assignment operator for decimal float types,
   using it with other decimal float types, integers, and other binary
   float types cast to decimal float types.  */

extern void abort (void);

#define OPERATE(OPRD1,OPRT,OPRD2,RLT)		\
  if (( OPRD1 OPRT OPRD2 )!= RLT)		\
    abort ();

#define DECIMAL_COMPOUND_ASSIGNMENT(TYPE, OPRD)	\
{						\
  _Decimal##TYPE d = OPRD;			\
  OPERATE(d,+=,1,(OPRD + 1));		\
  d = OPRD;				\
  OPERATE(d,+=,0,OPRD);			\
  d = OPRD;				\
  OPERATE(d,+=,(-1),(OPRD - 1));	\
  d = OPRD;				\
  OPERATE(d,+=,d32a,(OPRD + d32a));	\
  d = OPRD;				\
  OPERATE(d,+=,d64a,(OPRD + d64a));	\
  d = OPRD;				\
  OPERATE(d,+=,d128a,(OPRD + d128a));	\
  d = OPRD;				\
  OPERATE(d,+=,(_Decimal##TYPE)1.1,(OPRD + (_Decimal##TYPE)1.1));	\
  d = OPRD;				\
  OPERATE(d,+=,(_Decimal##TYPE)2.2f,(OPRD + (_Decimal##TYPE)2.2f));	\
  d = OPRD;				\
  OPERATE(d,-=,1,(OPRD - 1));		\
  d = OPRD;				\
  OPERATE(d,-=,0,OPRD);			\
  d = OPRD;				\
  OPERATE(d,-=,(-1),(OPRD + 1));	\
  d = OPRD;				\
  OPERATE(d,-=,d32a,OPRD-d32a);		\
  d = OPRD;				\
  OPERATE(d,-=,d64a,OPRD-d64a);		\
  d = OPRD;				\
  OPERATE(d,-=,d128a,OPRD-d128a);	\
}

int
main ()
{
  _Decimal32 d32 = 1.23456fd, d32a = 1.2df;
  _Decimal64 d64 = 23.456789dd, d64a = 2.8dd;
  _Decimal128 d128 = 345.67890123456789ld, d128a = 4.7dl;

  DECIMAL_COMPOUND_ASSIGNMENT(32, d32);
  DECIMAL_COMPOUND_ASSIGNMENT(64, d64);
  DECIMAL_COMPOUND_ASSIGNMENT(128, d128);

  return 0;
}
