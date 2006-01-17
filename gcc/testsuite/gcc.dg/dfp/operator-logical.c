/* { dg-options "-std=gnu99" } */

/* C99 Logical AND operator.
   C99 Logical OR operator.
   Test with decimal float operands.  */

extern void abort (void);

#define OPERATE(OPRD1,OPRT,OPRD2,RLT)	\
do					\
{					\
  if (( (OPRD1) OPRT (OPRD2) )!= RLT)	\
    abort ();				\
} while (0)

#define DECIMAL_LOGICAL_OR(OPRD)	\
do					\
{					\
  OPRD = 1.0;				\
  OPERATE(1,||,OPRD,1);			\
  OPERATE(0,||,OPRD,1);			\
  OPERATE(OPRD,||,1,1);			\
  OPERATE(OPRD,||,0,1);			\
  OPRD = 0.0;				\
  OPERATE(1,||,OPRD,1);			\
  OPERATE(0,||,OPRD,0);			\
  OPERATE(OPRD,||,1,1);			\
  OPERATE(OPRD,||,0,0);			\
} while (0)

#define DECIMAL_LOGICAL_AND(OPRD)	\
do					\
{					\
  OPRD = 1.0;				\
  OPERATE(1,&&,OPRD,1);			\
  OPERATE(0,&&,OPRD,0);			\
  OPERATE(OPRD,&&,1,1);			\
  OPERATE(OPRD,&&,0,0);			\
  OPRD = 0.0;				\
  OPERATE(1,&&,OPRD,0);			\
  OPERATE(0,&&,OPRD,0);			\
  OPERATE(OPRD,&&,1,0);			\
  OPERATE(OPRD,&&,0,0);			\
} while (0)

int
main ()
{
  _Decimal32 d32;
  _Decimal64 d64;
  _Decimal128 d128;

  /* C99 Section 6.5.{13,14} Logical operator.  Constraints Each of the
     operands shall have scalar type.  DFP types would obey this.  */
  DECIMAL_LOGICAL_OR (d32);
  DECIMAL_LOGICAL_AND (d32);

  DECIMAL_LOGICAL_OR (d64);
  DECIMAL_LOGICAL_AND (d64);

  DECIMAL_LOGICAL_OR (d128);
  DECIMAL_LOGICAL_AND (d128);

  return 0;
}
