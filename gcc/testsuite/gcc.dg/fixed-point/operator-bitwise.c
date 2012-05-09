/* { dg-do compile } */
/* { dg-options "-std=gnu99 -ftrack-macro-expansion=0" } */

/* C99 6.5.10: Bitwise AND operator.
   C99 6.5.11: Bitwise exclusive OR operator.
   C99 6.5.12: Bitwise inclusive OR operator.

   Test that these operators do not accept fixed-point operands.
   Based on the test from ../dfp/.  */

extern void abort (void);

#define OPERATE(OPRD1,OPRT,OPRD2)		\
do						\
{						\
  OPRD1 OPRT OPRD2;				\
} while (0)



#define BITWISE_OPERATOR(OPRT,OPRD)		\
do						\
{						\
OPERATE(OPRD,OPRT,1);				\
OPERATE(OPRD,OPRT,0);				\
OPERATE(OPRD,OPRT,0x15);			\
OPERATE(0,OPRT,OPRD);				\
OPERATE(1,OPRT,OPRD);				\
OPERATE(0x15,OPRT,OPRD);			\
} while (0)

void operator_notfor_fixed_point()
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

  /* C99 Section 6.5.{10,11,12} Bitwise operator.  Constraints: Each of
   the operands shall have integer type.  Fixed-point type is rejected
   by compiler when bitwise operation is performed.  */

  BITWISE_OPERATOR(&,sf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,sf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,sf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,f); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,f); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,f); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,lf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,lf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,lf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,llf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,llf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,llf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,usf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,usf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,usf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,uf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,uf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,uf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,ulf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,ulf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,ulf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,ullf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,ullf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,ullf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Ssf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Ssf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Ssf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Sf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Sf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Sf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Slf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Slf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Slf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Sllf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Sllf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Sllf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Susf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Susf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Susf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Suf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Suf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Suf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Sulf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Sulf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Sulf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Sullf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Sullf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Sullf); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,sa); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,sa); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,sa); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,a); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,a); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,a); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,la); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,la); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,la); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,lla); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,lla); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,lla); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,usa); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,usa); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,usa); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,ua); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,ua); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,ua); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,ula); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,ula); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,ula); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,ulla); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,ulla); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,ulla); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Ssa); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Ssa); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Ssa); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Sa); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Sa); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Sa); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Sla); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Sla); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Sla); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Slla); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Slla); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Slla); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Susa); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Susa); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Susa); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Sua); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Sua); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Sua); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Sula); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Sula); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Sula); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(&,Sulla); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(|,Sulla); /* { dg-error "invalid operands to binary" } */
  BITWISE_OPERATOR(^,Sulla); /* { dg-error "invalid operands to binary" } */
}
