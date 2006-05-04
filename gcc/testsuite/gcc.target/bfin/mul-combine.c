/* Make sure combine eliminates all unnecessary instructions for the
   sixteen cases of hi/lo multiplications.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-not "<<" } } */
/* { dg-final { scan-assembler-not "PACK" } } */

extern void abort (void);
extern void exit (int);

typedef int  __v2hi __attribute ((vector_size(4)));
typedef __v2hi fract2x16;
typedef short fract16;

#define GETVECT(HILO1,HILO2,IN1,IN2)					\
  __builtin_bfin_compose_2x16 ((HILO2) ? __builtin_bfin_extract_hi (IN1) : __builtin_bfin_extract_lo (IN1), \
			  (HILO1) ? __builtin_bfin_extract_hi (IN2) : __builtin_bfin_extract_lo (IN2))
#define DOTEST(IN1, IN2, HL1, HL2, HL3, HL4)					\
  __builtin_bfin_multr_fr2x16 (GETVECT (HL1, HL2, IN1, IN1), \
			  GETVECT (HL3, HL4, IN2, IN2))

#define FUNC(HL1, HL2, HL3, HL4) \
  fract2x16 foo ## HL1 ## HL2 ## HL3 ## HL4 (fract2x16 a, fract2x16 b)\
  { \
    return DOTEST(a, b, HL1, HL2, HL3, HL4);\
  }

FUNC (0, 0, 0, 0)
FUNC (1, 0, 0, 0)
FUNC (0, 1, 0, 0)
FUNC (1, 1, 0, 0)
FUNC (0, 0, 1, 0)
FUNC (1, 0, 1, 0)
FUNC (0, 1, 1, 0)
FUNC (1, 1, 1, 0)
FUNC (0, 0, 0, 1)
FUNC (1, 0, 0, 1)
FUNC (0, 1, 0, 1)
FUNC (1, 1, 0, 1)
FUNC (0, 0, 1, 1)
FUNC (1, 0, 1, 1)
FUNC (0, 1, 1, 1)
FUNC (1, 1, 1, 1)
