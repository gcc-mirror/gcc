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

#define RES1 0x1400
#define RES2 0x1e00
#define RES3 0x1c00
#define RES4 0x2a00


int main ()
{
  fract2x16 a, b, c;
  fract16 t1, t2;
  a = __builtin_bfin_compose_2x16 (0x3000, 0x2000);
  b = __builtin_bfin_compose_2x16 (0x7000, 0x5000);

  c = foo0000 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES1 || t2 != RES1)
    abort ();

  c = foo1000 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES2 || t2 != RES1)
    abort ();

  c = foo0100 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES1 || t2 != RES2)
    abort ();

  c = foo1100 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES2 || t2 != RES2)
    abort ();

  c = foo0010 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES3 || t2 != RES1)
    abort ();

  c = foo1010 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES4 || t2 != RES1)
    abort ();

  c = foo0110 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES3 || t2 != RES2)
    abort ();

  c = foo1110 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES4 || t2 != RES2)
    abort ();

  c = foo0001 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES1 || t2 != RES3)
    abort ();

  c = foo1001 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES2 || t2 != RES3)
    abort ();

  c = foo0101 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES1 || t2 != RES4)
    abort ();

  c = foo1101 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES2 || t2 != RES4)
    abort ();

  c = foo0011 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES3 || t2 != RES3)
    abort ();

  c = foo1011 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES4 || t2 != RES3)
    abort ();

  c = foo0111 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES3 || t2 != RES4)
    abort ();

  c = foo1111 (a, b);
  t1 = __builtin_bfin_extract_lo (c);
  t2 = __builtin_bfin_extract_hi (c);
  if (t1 != RES4 || t2 != RES4)
    abort ();

  exit (0);
}

