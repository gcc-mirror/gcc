/* { dg-options { -std=gnu99 } } */

void f7_clr (void *cc)
{
  __asm ("%~call __f7_clr_asm" :: "z" (cc) : "memory");
}

void* f7_copy (void *cc, const void *aa)
{
  extern void __f7_copy_asm (void);
  __asm ("%~call __f7_copy_asm" :: "z" (cc), "x" (aa) : "memory");
  return cc;
}

typedef _Bool bool;
typedef unsigned int uint16_t;
typedef unsigned char uint8_t;
typedef int int16_t;

typedef struct f7_t
{
  union
  {
    struct
    {
      uint8_t sign :1;
      uint8_t reserved1 :1;
      uint8_t is_nan :1;
      uint8_t reserved2 :4;
      uint8_t is_inf :1;
    };
    uint8_t flags;
  };

  uint8_t mant[7];
  int16_t expo;
} f7_t;


static inline __attribute__((__always_inline__))
void __f7_clr (f7_t *cc)
{
  extern void __f7_clr_asm (void);
  __asm ("%~call %x[f]"
  :
  : [f] "i" (__f7_clr_asm), "z" (cc)
  : "memory");
}

static inline __attribute__((__always_inline__))
bool __f7_signbit (const f7_t *aa)
{
  return aa->flags & (1 << 0);
}

static inline __attribute__((__always_inline__))
int16_t sub_ssat16 (int16_t a, int16_t b)
{
  _Sat _Fract sa = __builtin_avr_rbits (a);
  _Sat _Fract sb = __builtin_avr_rbits (b);
  return __builtin_avr_bitsr (sa - sb);
}

extern void __f7_Iadd (f7_t*, const f7_t*);
extern void __f7_addsub (f7_t*, const f7_t*, const f7_t*, bool neg_b);
extern uint8_t __f7_mulx (f7_t*, const f7_t*, const f7_t*, bool);
extern f7_t* __f7_normalize_asm (f7_t*);

void __f7_madd_msub (f7_t *cc, const f7_t *aa, const f7_t *bb, const f7_t *dd,
                   bool neg_d)
{
  f7_t xx7, *xx = &xx7;
  uint8_t x_lsb = __f7_mulx (xx, aa, bb, 1 );
  uint8_t x_sign = __f7_signbit (xx);
  int16_t x_expo = xx->expo;
  __f7_addsub (xx, xx, dd, neg_d);


  __f7_clr (cc);
  cc->expo = sub_ssat16 (x_expo, (8 * 7));
  cc->mant[7 - 1] = x_lsb;
  cc = __f7_normalize_asm (cc);
  cc->flags = x_sign;
  __f7_Iadd (cc, xx);
}

