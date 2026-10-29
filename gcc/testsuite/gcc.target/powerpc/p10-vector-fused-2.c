/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Make sure all of the fusion cases that generate the xxeval instruction
   actually generate it.  */
typedef vector unsigned int vector_t;

static inline vector_t
vector_and (vector_t x, vector_t y)
{
  return x & y;
}

static inline vector_t
vector_or (vector_t x, vector_t y)
{
  return x | y;
}

static inline vector_t
vector_xor (vector_t x, vector_t y)
{
  return x ^ y;
}

static inline vector_t
vector_andc (vector_t x, vector_t y)
{
  return x & ~y;
}

static inline vector_t
vector_orc (vector_t x, vector_t y)
{
  return x | ~y;
}

static inline vector_t
vector_nand (vector_t x, vector_t y)
{
  return ~(x & y);
}

static inline vector_t
vector_nor (vector_t x, vector_t y)
{
  return ~(x | y);
}

static inline vector_t
vector_eqv (vector_t x, vector_t y)
{
  return ~(x ^ y);
}

void
and_and (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,1.  */
  r = vector_and (a, vector_and (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
and_andc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,14.  */
  r = vector_andc (a, vector_and (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
and_or (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,31.  */
  r = vector_or (a, vector_and (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
and_orc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,239.  */
  r = vector_orc (a, vector_and (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
and_xor (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,30.  */
  r = vector_xor (a, vector_and (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
andc_and (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,14.  */
  r = vector_andc (a, vector_and (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
andc_andc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,11.  */
  r = vector_andc (a, vector_andc (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
andc_eqv (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,210.  */
  r = vector_eqv (a, vector_andc (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
andc_nand (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,239.  */
  r = vector_nand (a, vector_andc (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
andc_or (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,47.  */
  r = vector_or (a, vector_andc (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
andc_orc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,191.  */
  r = vector_orc (a, vector_andc (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
andc_xor (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,45.  */
  r = vector_xor (a, vector_andc (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
eqv_and (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,9.  */
  r = vector_and (a, vector_eqv (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
eqv_andc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,210.  */
  r = vector_eqv (a, vector_andc (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
eqv_eqv (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,105.  */
  r = vector_eqv (a, vector_eqv (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
eqv_or (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,159.  */
  r = vector_or (a, vector_eqv (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
eqv_orc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,111.  */
  r = vector_orc (a, vector_eqv (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
nand_and (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,14.  */
  r = vector_and (a, vector_nand (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
nand_andc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,1.  */
  r = vector_andc (a, vector_nand (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
nand_eqv (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,30.  */
  r = vector_eqv (a, vector_nand (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
nand_or (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,2.  */
  r = vector_nor (a, vector_nand (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
nand_orc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,31.  */
  r = vector_orc (a, vector_nand (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
nor_and (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,8.  */
  r = vector_and (a, vector_nor (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
nor_andc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,7.  */
  r = vector_andc (a, vector_nor (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
nor_eqv (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,120.  */
  r = vector_eqv (a, vector_nor (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
nor_nand (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,191.  */
  r = vector_nand (a, vector_nor (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
nor_or (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,143.  */
  r = vector_or (a, vector_nor (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
nor_orc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,127.  */
  r = vector_orc (a, vector_nor (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
or_and (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,7.  */
  r = vector_and (a, vector_or (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
or_andc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,8.  */
  r = vector_andc (a, vector_or (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
or_or (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,127.  */
  r = vector_or (a, vector_or (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
or_orc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,143.  */
  r = vector_orc (a, vector_or (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
or_xor (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,120.  */
  r = vector_xor (a, vector_or (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
orc_and (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,11.  */
  r = vector_and (a, vector_orc (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
orc_andc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,2.  */
  r = vector_andc (a, vector_orc (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
orc_eqv (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,75.  */
  r = vector_eqv (a, vector_orc (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
orc_nor (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,8.  */
  r = vector_nor (a, vector_orc (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
orc_or (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,191.  */
  r = vector_or (a, vector_orc (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
orc_orc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,47.  */
  r = vector_orc (a, vector_orc (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
orc_xor (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,180.  */
  r = vector_xor (a, vector_orc (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
xor_and (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,6.  */
  r = vector_and (a, vector_xor (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
xor_andc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,9.  */
  r = vector_andc (a, vector_xor (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
xor_nand (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,249.  */
  r = vector_nand (a, vector_xor (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
xor_or (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,111.  */
  r = vector_or (a, vector_xor (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
xor_orc (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,159.  */
  r = vector_orc (a, vector_xor (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

void
xor_xor (vector_t *p_a, vector_t *p_b, vector_t *p_c, vector_t *p_r)
{
  vector_t a = *p_a;
  vector_t b = *p_b;
  vector_t c = *p_c;
  vector_t r;

  __asm__ (" # force fpr registers, %x0,%x1,%x2"
	   : "+d" (a), "+d" (b), "+d" (c));

  /* xxeval r,a,b,c,105.  */
  r = vector_xor (a, vector_xor (b, c));

  __asm__ (" # force fpr result, %x0" : "+d" (r));
  *p_r = r;
  return;
}

/* Make sure none of traditional logical instructions are generated.  Skip
   checking for xxlor in case the register allocator decides to add some vector
   moves.  */
/* { dg-final { scan-assembler-not   {\mv(and|or|xor|andc|orc|nand|nor|eqv)\M} } } */
/* { dg-final { scan-assembler-not   {\mxxl(and|xor|andc|orc|nand|nor|eqv)\M}  } } */
/* { dg-final { scan-assembler-times {\mxxeval\M} 46 } } */
