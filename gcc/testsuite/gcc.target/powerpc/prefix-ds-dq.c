/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */
/* If -mstrict-align is enabled by default, we don't get the expected opcodes.
   { dg-additional-options "-mno-strict-align" { target opt_mstrict_align } } */

/* Tests whether we generate a prefixed load/store operation for addresses that
   don't meet DS/DQ offset constraints.  64-bit is needed for testing the use
   of the PLWA instruciton.  */

struct packed_struct
{
  long long pad;			/* offset  0 bytes.  */
  unsigned char pad_uc;			/* offset  8 bytes.  */
  unsigned char uc;			/* offset  9 bytes.  */

  unsigned char pad_sc[sizeof (long long) - sizeof (unsigned char)];
  unsigned char sc;			/* offset  17 bytes.  */

  unsigned char pad_us[sizeof (long long) - sizeof (signed char)];
  unsigned short us;			/* offset  25 bytes.  */

  unsigned char pad_ss[sizeof (long long) - sizeof (unsigned short)];
  short ss;				/* offset 33 bytes.  */

  unsigned char pad_ui[sizeof (long long) - sizeof (short)];
  unsigned int ui;			/* offset 41 bytes.  */

  unsigned char pad_si[sizeof (long long) - sizeof (unsigned int)];
  unsigned int si;			/* offset 49 bytes.  */

  unsigned char pad_f[sizeof (long long) - sizeof (int)];
  float f;				/* offset 57 bytes.  */

  unsigned char pad_d[sizeof (long long) - sizeof (float)];
  double d;				/* offset 65 bytes.  */
  __float128 f128;			/* offset 73 bytes.  */
} __attribute__((packed));

unsigned char
load_uc (struct packed_struct *p)
{
  return p->uc;				/* LBZ 3,9(3).  */
}

signed char
load_sc (struct packed_struct *p)
{
  return p->sc;				/* LBZ 3,17(3) + EXTSB 3,3.  */
}

unsigned short
load_us (struct packed_struct *p)
{
  return p->us;				/* LHZ 3,25(3).  */
}

short
load_ss (struct packed_struct *p)
{
  return p->ss;				/* LHA 3,33(3).  */
}

unsigned int
load_ui (struct packed_struct *p)
{
  return p->ui;				/* LWZ 3,41(3).  */
}

int
load_si (struct packed_struct *p)
{
  return p->si;				/* PLWA 3,49(3).  */
}

float
load_float (struct packed_struct *p)
{
  return p->f;				/* LFS 1,57(3).  */
}

double
load_double (struct packed_struct *p)
{
  return p->d;				/* LFD 1,65(3).  */
}

__float128
load_float128 (struct packed_struct *p)
{
  return p->f128;			/* PLXV 34,73(3).  */
}

void
store_uc (struct packed_struct *p, unsigned char uc)
{
  p->uc = uc;				/* STB 4,9(3).  */
}

void
store_sc (struct packed_struct *p, signed char sc)
{
  p->sc = sc;				/* STB 4,17(3).  */
}

void
store_us (struct packed_struct *p, unsigned short us)
{
  p->us = us;				/* STH 4,25(3).  */
}

void
store_ss (struct packed_struct *p, signed short ss)
{
  p->ss = ss;				/* STH 4,33(3).  */
}

void
store_ui (struct packed_struct *p, unsigned int ui)
{
  p->ui = ui;				/* STW 4,41(3).  */
}

void
store_si (struct packed_struct *p, signed int si)
{
  p->si = si;				/* STW 4,49(3).  */
}

void
store_float (struct packed_struct *p, float f)
{
  p->f = f;				/* STFS 1,57(3).  */
}

void
store_double (struct packed_struct *p, double d)
{
  p->d = d;				/* STFD 1,65(3).  */
}

void
store_float128 (struct packed_struct *p, __float128 f128)
{
  p->f128 = f128;			/* PSTXV 34,1(3).  */
}

/* { dg-final { scan-assembler-times {\mextsb\M} 1 } } */
/* { dg-final { scan-assembler-times {\mlbz\M}   2 } } */
/* { dg-final { scan-assembler-times {\mlfd\M}   1 } } */
/* { dg-final { scan-assembler-times {\mlfs\M}   1 } } */
/* { dg-final { scan-assembler-times {\mlha\M}   1 } } */
/* { dg-final { scan-assembler-times {\mlhz\M}   1 } } */
/* { dg-final { scan-assembler-times {\mlwz\M}   1 } } */
/* { dg-final { scan-assembler-times {\mplwa\M}  1 } } */
/* { dg-final { scan-assembler-times {\mplxv\M}  1 } } */
/* { dg-final { scan-assembler-times {\mpstxv\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstb\M}   2 } } */
/* { dg-final { scan-assembler-times {\mstfd\M}  1 } } */
/* { dg-final { scan-assembler-times {\mstfs\M}  1 } } */
/* { dg-final { scan-assembler-times {\msth\M}   2 } } */
/* { dg-final { scan-assembler-times {\mstw\M}   2 } } */
