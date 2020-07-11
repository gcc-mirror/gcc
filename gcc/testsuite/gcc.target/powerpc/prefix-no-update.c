/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Make sure that we don't generate a prefixed form of the load and store with
   update instructions (i.e. instead of generating LWZU we have to generate
   PLWZ plus a PADDI).  */

#ifndef SIZE
#define SIZE 50000
#endif

struct foo {
  unsigned int field;
  char pad[SIZE];
};

struct foo *inc_load (struct foo *p, unsigned int *q)
{
  *q = (++p)->field;	/* PLWZ, PADDI, STW.  */
  return p;
}

struct foo *dec_load (struct foo *p, unsigned int *q)
{
  *q = (--p)->field;	/* PLWZ, PADDI, STW.  */
  return p;
}

struct foo *inc_store (struct foo *p, unsigned int *q)
{
  (++p)->field = *q;	/* LWZ, PADDI, PSTW.  */
  return p;
}

struct foo *dec_store (struct foo *p, unsigned int *q)
{
  (--p)->field = *q;	/* LWZ, PADDI, PSTW.  */
  return p;
}

/* { dg-final { scan-assembler-times {\mlwz\M}    2 } } */
/* { dg-final { scan-assembler-times {\mstw\M}    2 } } */
/* { dg-final { scan-assembler-times {\mpaddi\M}  4 } } */
/* { dg-final { scan-assembler-times {\mplwz\M}   2 } } */
/* { dg-final { scan-assembler-times {\mpstw\M}   2 } } */
/* { dg-final { scan-assembler-not   {\mplwzu\M}    } } */
/* { dg-final { scan-assembler-not   {\mpstwu\M}    } } */
/* { dg-final { scan-assembler-not   {\maddis\M}    } } */
/* { dg-final { scan-assembler-not   {\maddi\M}     } } */
