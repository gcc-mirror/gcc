/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_assemble_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

/* Test whether the lxvrl and stxvrl instructions are generated for
   -mcpu=future on memory copy operations.  */

#ifndef VSIZE
#define VSIZE 2
#endif

#ifndef LSIZE
#define LSIZE 5
#endif

struct foo {
  vector unsigned char vc[VSIZE];
  unsigned char leftover[LSIZE];
};

void memcpy_ptr (struct foo *p, struct foo *q)
{
  __builtin_memcpy ((void *) p,		/* lxvrl and stxvrl.  */
		    (void *) q,
		    (sizeof (vector unsigned char) * VSIZE) + LSIZE);
}

/* { dg-final { scan-assembler     {\mlxvrl\M}  } } */
/* { dg-final { scan-assembler     {\mstxvrl\M} } } */
/* { dg-final { scan-assembler-not {\mlxvl\M}   } } */
/* { dg-final { scan-assembler-not {\mstxvl\M}  } } */
