/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

#define TYPE	int
#define LARGE	0x20000

/* Test whether we get the right number of PCREL_OPT optimizations for int.  */
extern TYPE ext[];

void
store (TYPE a)
{
  ext[0] = a;			/* PCREL_OPT relocation.  */
}

void
store2 (TYPE a)
{
  ext[2] = a;			/* PCREL_OPT relocation.  */
}

void
store_large (TYPE a)
{
  ext[LARGE] = a;		/* No PCREL_OPT (store is prefixed).  */
}

void
store_variable (TYPE a, unsigned long n)
{
  ext[n] = a;			/* No PCREL_OPT (store is indexed).  */
}

void
store_double (double a)
{
  ext[0] = (TYPE) a;		/* No PCREL_OPT (STFIWX is indexed).  */
}

/* { dg-final { scan-assembler-times "R_PPC64_PCREL_OPT"  2 } } */
