/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

#define TYPE	int
#define LARGE	0x20000

/* Test whether we get the right number of PCREL_OPT optimizations for int.  */
extern TYPE ext[];

TYPE
get (void)
{
  return ext[0];		/* PCREL_OPT relocation.  */
}

TYPE
get2 (void)
{
  return ext[2];		/* PCREL_OPT relocation.  */
}

TYPE
get_large (void)
{
  return ext[LARGE];		/* No PCREL_OPT (load is  prefixed).  */
}

TYPE
get_variable (unsigned long n)
{
  return ext[n];		/* No PCREL_OPT (load is indexed).  */
}

double
get_double (void)
{
  return (double) ext[0];	/* No PCREL_OPT (LFIWAX is indexed).  */
}

/* { dg-final { scan-assembler-times "R_PPC64_PCREL_OPT"  2 } } */
