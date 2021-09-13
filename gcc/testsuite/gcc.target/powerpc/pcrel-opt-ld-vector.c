/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -mpcrel-opt" } */

#define TYPE	vector double
#define LARGE	0x20000

/* Test whether we get the right number of PCREL_OPT optimizations for
   vector double.  */
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

/* { dg-final { scan-assembler-times "R_PPC64_PCREL_OPT"  2 } } */
