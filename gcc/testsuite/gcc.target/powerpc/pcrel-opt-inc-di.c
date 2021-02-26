/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

#define TYPE	unsigned int

/* Test whether using an external variable twice (doing an increment) prevents
   the PCREL_OPT optimization.  */
extern TYPE ext;

void
inc (void)
{
  ext++;		/* No PCREL_OPT (uses address twice).  */
}

/* { dg-final { scan-assembler-not "R_PPC64_PCREL_OPT" } } */
