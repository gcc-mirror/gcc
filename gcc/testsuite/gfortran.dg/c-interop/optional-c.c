#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ftest (int n, CFI_cdesc_t *a, int *b, char *c, double *d);
extern void ctest1 (CFI_cdesc_t *a, int *b, char *c, double *d);
extern void ctest2 (int n, CFI_cdesc_t *a, int *b, char *c, double *d);

static void *aa;
static int *bb;
static char *cc;
static double *dd; 

extern void
ctest1 (CFI_cdesc_t *a, int *b, char *c, double *d)
{
  /* Cache all the pointer arguments for later use by ctest2.  */
  aa = a->base_addr;
  bb = b;
  cc = c;
  dd = d;

  /* Test calling back into Fortran.  */
  ftest (0, NULL, NULL, NULL, NULL);
  ftest (1, a, NULL, NULL, NULL);
  ftest (2, a, b, NULL, NULL);
  ftest (3, a, b, c, NULL);
  ftest (4, a, b, c, d);
}

extern void
ctest2 (int n, CFI_cdesc_t *a, int *b, char *c, double *d)
{
  if (n >= 1)
    {
      if (!a)
	abort ();
      if (a->base_addr != aa)
	abort ();
    }
  else
    if (a)
      abort ();

  if (n >= 2)
    {
      if (!b)
	abort ();
      if (*b != *bb)
	abort ();
    }
  else
    if (b)
      abort ();

  if (n >= 3)
    {
      if (!c)
	abort ();
      if (*c != *cc)
	abort ();
    }
  else
    if (c)
      abort ();

  if (n >= 4)
    {
      if (!d)
	abort ();
      if (*d != *dd)
	abort ();
    }
  else
    if (d)
      abort ();

}


