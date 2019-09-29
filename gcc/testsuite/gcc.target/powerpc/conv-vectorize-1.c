/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -ftree-vectorize -mvsx" } */

/* Test vectorizer can exploit vector conversion instructions to convert
   unsigned/signed long long to float.  */

#include <stddef.h>

#define SIZE 32
#define ALIGN 16

float sflt_array[SIZE] __attribute__ ((__aligned__ (ALIGN)));
float uflt_array[SIZE] __attribute__ ((__aligned__ (ALIGN)));

unsigned long long ulong_array[SIZE] __attribute__ ((__aligned__ (ALIGN)));
signed long long slong_array[SIZE] __attribute__ ((__aligned__ (ALIGN)));

void
convert_slong_to_float (void)
{
  size_t i;

  for (i = 0; i < SIZE; i++)
    sflt_array[i] = (float) slong_array[i];
}

void
convert_ulong_to_float (void)
{
  size_t i;

  for (i = 0; i < SIZE; i++)
    uflt_array[i] = (float) ulong_array[i];
}

/* { dg-final { scan-assembler {\mxvcvsxdsp\M} } } */
/* { dg-final { scan-assembler {\mxvcvuxdsp\M} } } */
