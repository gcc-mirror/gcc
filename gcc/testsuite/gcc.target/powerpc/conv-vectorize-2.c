/* { dg-options "-O2 -ftree-vectorize -mvsx -fno-vect-cost-model" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Test vectorizer can exploit vector conversion instructions to convert
   float to unsigned/signed long long.  */

#include <stddef.h>

#define SIZE 32
#define ALIGN 16

float sflt_array[SIZE] __attribute__ ((__aligned__ (ALIGN)));
float uflt_array[SIZE] __attribute__ ((__aligned__ (ALIGN)));

unsigned long long ulong_array[SIZE] __attribute__ ((__aligned__ (ALIGN)));
signed long long slong_array[SIZE] __attribute__ ((__aligned__ (ALIGN)));

void
convert_float_to_slong (void)
{
  size_t i;

  for (i = 0; i < SIZE; i++)
    slong_array[i] = (signed long long) sflt_array[i];
}

void
convert_float_to_ulong (void)
{
  size_t i;

  for (i = 0; i < SIZE; i++)
    ulong_array[i] = (unsigned long long) uflt_array[i];
}

/* { dg-final { scan-assembler {\mxvcvspsxds\M} } } */
/* { dg-final { scan-assembler {\mxvcvspuxds\M} } } */
