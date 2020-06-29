/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

#define NumSamples 4

void
doTests00000001 (vector unsigned int a_sources [],
		 vector unsigned int b_sources [],
		 vector unsigned int c_sources []) {
  for (int i = 0; i < NumSamples; i++)
    for (int j = 0; j < NumSamples; j++)
      for (int k = 0; k < NumSamples; k++)
	{
	  vector unsigned int a = a_sources [i];
	  vector unsigned int b = b_sources [j];
	  vector unsigned int c = c_sources [k];
	  vector unsigned int result = vec_ternarylogic (a, b, c, 0x01);
	  vector unsigned int intended = (a & b & c);
	  if (!vec_all_eq (result, intended))
	    abort ();
	}
}

void doTests11100101 (vector unsigned int a_sources [],
		      vector unsigned int b_sources [],
		      vector unsigned int c_sources []) {
  for (int i = 0; i < NumSamples; i++)
    for (int j = 0; j < NumSamples; j++)
      for (int k = 0; k < NumSamples; k++)
	{
	  vector unsigned int a = a_sources [i];
	  vector unsigned int b = b_sources [j];
	  vector unsigned int c = c_sources [k];
	  vector unsigned int result = vec_ternarylogic (a, b, c, 0xe5);
	  vector unsigned int intended = { 0, 0, 0, 0 };
	  // Supposed to be a ? c: nand (b,c)
	  for (int l = 0; l < 4; l++)
	    {
	      for (int m = 0; m < 32; m++)
	      {
		unsigned int bit_selector = (0x01 << m);
		if (a[l] & bit_selector)
		  intended [l] |= c [l] & bit_selector;
		else if ((b [l] & c [l] & bit_selector) == 0)
		  intended [l] |= bit_selector;
	      }
	    }
	  if (!vec_all_eq (result, intended))
	    abort ();
	}
}

void doTests11110011 (vector unsigned int a_sources [],
		      vector unsigned int b_sources [],
		      vector unsigned int c_sources []) {
  for (int i = 0; i < NumSamples; i++)
    for (int j = 0; j < NumSamples; j++)
      for (int k = 0; k < NumSamples; k++)
	{
	  vector unsigned int a = a_sources [i];
	  vector unsigned int b = b_sources [j];
	  vector unsigned int c = c_sources [k];
	  vector unsigned int result = vec_ternarylogic (a, b, c, 0xfb);
	  vector unsigned int intended = { 0, 0, 0, 0 };
	  for (int i = 0; i < 4; i++)
	    intended [i] = b [i] | ~(a [i] & c [i]);
	  if (!vec_all_eq (result, intended))
	    abort ();
	}
}

int main (int argc, int *argv [])
{
  vector unsigned int a_sources [NumSamples] = {
    { 0x01234567, 0x89abcdef, 0x12345678, 0x9abcdef0 },
    { 0x55555555, 0x55555555, 0xffffffff, 0xffffffff },
    { 0xcccccccc, 0x55555555, 0x00000000, 0x00000000 },
    { 0xe7e7e7e7, 0xe7e7e7e7, 0x69696969, 0x69696969 },
  };
  vector unsigned int b_sources [NumSamples] = {
    { 0x01234567, 0x89abcdef, 0x12345678, 0x9abcdef0 },
    { 0x55555555, 0x55555555, 0xffffffff, 0xffffffff },
    { 0xcccccccc, 0x55555555, 0x00000000, 0x00000000 },
    { 0xe7e7e7e7, 0xe7e7e7e7, 0x69696969, 0x69696969 },
  };
  vector unsigned int c_sources [NumSamples] = {
    { 0x01234567, 0x89abcdef, 0x12345678, 0x9abcdef0 },
    { 0x55555555, 0x55555555, 0xffffffff, 0xffffffff },
    { 0xcccccccc, 0x55555555, 0x00000000, 0x00000000 },
    { 0xe7e7e7e7, 0xe7e7e7e7, 0x69696969, 0x69696969 },
  };

  doTests00000001 (a_sources, b_sources, c_sources);
  doTests11100101 (a_sources, b_sources, c_sources);
  doTests11110011 (a_sources, b_sources, c_sources);

  return 0;
}

/* { dg-final { scan-assembler {\mxxeval\M} } } */
