/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -save-temps" } */

#include <altivec.h>

extern void abort (void);

#define NumSamples 4

void
doTests00000001 (vector unsigned short int a_sources [],
		 vector unsigned short int b_sources [],
		 vector unsigned short int c_sources []) {
  for (int i = 0; i < NumSamples; i++)
    for (int j = 0; j < NumSamples; j++)
      for (int k = 0; k < NumSamples; k++)
	{
	  vector unsigned short a = a_sources [i];
	  vector unsigned short b = b_sources [j];
	  vector unsigned short c = c_sources [k];
	  vector unsigned short result = vec_ternarylogic (a, b, c, 0x01);
	  vector unsigned short intended = (a & b & c);
	  if (!vec_all_eq (result, intended))
	    abort ();
	}
}

void doTests11100101 (vector unsigned short int a_sources [],
		      vector unsigned short int b_sources [],
		      vector unsigned short int c_sources []) {
  for (int i = 0; i < NumSamples; i++)
    for (int j = 0; j < NumSamples; j++)
      for (int k = 0; k < NumSamples; k++)
	{
	  vector unsigned short a = a_sources [i];
	  vector unsigned short b = b_sources [j];
	  vector unsigned short c = c_sources [k];
	  vector unsigned short result = vec_ternarylogic (a, b, c, 0xe5);
	  vector unsigned short intended =
	    { 0, 0, 0, 0, 0, 0, 0, 0 };
	  // Supposed to be a ? c: nand (b,c)
	  for (int l = 0; l < 8; l++)
	    {
	      for (int m = 0; m < 16; m++)
	      {
		unsigned short int bit_selector = (0x01 << m);
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

void doTests11110011 (vector unsigned short int a_sources [],
		      vector unsigned short int b_sources [],
		      vector unsigned short int c_sources []) {
  for (int i = 0; i < NumSamples; i++)
    for (int j = 0; j < NumSamples; j++)
      for (int k = 0; k < NumSamples; k++)
	{
	  vector unsigned short a = a_sources [i];
	  vector unsigned short b = b_sources [j];
	  vector unsigned short c = c_sources [k];
	  vector unsigned short result = vec_ternarylogic (a, b, c, 0xfb);
	  vector unsigned short intended = { 0, 0, 0, 0, 0, 0, 0, 0 };
	  for (int i = 0; i < 8; i++)
	    intended [i] = b [i] | ~(a [i] & c [i]);
	  if (!vec_all_eq (result, intended))
	    abort ();
	}
}

int main (int argc, short *argv [])
{
  vector unsigned short int a_sources [NumSamples] = {
    { 0x0123, 0x4567, 0x89ab, 0xcdef, 0x1234, 0x5678, 0x9abc, 0xdef0 },
    { 0x5555, 0x5555, 0x5555, 0x5555, 0xffff, 0xffff, 0xffff, 0xffff },
    { 0xcccc, 0xcccc, 0x5555, 0x5555, 0x0000, 0x0000, 0x0000, 0x0000 },
    { 0xe7e7, 0xe7e7, 0xe7e7, 0xe7e7, 0x6969, 0x6969, 0x6969, 0x6969 },
  };
  vector unsigned short int b_sources [NumSamples] = {
    { 0x0123, 0x4567, 0x89ab, 0xcdef, 0x1234, 0x5678, 0x9abc, 0xdef0 },
    { 0x5555, 0x5555, 0x5555, 0x5555, 0xffff, 0xffff, 0xffff, 0xffff },
    { 0xcccc, 0xcccc, 0x5555, 0x5555, 0x0000, 0x0000, 0x0000, 0x0000 },
    { 0xe7e7, 0xe7e7, 0xe7e7, 0xe7e7, 0x6969, 0x6969, 0x6969, 0x6969 },
  };
  vector unsigned short int c_sources [NumSamples] = {
    { 0x0123, 0x4567, 0x89ab, 0xcdef, 0x1234, 0x5678, 0x9abc, 0xdef0 },
    { 0x5555, 0x5555, 0x5555, 0x5555, 0xffff, 0xffff, 0xffff, 0xffff },
    { 0xcccc, 0xcccc, 0x5555, 0x5555, 0x0000, 0x0000, 0x0000, 0x0000 },
    { 0xe7e7, 0xe7e7, 0xe7e7, 0xe7e7, 0x6969, 0x6969, 0x6969, 0x6969 },
  };

  doTests00000001 (a_sources, b_sources, c_sources);
  doTests11100101 (a_sources, b_sources, c_sources);
  doTests11110011 (a_sources, b_sources, c_sources);

  return 0;
}

/* { dg-final { scan-assembler {\mxxeval\M} } } */
