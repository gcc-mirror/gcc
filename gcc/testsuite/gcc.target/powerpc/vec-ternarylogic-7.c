/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

#define NumSamples 4

void
doTests00000001 (vector unsigned long long int a_sources [],
		 vector unsigned long long int b_sources [],
		 vector unsigned long long int c_sources []) {
  for (int i = 0; i < NumSamples; i++)
    for (int j = 0; j < NumSamples; j++)
      for (int k = 0; k < NumSamples; k++)
	{
	  vector unsigned long long a = a_sources [i];
	  vector unsigned long long b = b_sources [j];
	  vector unsigned long long c = c_sources [k];
	  vector unsigned long long result = vec_ternarylogic (a, b, c, 0x01);
	  vector unsigned long long intended = (a & b & c);
	  if (!vec_all_eq (result, intended))
	    abort ();
	}
}

void doTests11100101 (vector unsigned long long int a_sources [],
		      vector unsigned long long int b_sources [],
		      vector unsigned long long int c_sources []) {
  for (int i = 0; i < NumSamples; i++)
    for (int j = 0; j < NumSamples; j++)
      for (int k = 0; k < NumSamples; k++)
	{
	  vector unsigned long long a = a_sources [i];
	  vector unsigned long long b = b_sources [j];
	  vector unsigned long long c = c_sources [k];
	  vector unsigned long long result = vec_ternarylogic (a, b, c, 0xe5);
	  vector unsigned long long intended = { 0, 0 };
	  // Supposed to be a ? c: nand (b,c)
	  for (int l = 0; l < 2; l++)
	    {
	      for (int m = 0; m < 64; m++)
	      {
		unsigned long long int bit_selector = (0x01ll << m);
		if (a[l] & bit_selector)
		  intended [l] |= c [l] & bit_selector;
		else if ((b [l] & c [l] & bit_selector) == 0)
		  intended [l] |= (0x01ll << m);
	      }
	    }
	  if (!vec_all_eq (result, intended))
	    abort ();
	}
}

void doTests11110011 (vector unsigned long long int a_sources [],
		      vector unsigned long long int b_sources [],
		      vector unsigned long long int c_sources []) {
  for (int i = 0; i < NumSamples; i++)
    for (int j = 0; j < NumSamples; j++)
      for (int k = 0; k < NumSamples; k++)
	{
	  vector unsigned long long a = a_sources [i];
	  vector unsigned long long b = b_sources [j];
	  vector unsigned long long c = c_sources [k];
	  vector unsigned long long result = vec_ternarylogic (a, b, c, 0xfb);
	  vector unsigned long long intended = { 0, 0 };
	  intended [0] = b [0] | ~(a [0] & c [0]);
	  intended [1] = b [1] | ~(a [1] & c [1]);
	  if (!vec_all_eq (result, intended))
	    abort ();
	}
}

int main (int argc, char *argv [])
{
  vector unsigned long long int a_sources [NumSamples] = {
    { 0x0123456789abcdef, 0x123456789abcdef0 },
    { 0x5555555555555555, 0xffffffffffffffff },
    { 0xcccccccc55555555, 0x0000000000000000 },
    { 0xe7e7e7e7e7e7e7e7, 0x6969696969696969 },
  };
  vector unsigned long long int b_sources [NumSamples] = {
    { 0x0123456789abcdef, 0x123456789abcdef0 },
    { 0x5555555555555555, 0xffffffffffffffff },
    { 0xcccccccc55555555, 0x0000000000000000 },
    { 0xe7e7e7e7e7e7e7e7, 0x6969696969696969 },
  };
  vector unsigned long long int c_sources [NumSamples] = {
    { 0x0123456789abcdef, 0x123456789abcdef0 },
    { 0x5555555555555555, 0xffffffffffffffff },
    { 0xcccccccc55555555, 0x0000000000000000 },
    { 0xe7e7e7e7e7e7e7e7, 0x6969696969696969 },
  };

  doTests00000001 (a_sources, b_sources, c_sources);
  doTests11100101 (a_sources, b_sources, c_sources);
  doTests11110011 (a_sources, b_sources, c_sources);

  return 0;
}
