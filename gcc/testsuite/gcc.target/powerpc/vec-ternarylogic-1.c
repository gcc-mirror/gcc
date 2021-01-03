/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

#define NumSamples 4

void
doTests00000001 (vector unsigned char a_sources [],
		 vector unsigned char b_sources [],
		 vector unsigned char c_sources []) {
  for (int i = 0; i < NumSamples; i++)
    for (int j = 0; j < NumSamples; j++)
      for (int k = 0; k < NumSamples; k++)
	{
	  vector unsigned char a = a_sources [i];
	  vector unsigned char b = b_sources [j];
	  vector unsigned char c = c_sources [k];
	  vector unsigned char result = vec_ternarylogic (a, b, c, 0x01);
	  vector unsigned char intended = (a & b & c);
	  if (!vec_all_eq (result, intended))
	    abort ();
	}
}

void
doTests11100101 (vector unsigned char a_sources [],
		 vector unsigned char b_sources [],
		 vector unsigned char c_sources []) {
  for (int i = 0; i < NumSamples; i++)
    for (int j = 0; j < NumSamples; j++)
      for (int k = 0; k < NumSamples; k++)
	{
	  vector unsigned char a = a_sources [i];
	  vector unsigned char b = b_sources [j];
	  vector unsigned char c = c_sources [k];
	  vector unsigned char result = vec_ternarylogic (a, b, c, 0xe5);
	  vector unsigned char intended =
	    { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	  // Supposed to be a ? c: nand (b,c)
	  for (int l = 0; l < 16; l++)
	    {
	      for (int m = 0; m < 8; m++)
	      {
		unsigned char bit_selector = (0x01 << m);
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

void
doTests11110011 (vector unsigned char a_sources [],
		 vector unsigned char b_sources [],
		 vector unsigned char c_sources []) {
  for (int i = 0; i < NumSamples; i++)
    for (int j = 0; j < NumSamples; j++)
      for (int k = 0; k < NumSamples; k++)
	{
	  vector unsigned char a = a_sources [i];
	  vector unsigned char b = b_sources [j];
	  vector unsigned char c = c_sources [k];
	  vector unsigned char result = vec_ternarylogic (a, b, c, 0xfb);
	  vector unsigned char intended = {
	    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	  for (int i = 0; i < 16; i++)
	    intended [i] = b [i] | ~(a [i] & c [i]);
	  if (!vec_all_eq (result, intended))
	    abort ();
	}
}

int main (int argc, char *argv [])
{
  vector unsigned char a_sources [NumSamples] = {
    { 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
      0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0 },
    { 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55,
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff },
    { 0xcc, 0xcc, 0xcc, 0xcc, 0x55, 0x55, 0x55, 0x55,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 },
    { 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7,
      0x69, 0x69, 0x69, 0x69, 0x69, 0x69, 0x69, 0x69 },
  };
  vector unsigned char b_sources [NumSamples] = {
    { 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
      0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0 },
    { 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55,
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff },
    { 0xcc, 0xcc, 0xcc, 0xcc, 0x55, 0x55, 0x55, 0x55,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 },
    { 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7,
      0x69, 0x69, 0x69, 0x69, 0x69, 0x69, 0x69, 0x69 },
  };
  vector unsigned char c_sources [NumSamples] = {
    { 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
      0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0 },
    { 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55,
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff },
    { 0xcc, 0xcc, 0xcc, 0xcc, 0x55, 0x55, 0x55, 0x55,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 },
    { 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7, 0xe7,
      0x69, 0x69, 0x69, 0x69, 0x69, 0x69, 0x69, 0x69 },
  };

  doTests00000001 (a_sources, b_sources, c_sources);
  doTests11100101 (a_sources, b_sources, c_sources);
  doTests11110011 (a_sources, b_sources, c_sources);

  return 0;
}
