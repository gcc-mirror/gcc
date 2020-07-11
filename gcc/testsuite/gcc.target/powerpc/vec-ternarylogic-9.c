/* { dg-do run } */
/* { dg-require-effective-target power10_hw } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

#define NumSamples 4

/* vec_all_eq not yet supported for arguments of type
   vector unsigned __int128.  */
int
vector_equal (vector unsigned __int128 a, vector unsigned __int128 b)
{
  return a[0] == b[0];
}

void
doTests00000001 (vector unsigned __int128 a_sources [],
		 vector unsigned __int128 b_sources [],
		 vector unsigned __int128 c_sources []) {
  for (int i = 0; i < NumSamples; i++)
    for (int j = 0; j < NumSamples; j++)
      for (int k = 0; k < NumSamples; k++)
	{
	  vector unsigned __int128 a = a_sources [i];
	  vector unsigned __int128 b = b_sources [j];
	  vector unsigned __int128 c = c_sources [k];
	  vector unsigned __int128 result = vec_ternarylogic (a, b, c, 0x01);
	  vector unsigned __int128 intended = (a & b & c);
	  if (!vector_equal (result, intended))
	    abort ();
	}
}

void
doTests11100101 (vector unsigned __int128 a_sources [],
		 vector unsigned __int128 b_sources [],
		 vector unsigned __int128 c_sources []) {
  for (int i = 0; i < NumSamples; i++)
    for (int j = 0; j < NumSamples; j++)
      for (int k = 0; k < NumSamples; k++)
	{
	  vector unsigned __int128 a = a_sources [i];
	  vector unsigned __int128 b = b_sources [j];
	  vector unsigned __int128 c = c_sources [k];
	  vector unsigned __int128 result = vec_ternarylogic (a, b, c, 0xe5);
	  vector unsigned __int128 intended = { 0 };
	  // Supposed to be a ? c: nand (b,c)
	  for (int l = 0; l < 1; l++)
	    {
	      for (int m = 0; m < 128; m++)
	      {
		unsigned __int128 bit_selector = 0x01;
		bit_selector = bit_selector << m;

		if (a[l] & bit_selector)
		  intended [l] |= c [l] & bit_selector;
		else if ((b [l] & c [l] & bit_selector) == 0)
		  intended [l] |= bit_selector;
	      }
	    }
	  if (!vector_equal (result, intended))
	    abort ();
	}
}

void
doTests11110011 (vector unsigned __int128 a_sources [],
		 vector unsigned __int128 b_sources [],
		 vector unsigned __int128 c_sources []) {
  for (int i = 0; i < NumSamples; i++)
    for (int j = 0; j < NumSamples; j++)
      for (int k = 0; k < NumSamples; k++)
	{
	  vector unsigned __int128 a = a_sources [i];
	  vector unsigned __int128 b = b_sources [j];
	  vector unsigned __int128 c = c_sources [k];
	  vector unsigned __int128 result = vec_ternarylogic (a, b, c, 0xfb);
	  vector unsigned __int128 intended = { 0 };
	  for (int i = 0; i < 1; i++)
	    intended [i] = b [i] | ~(a [i] & c [i]);
	  if (!vector_equal (result, intended))
	    abort ();
	}
}

int main (int argc, int *argv [])
{
  vector unsigned __int128 a_sources [NumSamples];
  vector unsigned __int128 b_sources [NumSamples];
  vector unsigned __int128 c_sources [NumSamples];

  a_sources [0][0] = 0x0123456789abcdefull;
  a_sources [0][0] = a_sources [0][0] << 64 | 0x123456789abcdef0ull;
  a_sources [1][0] = 0x5555555555555555ull;
  a_sources [1][0] = a_sources [1][0] << 64 | 0xffffffffffffffffull;
  a_sources [2][0] = 0xcccccccc55555555ull;
  a_sources [2][0] = a_sources [2][0] << 64 | 0x0000000000000000ull;
  a_sources [3][0] = 0xe7e7e7e7e7e7e7e7ull;
  a_sources [3][0] = a_sources [3][0] << 64 | 0x6969696969696969ull;

  b_sources [0][0] = 0x0123456789abcdefull;
  b_sources [0][0] = b_sources [0][0] << 64 | 0x123456789abcdef0ull;
  b_sources [1][0] = 0x5555555555555555ull;
  b_sources [1][0] = b_sources [1][0] << 64 | 0xffffffffffffffffull;
  b_sources [2][0] = 0xcccccccc55555555ull;
  b_sources [2][0] = b_sources [2][0] << 64 | 0x0000000000000000ull;
  b_sources [3][0] = 0xe7e7e7e7e7e7e7e7ull;
  b_sources [3][0] = b_sources [3][0] << 64 | 0x6969696969696969ull;

  c_sources [0][0] = 0x0123456789abcdefull;
  c_sources [0][0] = c_sources [0][0] << 64 | 0x123456789abcdef0ull;
  c_sources [1][0] = 0x5555555555555555ull;
  c_sources [1][0] = c_sources [1][0] << 64 | 0xffffffffffffffffull;
  c_sources [2][0] = 0xcccccccc55555555ull;
  c_sources [2][0] = c_sources [2][0] << 64 | 0x0000000000000000ull;
  c_sources [3][0] = 0xe7e7e7e7e7e7e7e7ull;
  c_sources [3][0] = c_sources [3][0] << 64 | 0x6969696969696969ull;

  doTests00000001 (a_sources, b_sources, c_sources);
  doTests11100101 (a_sources, b_sources, c_sources);
  doTests11110011 (a_sources, b_sources, c_sources);

  return 0;
}

/* { dg-final { scan-assembler {\mxxeval\M} } } */
