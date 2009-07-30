/* { dg-do run { target powerpc*-*-* } } */
/* { dg-options "-O2 -std=gnu99" } */

extern void abort (void);

static unsigned char bytes[] = { 0, 1, 2, 0x80, 0xff };

unsigned short b16a (unsigned short *p) { return __builtin_bswap16 (*p); }
void b16b (unsigned short *p, unsigned short a) { *p = __builtin_bswap16 (a); }
int b16c (unsigned short a) { return __builtin_bswap16 (a); }

unsigned int b32a (unsigned int *p) { return __builtin_bswap32 (*p); }
void b32b (unsigned int *p, unsigned int a) { *p = __builtin_bswap32 (a); }
static unsigned int b32c (unsigned int a) { return __builtin_bswap32 (a); }

unsigned long long b64a (unsigned long long *p) { return __builtin_bswap64 (*p); }
void b64b (unsigned long long *p, unsigned long long a) { *p = __builtin_bswap64 (a); }
unsigned long long b64c (unsigned long long a) { return __builtin_bswap64 (a); }

int
main (void)
{
  unsigned i1, i2, i3, i4, i5;
  unsigned b1, b2, b3, b4, b5;
  unsigned short b16_inp, b16_exp, b16_var;
  unsigned int b32_inp, b32_exp, b32_var;
  unsigned long long b64_inp, b64_exp, b64_var;

  for (i1 = 0; i1 < sizeof (bytes); i1++)
    {
      b1 = bytes[i1];
      for (i2 = 0; i2 < sizeof (bytes); i2++)
	{
	  b2 = bytes[i2];
	  b16_inp = (b1 << 8) | b2;
	  b16_exp = (b2 << 8) | b1;

	  if (b16a (&b16_inp) != b16_exp)
	    abort ();

	  b16b (&b16_var, b16_inp);
	  if (b16_var != b16_exp)
	    abort ();

	  if (b16c (b16_inp) != b16_exp)
	    abort ();

	  for (i3 = 0; i3 < sizeof (bytes); i3++)
	    {
	      b3 = bytes[i3];
	      for (i4 = 0; i4 < sizeof (bytes); i4++)
		{
		  b4 = bytes[i4];
		  b32_inp = (b1 << 24) | (b2 << 16) | (b3 << 8) | b4;
		  b32_exp = (b4 << 24) | (b3 << 16) | (b2 << 8) | b1;

		  if (b32a (&b32_inp) != b32_exp)
		    abort ();

		  b32b (&b32_var, b32_inp);
		  if (b32_var != b32_exp)
		    abort ();

		  if (b32c (b32_inp) != b32_exp)
		    abort ();

		  for (i5 = 0; i5 < sizeof (bytes); i5++)
		    {
		      b5 = bytes[i5];
		      b64_inp = (((unsigned long long)b32_inp) << 32) | b5;
		      b64_exp = (((unsigned long long)b5) << 56) | b32_exp;

		      if (b64a (&b64_inp) != b64_exp)
			abort ();

		      b64b (&b64_var, b64_inp);
		      if (b64_var != b64_exp)
			abort ();

		      if (b64c (b64_inp) != b64_exp)
			abort ();

		      b64_inp = (((unsigned long long)b5) << 56) | b32_inp;
		      b64_exp = (((unsigned long long)b32_exp) << 32) | b5;

		      if (b64a (&b64_inp) != b64_exp)
			abort ();

		      b64b (&b64_var, b64_inp);
		      if (b64_var != b64_exp)
			abort ();

		      if (b64c (b64_inp) != b64_exp)
			abort ();
		    }
		}
	    }
	}
    }

  return 0;
}
