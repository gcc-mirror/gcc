/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse2" } */
#include <xmmintrin.h>
#include "i386-cpuid.h"

#ifndef NOINLINE
#define NOINLINE __attribute__ ((noinline))
#endif

#define SHIFT (4)

typedef union {
  __m128i v;
  unsigned long s[4];
  unsigned short int t[8];
  unsigned long long u[2];
  unsigned char c[16];
}vecInLong;

typedef union {
  __m64 v;
  unsigned char c[8];
  unsigned short int s[4];
  unsigned long long t;
  unsigned int u[2];
}vecInWord;

void mmx_tests (void) NOINLINE;
void sse_tests (void) NOINLINE;
void sse2_tests (void) NOINLINE;
void dump64_16 (char *, char *, vecInWord);
void dump64_32 (char *, char *, vecInWord);
void dump64_64 (char *, char *, vecInWord);
void dump128_16 (char *, char *, vecInLong);
void dump128_32 (char *, char *, vecInLong);
void dump128_64 (char *, char *, vecInLong);
void dump128_128 (char *, char *, vecInLong);
int check (const char *, const char *[]);

char buf[8000];
char comparison[8000];
static int errors = 0;

vecInLong a128, b128, c128, d128, e128, f128;
vecInWord a64, b64, c64, d64, e64;
__m128i m128_16, m128_32, s128, m128_64, m128_128;
__m64 m64_16, s64, m64_32, m64_64;

const char *reference_mmx[] = {
  "_mm_srai_pi16 0012 0012 0012 0012 \n",
  "_mm_sra_pi16 0012 0012 0012 0012 \n",
  "_mm_srai_pi32 00123456 00123456 \n",
  "_mm_sra_pi32 00123456 00123456 \n",
  "_mm_srli_pi16 0012 0012 0012 0012 \n",
  "_mm_srl_pi16 0012 0012 0012 0012 \n",
  "_mm_srli_pi32 00123456 00123456 \n",
  "_mm_srl_pi32 00123456 00123456 \n",
  "_mm_srli_si64 00123456789abcde\n",
  "_mm_srl_si64 00123456789abcde\n",
  "_mm_slli_pi16 1230 1230 1230 1230 \n",
  "_mm_sll_pi16 1230 1230 1230 1230 \n",
  "_mm_slli_pi32 12345670 12345670 \n",
  "_mm_sll_pi32 12345670 12345670 \n",
  "_mm_slli_si64 123456789abcdef0\n",
  "_mm_sll_si64 123456789abcdef0\n",
  ""
};

const char *reference_sse[] = {
  "_mm_shuffle_pi16 0123 4567 89ab cdef \n",
  ""
};

const char *reference_sse2[] = {
  "_mm_srai_epi16 0012 0012 0012 0012 0012 0012 0012 0012 \n",
  "_mm_sra_epi16 0012 0012 0012 0012 0012 0012 0012 0012 \n",
  "_mm_srai_epi32 00123456 00123456 00123456 00123456 \n",
  "_mm_sra_epi32 00123456 00123456 00123456 00123456 \n",
  "_mm_srli_epi16 0012 0012 0012 0012 0012 0012 0012 0012 \n",
  "_mm_srl_epi16 0012 0012 0012 0012 0012 0012 0012 0012 \n",
  "_mm_srli_epi32 00123456 00123456 00123456 00123456 \n",
  "_mm_srl_epi32 00123456 00123456 00123456 00123456 \n",
  "_mm_srli_epi64 00123456789abcde 00123456789abcde \n",
  "_mm_srl_epi64 00123456789abcde 00123456789abcde \n",
  "_mm_srli_si128 (byte shift)  00000000ffeeddccbbaa998877665544\n",
  "_mm_slli_epi16 1230 1230 1230 1230 1230 1230 1230 1230 \n",
  "_mm_sll_epi16 1230 1230 1230 1230 1230 1230 1230 1230 \n",
  "_mm_slli_epi32 12345670 12345670 12345670 12345670 \n",
  "_mm_sll_epi32 12345670 12345670 12345670 12345670 \n",
  "_mm_slli_epi64 123456789abcdef0 123456789abcdef0 \n",
  "_mm_sll_epi64 123456789abcdef0 123456789abcdef0 \n",
  "_mm_sll_si128 (byte shift) bbaa9988776655443322110000000000\n",
  "_mm_shuffle_epi32 ffeeddcc bbaa9988 77665544 33221100 \n",
  "_mm_shuffelo_epi16 7766 5544 3322 1100 9988 bbaa ddcc ffee \n",
  "_mm_shuffehi_epi16 1100 3322 5544 7766 ffee ddcc bbaa 9988 \n",
  ""
};

int main()
{
  unsigned long *p;
  unsigned long cpu_facilities;

  cpu_facilities = i386_cpuid ();

  if (!cpu_facilities)	/* If host has no vector support, pass.  */
    exit (0);

  a128.s[0] = 0x01234567;
  a128.s[1] = 0x01234567;
  a128.s[2] = 0x01234567;
  a128.s[3] = 0x01234567;

  m128_32 = a128.v;

  d128.u[0] = 0x0123456789abcdefULL;
  d128.u[1] = 0x0123456789abcdefULL;

  m128_64 = d128.v;

  /* This is the 128-bit constant 0x00112233445566778899aabbccddeeff,
     expressed as two little-endian 64-bit words.  */
  e128.u[0] = 0x7766554433221100ULL;
  e128.u[1] = 0xffeeddccbbaa9988ULL;

  f128.t[0] = 0x0123;
  f128.t[1] = 0x0123;
  f128.t[2] = 0x0123;
  f128.t[3] = 0x0123;
  f128.t[4] = 0x0123;
  f128.t[5] = 0x0123;
  f128.t[6] = 0x0123;
  f128.t[7] = 0x0123;

  m128_16 = f128.v;

  m128_128 = e128.v;

  d64.u[0]  = 0x01234567;
  d64.u[1]  = 0x01234567;

  m64_32 = d64.v;

  e64.t  = 0x0123456789abcdefULL;

  m64_64 = e64.v;

  a64.s[0] = 0x0123;
  a64.s[1] = 0x0123;
  a64.s[2] = 0x0123;
  a64.s[3] = 0x0123;

  m64_16 = a64.v;

  b128.s[0] = SHIFT;
  b128.s[1] = 0;
  b128.s[2] = 0;
  b128.s[3] = 0;

  s128 = b128.v;

  b64.s[0] = SHIFT;
  b64.s[1] = 0;
  b64.s[2] = 0;
  b64.s[3] = 0;

  s64 = b64.v;

  if (cpu_facilities & bit_MMX)
    {
      mmx_tests();
      check (buf, reference_mmx);
#ifdef DEBUG
      printf ("mmx testing:\n");
      printf (buf);
      printf ("\ncomparison:\n");
      printf (comparison);
#endif
      buf[0] = '\0';
    }

  if (cpu_facilities & bit_SSE)
    {
      sse_tests();
      check (buf, reference_sse);
#ifdef DEBUG
      printf ("sse testing:\n");
      printf (buf);
      printf ("\ncomparison:\n");
      printf (comparison);
#endif
      buf[0] = '\0';
    }

  if (cpu_facilities & bit_SSE2)
    {
      sse2_tests();
      check (buf, reference_sse2);
#ifdef DEBUG
      printf ("sse2 testing:\n");
      printf (buf);
      printf ("\ncomparison:\n");
      printf (comparison);
#endif
      buf[0] = '\0';
    }

  if (errors != 0)
    abort ();
  exit (0);
}

void NOINLINE
mmx_tests (void)
{
  /* psraw */
  c64.v = _mm_srai_pi16 (m64_16, SHIFT);
  dump64_16 (buf, "_mm_srai_pi16", c64);
  c64.v  = _mm_sra_pi16 (m64_16, s64);
  dump64_16 (buf, "_mm_sra_pi16", c64);

  /* psrad */
  c64.v  = _mm_srai_pi32 (m64_32, SHIFT);
  dump64_32 (buf, "_mm_srai_pi32", c64);
  c64.v = _mm_sra_pi32 (m64_32, s64);
  dump64_32 (buf, "_mm_sra_pi32", c64);

  /* psrlw */
  c64.v = _mm_srli_pi16 (m64_16, SHIFT);
  dump64_16 (buf, "_mm_srli_pi16", c64);
  c64.v = _mm_srl_pi16 (m64_16, s64);
  dump64_16 (buf, "_mm_srl_pi16", c64);

  /* psrld */
  c64.v = _mm_srli_pi32 (m64_32, SHIFT);
  dump64_32 (buf, "_mm_srli_pi32", c64);
  c64.v = _mm_srl_pi32 (m64_32, s64);
  dump64_32 (buf, "_mm_srl_pi32", c64);

  /* psrlq */
  c64.v = _mm_srli_si64 (m64_64, SHIFT);
  dump64_64 (buf, "_mm_srli_si64", c64);
  c64.v = _mm_srl_si64 (m64_64, s64);
  dump64_64 (buf, "_mm_srl_si64", c64);

  /* psllw */
  c64.v = _mm_slli_pi16 (m64_16, SHIFT);
  dump64_16 (buf, "_mm_slli_pi16", c64);
  c64.v = _mm_sll_pi16 (m64_16, s64);
  dump64_16 (buf, "_mm_sll_pi16", c64);

  /* pslld */
  c64.v = _mm_slli_pi32 (m64_32, SHIFT);
  dump64_32 (buf, "_mm_slli_pi32", c64);
  c64.v = _mm_sll_pi32 (m64_32, s64);
  dump64_32 (buf, "_mm_sll_pi32", c64);

  /* psllq */
  c64.v = _mm_slli_si64 (m64_64, SHIFT);
  dump64_64 (buf, "_mm_slli_si64", c64);
  c64.v = _mm_sll_si64 (m64_64, s64);
  dump64_64 (buf, "_mm_sll_si64", c64);
}

void NOINLINE
sse_tests (void)
{
  /* pshufw */
  c64.v = _mm_shuffle_pi16 (m64_64, 0x1b);
  dump64_16 (buf, "_mm_shuffle_pi16", c64);
}

void NOINLINE
sse2_tests (void)
{
  /* psraw */
  c128.v = _mm_srai_epi16 (m128_16, SHIFT);
  dump128_16 (buf, "_mm_srai_epi16", c128);
  c128.v = _mm_sra_epi16 (m128_16, s128);
  dump128_16 (buf, "_mm_sra_epi16", c128);

  /* psrad */
  c128.v = _mm_srai_epi32 (m128_32, SHIFT);
  dump128_32 (buf, "_mm_srai_epi32", c128);
  c128.v = _mm_sra_epi32 (m128_32, s128);
  dump128_32 (buf, "_mm_sra_epi32", c128);

  /* psrlw */
  c128.v = _mm_srli_epi16 (m128_16, SHIFT);
  dump128_16 (buf, "_mm_srli_epi16", c128);
  c128.v = _mm_srl_epi16 (m128_16, s128);
  dump128_16 (buf, "_mm_srl_epi16", c128);

  /* psrld */
  c128.v = _mm_srli_epi32 (m128_32, SHIFT);
  dump128_32 (buf, "_mm_srli_epi32", c128);
  c128.v = _mm_srl_epi32 (m128_32, s128);
  dump128_32 (buf, "_mm_srl_epi32", c128);

  /* psrlq */
  c128.v = _mm_srli_epi64 (m128_64, SHIFT);
  dump128_64 (buf, "_mm_srli_epi64", c128);
  c128.v = _mm_srl_epi64 (m128_64, s128);
  dump128_64 (buf, "_mm_srl_epi64", c128);

  /* psrldq */
  c128.v = _mm_srli_si128 (m128_128, SHIFT);
  dump128_128 (buf, "_mm_srli_si128 (byte shift) ", c128);

  /* psllw */
  c128.v = _mm_slli_epi16 (m128_16, SHIFT);
  dump128_16 (buf, "_mm_slli_epi16", c128);
  c128.v = _mm_sll_epi16 (m128_16, s128);
  dump128_16 (buf, "_mm_sll_epi16", c128);

  /* pslld */
  c128.v = _mm_slli_epi32 (m128_32, SHIFT);
  dump128_32 (buf, "_mm_slli_epi32", c128);
  c128.v = _mm_sll_epi32 (m128_32, s128);
  dump128_32 (buf, "_mm_sll_epi32", c128);

  /* psllq */
  c128.v = _mm_slli_epi64 (m128_64, SHIFT);
  dump128_64 (buf, "_mm_slli_epi64", c128);
  c128.v = _mm_sll_epi64 (m128_64, s128);
  dump128_64 (buf, "_mm_sll_epi64", c128);

  /* pslldq */
  c128.v = _mm_slli_si128 (m128_128, SHIFT);
  dump128_128 (buf, "_mm_sll_si128 (byte shift)", c128);

  /* Shuffle constant 0x1b == 0b_00_01_10_11, e.g. swap words: ABCD => DCBA.  */

  /* pshufd */
  c128.v = _mm_shuffle_epi32 (m128_128, 0x1b);
  dump128_32 (buf, "_mm_shuffle_epi32", c128);

  /* pshuflw */
  c128.v = _mm_shufflelo_epi16 (m128_128, 0x1b);
  dump128_16 (buf, "_mm_shuffelo_epi16", c128);

  /* pshufhw */
  c128.v = _mm_shufflehi_epi16 (m128_128, 0x1b);
  dump128_16 (buf, "_mm_shuffehi_epi16", c128);
}

void
dump64_16 (char *buf, char *name, vecInWord x)
{
  int i;
  char *p = buf + strlen (buf);

  sprintf (p, "%s ", name);
  p += strlen (p);

  for (i=0; i<4; i++)
    {
      sprintf (p, "%4.4x ", x.s[i]);
      p += strlen (p);
    }
  strcat (p, "\n");
}

void
dump64_32 (char *buf, char *name, vecInWord x)
{
  int i;
  char *p = buf + strlen (buf);

  sprintf (p, "%s ", name);
  p += strlen (p);

  for (i=0; i<2; i++)
    {
      sprintf (p, "%8.8x ", x.u[i]);
      p += strlen (p);
    }
  strcat (p, "\n");
}

void
dump64_64 (char *buf, char *name, vecInWord x)
{
  int i;
  char *p = buf + strlen (buf);

  sprintf (p, "%s ", name);
  p += strlen (p);

  sprintf (p, "%16.16llx\n", x.t);
}

void
dump128_16 (char *buf, char *name, vecInLong x)
{
  int i;
  char *p = buf + strlen (buf);

  sprintf (p, "%s ", name);
  p += strlen (p);

  for (i=0; i<8; i++)
    {
      sprintf (p, "%4.4x ", x.t[i]);
      p += strlen (p);
    }
  strcat (p, "\n");
}

void
dump128_32 (char *buf, char *name, vecInLong x)
{
  int i;
  char *p = buf + strlen (buf);

  sprintf (p, "%s ", name);
  p += strlen (p);

  for (i=0; i<4; i++)
    {
      sprintf (p, "%8.8x ", x.s[i]);
      p += strlen (p);
    }
  strcat (p, "\n");
}

void
dump128_64 (char *buf, char *name, vecInLong x)
{
  int i;
  char *p = buf + strlen (buf);

  sprintf (p, "%s ", name);
  p += strlen (p);

  for (i=0; i<2; i++)
    {
      sprintf (p, "%16.16llx ", x.u[i]);
      p += strlen (p);
    }
  strcat (p, "\n");
}

void
dump128_128 (char *buf, char *name, vecInLong x)
{
  int i;
  char *p = buf + strlen (buf);

  sprintf (p, "%s ", name);
  p += strlen (p);

  for (i=15; i>=0; i--)
    {
      /* This is cheating; we don't have a 128-bit int format code.
	 Running the loop backwards to compensate for the
	 little-endian layout. */
      sprintf (p, "%2.2x", x.c[i]);
      p += strlen (p);
    }
  strcat (p, "\n");
}

int
check (const char *input, const char *reference[])
{
  int broken, i, j, len;
  const char *p_input;
  char *p_comparison;
  int new_errors = 0;

  p_comparison = &comparison[0];
  p_input = input;

  for (i = 0; *reference[i] != '\0'; i++)
    {
      broken = 0;
      len = strlen (reference[i]);
      for (j = 0; j < len; j++)
	{
	  /* Ignore the terminating NUL characters at the end of every string in 'reference[]'.  */
	  if (!broken && *p_input != reference[i][j])
	    {
	      *p_comparison = '\0';
	      strcat (p_comparison, " >>> ");
	      p_comparison += strlen (p_comparison);
	      new_errors++;
	      broken = 1;
	    }
	  *p_comparison = *p_input;
	  p_comparison++;
	  p_input++;
	}
      if (broken)
	{
	  *p_comparison = '\0';
	  strcat (p_comparison, "expected:\n");
	  strcat (p_comparison, reference[i]);
	  p_comparison += strlen (p_comparison);
	}
    }
  *p_comparison = '\0';
  strcat (p_comparison, new_errors ? "failure\n\n" : "O.K.\n\n") ;
  errors += new_errors;
}
