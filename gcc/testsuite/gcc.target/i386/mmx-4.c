/* { dg-do run } */
/* { dg-options "-O2 -mmmx" } */

#include "mmx-check.h"

#include <mmintrin.h>
#include <string.h>

#define SHIFT (4)

typedef union {
  __m64 v;
  unsigned char c[8];
  unsigned short int s[4];
  unsigned long long t;
  unsigned int u[2];
}vecInWord;

void mmx_tests (void) __attribute__((noinline));
void dump64_16 (char *, char *, vecInWord);
void dump64_32 (char *, char *, vecInWord);
void dump64_64 (char *, char *, vecInWord);
int check (const char *, const char *[]);

char buf[8000];
char comparison[8000];
static int errors = 0;

vecInWord a64, b64, c64, d64, e64;
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


static void
mmx_test (void)
{
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

  b64.s[0] = SHIFT;
  b64.s[1] = 0;
  b64.s[2] = 0;
  b64.s[3] = 0;

  s64 = b64.v;

  mmx_tests();
  check (buf, reference_mmx);
#ifdef DEBUG
  printf ("mmx testing:\n");
  printf (buf);
  printf ("\ncomparison:\n");
  printf (comparison);
#endif
  buf[0] = '\0';

  if (errors != 0)
    abort ();
}

void __attribute__((noinline))
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
  char *p = buf + strlen (buf);

  sprintf (p, "%s ", name);
  p += strlen (p);

#if defined(_WIN32) && !defined(__CYGWIN__)
  sprintf (p, "%16.16I64x\n", x.t);
#else
  sprintf (p, "%16.16llx\n", x.t);
#endif
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
  return 0;
}
