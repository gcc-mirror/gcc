/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic" } */

extern char *strcpy (char *, const char *);

void
foo (char *s)
{
  strcpy (s,
	  "1234567890abcdef123456abcdef5678123456abcdef567abcdef678"
	  "1234567");
}

/* { dg-final { scan-assembler-times "movdqa\[ \\t\]+\[^\n\]*%xmm" 4 } } */
/* { dg-final { scan-assembler-times "movups\[ \\t\]+\[^\n\]*%xmm" 4 } } */
