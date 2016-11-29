/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2 -mtune=lakemont" } */

extern char *strcpy (char *, const char *);

void
foo (char *s)
{
  strcpy (s,
	  "12345678123456781234567812345678123456781234567812345678"
	  "1234567");
}

/* { dg-final { scan-assembler-times "movl\[ \\t\]+\\$\[0-9\]+, \[0-9\]*\\(%\[^,\]+\\)" 16 } } */
/* { dg-final { scan-assembler-not "rep movsl" } } */
/* { dg-final { scan-assembler-not "rep movsb" } } */
