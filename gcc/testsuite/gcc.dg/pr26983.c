/* { dg-do link } */
/* { dg-require-effective-target indirect_jumps } */

/* This used to cause a linker failure because GCC would output
   assembler code referencing labels that it had not output.  */

void *jmpbuf[6];

void
foo (void)
{
  __builtin_setjmp (jmpbuf);
}

int
main (void)
{
  return 0;
}

