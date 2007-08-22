/* Verify that rolb instruction is emitted on IA-32/x86-64.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void foo (unsigned char *);

int
main (void)
{
  unsigned char c = 0;
  foo (&c);
  c = c >> 1 | c << 7;
  return c;
}

/* { dg-final { scan-assembler "rolb" } } */
