/* PR target/108516 */
/* { dg-do compile } */
/* { dg-options "-O2 -dp" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */

struct S
{
  unsigned char e1;
  unsigned char e2;
  unsigned char e3;
};

unsigned int
f2 (struct S s)
{
  return s.e2;
}

/* { dg-final { scan-assembler-not "\\*zero_extend" } } */
