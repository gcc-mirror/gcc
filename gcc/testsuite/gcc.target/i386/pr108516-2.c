/* PR target/108516 */
/* { dg-do compile } */
/* { dg-options "-O2 -dp" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */

struct S
{
  signed char e1;
  signed char e2;
  signed char e3;
};

int
f2 (struct S s)
{
  return s.e2;
}

/* { dg-final { scan-assembler-not "\\*extzv" } } */
