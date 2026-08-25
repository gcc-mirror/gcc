/* PR target/48609 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2" } */
typedef _Complex float SCtype;
extern SCtype bar;
void foo (SCtype x)
{
  bar = x;
}

/* { dg-final { scan-assembler-not "movdqa" } } */
/* { dg-final { scan-assembler-not "shufps" } } */
/* { dg-final { scan-assembler-not "unpcklps" } } */
