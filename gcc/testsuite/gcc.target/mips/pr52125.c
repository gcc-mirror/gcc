/* { dg-options "-mno-gpopt -msym32 addressing=absolute" } */

int a, b, c, d;

NOMIPS16 void
foo (void)
{
  asm ("%1 %z3"
       : "=m" (a), "=m" (b)
       : "m" (c), "m" (d));
}

/* { dg-final { scan-assembler-not "%hi\\(a\\)" } } */
/* { dg-final { scan-assembler-not "%lo\\(a\\)" } } */
/* { dg-final { scan-assembler "%hi\\(b\\)" } } */
/* { dg-final { scan-assembler "%lo\\(b\\)" } } */
/* { dg-final { scan-assembler-not "%hi\\(c\\)" } } */
/* { dg-final { scan-assembler-not "%lo\\(c\\)" } } */
/* { dg-final { scan-assembler "%hi\\(d\\)" } } */
/* { dg-final { scan-assembler "%lo\\(d\\)" } } */
