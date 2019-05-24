/* { dg-do compile } */
/* { dg-options "-O2" } */

struct B { unsigned bit0 : 1; unsigned bit1 : 1; };

void
foo (struct B *b)
{
    b->bit0 = b->bit0 | b->bit1;
}

/* { dg-final { scan-assembler-times {\tand[l|q]} 1 } } */
/* { dg-final { scan-assembler-times {\tor[l|q]} 1 } } */
