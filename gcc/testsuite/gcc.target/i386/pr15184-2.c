/* PR 15184 second two tests
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -march=pentiumpro" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */

#define regparm __attribute__((__regparm__(1)))

extern unsigned short y;

void regparm g0(unsigned char c)
{
        y = (y & 0xFF00) | (unsigned short)c;
}

void regparm g1(unsigned char c)
{
        y = (y & 0x00FF) | ((unsigned short)c << 8);
}

/* Each function should compile down to a byte move from
   the input register into y, possibly at an offset within y.  */
/* { dg-final { scan-assembler-times "movb\[ \\t\]+%al" 2 } } */

