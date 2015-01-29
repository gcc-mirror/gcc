/* PR 15184 second two tests
/* { dg-do compile } */
/* { dg-options "-O2 -m32 -march=pentiumpro" } */

#define regparm __attribute__((__regparm__(3)))

extern unsigned int x;
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
   the input register into x, possibly at an offset within x.  */
/* { dg-final { scan-assembler-times "\tmovb\t%al, y" 2 } } */

