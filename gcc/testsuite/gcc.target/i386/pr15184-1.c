/* PR 15184 first two tests, plus two addition ones.  */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -march=pentiumpro" } */

#define regparm __attribute__((__regparm__(1)))

extern unsigned int x;

void regparm f0(unsigned char c)
{
       x = (x & 0xFFFFFF00) | (unsigned int)c;
}

void regparm f1(unsigned char c)
{
     x = (x & 0xFFFF00FF) | ((unsigned int)c << 8);
}

void regparm f2(unsigned char c)
{
     x = (x & 0xFF00FFFF) | ((unsigned int)c << 16);
}
void regparm f3(unsigned char c)
{
     x = (x & 0x00FFFFFF) | ((unsigned int)c << 24);
}


/* Each function should compile down to a byte move from
   the input register into x, possibly at an offset within x.  */
/* { dg-final { scan-assembler-times "movb\[ \\t\]+%al" 4 } } */

