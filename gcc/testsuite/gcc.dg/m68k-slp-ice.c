/* From PR 7872, test for optabs segfault when strict low part is present.  */
/* { dg-do compile { target m68k-*-* } }  */
/* { dg-options "-O0" }  */
extern void (**table)(void);

typedef unsigned short uw16;
typedef unsigned int gshort;

register uw16 *pc asm("%a4");
register gshort code asm("%d6");

void QMExecuteLoop(uw16 *oldPC)
{
  table[code=(*(uw16*)(pc++))]();
}
