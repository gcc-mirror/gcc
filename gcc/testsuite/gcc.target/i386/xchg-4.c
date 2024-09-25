/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

void ext(int x);
void foo(int x) 
{
    ext((x&~0xffff)|((x>>8)&0xff)|((x&0xff)<<8));
}

/* { dg-final { scan-assembler "rolw" } } */
/* { dg-final { scan-assembler-not "mov" } } */
