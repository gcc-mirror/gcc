/* { dg-do compile } */
/* { dg-options "-O2 -mcmodel=normal -mexplicit-relocs=none" } */

extern char a[8];
extern char b[8];

#ifndef TEST_TARGET_PRAGMA
__attribute__ ((target ("cmodel=extreme")))
#else
#pragma GCC target ("cmodel=extreme")
#endif
void
test (void)
{
  a[0] = b[1];	
  a[1] = b[2];	
  a[2] = b[3];	
  a[3] = b[4];	
}

/* { dg-final { scan-assembler "la.global\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,a" } } */
