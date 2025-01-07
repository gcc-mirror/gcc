/* { dg-do compile } */
/* { dg-options "-O2 -mstrict-align" } */
extern char a[8];
extern char b[8];

#ifndef TEST_TARGET_PRAGMA
__attribute__ ((target ("no-strict-align")))
#else
#pragma GCC target ("no-strict-align")
#endif
void
test (void)
{
  a[0] = b[1];	
  a[1] = b[2];	
  a[2] = b[3];	
  a[3] = b[4];	
}


/* { dg-final { scan-assembler-not "ld.bu" } } */
