/* { dg-options "-O2" } */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */

/* The problem was we were not striping the long cast here.
   Note this really should be invalid code but not for the
   current release (4.0) as we have people using it.  */

void blockCopy_MMX2(int*);
void postProcess_MMX2()
{
  int c, x,y, width;
  asm( "" :: "m" ((long)x));
  blockCopy_MMX2(&c);
}
