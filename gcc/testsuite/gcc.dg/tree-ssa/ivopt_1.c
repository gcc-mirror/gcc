/* { dg-do compile { target {{ i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-O2 -m64 -fdump-tree-ivopts" } */
#define TYPE char*

/* Testing that only one induction variable is selected after IVOPT on
   the given target instead of 3.  */
void foo (int i_width, TYPE dst, TYPE src1, TYPE src2)
{
      int x;
       for( x = 0; x < i_width; x++ )
       {
           dst[x] = ( src1[x] + src2[x] + 1 ) >> 1;
       }
}


/* { dg-final { scan-tree-dump-times "ivtmp.\[0-9_\]* = PHI <" 1 "ivopts"} } */
