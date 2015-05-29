/* { dg-do compile { target {{ i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-O2 -m64 -fdump-tree-ivopts" } */

#define TYPE char*

/* Make sure only 1 iv candidate is selected after IVOPT.  */
void foo (int i_width, char* dst, char* src1, char* src2)
{
      int x;
       for( x = 0; x < i_width; x++ )
       {
           *((TYPE)dst) = ( *((TYPE)src1) + *((TYPE)src2) + 1 ) >> 1;
	   dst+=sizeof(TYPE);
	   src1+=sizeof(TYPE);
	   src2+=sizeof(TYPE);
       }
}

/* { dg-final { scan-tree-dump-times "ivtmp.\[0-9_\]* = PHI <" 1 "ivopts"} } */
