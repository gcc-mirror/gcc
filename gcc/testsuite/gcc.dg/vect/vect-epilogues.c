/* { dg-do compile } */

/* Copied from PR 88915.  */
void pixel_avg( unsigned char *dst, int i_dst_stride,
                               unsigned char *src1, int i_src1_stride,
                               unsigned char *src2, int i_src2_stride,
                               int i_width, int i_height )
 {
     for( int y = 0; y < i_height; y++ )
     {
         for( int x = 0; x < i_width; x++ )
             dst[x] = ( src1[x] + src2[x] + 1 ) >> 1;
         dst += i_dst_stride;
         src1 += i_src1_stride;
         src2 += i_src2_stride;
     }
 }

/* { dg-final { scan-tree-dump "LOOP EPILOGUE VECTORIZED" "vect" { target vect_multiple_sizes xfail { arm32 && be } } } }  */
