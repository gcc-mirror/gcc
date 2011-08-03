/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-std=gnu99" } */

typedef long unsigned int size_t;
typedef long int ssize_t;
typedef ssize_t index_type;
typedef __int128_t GFC_INTEGER_16;
typedef struct descriptor_dimension
{
  index_type _stride;
  index_type _lbound;
  index_type _ubound;
}
descriptor_dimension;
typedef struct { GFC_INTEGER_16 *data; size_t offset; index_type dtype; descriptor_dimension dim[7];} gfc_array_i16;
void
matmul_i16 (gfc_array_i16 * const restrict retarray,
	    gfc_array_i16 * const restrict a,
	    GFC_INTEGER_16 bbase_yn)
{
  GFC_INTEGER_16 * restrict dest;
  index_type rxstride, rystride;
  index_type x, y, n, count, xcount;
  GFC_INTEGER_16 * restrict dest_y;
  GFC_INTEGER_16 s;
  const GFC_INTEGER_16 * restrict abase_n;
  rxstride = ((retarray)->dim[0]._stride);
  rystride = ((retarray)->dim[1]._stride);
  xcount = ((a)->dim[0]._ubound + 1 - (a)->dim[0]._lbound);
  dest = retarray->data;
  dest_y = &dest[y*rystride];
  for (x = 0; x < xcount; x++)
    dest_y[x] += abase_n[x] * bbase_yn;
  for (x = 0; x < xcount; x++)
    {
      for (n = 0; n < count; n++)
	dest_y[x*rxstride] = (GFC_INTEGER_16) 0;
    }
}
