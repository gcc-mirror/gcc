/* { dg-do compile } */

typedef short DCTELEM;
typedef unsigned char uint8_t;

void diff_pixels_c(DCTELEM *__restrict__ block, const uint8_t *s1,
		   const uint8_t *s2, int stride)
{
  int i;

  for(i=0;i<8;i++)
    {
      block[0] = s1[0] - s2[0];
      block[1] = s1[1] - s2[1];
      block[2] = s1[2] - s2[2];
      block[3] = s1[3] - s2[3];
      block[4] = s1[4] - s2[4];
      block[5] = s1[5] - s2[5];
      block[6] = s1[6] - s2[6];
      block[7] = s1[7] - s2[7];
      s1 += stride;
      s2 += stride;
      block += 8;
    }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target { vect_int && { vect_unpack && vect_hw_misalign } } } } } */
