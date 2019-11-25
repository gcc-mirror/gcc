/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-mavx2" { target { i?86-*-* x86_64-*-* } } } */

typedef struct {
    unsigned short mprr_2[5][16][16];
} ImageParameters;
int s[16][2];
void intrapred_luma_16x16(ImageParameters *img, int s0)
{
  for (int j=0; j < 16; j++)
    for (int i=0; i < 16; i++)
      {
	img->mprr_2[1 ][j][i]=s[j][1];
	img->mprr_2[2 ][j][i]=s0;
      }
}
