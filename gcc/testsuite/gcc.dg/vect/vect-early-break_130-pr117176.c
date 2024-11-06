/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

struct ColorSpace {
  int componentCt;
};

struct Psnr {
  double psnr[3];
};

int f(struct Psnr psnr, struct ColorSpace colorSpace) {
  int i, hitsTarget = 1;

  for (i = 1; i < colorSpace.componentCt && hitsTarget; ++i)
    hitsTarget = !(psnr.psnr[i] < 1);

  return hitsTarget;
}
