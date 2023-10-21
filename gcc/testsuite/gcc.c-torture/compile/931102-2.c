/* { dg-additional-options "-std=gnu89" } */

typedef struct {
  int a;
} VCR;

typedef struct {
  VCR vcr[8];
} VCRC;

typedef struct {
  char vcr;
} OWN;

OWN Own[16];

f (x, own)
  VCRC *x;
  OWN *own;
{
  x[own->vcr / 8].vcr[own->vcr % 8].a--;
  x[own->vcr / 8].vcr[own->vcr % 8].a = x[own->vcr / 8].vcr[own->vcr % 8].a;
}
