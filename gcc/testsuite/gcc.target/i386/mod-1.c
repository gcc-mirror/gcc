/* { dg-do compile } */
/* { dg-options "-Os -mtune=generic" } */

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

void
f (VCRC *x, OWN *own)
{
  x[own->vcr / 8].vcr[own->vcr % 8].a--;
  x[own->vcr / 8].vcr[own->vcr % 8].a = x[own->vcr / 8].vcr[own->vcr % 8].a;
}

/* { dg-final { scan-assembler-times "idivb" 1 } } */
/* { dg-final { scan-assembler-not "incl" } } */
/* { dg-final { scan-assembler-not "orl" } } */
/* { dg-final { scan-assembler-not "andb" } } */
/* { dg-final { scan-assembler-not "jns" } } */
