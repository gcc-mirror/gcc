/* { dg-do compile } */
/* { dg-options "-mlasx -O3" } */
/* { dg-final { scan-assembler "vshuf\.w" } } */

#define V __attribute__ ((vector_size (16)))

int a V;
float b V;
float c V;
float d V;

void
test (void)
{
  d = __builtin_shuffle (b, c, a);
}
