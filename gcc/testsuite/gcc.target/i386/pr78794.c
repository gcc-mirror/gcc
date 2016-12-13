/* PR target/pr78794 */
/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -march=slm -mno-bmi -mno-stackrealign" } */
/* { dg-final { scan-assembler "pandn" } } */

typedef unsigned long long ull;

struct S1
{
  float x;
  ull y;
};


struct S2
{
  int a1;
  struct S1 *node;
  int *a2;
};

void
foo(int c1, int c2, int c3, struct S2 *reg)
{
  int i;
  for(i=0; i<reg->a1; i++)
    if(reg->node[i].y & ((ull) 1 << c1))
      {
	if(reg->node[i].y & ((ull) 1 << c2))
	  reg->node[i].y ^= ((ull) 1 << c3);
      }
}
