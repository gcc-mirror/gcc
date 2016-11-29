/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mcpu=nps400 -O2 -mbitops" } */

struct thing
{
  union
  {
    int raw;
    struct
    {
      unsigned a : 1;
      unsigned b : 1;
      unsigned c : 28;
      unsigned d : 1;
      unsigned e : 1;
    };
  };
};

extern void func (int);

void
blah ()
{
  struct thing xx;
  xx.a = xx.b = 1;
  func (xx.raw);
}

void
woof ()
{
  struct thing xx;
  xx.d = xx.e = 1;
  func (xx.raw);
}

/* { dg-final { scan-assembler "movh\.cl r\[0-9\]+,0xc0000000>>16" } } */
