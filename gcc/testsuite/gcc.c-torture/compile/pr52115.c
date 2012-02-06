struct S
{
  float f;
  long l;
};

extern int gi;
extern float gf;

long foo (long p)
{
  struct S s;
  float *pf;

  s.l = p;

  pf = &s.f;

  pf++;
  pf--;

  gf = *pf + 3.3;
  gi = *((short *)pf) + 2;

  return s.l + 6;
}
