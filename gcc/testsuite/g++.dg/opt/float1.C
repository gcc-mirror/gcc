// PR optimization/11637
// Origin: <nick@ilm.com>

// This used to fail to assemble on x86 because a decimal
// floating point litteral was emitted, which originated
// from a bogus REG_EQUAL note not removed by the combiner.

// { dg-do assemble }
// { dg-options "-O2 -fnon-call-exceptions" }

void f(long int seed);

void g(float &o)
{
  float a = 0.05f;
  float b = 1.0 - a;
  float c = 1.0 + a;

  f(0);
  o = a;
}
