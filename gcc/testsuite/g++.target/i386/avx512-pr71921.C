// PR target/116925
// { dg-do compile }
// { dg-options "-O2 -march=x86-64-v4" }
// { dg-final { scan-assembler-not "vcmpltps" } }
// { dg-final { scan-assembler-times "vminps" 2 } }
// { dg-final { scan-assembler-times "vmaxps" 2 } }

#include <vector>

void relu(float * __restrict__ output, const float * __restrict__ input, int size)
{
  int i;
  int s2;

  s2 = size / 4;
  for (i = 0; i < 10000; i++) {
    float t;
    t = input[i];
    output[i] = std::max(t, float(0));
  }
}

void relu1(float * __restrict__ output, const float * __restrict__ input, int size)
{
  int i;
  int s2;

  s2 = size / 4;
  for (i = 0; i < 10000; i++) {
    float t;
    t = input[i];
    output[i] = std::max(float(0), t);
  }
}

void relu2(float * __restrict__ output, const float * __restrict__ input, int size)
{
  int i;
  int s2;

  s2 = size / 4;
  for (i = 0; i < 10000; i++) {
    float t;
    t = input[i];
    output[i] = std::min(t, float(0));
  }
}

void relu3(float * __restrict__ output, const float * __restrict__ input, int size)
{
  int i;
  int s2;

  s2 = size / 4;
  for (i = 0; i < 10000; i++) {
    float t;
    t = input[i];
    output[i] = std::min(float(0), t);
  }
}
