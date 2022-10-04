// PR c++/107080
// { dg-do compile }
// { dg-options "-mfp16-format=ieee" }

template <typename T, typename T1> 
int
foo (T x, T1 y)
{
  return 3;
}

int
main ()
{
  return (foo (0.0f16, 0.0f16) + foo (0.0f16, (__fp16) 0.0)) != 6;
}

// { dg-final { scan-assembler "_Z3fooIDF16_DF16_EiT_T0_" } }
// { dg-final { scan-assembler "_Z3fooIDF16_DhEiT_T0_" } }
