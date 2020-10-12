/* { dg-options "-fipa-modref -fipa-icf" } */
float fma_test1(float a, float b, float c) {
  float x = a * b + c;
  return x;
}
float fma_test2(float a, float b, float c) {
  float x = a * b + c;
  return x;
}

