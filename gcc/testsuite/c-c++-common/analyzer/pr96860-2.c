/* { dg-additional-options "--param analyzer-max-svalue-depth=0 -Wno-analyzer-symbol-too-complex" } */

void x7 (void)
{
  long z5[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1,
  };
}
