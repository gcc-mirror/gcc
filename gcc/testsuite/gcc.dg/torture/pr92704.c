/* { dg-do compile } */
/* { dg-additional-options "-fexceptions -fnon-call-exceptions -fno-tree-dce -ftree-loop-if-convert" } */
int zr, yx;

void __attribute__ ((simd))
oj (int rd, int q7)
{
  int wo = (__UINTPTR_TYPE__)&rd;

  while (q7 < 1)
    {
      int kv;
      short int v3;

      for (v3 = 0; v3 < 82; v3 += 3)
        {
        }

      kv = zr ? 0 : v3;
      yx = kv < rd;
      zr = zr && yx;
      ++q7;
    }
}
