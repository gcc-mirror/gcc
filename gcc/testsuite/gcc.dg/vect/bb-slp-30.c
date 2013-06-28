/* { dg-require-effective-target vect_int } */

int a[32];

void __attribute__((noinline))
test1(void)
{
  a[0] = 1;
  a[1] = 1;
  a[2] = 1;
  a[3] = 1;
  a[4] = 1;
  a[5] = 1;
  a[6] = 1;
  a[7] = 1;
  a[8] = 1;
  a[9] = 1;
  a[10] = 1;
  a[11] = 1;
  a[12] = 1;
  a[13] = 1;
  a[14] = 1;
  a[15] = 1;
  a[16] = 1;
  a[17] = 1;
  a[18] = 1;
  a[19] = 1;
  a[20] = 1;
  a[21] = 1;
  a[22] = 1;
  a[23] = 1;
  a[24] = 1;
  a[25] = 1;
  a[26] = 1;
  a[27] = 1;
  a[28] = 1;
  a[29] = 1;
  a[30] = 1;
  a[31] = 1;
  asm ("" : : : "memory");
  a[21] = 0;
}

int main() { test1(); return a[21]; }

/* { dg-final { scan-tree-dump-times "Vectorized basic-block" 1 "slp" } } */
/* { dg-final { cleanup-tree-dump "slp" } } */
