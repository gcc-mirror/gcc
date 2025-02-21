/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-std=gnu99 -Wno-pedantic" } */

unsigned char a;
long long r;
_Bool h = 1;
short j[23];
_Bool k[3][23];

void b(_Bool h, short j[], _Bool k[][23]) {
  for (int m = 0; m < 23; m += 3)
    for (short n = 0; n < 22; n += 4)
      a = ({
        unsigned char o = a;
        unsigned char p = j[n] ? h : k[m][n];
        o > p ? o : p;
      });
}

int main() {
  for (int m = 0; m < 23; ++m)
    j[m] = 10;
  b(h, j, k);
  r = a;
  if (r != 1)
    __builtin_abort ();
}
