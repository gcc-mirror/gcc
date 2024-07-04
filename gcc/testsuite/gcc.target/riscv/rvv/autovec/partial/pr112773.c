/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvl256b -mabi=ilp32d -O3 -fno-vect-cost-model" } */

long long a;
int b, c;
int *d;
void e(unsigned f) {
  for (;; ++c)
    if (f) {
      a = 0;
      for (; a <= 3; a++) {
        f = 0;
        for (; f <= 0; f++)
          if ((long)a)
            break;
      }
      if (b)
        *d = f;
    }
}
