/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O3 -march=rv64gcv_zvl256b -mabi=lp64d -fdump-rtl-expand-details" } */

int b[24];
_Bool c[24];

int main() {
  for (int f = 0; f < 4; ++f)
    b[f] = 6;

  for (int f = 0; f < 24; f += 4)
    c[f] = ({
      int g = ({
        unsigned long g = -b[f];
        1 < g ? 1 : g;
      });
      g;
    });

  if (c[0] != 1)
    __builtin_abort ();
}

/* { dg-final { scan-rtl-dump-not ".SAT_TRUNC " "expand" } } */
