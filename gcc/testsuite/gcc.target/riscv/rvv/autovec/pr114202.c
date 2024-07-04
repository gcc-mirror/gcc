/* { dg-do run { target { riscv_v && rv64 } } } */
/* { dg-options { -march=rv64gcv -mabi=lp64d -O3 -fwrapv } } */

signed char a = 0, d = 0;
_Bool b;
signed char c[324];
int e;

int main() {
  c[63] = 50;
  for (int f = 0; f < 9; f++) {
    for (unsigned g = 0; g < 12; g++)
      for (char h = 0; h < 8; h++)
        e = a += c[g * 9];
    b = e ? d : 0;
  }

  if (a != 16)
    __builtin_abort ();
}
