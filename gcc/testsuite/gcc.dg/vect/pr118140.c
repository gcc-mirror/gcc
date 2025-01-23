/* { dg-do run { target { aarch64*-*-* || riscv*-*-* } } } */
/* { dg-additional-options "-std=gnu99" } */

long long a;
_Bool d;
char e;
_Bool f[17];
_Bool f_3;

int main() {
  for (char g = 3; g < 16; g++) {
      d |= ({
        int h = f[g - 1] ? 2 : 0;
        _Bool t;
        if (f[g - 1])
          t = f_3;
        else
          t = 0;
        int i = t;
        h > i;
      });
    e += f[g + 1];
  }

  if (d != 0)
    __builtin_abort ();
}
