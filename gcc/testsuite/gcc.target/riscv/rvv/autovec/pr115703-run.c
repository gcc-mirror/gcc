/* { dg-do run } */
/* { dg-require-effective-target rvv_zvl256b_ok } */
/* { dg-options "-O3 -march=rv64gcv_zvl256b -mabi=lp64d -fwhole-program -fwrapv" } */

int a, i;
unsigned long b;
unsigned c, f;
long long d = 1;
short e, m;
long g, h;

__attribute__ ((noipa))
void check (unsigned long long x)
{
  if (x != 13667643351234938049ull)
    __builtin_abort ();
}

int main() {
  for (int q = 0; q < 2; q += 1) {
    for (short r = 0; r < 2; r += 1)
      for (char s = 0; s < 6; s++)
        for (short t = 0; t < 011; t += 12081 - 12080)
          for (short u = 0; u < 11; u++) {
            a = ({ a > 1 ? a : 1; });
            b = ({ b > 5 ? b : 5; });
            for (short j = 0; j < 2; j = 2080)
              c = ({ c > 030 ? c : 030; });
            for (short k = 0; k < 2; k += 2080)
              d *= 7;
            e *= 10807;
            f = ({ f > 3 ? f : 3; });
          }
    for (int l = 0; l < 21; l += 1)
      for (int n = 0; n < 16; n++) {
        g = ({ m ? g : m; });
        for (char o = 0; o < 7; o += 1)
          h *= 3;
        i = ({ i < 0 ? i : 0; });
      }
  }

  check (d);
}
