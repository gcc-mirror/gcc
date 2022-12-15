/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" } } */

int c;

int main() {
  for (;;) {
    short h = c * 100;
    if (h & 0x7ff0)
      break;
  }
}

/* { dg-final { scan-assembler-times "andi\t" 1 } } */
/* { dg-final { scan-assembler-times "srli\t" 1 } } */

