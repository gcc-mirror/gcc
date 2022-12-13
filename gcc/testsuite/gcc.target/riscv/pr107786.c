/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" } } */

int c;

int main() {
  for (;;) {
    char h = c * 100;
    if (h)
      break;
  }
}

/* { dg-final { scan-assembler-times "andi\t" 1 } } */
/* { dg-final { scan-assembler-not "srli\t" } } */

