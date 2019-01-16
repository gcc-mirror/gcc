/* { dg-do compile } */
/* { dg-options "-O2" } */

struct a {
  long x;
  long y;
  long z;
};

int passlibstruct (int b, struct a aa);

int main() {
  struct a aa = { 55, 66, 77 };

  return passlibstruct(-1, aa);
}

/* Ensure we pass a stack reference in the second arg.  */
/* { dg-final { scan-assembler-times "r4, r1, " 1 } } */
