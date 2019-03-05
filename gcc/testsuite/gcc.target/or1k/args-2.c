/* { dg-do compile } */
/* { dg-options "-O2" } */

struct a {
  long x;
  long y;
  long z;
};

int passstruct (int b, struct a aa) {
  return aa.z + aa.y + b;
}

/* Ensure our struct reads are offset from the address in arg 2.  */
/* { dg-final { scan-assembler-times "l.lwz\\s+r\\d+, \\d+.r4." 2 } } */
