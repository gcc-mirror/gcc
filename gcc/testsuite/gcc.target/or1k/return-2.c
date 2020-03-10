/* Large structs are returned at a memory address passed in r3.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

struct a {
  long x;
  long y;
  long z;
};

struct a getstruct (long aa) {
  struct a as = { 22, aa, -5 };
  return as;
}

/* Ensure our return value is returned on stack.  */
/* { dg-final { scan-assembler-not "r12," } } */
/* { dg-final { scan-assembler "l.or\\s+r11, r3, r3" } } */
/* { dg-final { scan-assembler-times "l.sw\\s+\\d+.r11.," 3 } } */
