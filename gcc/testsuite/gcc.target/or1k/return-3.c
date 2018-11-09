/* { dg-do compile } */
/* { dg-options "-O2" } */

struct a {
  long x;
  long y;
  long z;
};

struct a getlibstruct (long aa);

int main() {
  struct a rs = getlibstruct(123);

  return rs.x;
}

/* Ensure our return value is read from memory.  */
/* { dg-final { scan-assembler "l.lwz\\s+r11," } } */
