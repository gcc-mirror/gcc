/* { dg-do run } */
/* { dg-options "-O" } */

/* Check that conversions are not thrown away.  */

struct s {
  unsigned char a;
  unsigned short b;
} __attribute__ ((aligned (4)));

struct s p = { 42, 0xfe };
struct s q = { 42, 0xfe | (2 << (__CHAR_BIT__ - 1)) };

void f (void) {
  if (0
      || p.a != q.a
      || (unsigned char)p.b != (unsigned char)q.b
      )
    __builtin_abort ();
}

int main () {
  f ();
  return 0;
}
