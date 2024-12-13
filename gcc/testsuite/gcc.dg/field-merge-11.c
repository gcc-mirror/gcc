/* { dg-do run } */
/* { dg-options "-O" } */

/* Check that narrowing casts aren't ignored, and that same-field tests at
   different widths aren't misoptimized.  */

struct s {
  short a;
  unsigned short b;
  int c;
} __attribute__ ((aligned (4)));

struct s p = { 42, (short)(0xef1 - 0x1000), 0x12345678 };

void f (void) {
  if (0
      || (p.a & 0xcc) != 8
      || p.a != 42
      || (int)(signed char)p.b != (int)(signed char)(0xef1 - 0x1000)
      || (unsigned)(unsigned char)p.b != (unsigned)(unsigned char)(0xef1 - 0x1000)
      || (unsigned)p.b != (unsigned short)(0xef1 - 0x1000)
      || (int)(short)p.b != (int)(0xef1 - 0x1000)
      || (long)(unsigned char)(p.c >> 8) != (long)(unsigned char)0x123456
      || p.c != 0x12345678
      )
    __builtin_abort ();
}

int main () {
  f ();
  return 0;
}
