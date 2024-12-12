/* { dg-do run } */
/* { dg-options "-O" } */

const int BIG_ENDIAN_P = (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__);

struct T1 {
  unsigned char p[2];
  unsigned short a;
  unsigned int z;
} __attribute__((__aligned__(8)));

struct T2 {
  unsigned short p;
  unsigned short a;
  unsigned int z;
} __attribute__((__aligned__(8)));

#define vc 0xaa
#define vi 0x12345678

struct T1 v1 = { { vc + !BIG_ENDIAN_P, vc + BIG_ENDIAN_P }, vc, vi };
struct T2 v2 = { (vc << 8) | (vc - 1), vc, vi };

void f (void) {
  if (0
      || v1.p[!BIG_ENDIAN_P] != v2.p >> 8
      || v1.a != v2.a
      || ((v1.z ^ v2.z) & 0xff00ff00) != 0
      || ((v1.z ^ v2.z) & 0x00ff00ff) != 0)
    __builtin_abort ();
}

int main () {
  f ();
  return 0;
}
