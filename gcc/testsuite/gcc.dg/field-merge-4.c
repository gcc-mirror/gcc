/* { dg-do run } */
/* { dg-options "-O" } */

struct T1 {
  unsigned int zn;
  unsigned char p;
  unsigned char qn;
  unsigned short a;
  unsigned int z;
} __attribute__((__packed__, __aligned__(4)));

struct T2 {
  unsigned int zn;
  unsigned char rn;
  unsigned char p;
  unsigned char qn;
  unsigned short a;
  unsigned int z;
} __attribute__((__packed__, __aligned__(4)));

#define vc (unsigned char)0xaa
#define vs (unsigned short)0xccdd
#define vi (unsigned int)0x12345678

struct T1 v1 = { -1, vc, 1, vs, vi };
struct T2 v2 = { -1, 0, vc, 1, vs, vi };

void f (void) {
  if (0
      || v1.p != v2.p
      || v1.a != v2.a
      || v1.z != v2.z
      )
    __builtin_abort ();
}

int main () {
  f ();
  return 0;
}
