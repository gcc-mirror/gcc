/* { dg-do run } */
/* { dg-options "-O" } */

struct TL {
  unsigned short a;
  unsigned short b;
} __attribute__ ((packed, aligned (8)));

struct TB {
  unsigned char p;
  unsigned short a;
  unsigned short b;
} __attribute__ ((packed, aligned (8)));

#define vc 0xaa

struct TL vL = { vc, vc };
struct TB vB = { vc, vc, vc };

void f (void) {
  if (0
      || vL.b != vB.b
      || vL.a != vB.a
      )
    __builtin_abort ();
}

int main () {
  f ();
  return 0;
}
