/* { dg-do run } */
/* { dg-options "-O" } */

/* Check that we don't move loads across stores.  */

struct s {
  short a;
  short b;
} __attribute__ ((aligned (4)));

struct s p[2] = {
  { 42, 0xfe01 },
  { 42, 0xfe10 }
};

void f (void) {
  short a0 = p[0].a;
  short b0 = p[0].b;
  short a1 = p[1].a;
  short b1 = p[1].b;
  __builtin_memset (p, 0, sizeof (p));
  asm ("" : "+m" (p));
  if (0
      || p[0].a != p[1].a
      || p[0].b != p[1].b
      )
    __builtin_abort ();
  if (a0 == a1)
    if (b0 == b1)
      __builtin_abort ();
}

int main () {
  f ();
  return 0;
}
