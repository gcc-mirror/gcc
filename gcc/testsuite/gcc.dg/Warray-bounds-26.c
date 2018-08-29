/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds" } */

struct Rec {
  unsigned char data[1];  // actually variable length
};

union U {
  unsigned char buf[42];
  struct Rec rec;
};

int Load()
{
  union U u;
  return u.rec.data[1]; /* { dg-bogus "array bound" } */
}
