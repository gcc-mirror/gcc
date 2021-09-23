/* PR c++/95768 - pretty-printer ICE on -Wuninitialized with allocated storage
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds" } */

extern "C" void *malloc (__SIZE_TYPE__);

struct f
{
  int i;
  static int e (int);
  void operator= (int) { e (i); }
};

struct m {
  int i;
  f length;
};

struct n {
  m *o() { return (m *)this; }
};

struct p {
  n *header;
  p () {
    header = (n *)malloc (0);
    m b = *header->o();       // { dg-warning "\\\[-Wuninitialized" }
    b.length = 0;
  }
};

void detach2() { p(); }
