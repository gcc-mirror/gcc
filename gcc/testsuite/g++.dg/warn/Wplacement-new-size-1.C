// PR c++/69662 - -Wplacement-new on allocated one element array members
// Exercising the more permissive -Wplacement-new=1.  The difference
// between -Wplacement-new=1 is denoted by "no warning at level 1" in
// the comments below.
// { dg-do compile }
// { dg-options "-Wno-pedantic -Wplacement-new=1" }

typedef __typeof__ (sizeof 0) size_t;

void* operator new (size_t, void *p) { return p; }
void* operator new[] (size_t, void *p) { return p; }

struct Ax { char n, a []; };
struct A0 { char n, a [0]; };
struct A1 { char n, a [1]; };
struct A2 { char n, a [2]; };

typedef __INT16_TYPE__ Int16;
typedef __INT32_TYPE__ Int32;

void fAx (Ax *px, Ax &rx)
{
  Ax ax;
  new (ax.a) Int32;    // { dg-warning "placement" }
  new (px->a) Int32;
  new (rx.a) Int32;
}

void fAx2 ()
{
  static Ax ax2 = { 1, { 2, 3 } };

  new (ax2.a) Int16;
  new (ax2.a) Int32;    // { dg-warning "placement" }
}

void fA0 (A0 *p0, A0 &r0)
{
  A0 a0;
  new (a0.a) Int32;    // { dg-warning "placement" }
  new (p0->a) Int32;
  new (r0.a) Int32;
}

void fA1 (A1 *p1, A1 &r1)
{
  A1 a1;
  new (a1.a) Int32;    // { dg-warning "placement" }
  new (p1->a) Int32;   // no warning at level 1
  new (r1.a) Int32;    // no warning at level 1
}

void fA2 (A2 *p2, A2 &r2)
{
  A2 a2;
  new (a2.a) Int32;    // { dg-warning "placement" }
  new (p2->a) Int32;   // { dg-warning "placement" }
  new (r2.a) Int32;    // { dg-warning "placement" }
}

struct BAx { int i; Ax ax; };
struct BA0 { int i; A0 a0; };
struct BA1 { int i; A1 a1; };
struct BA2 { int i; A2 a2; };

void fBx (BAx *pbx, BAx &rbx)
{
  BAx bax;
  new (bax.ax.a) char;     // { dg-warning "placement" }
  new (bax.ax.a) Int16;    // { dg-warning "placement" }
  new (bax.ax.a) Int32;    // { dg-warning "placement" }

  new (pbx->ax.a) char;
  new (rbx.ax.a) char;
  new (pbx->ax.a) Int16;
  new (rbx.ax.a) Int16;
  new (pbx->ax.a) Int32;
  new (rbx.ax.a) Int32;
  new (pbx->ax.a) int[1234];
  new (rbx.ax.a) int[5678];
}

void fBx1 ()
{
  static BAx bax1 = { 1, /* Ax = */ { 2, /* a[] = */ {} } };

  new (bax1.ax.a) char;	    // { dg-warning "placement" }
  new (bax1.ax.a) char[2];  // { dg-warning "placement" }
  new (bax1.ax.a) Int16;    // { dg-warning "placement" }
  new (bax1.ax.a) Int32;    // { dg-warning "placement" }
}

void fB0 (BA0 *pb0, BA0 &rb0)
{
  BA0 ba0;
  new (ba0.a0.a) Int32;     // { dg-warning "placement" }
  new (pb0->a0.a) Int32;
  new (rb0.a0.a) Int32;
}

void fB1 (BA1 *pb1, BA1 &rb1)
{
  BA1 ba1;
  new (ba1.a1.a) Int32;     // { dg-warning "placement" }
  new (pb1->a1.a) Int32;    // no warning at level 1
  new (rb1.a1.a) Int32;     // no warning at level 1
}

void fB2 (BA2 *pb2, BA2 &rb2)
{
  BA2 ba2;
  new (ba2.a2.a) Int32;     // { dg-warning "placement" }
  new (pb2->a2.a) Int32;    // { dg-warning "placement" }
  new (rb2.a2.a) Int32;     // { dg-warning "placement" }
}
