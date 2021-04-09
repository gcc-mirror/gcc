/* PR middle-end/99502 - missing -Warray-bounds on partial out of bounds
   access in C++
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

inline void* operator new (__SIZE_TYPE__, void *p) { return p; }

struct B0 { int b0i; };
struct B1: virtual B0 { int b1i; };
struct B2: virtual B0 { int b2i; };
struct D1: B1, B2 { int d1i; };
struct D2: D1 { int d2i; };
struct D3: D2 { long d3i, d3ax[]; };

void sink (void*);


void test_D1 ()
{
  {
    char *p = new char[sizeof (D1)];
    new (p) D1 ();
    sink (p);
  }

  {
    char *p = new char[sizeof (D1) - 1];
    new (p) D1 ();                      // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D1_b0i ()
{
  {
    D1 *p = (D1*)new char[sizeof (D1)];
    p->b0i = __LINE__;
    sink (p);
  }

  {
    D1 *p = (D1*)new char[3];
    p->b0i = __LINE__;                   // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D1_b1i ()
{
  {
    D1 *p = (D1*)new char[sizeof (D1)];
    p->b1i = __LINE__;
    sink (p);
  }

  {
    D1 *p = (D1*)new char[3];
    p->b1i = __LINE__;                   // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D1_b2i ()
{
  {
    D1 *p = (D1*)new char[sizeof (D1)];
    p->b2i = __LINE__;
    sink (p);
  }

  {
    D1 *p = (D1*)new char[3];
    p->b2i = __LINE__;                   // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D1_d1i ()
{
  {
    D1 *p = (D1*)new char[sizeof (D1)];
    p->d1i = __LINE__;
    sink (p);
  }

  {
    D1 *p = (D1*)new char[3];
    p->d1i = __LINE__;                   // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}


void test_D2 ()
{
  {
    char *p = new char[sizeof (D2)];
    new (p) D2 ();
    sink (p);
  }

  {
    char *p = new char[sizeof (D2) - 1];
    new (p) D2 ();                      // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D2_b0i ()
{
  {
    D2 *p = (D2*)new char[sizeof (D2)];
    p->b0i = __LINE__;
    sink (p);
  }

  {
    D2 *p = (D2*)new char[3];
    p->b0i = __LINE__;                   // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D2_b1i ()
{
  {
    D2 *p = (D2*)new char[sizeof (D2)];
    p->b1i = __LINE__;
    sink (p);
  }

  {
    D2 *p = (D2*)new char[3];
    p->b1i = __LINE__;                   // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D2_b2i ()
{
  {
    D2 *p = (D2*)new char[sizeof (D2)];
    p->b2i = __LINE__;
    sink (p);
  }

  {
    D2 *p = (D2*)new char[3];
    p->b2i = __LINE__;                   // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D2_d1i ()
{
  {
    D2 *p = (D2*)new char[sizeof (D2)];
    p->d1i = __LINE__;
    sink (p);
  }

  {
    D2 *p = (D2*)new char[3];
    p->d1i = __LINE__;                   // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D2_d2i ()
{
  {
    D2 *p = (D2*)new char[sizeof (D2)];
    p->d2i = __LINE__;
    sink (p);
  }

  {
    D2 *p = (D2*)new char[3];
    p->d2i = __LINE__;                   // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}


void test_D3 ()
{
  {
    char *p = new char[sizeof (D3)];
    new (p) D3 ();
    sink (p);
  }

  {
    char *p = new char[sizeof (D3) - 1];
    new (p) D3 ();                      // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D3_b0i ()
{
  {
    D3 *p = (D3*)new char[sizeof (D3)];
    p->b0i = __LINE__;
    sink (p);
  }

  {
    D3 *p = (D3*)new char[3];
    p->b0i = __LINE__;                   // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D3_b1i ()
{
  {
    D3 *p = (D3*)new char[sizeof (D3)];
    p->b1i = __LINE__;
    sink (p);
  }

  {
    D3 *p = (D3*)new char[3];
    p->b1i = __LINE__;                   // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D3_b2i ()
{
  {
    D3 *p = (D3*)new char[sizeof (D3)];
    p->b2i = __LINE__;
    sink (p);
  }

  {
    D3 *p = (D3*)new char[3];
    p->b2i = __LINE__;                   // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D3_d1i ()
{
  {
    D3 *p = (D3*)new char[sizeof (D3)];
    p->d1i = __LINE__;
    sink (p);
  }

  {
    D3 *p = (D3*)new char[3];
    p->d1i = __LINE__;                   // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D3_d2i ()
{
  {
    D3 *p = (D3*)new char[sizeof (D3)];
    p->d2i = __LINE__;
    sink (p);
  }

  {
    D3 *p = (D3*)new char[3];
    p->d2i = __LINE__;                   // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D3_d3i ()
{
  {
    D3 *p = (D3*)new char[sizeof (D3)];
    p->d3i = __LINE__;
    sink (p);
  }

  {
    D3 *p = (D3*)new char[3];
    p->d3i = __LINE__;                   // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D3_d3ax ()
{
  {
    D3 *p = (D3*)new char[sizeof (D3)];
    *p->d3ax = __LINE__;                // { dg-warning "-Warray-bounds" "pr?????" { xfail *-*-* } }
    p->d3ax[9] = __LINE__;              // { dg-warning "-Warray-bounds" "pr?????" { xfail *-*-* } }
    sink (p);
  }

  {
    D3 *p = (D3*)new char[sizeof (D3) + sizeof (D3::d3ax[0])];
    p->d3ax[0] = __LINE__;
    p->d3ax[1] = __LINE__;              // { dg-warning "-Warray-bounds" "pr?????" { xfail *-*-* } }
    sink (p);
  }

  {
    D3 *p = (D3*)new char[3];
    *p->d3ax = __LINE__;                // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

struct D4: D2
{
  // D4:d4a0 overlaps with B0:b0i!
  long d4i, d4a0[0];
};

void test_D4_d3a0 ()
{
  {
    D4 *p = (D4*)new char[sizeof (D4)];
    *p->d4a0 = __LINE__;                // { dg-warning "-Warray-bounds" }
    p->d4a0[1] = __LINE__;              // { dg-warning "-Warray-bounds" }
    p->d4a0[9] = __LINE__;              // { dg-warning "-Warray-bounds" }
    sink (p);
  }

  {
    D4 *p = (D4*)new char[sizeof (D4) + sizeof (D4::d4a0[0])];
    /* The access to d4a0[0] should get a -Wzero-length-bounds because
       it's in bounds but overlaps p->b0i.  */
    p->d4a0[0] = __LINE__;              // { dg-warning "-Wzero-length-bounds" "pr99635" { xfail *-*-* } }
                                        // { dg-warning "-Warray-bounds" "actual" { target *-*-* } .-1 }
    p->d4a0[1] = __LINE__;              // { dg-warning "-Warray-bounds" }
    p->d4a0[9] = __LINE__;              // { dg-warning "-Warray-bounds" }
    sink (p);
  }

  {
    D4 *p = (D4*)new char[3];
    *p->d4a0 = __LINE__;                // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}
