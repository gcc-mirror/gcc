/* PR middle-end/99502 - missing -Warray-bounds on partial out of bounds
   access in C++
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

inline void* operator new (__SIZE_TYPE__, void *p) { return p; }

struct B0 { int b0i, b0a[0]; };
struct B1: virtual B0 { int b1i, b1a[0]; };
struct B2: virtual B0 { int b2i, b2a[0]; };
struct D1: B1, B2 { int d1i, d1a[0]; };
struct D2: D1 { int d2i, d2a[0]; };
struct D3: D2 { int d3i, d3a[0]; };

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

void test_D1_b0a ()
{
  {
    D1 *p = (D1*)new char[sizeof (D1)];
    *p->b0a = __LINE__;                 // { dg-warning "-Warray-bounds" "pr99630" { xfail *-*-* } }
    sink (p);
  }

  {
    D1 *p = (D1*)new char[3];
    *p->b0a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D1_b1a ()
{
  {
    D1 *p = (D1*)new char[sizeof (D1)];
    *p->b1a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }

  {
    D1 *p = (D1*)new char[3];
    *p->b1a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D1_b2a ()
{
  {
    D1 *p = (D1*)new char[sizeof (D1)];
    *p->b2a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }

  {
    D1 *p = (D1*)new char[3];
    *p->b2a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D1_d1a ()
{
  {
    D1 *p = (D1*)new char[sizeof (D1)];
    *p->d1a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }

  {
    D1 *p = (D1*)new char[3];
    *p->d1a = __LINE__;                 // { dg-warning "-Warray-bounds" }
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

void test_D2_b0a ()
{
  {
    D2 *p = (D2*)new char[sizeof (D2)];
    *p->b0a = __LINE__;                 // { dg-warning "-Warray-bounds" "pr99630" { xfail *-*-* } }
    sink (p);
  }

  {
    D2 *p = (D2*)new char[3];
    *p->b0a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D2_b1a ()
{
  {
    D2 *p = (D2*)new char[sizeof (D2)];
    *p->b1a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }

  {
    D2 *p = (D2*)new char[3];
    *p->b1a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D2_b2a ()
{
  {
    D2 *p = (D2*)new char[sizeof (D2)];
    *p->b2a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }

  {
    D2 *p = (D2*)new char[3];
    *p->b2a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D2_d1a ()
{
  {
    D2 *p = (D2*)new char[sizeof (D2)];
    *p->d1a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }

  {
    D2 *p = (D2*)new char[3];
    *p->d1a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D2_d2a ()
{
  {
    D2 *p = (D2*)new char[sizeof (D2)];
    *p->d2a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }

  {
    D2 *p = (D2*)new char[3];
    *p->d2a = __LINE__;                 // { dg-warning "-Warray-bounds" }
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

void test_D3_b0a ()
{
  {
    D3 *p = (D3*)new char[sizeof (D3)];
    *p->b0a = __LINE__;                 // { dg-warning "-Warray-bounds" "pr99630" { xfail *-*-* } }
    sink (p);
  }

  {
    D3 *p = (D3*)new char[3];
    *p->b0a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D3_b1a ()
{
  {
    D3 *p = (D3*)new char[sizeof (D3)];
    *p->b1a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }

  {
    D3 *p = (D3*)new char[3];
    *p->b1a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D3_b2a ()
{
  {
    D3 *p = (D3*)new char[sizeof (D3)];
    *p->b2a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }

  {
    D3 *p = (D3*)new char[3];
    *p->b2a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D3_d1a ()
{
  {
    D3 *p = (D3*)new char[sizeof (D3)];
    *p->d1a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }

  {
    D3 *p = (D3*)new char[3];
    *p->d1a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D3_d2a ()
{
  {
    D3 *p = (D3*)new char[sizeof (D3)];
    *p->d2a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }

  {
    D3 *p = (D3*)new char[3];
    *p->d2a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

void test_D3_d3a ()
{
  {
    D3 *p = (D3*)new char[sizeof (D3)];
    *p->d3a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }

  {
    D3 *p = (D3*)new char[3];
    *p->d3a = __LINE__;                 // { dg-warning "-Warray-bounds" }
    sink (p);
  }
}

