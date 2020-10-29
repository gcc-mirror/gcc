// PR c++/97573
// { dg-do compile { target c++20 } }
// { dg-options "-Wno-deprecated -Wno-enum-compare" }

enum E1 { e } e1;
enum E2 { f } e2;
__extension__ static enum { } u1;
__extension__ static enum { } u2;
static double d;

void
conv ()
{
  bool b1 = e == e1;
  bool b2 = e == f;
  bool b3 = e == 0.0;
  bool b4 = 0.0 == f;
  int n1 = true ? e : f;
  int n2 = true ? e : 0.0;
}

int
enum_enum (bool b)
{
  int r = 0;
  const E1 e1c = e;

  r += e - e;
  r += e - e1;
  r += e - f;
  r += f - e;

  r += f + f;
  r += f + e;
  r += e + f;

  r += e1 - e2;
  r += e1 - e1c;
  r += e1c - e1;

  r += e * f;
  r += f * e;
  r += e * e;

  r += e1 < e1c;
  r += e < e1;
  r += e1 < e2;
  r += e < f;
  r += f < e;

  r += e1 == e1c;
  r += e == e1;
  r += e == f;
  r += f == e;
  r += e1 == e2;
  r += e2 == e1;

  r += b ? e1 : e1c;
  r += b ? e1 : e;
  r += b ? f : e;
  r += b ? e1 : e2;

  r += e | f;
  r += e ^ f;
  r += e & f;
  r += !e;
  r += e1 | e;

  r += e << f;
  r += e >> f;
  r += e || f;
  r += e && f;
  e1 = e1c;

  // Anonymous enum.
  r += u1 - u1;
  r += u1 + u2;
  r += u1 * u2;
  r += u1 == u2;
  r += u1 & u2;

  return r;
}

double
enum_float (bool b)
{
  double r = 0.0;

  r += e1 - d;
  r += d - e1;
  r += e1 + d;
  r += d + e1;
  r += e1 * d;
  r += d * e1;
  r += u1 * d;
  r += d * u1;

  r += e1 < d;
  r += d < e1;
  r += d == e1;
  r += e1 == d;
  r += u1 == d;
  r += d == u1;

  r += b ? e1 : d;
  r += b ? d : e1;
  r += b ? d : u1;
  r += b ? u1 : d;

  d += e1;
  d = e1;

  return r;
}
