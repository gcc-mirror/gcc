// PR c++/97573
// { dg-do compile { target { c++17_down } } }
// { dg-options "-Wenum-conversion" }

enum E1 { e } e1;
enum E2 { f } e2;
__extension__ static enum { } u1;
__extension__ static enum { } u2;
static double d;

void
conv ()
{
  bool b1 = e == e1;
  bool b2 = e == f; // { dg-warning "comparison between .enum E1. and .enum E2." }
  bool b3 = e == 0.0; // { dg-warning "comparison of enumeration type .E1. with floating-point type .double." }
  bool b4 = 0.0 == f; // { dg-warning "comparison of floating-point type .double. with enumeration type .E2." }
  int n1 = true ? e : f; // { dg-warning "enumerated mismatch" }
  int n2 = true ? e : 0.0; // { dg-warning "enumerated and non-enumerated type in conditional expression" }
}

int
enum_enum (bool b)
{
  int r = 0;
  const E1 e1c = e;

  r += e - e;
  r += e - e1;
  r += e - f; // { dg-warning "arithmetic between different enumeration types .E1. and .E2." }
  r += f - e; // { dg-warning "arithmetic between different enumeration types .E2. and .E1." }

  r += f + f;
  r += f + e; // { dg-warning "arithmetic between different enumeration types .E2. and .E1." }
  r += e + f; // { dg-warning "arithmetic between different enumeration types .E1. and .E2." }

  r += e1 - e2; // { dg-warning "arithmetic between different enumeration types .E1. and .E2." }
  r += e1 - e1c;
  r += e1c - e1;

  r += e * f; // { dg-warning "arithmetic between different enumeration types .E1. and .E2." }
  r += f * e; // { dg-warning "arithmetic between different enumeration types .E2. and .E1." }
  r += e * e;

  r += e1 < e1c;
  r += e < e1;
  r += e1 < e2; // { dg-warning "comparison between .enum E1. and .enum E2." }
  r += e < f; // { dg-warning "comparison between .enum E1. and .enum E2." }
  r += f < e; // { dg-warning "comparison between .enum E2. and .enum E1." }

  r += e1 == e1c;
  r += e == e1;
  r += e == f; // { dg-warning "comparison between .enum E1. and .enum E2." }
  r += f == e; // { dg-warning "comparison between .enum E2. and .enum E1." }
  r += e1 == e2; // { dg-warning "comparison between .enum E1. and .enum E2." }
  r += e2 == e1; // { dg-warning "comparison between .enum E2. and .enum E1." }

  r += b ? e1 : e1c;
  r += b ? e1 : e;
  r += b ? f : e; // { dg-warning "enumerated mismatch in conditional expression: .E2. vs .E1." }
  r += b ? e1 : e2; // { dg-warning "enumerated mismatch in conditional expression: .E1. vs .E2." }

  r += e | f; // { dg-warning "bitwise operation between different enumeration types .E1. and .E2." }
  r += e ^ f; // { dg-warning "bitwise operation between different enumeration types .E1. and .E2." }
  r += e & f; // { dg-warning "bitwise operation between different enumeration types .E1. and .E2." }
  r += !e;
  r += e1 | e;

  r += e << f;
  r += e >> f;
  r += e || f;
  r += e && f;
  e1 = e1c;

  // Anonymous enum.
  r += u1 - u1;
  r += u1 + u2; // { dg-warning "arithmetic between different enumeration types" }
  r += u1 * u2; // { dg-warning "arithmetic between different enumeration types" }
  r += u1 == u2; // { dg-warning "comparison between" }
  r += u1 & u2; // { dg-warning "bitwise operation between different enumeration types" }

  return r;
}

double
enum_float (bool b)
{
  double r = 0.0;

  r += e1 - d; // { dg-warning "arithmetic between enumeration type .E1. and floating-point type .double." }
  r += d - e1; // { dg-warning "arithmetic between floating-point type .double. and enumeration type .E1." }
  r += e1 + d; // { dg-warning "arithmetic between enumeration type .E1. and floating-point type .double." }
  r += d + e1; // { dg-warning "arithmetic between floating-point type .double. and enumeration type .E1." }
  r += e1 * d; // { dg-warning "arithmetic between enumeration type .E1. and floating-point type .double." }
  r += d * e1; // { dg-warning "arithmetic between floating-point type .double. and enumeration type .E1." }
  r += u1 * d; // { dg-warning "arithmetic between enumeration type" }
  r += d * u1; // { dg-warning "arithmetic between floating-point type" }

  r += e1 < d;  // { dg-warning "comparison of enumeration type .E1. with floating-point type .double." }
  r += d < e1;  // { dg-warning "comparison of floating-point type .double. with enumeration type .E1." }
  r += d == e1; // { dg-warning "comparison of floating-point type .double. with enumeration type .E1." }
  r += e1 == d; // { dg-warning "comparison of enumeration type .E1. with floating-point type .double." }
  r += u1 == d; // { dg-warning "comparison of enumeration type" }
  r += d == u1; // { dg-warning "comparison of floating-point type" }

  r += b ? e1 : d; // { dg-warning "enumerated and non-enumerated type in conditional expression" }
  r += b ? d : e1; // { dg-warning "enumerated and non-enumerated type in conditional expression" }
  r += b ? d : u1; // { dg-warning "enumerated and non-enumerated type in conditional expression" }
  r += b ? u1 : d; // { dg-warning "enumerated and non-enumerated type in conditional expression" }

  d += e1; // { dg-warning "arithmetic between floating-point type .double. and enumeration type .E1." }
  d = e1;

  return r;
}
