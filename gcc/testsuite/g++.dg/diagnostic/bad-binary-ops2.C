// PR c++/107382
// PR c++/107383
// { dg-do compile }
// { dg-options "-O2 -fexcess-precision=standard" }

void
foo ()
{
  float t[2] = { 1, 2 };
  int const *s = 0;
  t[1] / s;	// { dg-error "invalid operands of types 'float' and 'const int\\\*' to binary 'operator/'" }
}

void
bar ()
{
  float t[2] = { 1, 2 };
  int const *s[2] = { 0, 0 };
  t[1] / s[0];	// { dg-error "invalid operands of types 'float' and 'const int\\\*' to binary 'operator/'" }
}

void
baz (float a, int* b)
{
  a -= b;	// { dg-error "invalid operands of types 'float' and 'int\\\*' to binary 'operator-'" }
}
