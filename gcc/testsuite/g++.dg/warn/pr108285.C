// PR c++/108285
// { dg-do compile }
// { dg-options "-fexcess-precision=standard -Wfloat-conversion" }

void bar (double);

void
foo (float x)
{
  bar (2 * x);	// { dg-bogus "conversion from '\[^\n\r]\*' to 'double' may change value" }
}
