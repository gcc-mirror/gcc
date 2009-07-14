// PR c++/34394
// { dg-do compile }

extern "C" double fabs (double);

void foo (double x)
{
  fabs (x) ();	// { dg-error "__builtin_abs" }
}
