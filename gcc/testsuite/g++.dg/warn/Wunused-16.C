// { dg-do compile }
// { dg-options "-Wunused-value" }

extern void f1();
void
f(bool b)
{
  b ? f1(), 0 : 0;	// { dg-bogus "has no effect" }
}
