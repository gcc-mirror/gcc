// { dg-do compile }
// { dg-additional-options "-feliminate-unused-debug-symbols" }

template <class> class A;
void f ()
{
  extern A <double> b;
}
