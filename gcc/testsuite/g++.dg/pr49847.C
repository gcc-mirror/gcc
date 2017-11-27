/* { dg-do compile } */
/* { dg-options "-O -fnon-call-exceptions -Wno-return-type" } */
int f (float g)
{
  try { return g >= 0; }
  catch (...) {}
}
