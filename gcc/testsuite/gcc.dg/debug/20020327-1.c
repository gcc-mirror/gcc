/* { dg-do link } */

#ifndef __powerpc64__
/* Fails on powerpc64-linux due to the function Letext using a global
   .Letext symbol that conflicts with .Letext emitted by gcc with
   -gstabs.  Some day the linker will be fixed to not require global
   "dot" syms, but for now disable this test entirely for powerpc64.
   Using xfail doesn't work, nor does dg-excess-errors because some
   combinations of command line options won't cause this test to fail.  */
void Letext (void) { }
#endif
int main() { return 0; }
