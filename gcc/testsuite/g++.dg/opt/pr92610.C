// PR rtl-optimization/92610
// { dg-do compile }
// { dg-options "-w -fdelete-dead-exceptions --param=sccvn-max-alias-queries-per-access=0 -fno-dse -fnon-call-exceptions -Os -funroll-loops -ftrapv" }

struct C { int x; ~C () {} };

int
main ()
{
  C *buffer = new C[42];
  buffer[-3].x = 42;
  delete [] buffer;
}
