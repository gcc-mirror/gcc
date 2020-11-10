// PR c++/97670

struct S { int s; };

void
foo ()
{
  S s[1] = { S () };
#pragma omp parallel reduction (+: s) allocate(s)	// { dg-error "user defined reduction not found for 's'" }
  s[0].s++;
}
