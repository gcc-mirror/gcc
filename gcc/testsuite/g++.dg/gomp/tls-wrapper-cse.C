// Test for CSE of the wrapper function: we should only call it once
// for the two references to ir.
// { dg-options "-fopenmp -O -fno-inline" }
// { dg-require-effective-target tls }
// { dg-final { scan-assembler-times "call *_ZTW2ir" 1 { xfail *-*-* } } }

// XFAILed until the back end supports a way to mark a function as cseable
// though not pure.

int f() { return 42; }

int ir = f();
#pragma omp threadprivate (ir)

int main()
{
  return ir + ir - 84;
}
