// Test for CSE of the wrapper function: we should only call it once
// for the two references to ir.
// { dg-do run { target c++11 } }
// { dg-options "-O -fno-inline -save-temps" }
// { dg-add-options tls }
// { dg-require-effective-target tls_runtime }
// { dg-final { scan-assembler-times "call *_ZTW2ir" 1 { xfail *-*-* } } }

// XFAILed until the back end supports a way to mark a function as cseable
// though not pure.

int f() { return 42; }

thread_local int ir = f();

int main()
{
  return ir + ir - 84;
}
