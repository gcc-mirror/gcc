// The reference temp should be TLS, not normal data.
// { dg-require-effective-target c++11 }
// { dg-final { scan-assembler-not "\\.data" { target tls_native xfail powerpc-*-aix* } } }
// { dg-final { scan-assembler-symbol-section {^_?ir$} {^\.tbss|\[TL\]} } }
// { dg-final { scan-assembler-symbol-section {^_?_ZGR2ir_$} {^\.tdata|\[TL\]} } }

extern int&& ir;
#pragma omp threadprivate (ir)
int&& ir = 42;

void f()
{
  ir = 24;
}
