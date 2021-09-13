// The reference temp should be TLS, not normal data.
// { dg-require-effective-target c++11 }
// { dg-final { scan-assembler-not "\\.data" { target tls_native xfail powerpc-*-aix* } } }
// { dg-final { scan-assembler-symbol-section {^_?ir$} {^\.tbss} { target tls_native xfail powerpc-*-aix* } } }
// { dg-final { scan-assembler-symbol-section {^_?_ZGR2ir_$} {^\.tdata|\[TL\]} { target tls_native } } }

extern int&& ir;
#pragma omp threadprivate (ir)
int&& ir = 42;

void f()
{
  ir = 24;
}
