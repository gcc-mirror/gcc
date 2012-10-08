// { dg-options "-std=c++11" }
// { dg-require-effective-target tls }
// { dg-require-alias }

// The reference temp should be TLS, not normal data.
// { dg-final { scan-assembler-not "\\.data" } }

thread_local int&& ir = 42;

void f()
{
  ir = 24;
}
