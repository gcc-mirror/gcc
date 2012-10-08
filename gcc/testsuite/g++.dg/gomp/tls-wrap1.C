// If we can see the definition at the use site, we don't need to bother
// with a wrapper.

// { dg-require-effective-target tls }
// { dg-final { scan-assembler-not "_ZTW1i" } }

int i = 42;
#pragma omp threadprivate (i)

int main()
{
  return i - 42;
}
