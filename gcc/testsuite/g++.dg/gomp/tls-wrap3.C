// If we can't see the definition at all, we need to assume there might be
// an init function.

// { dg-require-alias "" }
// { dg-require-effective-target tls }
// { dg-final { scan-assembler "_ZTW1i" } }
// { dg-final { scan-assembler "_ZTH1i" } }

extern int i;
#pragma omp threadprivate (i)

int main()
{
  return i - 42;
}
