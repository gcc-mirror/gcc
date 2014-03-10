// If we can't see the definition at all, we need to assume there might be
// an init function.

// { dg-require-alias "" }
// { dg-require-effective-target tls }
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "_ZTW1i" } }
// { dg-final { scan-assembler "_ZTH1i" } }

extern thread_local int i;

int main()
{
  return i - 42;
}
