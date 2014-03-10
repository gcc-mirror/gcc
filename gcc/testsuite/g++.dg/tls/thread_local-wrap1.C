// If we can see the definition at the use site, we don't need to bother
// with a wrapper.

// { dg-do compile { target c++11 } }
// { dg-require-effective-target tls }
// { dg-final { scan-assembler-not "_ZTW1i" } }

thread_local int i = 42;

int main()
{
  return i - 42;
}
