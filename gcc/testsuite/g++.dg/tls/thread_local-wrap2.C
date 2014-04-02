// If we can't see the definition at the use site, but it's in this translation
// unit, we build a wrapper but don't bother with an init function.

// { dg-do compile { target c++11 } }
// { dg-require-effective-target tls }
// { dg-final { scan-assembler "_ZTW1i" } }
// { dg-final { scan-assembler-not "_ZTH1i" } }

extern thread_local int i;

int main()
{
  return i - 42;
}

thread_local int i = 42;
