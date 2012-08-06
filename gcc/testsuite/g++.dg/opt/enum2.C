// PR c++/43680
// Test that we don't make excessively aggressive assumptions about what
// values an enum variable can have.
// { dg-do run { target fpic } }
// { dg-options "-O2 -fPIC" }

extern "C" void abort ();

enum E { A, B, C, D };

void
CheckE(const E value)
{
  long v = value;
  if (v <= D)
    abort ();
}

int main() {
  CheckE(static_cast<E>(5));
}
