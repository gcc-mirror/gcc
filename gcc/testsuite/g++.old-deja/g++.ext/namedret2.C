// { dg-do assemble { target alpha*-*-* } }
// { dg-options "-Wno-deprecated" }

// This test verifies that return type promotion is working correctly.  
// The Alpha ABI specifies that 32-bit return values have bit 31 propagated,
// i.e. the value is sign-extended even if the unpromoted type is unsigned.

unsigned int f(unsigned int x) return y(x) { } // { dg-error "" } 

extern "C" void abort ();

int main()
{
  typedef long (*long_func)(long);
  long_func g = reinterpret_cast<long_func>(f);

  if (g(-1L) != -1L)
    abort ();
  return 0;
}
