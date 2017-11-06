// { dg-do assemble  }
// { dg-options "-Wno-deprecated" }

int f(int x) return y(x) { return 0; } // { dg-error "" } 

extern "C" void abort ();

int main()
{
  if (f(1) != 1 || f(2) != 2 || f(3) != 3)
    abort ();
  return 0;
}
