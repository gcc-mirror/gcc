// PR c++/65821
// { dg-options "-gdwarf-2 -dA" }

int b = 12;

inline void foo(const int &x = (b+3))
{
  b = x;
}

int main()
{
  foo();	      // { dg-final { scan-assembler-not "default-arg1.C:6" } }
}
