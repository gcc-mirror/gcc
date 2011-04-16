// { dg-options -std=c++0x }

extern "C" int printf (const char *, ...);

enum E { e1, e2, e3, X };
E operator*(E e) { return e; }
E begin(E e) { return e; }
E end(E e) { return X; };
E operator++(E& e) { return e = E(e+1); }

int main()
{
  for (auto e: e1)
    {
      printf ("%d ", e);
    }
}
