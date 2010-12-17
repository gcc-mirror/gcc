// { dg-options -std=c++0x }

struct A
{
  const int *p[2];
};

constexpr const int * f(const int *p) { return p; }

int main()
{
  constexpr int i = 42;
  constexpr int j = *&i;	   // OK
  constexpr int k = *A{{&i}}.p[0]; // OK
  constexpr int l = *f(&i);	   // OK
}
