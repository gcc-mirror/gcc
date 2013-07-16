// Binding an rvalue to && beats binding it to const& (13.3.3.2).

// { dg-require-effective-target c++11 }

struct A
{
  int operator+(int) &&;
};

void operator+ (const A&, int);

int main()
{
  return A() + 42;
}
