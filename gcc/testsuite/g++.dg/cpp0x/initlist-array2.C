// { dg-options -std=c++0x }

typedef int IA[2];
typedef double DA[2];

void f(const IA&) { }
void f(const DA&);

int main()
{
  f({1,2});
}
