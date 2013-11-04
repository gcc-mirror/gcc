// { dg-options -std=c++11 }

struct A			// { dg-error "const|operator=" }
{
  const int i;
};

int main()
{
  A a = { 0 };
  a = a;			// { dg-error "deleted" }
}
