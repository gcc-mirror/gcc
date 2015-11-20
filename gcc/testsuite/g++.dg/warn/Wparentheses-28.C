// PR bootstrap/68361
// { dg-options -Wparentheses }

struct A
{
  int p: 2;
};

A a, b;

int main()
{
  bool t = (a.p = b.p);
}
