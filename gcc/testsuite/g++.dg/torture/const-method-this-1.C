// PR tree-optimization/127133
// { dg-do run }
// { dg-additional-options "-fallow-store-data-races" }

struct s1
{
  void f(int b, int d) const;
  int t;
};

__attribute__((noipa))
void
s1::f(int b, int d) const
{
  s1 &c = const_cast<s1&>(*this);
  int p = t;
  if (b)
    c.t = d | p;
}


const s1 a = {1};

int main()
{
  a.f(0,0);
}
