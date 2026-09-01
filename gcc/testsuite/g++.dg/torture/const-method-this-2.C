// PR tree-optimization/127134
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
  for(int i = 0; i < b; i++) {
    s1 &c = const_cast<s1&>(*this);
    int p = 1;
    if (d)
      c.t = p;
  }
}

const s1 a = {1};

int main()
{
  a.f(1,0);
}
