// { dg-do compile }
// { dg-options "-O -fdump-tree-optimized" }

struct pf
{
  inline pf(int(*x)(int)) : x(x) {}

  inline int operator()(int a) const
    {
      return x(a);
    }

  int (*x)(int);
};

inline int g(int x) { return x/x - 1; }

int main(int argc, char* argv[])
{
  pf f(g);
  return f(3);
}

// { dg-final { scan-tree-dump "return 0" "optimized" } }
