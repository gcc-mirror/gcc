// { dg-do compile }
// { dg-require-effective-target vect_int }

struct A
{
  unsigned int a, b, c, d;

  A& operator+= (A const& that)
    {
      a += that.a;
      b += that.b;
      c += that.c;
      d += that.d;
      return *this;
    }

  A& operator-= (A const& that)
    {
      a -= that.a;
      b -= that.b;
      c -= that.c;
      d -= that.d;
      return *this;
    }
};

void test(A& x, A const& y1, A const& y2)
{
  x += y1;
  x -= y2;
}

// We want to SLP vectorize a single connected SLP subgraph with two instances
// { dg-final { scan-tree-dump-not "removing SLP instance" "slp2" } }
// { dg-final { scan-tree-dump-times "SLPing BB part" 1 "slp2" } }
// { dg-final { scan-tree-dump-times "Vectorizing SLP" 2 "slp2" } }
