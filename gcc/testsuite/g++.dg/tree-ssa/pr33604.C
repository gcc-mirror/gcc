/* { dg-do run } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

struct Value
{
  double value;
  Value(double value_) : value (value_) {}
  operator double() const { return value; }
  Value& operator=(double other) { value = other; }
};

struct Ref
{
  const Value& m;
  Ref(const Value& m_) : m(m_) {}
  operator double() const { return m; }
};

struct Diff
{
  const Ref lhs, rhs;
  Diff(const Value& lhs_, const Value& rhs_) : lhs(lhs_), rhs(rhs_) {}
  operator double() const { return lhs - rhs; }
};

extern "C" void abort (void);
int main(int argc, char *argv[])
{
  Value I(1), m(4);
  for(int a = 0; a < 1000; a++)
    m = Diff (I, m);

  if (!(m / 4 == I))
    abort ();
  return 0;
}

/* Check that we forward propagated
     D.2182_13 = (struct Ref *) &D.2137.lhs;
   to
     D.2182_13->lhs.m ={v} &I;
   yielding
     D.2137.lhs.m ={v} &I;  */

/* { dg-final { scan-tree-dump-times "D\\\.....\\\..hs\\\.m =" 2 "forwprop1" } } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */
