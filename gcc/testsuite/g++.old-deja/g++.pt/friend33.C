// { dg-do link  }
// { dg-options "-g" }

template <class P1>
struct S1
{
  struct SS1
  {
  };
  friend void Foo (const SS1& ss1)
    {
    }
};

template <class P1>
void Foo(const S1<P1>& s1)
{
  typedef typename S1<P1>::SS1 TYPE;
  TYPE t;
  Foo(t);
}

int main ()
{
  S1<double> obj;
  Foo(obj);
}

