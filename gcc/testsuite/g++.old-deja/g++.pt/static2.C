// { dg-do assemble  }

template <class A>
class TEST
{
public:
  TEST (A) {}
};

template <class A>
class TEST2
{
  static A i;
};

template <class A>
A TEST2 <A>::i (0);

TEST2 <TEST <int> > a;

template class TEST2 <TEST <int> >;
