class A;

template<int A::* P>
class B
{
public:
  static int A::* const p = P; // { dg-error "" }
};

class A
{
public:

int dummy;

B<&A::dummy> d;
};
