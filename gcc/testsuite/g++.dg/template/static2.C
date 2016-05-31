class A;

template<int A::* P>
class B
{
public:
  static int A::* const p = P; // { dg-error "25:'constexpr' needed" "" { target c++11 } }
  // { dg-error "25:invalid in-class" "" { target c++98_only } 7 }
  // { dg-error "29:template parameter" "" { target c++98_only } 7 }
};

class A
{
public:

int dummy;

B<&A::dummy> d;
};
