// { dg-do assemble  }
// GROUPS passed initialization
class A
{
public:
    A(const A & a) : i_member(a.i_member)
    {
    }
    A(const int & i) : i_member(i)
    {
    }
    union
    {
        int i_member;
    };
};
