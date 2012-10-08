// { dg-do compile { target c++11 } }

struct [[gnu::packed]] A
{
  void f () const;
};

void
A::f () const
{
}
