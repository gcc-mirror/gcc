/* { dg-do compile } */
/* { dg-options "-O2 -fnon-call-exceptions" } */
struct A
{
  int i;
  virtual ~A ()
  {}
};

struct B : virtual A
{};

struct C : public B
{
  C ();
  ~C (){}
};

void foo ()
{
  C c;
}
