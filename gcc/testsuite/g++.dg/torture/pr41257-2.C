/* { dg-do link } */

struct A
{
  virtual ~A();
};

struct B : virtual A
{
  virtual ~B() {}
};

int main()
{
  return 0;
}
