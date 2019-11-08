/* PR c++/92339  */
/* { dg-options "-std=c++11" } */

class classA
{
  template <typename typeB, typeB classA::*> struct typeC
  {
    typeC (classA *);
  };
  int m_fn1();
  unsigned long fieldD;
  using typeE = typeC<unsigned long, &classA::fieldD>;
};
int
classA::m_fn1 ()
{
  typeE (this);
  return 0;
}
