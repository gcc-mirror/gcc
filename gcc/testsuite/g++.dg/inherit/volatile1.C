// PR c++/19299
// Origin: Andrew Pinski <pinskia@gcc.gnu.org>

// { dg-do compile }

struct V
{
  virtual void foo() = 0;
};

void bar(V volatile* p)
{
  p->V::~V();
}
