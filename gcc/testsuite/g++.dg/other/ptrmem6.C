// PR c++/19666
// Origin: Volker Reichelt <reichelt@gcc.gnu.org>
// { dg-do compile }

struct A { int i; };

int foo (A *p)
{
  return &p->i - &(p->*&A::i);
}
