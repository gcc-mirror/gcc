// Origin: jason@redhat.com
// { dg-do compile }

struct A { A(); A(const A&); int i; };
struct B: public A { };

int f (bool b, A& ar, B& br)
{
  return (b?ar:br).i;
}
