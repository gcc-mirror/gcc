// { dg-do assemble  }
// Origin: Neil Booth <neilb@earthling.net> from bug #27.

struct A{};

struct B:A{};

struct C:B{};

struct CX
{
  C  c;

  operator C&(){return c;}
};

// viable functions for call below
void f(A&);
void f(B&);

int main()
{
  CX cx;
  C  c;

  f(c);   // the standard conversion to B& is better than to A& 

  f(cx);  // after user defined conversion to C&
  // the standard conversion to B& is better than to A& 
}
