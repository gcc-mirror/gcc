// { dg-do assemble  }
// Origin: Jason Merrill <jason@cygnus.com>

  struct A {
    int operator * ();
  };
  struct B : public A { };
  int operator * (B &);

  int main ()
  {
    B b;
    B& br = b;
    *br;
  }
