// { dg-do assemble  }
// Origin: Jason Merrill <jason@cygnus.com>

template <class T> void f()
{
  extern void g ();
}

int main()
{
  f<int>();
}

