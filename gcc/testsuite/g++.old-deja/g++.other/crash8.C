// { dg-do assemble  }
// Origin: Jason Merrill <jason@cygnus.com>

int main()
{
  void f();
  class A {
    friend void f();
  };
}
