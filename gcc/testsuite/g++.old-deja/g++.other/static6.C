// { dg-do assemble  }
// Origin: Jason Merrill <jason@cygnus.com>

void f (int i)
{
  static int a[] = { i };
}
