// { dg-do run  }
// Origin: Jason Merrill <jason@cygnus.com>

int j = 42;

int main ()
{
  static int i = j;
  return (i != 42);
}
