// { dg-do assemble  }
// Origin: Jason Merrill <jason@redhat.com>

int main ()
{
  static const int n = 10;
  static const int *p = &n;
}
