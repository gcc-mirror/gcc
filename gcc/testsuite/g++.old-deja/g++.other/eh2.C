// { dg-do assemble  }
// { dg-options "-O2" }
// Origin: Jakub Jelinek <jakub@redhat.com>

class a {
public:
  double b;
  int c;
  ~a() { }
};

int bar(a x);
a foo(double x);

int baz(double x, int y)
{
   return bar(foo(x));
}
