// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>
// Special g++ Options: -O2

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
