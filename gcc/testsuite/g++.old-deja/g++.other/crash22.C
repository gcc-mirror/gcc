// Build don't run:
// Special g++ Options: -O2

class foo
{
};

typedef void *voidp;
class vect : public foo
{
public:
  voidp& a();
  int b();
};

class bar
{
public:
  bar *c(bool (*f)(bar *node), voidp g)
    {
      int i=0;
      bool j;
      while (i < d.b()){
        j = (f == __null) || f((bar*)d.a());
        if (j)
          ((bar*)d.a())->c(f, g);
        i++;
      }
      return this;
    }
 public:
  vect d;
  bar *e(foo *k);
};

bar *bar::e(foo *k)
{
  return c(__null, k);
}

voidp &vect::a()
{
  static voidp x;
  return x;
}

int vect::b()
{
  static int x;
  return x;
}

int main()
{
  return 0;
}
