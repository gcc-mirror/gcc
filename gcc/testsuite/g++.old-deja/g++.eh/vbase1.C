// Origin: Mark Mitchell <mark@codesourcery.com>

int i;
int j;

struct B
{
  B() { i = 1; }
  ~B() { j = 7; }
};

struct D : virtual public B {
  D () { throw 3; }
};

int main ()
{
  try {
    D d;
  } catch (int) {
    if (i != 1 || j != 7)
      return 1;
  }
}
