// Origin: Mark Mitchell <mark@codesourcery.com>

int i;

struct B {
  B () {}
  B (B&) { i = 1; }
  B (const B&) { i = 2; }
};

struct D : public B {
  D () {}
};

int main ()
{
  D d;
  D d2 (d);
  if (i != 2)
    return 1;
}
