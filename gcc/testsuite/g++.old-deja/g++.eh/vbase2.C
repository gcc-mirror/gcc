// Origin: Mark Mitchell <mark@codesourcery.com>

int i;

struct A
{
  A () { i++; }
  ~A () { i--; }
};

struct B : public virtual A
{
  B () { throw 1; }
};

struct D: public B, virtual public A
{
};

void f()
{
  D d;
}

int main ()
{
  try {
    f();
  } catch (int) {
  }

  return i;
}

