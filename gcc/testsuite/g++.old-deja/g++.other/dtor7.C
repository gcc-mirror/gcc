// Origin: Mark Mitchell <mitchell@codesourcery.com>
// Special g++ Options: -O2

int i;
int j;

struct A
{
  A ();
  A (const A&);
  ~A ();
};

A::A ()
{
  ++i;
}

A::A (const A&)
{
  ++i;
}

A::~A () 
{
  --i;
}

A f () 
{
  return A ();
}

void g (const A&)
{
}

int main ()
{
  g (f ());
  return i;
}
