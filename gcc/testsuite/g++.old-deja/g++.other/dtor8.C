// Special g++ Options: -O1
// Origin: Mark Mitchell <mark@codesourcery.com>

int result;

struct S
{
  S ();
  S (const S&);
  ~S ();
  
  int i;
  double d[18];
};

S* s;

S::S ()
{
  s = this;
}

S::~S ()
{
  if (s != this)
    result = 1;
}

inline S f ()
{
  return S ();
}

int main ()
{
  f ();
  return result;
}
