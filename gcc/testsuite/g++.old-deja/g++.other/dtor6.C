// Origin: Mark Mitchell <mark@codesourcery.com>

extern "C" void abort ();

int count;

struct S
{
  S ();
  S (const S&);
  ~S ();

  int i;
};

S::S ()
{
  i = count++;
}

S::S (const S&)
{
  i = count++;
}

S::~S ()
{
  if (--count != i)
    abort ();
}

void f (S, S)
{
}

int main ()
{
  {
    S s;
    f (s, s);
  }
  return count != 0;
}

