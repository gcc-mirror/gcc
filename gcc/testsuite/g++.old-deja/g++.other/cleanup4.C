// { dg-do assemble  }
// { dg-options "-O2 -fno-exceptions" }
// Origin: Jakub Jelinek  <jakub@redhat.com>

class foo
{
public:
  foo ();
  ~foo ();
};

class bar
{
public:
  bar ();
  bar (const foo&);
};

int i;
foo f ();

inline bar x ()
{
  switch (i)
    {
    case 0: return bar (f ());
    default: return bar ();
    }
}

bar y ()
{
  return x ();
}
