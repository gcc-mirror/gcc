// Build don't link:
// Origin: Jakub Jelinek  <jakub@redhat.com>
// Special g++ Options: -O2 -fno-exceptions

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
