// { dg-do run  }
// Bug: g++ crashed on empty initializers for unions.
// Bug: gcc and g++ didn't zero unions with empty initializers.
// Submitted by J"orn Rennecke <amylaar@cygnus.co.uk>

extern "C" void exit (int);

typedef union u
{
  union u *up;
  void *vp;
} u;

static u v = {};

void bar (u);
void baz (u);

void foo()
{
       u w = {};
       u x = { &v };
       baz (x);
       bar (w);
}

void baz (u w) { }

void bar (u w)
{
  if (w.up)
    exit (1);
}

int main ()
{
  foo ();
  return 0;
}
