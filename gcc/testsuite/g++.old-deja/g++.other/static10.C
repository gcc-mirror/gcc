// { dg-do assemble  }
// { dg-options "-w" }
// Origin: Ulrich Drepper <drepper@cygnus.com>

struct st
{
  int a;
};

int
foo (int a)
{
  static const st i = { 0 };

  if (i.a == a)
    return 0;
}
