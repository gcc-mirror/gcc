// Build don't link:
// Origin: Ulrich Drepper <drepper@cygnus.com>
// Special g++ Options: -w

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
