// Build don't link:
// Origin: Ulrich Drepper <drepper@cygnus.com>

struct st
{
  int a, b, c, d;
};

void g ()
{
  static const st i = { 0,1,2,3 };
}

void h ()
{
  static const st i = { 0,1,2,3 };
}
