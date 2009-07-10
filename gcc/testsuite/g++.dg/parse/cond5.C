// PR c++/40566

void
f (int x, int y)
{
  int c = x ? 23 : throw "bla";
  short d = y ? throw "bla" : 23;
  char e = x ? 23 : throw "bla";
  long f = x ? 23 : throw "bla";
}
