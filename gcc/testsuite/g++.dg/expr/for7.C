// PR c++/86769
// { dg-do run }

int v;

struct S
{
  S (int x) : s(x) { v++; }
  ~S () { v--; }
  int s;
  operator int () { if (!v) __builtin_abort (); return s; }
};

int
main ()
{
  int x = 10;
  for (int l = 1; S d = x - l; l = d + 1)
    ;
}
