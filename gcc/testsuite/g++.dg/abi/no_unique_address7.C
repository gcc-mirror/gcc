// PR c++/109039
// { dg-do run { target c++11 } }

struct X {
  signed short x0 : 7;
  signed short x1 : 8;
  X () : x0 (1), x1 (2) {}
  int get () { return x0 + x1; }
};

struct S {
  [[no_unique_address]] X x;
  signed char c;
  S () : c (0) {}
};

S s;

int
main ()
{
  if (s.x.x0 != 1 || s.x.x1 != 2 || s.c != 0)
    __builtin_abort ();
  s.x.x0 = -1;
  s.x.x1 = -1;
  if (s.x.x0 != -1 || s.x.x1 != -1 || s.c != 0)
    __builtin_abort ();
  s.c = -1;
  s.x.x0 = 0;
  s.x.x1 = 0;
  if (s.x.x0 != 0 || s.x.x1 != 0 || s.c != -1)
    __builtin_abort ();
}
