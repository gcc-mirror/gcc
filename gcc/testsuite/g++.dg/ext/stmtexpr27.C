// PR c++/86769
// { dg-do run }
// { dg-options "" }

struct A {
  int a;
  A (int x) : a(x) {}
  ~A () {}
  A (const A &x) : a(x.a) {}
  operator bool () { return a != 0; }
};

int
main ()
{
  int v = 0;
  for (int i = 0; i < 3; ++i)
    for (int j = 0; j < ({ if (i == 1 && j == 1) continue; 4; }); ++j)
      ++v;
  if (v != 9)
    __builtin_abort ();
  v = 0;
  for (int i = 0; i < 3; ++i)
    for (int j = 0; j < ({ if (i == 1 && j == 1) break; 4; }); ++j)
      ++v;
  if (v != 5)
    __builtin_abort ();
  v = 0;
  for (int i = 0; i < 3; ++i)
    for (int j = 0; j < 4; ({ if (i == 1 && j == 1) continue; 1; }), ++j)
      ++v;
  if (v != 10)
    __builtin_abort ();
  v = 0;
  for (int i = 0; i < 3; ++i)
    for (int j = 0; j < 4; ({ if (i == 1 && j == 1) break; 1; }), ++j)
      ++v;
  if (v != 6)
    __builtin_abort ();
  v = 0;
  for (int i = 0; i < 3; ++i)
    for (int j = 0; A c = j < ({ if (i == 1 && j == 1) continue; 4; }); ++j)
      ++v;
  if (v != 9)
    __builtin_abort ();
  v = 0;
  for (int i = 0; i < 3; ++i)
    for (int j = 0; A c = j < ({ if (i == 1 && j == 1) break; 4; }); ++j)
      ++v;
  if (v != 5)
    __builtin_abort ();
  v = 0;
  for (int i = 0; i < 3; ++i)
    for (int j = 0; A c = j < 4; ({ if (i == 1 && j == 1) continue; 1; }), ++j)
      ++v;
  if (v != 10)
    __builtin_abort ();
  v = 0;
  for (int i = 0; i < 3; ++i)
    for (int j = 0; A c = j < 4; ({ if (i == 1 && j == 1) break; 1; }), ++j)
      ++v;
  if (v != 6)
    __builtin_abort ();
}
