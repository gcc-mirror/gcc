// PR 99030 ICE with generic lambda accessing local extern
// { dg-do compile { target c++14 } }

void foo ()
{
  extern int a;
  [] (auto b) { a; } (1);
}

template<typename T> void bar ()
{
  extern T a;
  [] (auto b) { a; } (1);
}

template void bar<int> ();
