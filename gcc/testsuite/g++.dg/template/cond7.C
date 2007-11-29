// PR c++/34270
// { dg-do compile }
// { dg-options "" }

void foo ()
{
  __typeof__ (0 ?: 0) x;
  __decltype (0 ?: 0) y;
}

template<int> void bar ()
{
  __typeof__ (0 ?: 0) x;
  __decltype (0 ?: 0) y;
}
