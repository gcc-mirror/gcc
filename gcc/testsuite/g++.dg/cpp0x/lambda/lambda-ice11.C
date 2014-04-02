// PR c++/60252
// { dg-require-effective-target c++11 }

struct A
{
  int i;			// { dg-message "" }

  void foo()
  {
    [&](){ [&](int[i]){}; };	// { dg-error "" }
  }
};
