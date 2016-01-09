// PR c++/69113
// { dg-do compile }
// { dg-options "-fno-weak" }

struct foo
{
  static void bar ()
  {
    struct baz
    {
      static void m ()
      {
	static int n;
      }
    };
  }
};
