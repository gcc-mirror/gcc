// { dg-do run  }
// Bug: g++ re-evaluates the initializer for r before calling f(); since i has
// changed to an invalid index, this breaks.

class C
{
public:
  void f () { }
};

void foo (C * objs[])
{
  int i = 0;
  C & r = * objs[i];	/* make reference to element */

  i = 666;
  r.f ();		/* core dumps here */
}

int
main ()
{
  C * objs[1] = { new C };

  foo (objs);
}
