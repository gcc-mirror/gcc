/* { dg-do compile } */
/* { dg-options "-std=gnu++0x -fno-inline" } */
/* { dg-add-options bind_pic_locally } */

  struct B
  {
  constexpr B ():
    bp ()
    {}
    ~B ()
    {
      if (bp)
	bp->f ();
    }
    void f ();
    B *bp;
  };
struct A  {    B b;
};

void foo ()
{
  A a;
}
