// PR c++/20206
// { dg-do run }
// { dg-options "-O0" }

void
bar (int x)
{
  asm ("" : : "g" (x));
}

struct S { S () {}; virtual ~S () {}; };
struct T { virtual T *foo (int) { return 0; }; };
struct V : virtual S, virtual T {};
struct V v;
struct U : public S, public T
{
  bool a;
  U () {}
  virtual ~U () {}
  virtual V *foo (int x)
  {
    switch (x)
      {
      case 12:
	break;
      case 9:
	bar (7);
	break;
      case 10:
	bar (12);
	break;
      case 4:
	bar (18);
	break;
      case 2:
	bar (26);
	break;
      }
    return &v;
  }
};
U u;

int
main ()
{
}
