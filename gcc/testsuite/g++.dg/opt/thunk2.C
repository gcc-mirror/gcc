// PR c++/20206
// { dg-do run }
// { dg-options "-O0" }

void
bar (int x)
{
  asm ("" : : "g" (x));
}

struct S { S () {}; virtual ~S () {}; };
struct T { virtual void foo (int) = 0; };
struct U : public S, public T
{
  bool a;
  U () {}
  virtual ~U () {}
  virtual void foo (int x)
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
  }
};
U u;

int
main ()
{
}
