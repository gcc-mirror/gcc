// { dg-do compile }
// { dg-additional-options "-Wall" }

struct jmp_buf { long l[16]; };
extern "C" int setjmp (jmp_buf *);
struct S {
  void foo () { bar (); }
  virtual char bar () { return 0; }
};
void baz ();
jmp_buf *a;

void
qux (bool x, S *y)
{
  if (x)
    setjmp (a);
  y->foo ();
  baz ();
}
