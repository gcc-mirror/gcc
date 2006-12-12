/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort(void);
struct Bar { int p; };
struct Foo { struct Bar *p; };
struct Bar p0 = { 0 };
struct Bar p1 = { 1 };
void bar(struct Foo *f)
{
  f->p = &p0;
}
int foo(struct Foo *f)
{
  f->p->p = 1;
  bar(f);
  return f->p->p;
}
int main()
{
  struct Foo f;
  f.p = &p1;
  if (foo(&f) != 0)
    abort ();
  return 0;
}
