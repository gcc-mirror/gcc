// PR ipa/71146
// { dg-do compile }
// { dg-options "-O3 -fipa-pta" }

typedef enum { X } E;
struct A {
  virtual void bar ();
};
struct B {
  virtual E fn (const char *, int, int *) = 0;
};
struct C : A, B {
  E fn (const char *, int, int *);
  void fn2 ();
  B *foo;
};
void C::fn2 () {
  if (!foo)
    return;
  foo->fn (0, 0, 0);
}
E
C::fn (const char *, int, int *)
{
  fn2 ();
  foo = 0;
  fn (0, 0, 0);
  return X;
}
