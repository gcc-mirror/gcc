// PR ipa/93621
// { dg-do compile }
// { dg-options "-O3 --param ipa-cp-eval-threshold=100 --param large-function-growth=60 --param large-function-insns=10 --param uninlined-thunk-insns=1000" }

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
