/* { dg-do compile } */
/* { dg-options "-O" } */

struct A {
  virtual A *getThis();
};

struct B {
  virtual B *getThis();
};

struct AB : public A, public B {
  virtual AB *getThis() { return 0; }
};

void foo ()
{
  AB ab;
  B *b = &ab;
  b->getThis();
}

