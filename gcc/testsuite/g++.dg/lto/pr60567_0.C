// PR lto/60567
// { dg-lto-do link }
// { dg-lto-options { { -flto -fno-use-linker-plugin } } }
// { dg-extra-ld-options "-r" }

#pragma implementation
struct S {};

#pragma interface
struct T
{
  virtual void foo (const S &) = 0;
};

struct U
{
  virtual void bar (const S &) = 0;
};

struct V : public T, public U
{
  virtual void bar (const S &) {}
};
