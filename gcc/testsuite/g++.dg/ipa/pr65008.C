// PR ipa/65008
// { dg-do compile }
// { dg-options "-O2" }

struct A
{
  A ();
  virtual void foo () {}
};

static inline int __attribute__ ((always_inline)) call_foo (A *a)
{
  a->foo ();
}

A::A ()
{
  call_foo (this);
}
