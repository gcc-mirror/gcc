// { dg-do run  }
// Origin: Mark Mitchell <mark@codesourcery.com>

#if defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100

struct R
{
  virtual void r ();
};

struct S 
{
  virtual void f ();
};

struct T : virtual public S
{
  virtual void g ();
};

struct U : public R, virtual public T
{
  virtual void h ();
};

struct V : public R, virtual public S, virtual public T
{
  virtual void v ();
};

struct U1
{
  R r;
  T t;
};

int main ()
{
  if (sizeof (U) != sizeof (U1))
    return 1;
  if (sizeof (V) != sizeof (U1))
    return 2;
}

#else /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */

int main ()
{
}

#endif /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */

