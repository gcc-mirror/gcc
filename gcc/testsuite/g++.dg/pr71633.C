/* PR71633 */
/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && { ! x32 } } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2" } */

class c1
{
  virtual void fn1 ();
};

class c2
{
  virtual int *fn2 () const;
};

class c3 : c1, c2
{
  int *fn2 () const;
  int *fn3 (int) const;
};

int *c3::fn2 () const
{
  return 0;
}

int *c3::fn3 (int p) const
{
  return fn3 (p);
}
