/* PR71624 */
// { dg-do compile { target i?86-*-* x86_64-*-* } }
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2" } */

class c1
{
public:
  virtual int fn1 () const;
  int fn2 (const int *) const;
};

class c2
{
  int fn1 ();
  c1 obj;
};

int
c1::fn1 () const
{
  return 0;
}

int
c1::fn2 (const int *) const
{
  return this->fn1 ();
}

int
c2::fn1 ()
{
  return obj.fn2 (0);
}

