/* { dg-do compile } */
/* { dg-options "-O2"  } */
class SnmpSyntax
{
public:
  virtual SnmpSyntax *m_fn1 () const;
  ~SnmpSyntax () {}
  virtual SnmpSyntax &operator=(const SnmpSyntax &);
};

class A : public SnmpSyntax
{
public:
  A (int);
  SnmpSyntax *m_fn1 () const {}
  SnmpSyntax &operator=(const SnmpSyntax &);
};
int a;
void fn1 ()
{
  for (;; a++)
    switch (0)
    case 0:
      {
        A b (0);
        SnmpSyntax &c = b;
        c.m_fn1 ();
      }
}
// Devirtualization to A::m_fn1 would be possible, but we can not do it at the moment
