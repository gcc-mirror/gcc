/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ccp1"  } */
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
/* { dg-final { scan-tree-dump-not "OBJ_TYPE_REF" "ccp1"  } } */
