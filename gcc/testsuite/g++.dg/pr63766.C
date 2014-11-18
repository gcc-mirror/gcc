/* { dg-do compile } */
/* { dg-options "-std=c++11 -O2" } */

class A
{
 public:
  void
    getValueType ()
  {
  }
  void getTypeClass ();
};
template <typename ImplClass> class B
{
 public:
  void
    Visit (A *p1)
  {
    p1->getTypeClass ();
    static_cast<ImplClass *> (0)->VisitAtomicType (0);
  }
};
class C : B<C>
{
  template <typename Fn>
  void
    dumpChild (Fn p1)
    {
      p1 ();
    }

 public:
  void dumpTypeAsChild (int);
  void
    VisitAtomicType (A *p1)
  {
    p1->getValueType ();
    dumpTypeAsChild (0);
  }
};
void
C::dumpTypeAsChild (int)
{
  dumpChild ([=]
             {
               Visit (0);
             });
}
