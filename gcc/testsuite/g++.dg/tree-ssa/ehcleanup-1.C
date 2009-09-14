// { dg-options "-O2 -fdump-tree-ehcleanup1-details" }
extern void can_throw ();
class a
{
public:
  ~a ()
  {
    if (0)
      can_throw ();
  }
};
void
t (void)
{
  class a a;
  can_throw ();
}
// We ought to remove implicit cleanup, since destructor is empty. 
// { dg-final { scan-tree-dump-times "Empty EH handler" 1 "ehcleanup1" } }
//
// And as a result also contained control flow.
// { dg-final { scan-tree-dump-times "Removing unreachable" 2 "ehcleanup1" } }
//
// { dg-final { cleanup-tree-dump "ehcleanup1" } }
