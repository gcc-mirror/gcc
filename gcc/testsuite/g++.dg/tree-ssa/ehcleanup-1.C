// { dg-options "-O2 -fdump-tree-ehcleanup1-details" }

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#define NOEXCEPT_FALSE noexcept (false)
#else
#define NOEXCEPT_FALSE
#endif

extern void can_throw ();
class a
{
public:
  ~a () NOEXCEPT_FALSE
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
// { dg-final { scan-tree-dump-times "Empty EH handler" 2 "ehcleanup1" } }
//
// And as a result also contained control flow.
// { dg-final { scan-tree-dump-times "Removing unreachable" 6 "ehcleanup1" } }
//
// { dg-final { cleanup-tree-dump "ehcleanup1" } }
