/* Caused ICE in in make_decl_rtl, at varasm.c:1311.  */
/* { dg-do compile } */

class A
{
public:
  A () : value (123) {}
  int value;
};

template <typename StoredFunction> class B
{
public:
  template <typename F> B (F p1) : mFunction (p1) { mFunction (); }
  StoredFunction mFunction;
};
template <typename Function>
void
NS_NewRunnableFunction (Function p1)
{
  (B<Function> (p1));
}
class C
{
  void DispatchConnectionCloseEvent (A);
  void AsyncCloseConnectionWithErrorMsg (const A &);
};
void
C::AsyncCloseConnectionWithErrorMsg (const A &)
{
  {
    A message;
    NS_NewRunnableFunction (
      [this, message] { DispatchConnectionCloseEvent (message); });
  }
}
