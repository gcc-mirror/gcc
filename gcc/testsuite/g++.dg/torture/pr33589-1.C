// { dg-do compile }
struct base { void somemethod() {} };
struct derived : public base { };

struct smartpointer
{
  ~smartpointer() { }
  operator derived*() const
  {
    return 0;
  }
};
typedef void ( derived::* methodptr_type )();
methodptr_type getmemberptr()
{
        return &derived::somemethod;
}
void somefunction()
{
        smartpointer pObj;
        ( pObj->*getmemberptr() )();
}
