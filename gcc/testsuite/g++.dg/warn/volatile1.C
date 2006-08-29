// PR c++/26577

struct A
{
  A(const A&);
  A& operator=(const A&);
  void baz() volatile;
};
void A::baz() volatile
{
  *this;			// { dg-warning "will not be accessed" }
}
