// PR c++/44627
// { dg-do compile }

struct A
{
  A *foo ();
};

template <class T>
void
bar ()
{
  A::foo ().anything;	// { dg-error "request for member" }
}

void
baz ()
{
  bar <int> ();
}
