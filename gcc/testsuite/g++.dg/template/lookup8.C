// PR c++/38030
// The call to f should be resolved at template definition time.
// { dg-do link }

struct B { };
struct D : public B { };
D d;
void f (B &) { }
template < class T >
void g ()
{
  return f (d);
}
void f (D &);
int main ()
{
  g<int> ();
  return 0;
}
