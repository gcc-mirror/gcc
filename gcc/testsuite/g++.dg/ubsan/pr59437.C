// { dg-do compile }
// { dg-options "-fsanitize=null -fvtable-verify=std" }
// { dg-skip-if "" { *-*-* } { "-flto" } { "" } }

template < typename T > struct A
{
  T foo ();
};
template < typename T > struct C: virtual public A < T >
{
  C & operator<< (C & (C &));
};
template < typename T >
C < T > &endl (C < int > &c)
{
  c.foo ();
  return c;
}
C < int > cout;
void
fn ()
{
  cout << endl;
}
