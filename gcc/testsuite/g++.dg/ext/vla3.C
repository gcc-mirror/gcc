// PR c++/28903
// { dg-options "" }
// { dg-require-effective-target alloca }

template <class>
struct View 
{
  int n;
};
template <class ViewA>
struct ViewDom : View<ViewA>
{
  using View<ViewA>::n;
  ViewDom();
};
template <class ViewA>
ViewDom<ViewA>::ViewDom()
{
  char a[n];
}
void element( )
{
  ViewDom<int> a;
}

