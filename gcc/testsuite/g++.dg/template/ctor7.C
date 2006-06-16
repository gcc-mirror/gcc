// PR c++/27640

template < class T > struct refcounted : 
virtual T
{
  template < class A1 > refcounted (const A1 & a1) : T () { }
};
struct nfsserv {};
template < class T >
void
sfsserver_cache_alloc (int *ns)
{
  new refcounted < nfsserv > (*ns);
}
void
usage ()
{
  sfsserver_cache_alloc < int > ( 0);
}
