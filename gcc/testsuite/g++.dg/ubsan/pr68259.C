// PR middle-end/68259

// { dg-do compile }
// { dg-options "-fsanitize=undefined -w" }

namespace std {
  template < typename _Tp > class allocator { };
    template < typename _Tp, typename _Alloc
= std::allocator < _Tp >
>class vector {
  public:
typedef _Tp value_type;
    void push_back (const value_type & __x) { }
  };
}
class Foo;
class FooBar {
public:
Foo * primitive_context;
  FooBar () { }
  FooBar (const FooBar & pnhp);
};
template < class KEY, class CONTENT > class AVLTreeNode { };
template < class KEY, class CONTENT > class FooTree final
{
  FooBar insertPrimitive ();
public:
AVLTreeNode < KEY, CONTENT > *seek_no_lock (const KEY & key) { }
  void primitive_patterns ( std::vector < FooBar > &patterns);
};
template < class KEY, class CONTENT > void FooTree < KEY,
  CONTENT >::primitive_patterns ( std::vector <FooBar > &patterns)
{
    patterns.push_back (insertPrimitive());
}
template < class KEY, class CONTENT >
FooBar FooTree < KEY, CONTENT >::insertPrimitive ()
{
  FooBar place;
  seek_no_lock (place.primitive_context);
  return place;
}
class ManuverResults { };
class opc_info_t
{
public:
FooTree < Foo *, ManuverResults > *primitivecache;
};
static void
do_optical_prox_corr_tsafe (opc_info_t * opc_info)
{
  std::vector < FooBar > patterns;
  opc_info->primitivecache->primitive_patterns (patterns);
}
