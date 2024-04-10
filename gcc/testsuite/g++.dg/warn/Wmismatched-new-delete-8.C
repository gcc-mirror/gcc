/* PR tree-optimization/109804 */
/* { dg-do compile { target c++11 } } */
/* { dg-options "-Wall" } */

/* Here we used to ICE in new_delete_mismatch_p because
   we didn't handle unnamed types from the demangler (DEMANGLE_COMPONENT_UNNAMED_TYPE). */

template <typename T, typename ARGS>
static inline T * construct_at(void *at, ARGS && args)
{
 struct Placeable : T
 {
  Placeable(ARGS && args) : T(args) { }
  void * operator new (__SIZE_TYPE__, void *ptr) { return ptr; }
  void operator delete (void *, void *) { }
 };
 return new (at) Placeable(static_cast<ARGS &&>(args));
}
template <typename MT>
struct Reconstructible
{
  char _space[sizeof(MT)];
  Reconstructible() { }
};
template <typename MT>
struct Constructible : Reconstructible<MT>
{
 Constructible(){}
};
struct A { };
struct B
{
 Constructible<A> a { };
 B(int) { }
};
Constructible<B> b { };
void f()
{
  enum { ENUM_A = 1 };
  enum { ENUM_B = 1 };
  construct_at<B>(b._space, ENUM_B);
}
