// PR c++/50800
// { dg-do compile { target c++11 } }

template <typename> struct A;
template <typename _Up> struct A<_Up *> { typedef _Up type; };
template <typename T> struct B { typedef typename A<T>::type type; };
template <typename> struct C;
template <typename From> struct C<const From> {
  typedef typename B<From>::type SimpleType;
};
template <class> struct D { typedef int ret_type; };
template <class From> struct F {
  typedef typename D<typename C<From>::SimpleType>::ret_type ret_type;
};
template <class, class Y> typename F<Y>::ret_type cast(Y &);
class CompoundStmt;
class alignas(8) Stmt {
  Stmt *Children[1];
  CompoundStmt *getBlock() const { cast<CompoundStmt>(Children[0]); return 0; }
};
