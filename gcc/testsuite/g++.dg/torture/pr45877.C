// { dg-do compile }

namespace std __attribute__ ((__visibility__ ("default")))
{
  typedef __SIZE_TYPE__ size_t;
  template<typename _Alloc>     class allocator;
  template<class _CharT>     struct char_traits;
  template<typename _CharT, typename _Traits = char_traits<_CharT>,
      typename _Alloc = allocator<_CharT> >
	  class basic_string;
  typedef basic_string<char> string;
  template<class _T1, class _T2>     struct pair     { };
  template<typename _Tp>     class allocator    { };
  template<typename _Arg1, typename _Arg2, typename _Result>
      struct binary_function     {
	  typedef _Arg1 first_argument_type;
	  typedef _Arg2 second_argument_type;
	  typedef _Result result_type;
      };
  template<typename _CharT, typename _Traits, typename _Alloc>
  class basic_string {
  public:
      basic_string(const _CharT* __s, const _Alloc& __a = _Alloc());
  };
  class type_info   {
  public:
      const char* name() const;
  };
  extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__gnu_inline__, __artificial__))
  void * memcpy (void *__restrict __dest, __const void *__restrict __src, size_t __len) throw ()
  {
      return __builtin___memcpy_chk (__dest, __src, __len, __builtin_object_size (__dest, 0));
  }
  template <typename _Key, typename _Tp >
  class map {
      typedef _Key key_type;
      typedef _Tp mapped_type;
  public:
      mapped_type&       operator[](const key_type& __k);
  };
}
class CodeAlloc { };
using namespace std;
typedef void *Stack;
class basicForEachType;
typedef const basicForEachType * aType;
extern map<const string,basicForEachType *> map_type;
class AnyTypeWithOutCheck { };
typedef AnyTypeWithOutCheck AnyType;
template<typename T> AnyTypeWithOutCheck inline SetAny(const T & x)
{
  AnyTypeWithOutCheck any;
  memcpy(&any,&x,sizeof(x));
}
template<typename T> const T& GetAny(const AnyTypeWithOutCheck & x);
class E_F0;
class C_F0;
class Polymorphic;
typedef E_F0 * Expression;
class basicAC_F0;
extern Polymorphic * TheOperators, * TheRightOperators;
class basicForEachType : public CodeAlloc {
public:
    virtual C_F0 CastTo(const C_F0 & e) const ;
};
class E_F0 :public CodeAlloc    {
public:
    virtual AnyType operator()(Stack) const =0;
};
class E_F0mps : public E_F0 {
};
class ArrayOfaType : public CodeAlloc{
protected:
    aType * t;
};
class OneOperator : public ArrayOfaType {
public:
    OneOperator(aType rr,aType a,aType b);
    virtual E_F0 * code(const basicAC_F0 &) const =0;
};
class Polymorphic: public E_F0mps {
public:
    void Add(const char * op,OneOperator * p0 ,OneOperator * p1=0) const;
};
class C_F0 {
public:
    operator E_F0 * () const;
};
class basicAC_F0 {
public:
    const C_F0 & operator [] (int i) const;
};
struct OneBinaryOperatorMI { };
struct evalE_F2 { };
template<typename C,class MI=OneBinaryOperatorMI,class MIx=evalE_F2 >
class OneBinaryOperator : public OneOperator
{
  typedef typename C::result_type R;
  typedef typename C::first_argument_type A;
  typedef typename C::second_argument_type B;
  aType t0,t1;
  class Op : public E_F0 {
      Expression a,b;
  public:
      AnyType operator()(Stack s) const {
	  return SetAny<R>(static_cast<R>(C::f( GetAny<A>((*a)(s)),
						GetAny<B>((*b)(s)))));
      }
      Op(Expression aa,Expression bb) : a(aa),b(bb) { }
  };
public:
  E_F0 * code(const basicAC_F0 & args) const   {
      return new Op(t0->CastTo(args[0]),t1->CastTo(args[1]));
  }
  OneBinaryOperator()
      : OneOperator(map_type[typeid(R).name()],
		    map_type[typeid(A).name()],
		    map_type[typeid(B).name()]), t0(t[0]), t1(t[1]) { }
};
struct NothingType { };
class ShapeOfArray{ };
template<class R> class KN_: public ShapeOfArray { };
template <class T> struct affectation: binary_function<T, T, T> { };
template<class K,class L,class OP> struct set_A_BI
: public binary_function<KN_<K>,pair<KN_<K>, KN_<L> > *,KN_<K> >
{
  static KN_<K> f(const KN_<K> & a, pair<KN_<K>, KN_<L> > * const & b);
};
template<class K,class L,class OP> struct set_AI_B
: public binary_function<pair<KN_<K>, KN_<L> > * ,KN_<K>, NothingType >
{
  static NothingType f( pair<KN_<K>, KN_<L> > * const & b,const KN_<K> & a);
};
template<class K,class Z> void ArrayOperator()
{
  TheOperators->Add("=", new OneBinaryOperator<set_A_BI< K,Z,affectation<K> > >,
		    new OneBinaryOperator<set_AI_B< K,Z,affectation<K> > >);
}
void initArrayOperatorlong() {
    ArrayOperator<long,long>();
}
