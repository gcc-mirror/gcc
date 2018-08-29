// { dg-do compile }
// { dg-options "-O2" }

namespace std {
  struct type_info
  {
    virtual bool __do_catch(const type_info *__thr_type, void **__thr_obj,
       unsigned __outer) const;
  };
}

template< typename VALUE_T, typename TYPE >
struct List_policy
{
  typedef VALUE_T *Value_type;
  typedef TYPE **Type;
  typedef TYPE *Head_type;
  typedef TYPE Item_type;
};

template< typename POLICY >
class List
{
public:
  typedef typename POLICY::Value_type Value_type;
  class Iterator
  {
    typedef typename POLICY::Type Internal_type;
  public:
    typedef typename POLICY::Value_type value_type;
    typedef typename POLICY::Value_type Value_type;
    Value_type operator -> () const { return static_cast<Value_type>(*_c); }
    Internal_type _c;
  };
  Iterator begin() { return Iterator(); }
  Iterator end() { return Iterator(); }
  typename POLICY::Head_type _f;
};

template<typename ELEM_TYPE> class H_list_item_t { };

template< typename T, typename POLICY >
class H_list : public List<POLICY>
{
public:
  typedef typename POLICY::Item_type Item;
  typedef List<POLICY> Base;
  typedef typename Base::Iterator Iterator;
  Iterator insert(T *e, Iterator const &pred)
  {
    Item **x = &this->_f;
    *x = static_cast<Item*>(e);
    return Iterator();
  }
};

template< typename T >
struct H_list_t : H_list<T, List_policy< T, H_list_item_t<T> > >
{
  H_list_t(bool b) : H_list<T, List_policy< T, H_list_item_t<T> > >(b) {}
};

template< typename BASE, typename MATCH_RESULT >
struct Type_matcher : H_list_item_t<BASE>
{
  explicit Type_matcher(std::type_info const *type);
  typedef MATCH_RESULT Match_result;

private:
  std::type_info *_type;
  typedef H_list_t<BASE> List;
  typedef typename List::Iterator Iterator;
  static List _for_type;
};

template< typename BASE, typename MR >
Type_matcher<BASE, MR>::Type_matcher(std::type_info const *t)
{
  Iterator c = _for_type.begin();
  t->__do_catch(c->_type, 0, 0);
  _for_type.insert(static_cast<BASE*>(this), _for_type.begin());
}

template< typename VI, typename HW >
class Fa : public Type_matcher<Fa<VI, HW>, VI*>
{
public:
  typedef Fa<VI, HW> Self;
  virtual VI *do_match(HW *f) = 0;
  explicit Fa(std::type_info const *type) : Type_matcher<Self, VI*>(type) {}
};

class Res {};
typedef Fa<Res, Res> R_fac;

template< typename VI, typename HW_BASE, typename HW, typename BASE >
class Fa_t : public BASE
{
public:
  Fa_t() : BASE(&typeid(HW)) {}
  VI *do_match(HW_BASE *) { return 0; }
};

template< typename VI, typename HW >
class Resource_factory_t : public Fa_t<VI, Res, HW, R_fac > {};

class Foo {};
class Foo2;
class Foo3 : public Res {};
Resource_factory_t<Foo3, Foo> _x;
