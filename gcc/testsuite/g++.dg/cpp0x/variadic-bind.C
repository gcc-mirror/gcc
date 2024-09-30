// { dg-do run { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }
// A basic implementation of TR1's bind using variadic teplates
// Contributed by Douglas Gregor <doug.gregor@gmail.com>
#include <cassert>

// Trivial reference_wrapper
template<typename T>
struct reference_wrapper
{
  reference_wrapper(T& x) : ptr(&x) { }

  operator T&() const { return *ptr; }
  
  T& get() const { return *ptr; }

  T* ptr;
};

template<typename T> reference_wrapper<T> ref(T& x) { return x; }
template<typename T> reference_wrapper<const T> cref(const T& x) { return x; }

// Simple type-traits we'll need
template<typename T>
struct add_reference
{
  typedef T& type;
};

template<typename T>
struct add_reference<T&>
{
  typedef T& type;
};

template<typename T, typename U>
struct is_same
{
  static const bool value = false;
};

template<typename T>
struct is_same<T, T>
{
  static const bool value = true;
};

// For creating the constructor parameters of tuple<>
template<typename T>
struct add_const_reference
{
  typedef const T& type;
};

template<typename T>
struct add_const_reference<T&>
{
  typedef T& type;
};

// 6.1.3 Class template tuple: Needed for bind() implementation
template<typename... Values>
class tuple;

template<> class tuple<> { };

template<typename Head, typename... Tail>
class tuple<Head, Tail...> 
  : private tuple<Tail...>
{
  typedef tuple<Tail...> inherited;

 public:
  tuple() { }

  // implicit copy-constructor is okay

  tuple(typename add_const_reference<Head>::type v, 
        typename add_const_reference<Tail>::type... vtail)
    : m_head(v), inherited(vtail...) { }

  template<typename... VValues>
  tuple(const tuple<VValues...>& other)
    : m_head(other.head()), inherited(other.tail()) { }

  template<typename... VValues>
  tuple& operator=(const tuple<VValues...>& other)
  {
    m_head = other.head();
    tail() = other.tail();
    return *this;
  }

  typename add_reference<Head>::type       head()       { return m_head; }
  typename add_reference<const Head>::type head() const { return m_head; }
  inherited&                               tail()       { return *this; }
  const inherited&                         tail() const { return *this; }

 protected:
  Head m_head;
};

template<typename T>
struct make_tuple_result
{
  typedef T type;
};

template<typename T>
struct make_tuple_result<reference_wrapper<T> >
{
  typedef T& type;
};

// 6.1.3.2 Tuple creation functions
struct ignore_t { 
  template<typename T> ignore_t& operator=(const T&) { return *this; }
} ignore;

template<typename... Values>
tuple<typename make_tuple_result<Values>::type...> 
make_tuple(const Values&... values)
{
  return tuple<typename make_tuple_result<Values>::type...>(values...);
}

template<typename... Values>
tuple<Values&...> tie(Values&... values)
{
  return tuple<Values&...>(values...);
}

// 6.1.3.3 Tuple helper classes
template<typename Tuple>
struct tuple_size;

template<>
struct tuple_size<tuple<> >
{
  static const __SIZE_TYPE__ value = 0;
};

template<typename Head, typename... Tail>
struct tuple_size<tuple<Head, Tail...> >
{
  static const __SIZE_TYPE__ value = 1 + tuple_size<tuple<Tail...> >::value;
};

template<int I, typename Tuple>
struct tuple_element;

template<int I, typename Head, typename... Tail>
struct tuple_element<I, tuple<Head, Tail...> >
{
  typedef typename tuple_element<I-1, tuple<Tail...> >::type type;
};

template<typename Head, typename... Tail>
struct tuple_element<0, tuple<Head, Tail...> >
{
  typedef Head type;
};

// 6.1.3.4 Element access
template<int I, typename Tuple>
class get_impl;

template<int I, typename Head, typename... Values> 
class get_impl<I, tuple<Head, Values...> >
{
  typedef typename tuple_element<I-1, tuple<Values...> >::type Element;
  typedef typename add_reference<Element>::type RJ;
  typedef typename add_const_reference<Element>::type PJ;
  typedef get_impl<I-1, tuple<Values...> > Next;

 public:
  static RJ get(tuple<Head, Values...>& t)       
  { return Next::get(t.tail()); }

  static PJ get(const tuple<Head, Values...>& t) 
  { return Next::get(t.tail()); }
};

template<typename Head, typename... Values> 
class get_impl<0, tuple<Head, Values...> >
{
  typedef typename add_reference<Head>::type RJ;
  typedef typename add_const_reference<Head>::type PJ;

 public:
  static RJ get(tuple<Head, Values...>& t)       { return t.head(); }
  static PJ get(const tuple<Head, Values...>& t) { return t.head(); }
};

template<int I, typename... Values>
typename add_reference<
           typename tuple_element<I, tuple<Values...> >::type
         >::type
get(tuple<Values...>& t)
{
  return get_impl<I, tuple<Values...> >::get(t);
}

template<int I, typename... Values>
typename add_const_reference<
           typename tuple_element<I, tuple<Values...> >::type
         >::type
get(const tuple<Values...>& t)
{
  return get_impl<I, tuple<Values...> >::get(t);
}

// 6.1.3.5 Relational operators
inline bool operator==(const tuple<>&, const tuple<>&) { return true; }

template<typename T, typename... TTail, typename U, typename... UTail>
bool operator==(const tuple<T, TTail...>& t, const tuple<U, UTail...>& u)
{
  return t.head() == u.head() && t.tail() == u.tail();
}

template<typename... TValues, typename... UValues>
bool operator!=(const tuple<TValues...>& t, const tuple<UValues...>& u)
{
  return !(t == u);
}

inline bool operator<(const tuple<>&, const tuple<>&) { return false; }

template<typename T, typename... TTail, typename U, typename... UTail>
bool operator<(const tuple<T, TTail...>& t, const tuple<U, UTail...>& u)
{
  return (t.head() < u.head() || 
          (!(t.head() < u.head()) && t.tail() < u.tail()));
}

template<typename... TValues, typename... UValues>
bool operator>(const tuple<TValues...>& t, const tuple<UValues...>& u)
{
  return u < t;
}

template<typename... TValues, typename... UValues>
bool operator<=(const tuple<TValues...>& t, const tuple<UValues...>& u)
{
  return !(u < t);
}

template<typename... TValues, typename... UValues>
bool operator>=(const tuple<TValues...>& t, const tuple<UValues...>& u)
{
  return !(t < u);
}

// enable_if, the breakfast of champions
template<bool Cond, typename Type = void>
struct enable_if {
  typedef Type type;
};

template<typename Type>
struct enable_if<false, Type> { };

// 3.6 Function object binders

// 3.6.1 Class template is_bind_expression
template<typename T> 
struct is_bind_expression {
  static const bool value = false;
};

// 3.6.2 Class template is_placeholder
template<typename T>
struct is_placeholder {
  static const int value = 0;
};

// 3.6.3 Function template bind
template<int I> struct placeholder {} ;

template<int N> struct int_c { };

// A tuple of integer values
template<int...> struct int_tuple {};

// make_indexes_impl is a helper for make_indexes
template<int I, typename IntTuple, typename... Types>
struct make_indexes_impl;


template<int I, int... Indexes, typename T, typename... Types>
struct make_indexes_impl<I, int_tuple<Indexes...>, T, Types...>
{
  typedef typename make_indexes_impl<I+1,
                                     int_tuple<Indexes..., I>,
                                     Types...>::type type;
};

template<int I, int... Indexes>
struct make_indexes_impl<I, int_tuple<Indexes...> > {
  typedef int_tuple<Indexes...> type;
};

// make_indexes takes a variable-length number of N types and
// generates an int_tuple that contains <0, 1, 2, ..., N-1>. These can
// be used as indexes for tuple's get or tuple_element operation.
template<typename... Types> 
struct make_indexes : make_indexes_impl<0, int_tuple<>, Types...> { };

// Get the Ith tuple element, but only if I is in bounds.
template<int I, typename Tuple, typename = void> 
struct safe_tuple_element{ };

template<int I, typename... Values>
struct safe_tuple_element<I, tuple<Values...>, 
         typename enable_if<(I >= 0 && 
                             I < tuple_size<tuple<Values...> >::value)
                            >::type>
{
  typedef typename tuple_element<I, tuple<Values...> >::type type;
};

// mu maps a bound argument to an actual argument, given a tuple of
// the arguments passed to the function object returned by bind().

// Return the stored reference from reference_wrapper
template<typename T, typename... Args>
inline T& mu(reference_wrapper<T>& bound_arg, const tuple<Args&...>&)
{
  return bound_arg.get();
}

// Unwrap a tuple into separate arguments and forward to the function
// object f.
template<typename F, int... Indexes, typename... Args>
inline typename F::result_type
unwrap_and_forward(F& f, int_tuple<Indexes...>, const tuple<Args&...>& args)
{
  return f(get<Indexes>(args)...);
}

// Evaluate the inner bind expression
template<typename Bound, typename... Args>
inline typename enable_if<is_bind_expression<Bound>::value,
                          typename Bound::result_type>::type
mu(Bound& bound_arg, const tuple<Args&...>& args)
{
  typedef typename make_indexes<Args...>::type Indexes;
  return unwrap_and_forward(bound_arg, Indexes(), args);
}

// Retrieve the Ith argument from args
template<typename Bound, typename... Args>
inline typename safe_tuple_element<is_placeholder<Bound>::value - 1,
                                   tuple<Args...> >::type
mu(Bound& bound_arg, const tuple<Args&...>& args)
{
  return get<is_placeholder<Bound>::value-1>(args);
}

// Return the stored value.
template<typename T>
struct is_reference_wrapper {
  static const bool value = false;
};

template<typename T>
struct is_reference_wrapper<reference_wrapper<T> > {
  static const bool value = true;
};

template<typename Bound, typename... Args>
inline typename enable_if<(!is_bind_expression<Bound>::value
                           && !is_placeholder<Bound>::value
                           && !is_reference_wrapper<Bound>::value),
                          Bound&>::type
mu(Bound& bound_arg, const tuple<Args&...>&) 
{
  return bound_arg;
}

// 
template<typename F, typename... BoundArgs, int... Indexes, typename... Args>
typename F::result_type
apply_functor(F& f, tuple<BoundArgs...>& bound_args, int_tuple<Indexes...>,
              const tuple<Args&...>& args)
{
  return f(mu(get<Indexes>(bound_args), args)...);
}

template<typename F, typename... BoundArgs>
class bound_functor
{
  typedef typename make_indexes<BoundArgs...>::type indexes;

 public:
  typedef typename F::result_type result_type;

  explicit bound_functor(const F& f, const BoundArgs&... bound_args)
    : f(f), bound_args(bound_args...) { }

  template<typename... Args>
  typename F::result_type operator()(Args&... args) {
    return apply_functor(f, bound_args, indexes(), tie(args...));
  }

 private:
  F f;
  tuple<BoundArgs...> bound_args;
};

template<typename F, typename... BoundArgs>
struct is_bind_expression<bound_functor<F, BoundArgs...> > {
  static const bool value = true;
};

template<typename F, typename... BoundArgs>
inline bound_functor<F, BoundArgs...>
bind(const F& f, const BoundArgs&... bound_args) 
{
  return bound_functor<F, BoundArgs...>(f, bound_args...);
}


// 3.6.4 Placeholders
template<int I> 
struct is_placeholder<placeholder<I> > {
  static const int value = I;
};

placeholder<1> _1;
placeholder<2> _2;
placeholder<3> _3;
placeholder<4> _4;
placeholder<5> _5;
placeholder<6> _6;
placeholder<7> _7;
placeholder<8> _8;
placeholder<9> _9;

// Test code
template<typename T>
struct plus {
  typedef T result_type;
  
  T operator()(T x, T y) { return x + y; }
};

template<typename T>
struct multiplies {
  typedef T result_type;
  
  T operator()(T x, T y) { return x * y; }
};

template<typename T>
struct negate {
  typedef T result_type;
  
  T operator()(T x) { return -x; }
};

int main()
{
  int seventeen = 17;
  int forty_two = 42;

  assert(bind(plus<int>(), _1, _2)(seventeen, forty_two) == 59);
  assert(bind(plus<int>(), _1, _1)(seventeen, forty_two) == 34);
  assert(bind(plus<int>(), _2, _1)(seventeen, forty_two) == 59);
  assert(bind(plus<int>(), 5, _1)(seventeen, forty_two) == 22);
  assert(bind(plus<int>(), ref(seventeen), _2)(seventeen, forty_two) == 59);
  assert(bind(plus<int>(), bind(multiplies<int>(), 3, _1), _2)(seventeen, forty_two) 
         == 93);
  return 0;
}
