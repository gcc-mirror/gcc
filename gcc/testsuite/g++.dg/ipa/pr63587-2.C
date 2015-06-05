// PR ipa/63587
// { dg-do compile { target c++11 } }
// { dg-options "-O2" }

namespace boost {
class basic_cstring
{
public:
  basic_cstring (char *);
};
template <typename> struct identity
{
};
struct make_identity;
struct function_buffer
{
};
template <typename FunctionObj> struct function_obj_invoker0
{
  static int
  invoke (function_buffer &)
  {
    FunctionObj f;
    f ();
  }
};
template <typename FunctionObj> struct get_function_obj_invoker0
{
  typedef function_obj_invoker0<FunctionObj> type;
};
template <typename FunctionObj> struct apply
{
  typedef typename get_function_obj_invoker0<FunctionObj>::type invoker_type;
};
struct basic_vtable0
{
  typedef int (*invoker_type)(function_buffer &);
  template <typename F> void assign_to (F, function_buffer);
  invoker_type invoker;
};
class function0
{
public:
  template <typename Functor> function0 (Functor)
  {
    typedef typename apply<Functor>::invoker_type invoker_type;
    basic_vtable0 stored_vtable { invoker_type::invoke };
    stored_vtable.assign_to (0, functor);
  }
  function_buffer functor;
};
class function : function0
{
public:
  template <typename Functor> function (Functor f) : function0 (f) {}
};
class test_unit_generator
{
};
class test_case
{
public:
  test_case (basic_cstring, basic_cstring, int, function);
};
struct auto_test_unit_registrar
{
  auto_test_unit_registrar (test_unit_generator);
};
template <typename F> F unwrap (F, int);
struct for_each_impl
{
  template <typename Iterator, typename LastIterator, typename TransformFunc,
	    typename F>
  static void
  execute (Iterator, LastIterator, TransformFunc, F f)
  {
    identity<char> __trans_tmp_1;
    unwrap (f, 0)(__trans_tmp_1);
  }
};
template <typename, typename, typename F>
void
for_each (F f)
{
  for_each_impl::execute (0, 0, 0, f);
}
template <typename TestCaseTemplate> class test_case_template_invoker
{
public:
  void operator()()
  {
    TestCaseTemplate::run (0);
  }
};
template <typename Generator, typename TestCaseTemplate>
struct generate_test_case_4_type
{
  generate_test_case_4_type (basic_cstring, basic_cstring, int, Generator G)
    : m_test_case_name (0), m_test_case_file (0), m_holder (G)
  {
  }
  template <typename TestType> void operator()(identity<TestType>)
  {
    test_case (0, 0, 0, test_case_template_invoker<TestCaseTemplate> ());
  }
  basic_cstring m_test_case_name;
  basic_cstring m_test_case_file;
  Generator m_holder;
};
template <typename TestCaseTemplate>
class template_test_case_gen : public test_unit_generator
{
public:
  template_test_case_gen (basic_cstring, basic_cstring, int)
  {
    for_each<int, make_identity> (
      generate_test_case_4_type<template_test_case_gen, TestCaseTemplate> (
	0, 0, 0, *this));
  }
};
class attribute_name
{
  int m_id;

public:
  attribute_name (char);
};
template <typename> struct term;
namespace exprns_ {
template <typename> struct expr;
}
using exprns_::expr;
template <typename T> struct Trans_NS_proto_terminal
{
  typedef expr<term<T> > type;
};
namespace exprns_ {
template <typename Arg0> struct expr<term<Arg0> >
{
  Arg0 child0;
};
}
template <typename Expr> struct actor
{
  typename Trans_NS_proto_terminal<Expr>::type proto_expr_;
};
template <template <typename> class Actor = actor> struct terminal
{
  typedef Actor<int> type;
};
namespace log {
struct to_log_fun
{
};
class value_extractor;
template <typename, typename = value_extractor, typename = void,
	  template <typename> class = actor>
class attribute_actor;
class attribute_terminal
{
public:
  attribute_name m_name;
  attribute_name
  get_name ()
  {
    return m_name;
  }
};
template <typename, typename, typename, template <typename> class ActorT>
class attribute_actor : ActorT<attribute_terminal>
{
public:
  typedef int value_type;
  attribute_name
  get_name ()
  {
    return this->proto_expr_.child0.get_name ();
  }
};
template <typename AttributeValueT>
attribute_actor<AttributeValueT> attr (attribute_name); // { dg-warning "used but never defined" }
terminal<>::type stream;
template <typename LeftT, typename ImplT> class attribute_output_terminal
{
public:
  template <typename U>
  attribute_output_terminal (LeftT, attribute_name, ImplT, U);
};
template <typename LeftT> struct make_output_expression
{
  typedef attribute_output_terminal<LeftT, to_log_fun> type;
  template <typename RightT>
  static type
  make (LeftT left, RightT &right)
  {
    type (left, right.get_name (), to_log_fun (), 0);
  }
};
template <typename, typename RightT, typename = typename RightT::value_type>
struct make_output_actor;
template <template <typename> class ActorT, typename LeftExprT,
	  typename RightT, typename ValueT>
struct make_output_actor<ActorT<LeftExprT>, RightT, ValueT>
{
  typedef make_output_expression<ActorT<LeftExprT> > make_expression;
  typedef ActorT<typename make_expression::type> type;
  static type
  make (ActorT<LeftExprT> left, RightT &right)
  {
    type { make_expression::make (left, right) };
  }
};
template <typename LeftExprT, typename T, typename FallbackPolicyT,
	  typename TagT>
typename make_output_actor<actor<LeftExprT>, attribute_actor<TagT> >::type
operator<<(actor<LeftExprT> left,
	   attribute_actor<T, FallbackPolicyT, TagT> right)
{
  make_output_actor<actor<LeftExprT>, attribute_actor<T> >::make (left, right);
}
}
}
namespace logging = boost::log;
namespace expr = logging;
namespace {
class my_class;
}
template <typename> struct default_formatting
{
  void test_method ();
};
struct default_formatting_invoker
{
  static void
  run (void *)
  {
    default_formatting<int> t;
    t.test_method ();
  }
};
boost::auto_test_unit_registrar default_formatting_registrar56 (
  boost::template_test_case_gen<default_formatting_invoker> (0, 0, 0));
template <typename CharT>
void
default_formatting<CharT>::test_method ()
{
  expr::stream << expr::attr<my_class> (0);
  expr::stream << expr::attr<int> (0) << expr::attr<int> (0)
	       << expr::attr<int> (0);
}
