typedef unsigned long uintptr_t;
namespace v8 {
class Extension;
namespace internal {
class A {
public:
  A(int) {};
};
class B {
public:
  B(int) {};
};
class Scanner;
class FuncNameInferrer;
template <typename Traits> class ParserBase : Traits {
  class FunctionState;
  bool parenthesized_function_;
  typename Traits::Type::Scope *scope_;
  FunctionState *function_state_;
  v8::Extension *extension_;
  FuncNameInferrer *fni_;
  Scanner *scanner_;
  uintptr_t stack_limit_;
  bool stack_overflow_;
  bool allow_lazy_;
  bool allow_natives_syntax_;
  bool allow_generators_;
  bool allow_for_of_;
  typename Traits::Type::Zone *zone_;
};
class PreParserScope;
class F;
class PreParserTraits {
public:
  struct Type {
    typedef PreParserScope Scope;
    typedef void Zone;
  };

private:
  F *pre_parser_;
};
class F : ParserBase<PreParserTraits> {};
class C {
public:
  struct Type {
    typedef v8::internal::FuncNameInferrer Scope;
    typedef int Zone;
  };
};
class G : ParserBase<C> {
public:
  static int m_fn1();
  static int test();
  F reusable_preparser_;
};
class D {
public:
  D(int a) : function_(0), context_(0), nested_scope_chain_(0) { G::test(); }
  B function_;
  B context_;
  A nested_scope_chain_;
};
}
}



