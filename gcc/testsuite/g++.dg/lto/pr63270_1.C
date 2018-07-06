typedef unsigned long uintptr_t;
namespace v8
{
  
    int kPointerSize = 0;
  
  class Extension;
  namespace internal
  {
    class Token;
    class Scanner;
    int kCodeOffset = 0;
    int kOptimizedCodeMapOffset = 0;
    int kScopeInfoOffset = 0;

    class FuncNameInferrer;
      template < typename Traits > class ParserBase:Traits
    {
      class FunctionState;
      bool parenthesized_function_;
      typename Traits::Type::Scope * scope_;
      FunctionState *function_state_;
        v8::Extension * extension_;
      FuncNameInferrer *fni_;
      Scanner *scanner_;
      uintptr_t stack_limit_;
      bool stack_overflow_;
      bool allow_lazy_;
      bool allow_natives_syntax_;
      bool allow_generators_;
      bool allow_for_of_;
      typename Traits::Type::Zone * zone_;
    };
    class PreParserScope;
    class PreParser;
    class PreParserTraits
    {
    public:struct Type
      {
	typedef PreParserScope Scope;
	typedef void Zone;
      };
      PreParser *pre_parser_;
    };
    class PreParser:ParserBase < PreParserTraits >
    {
      int ParseMemberWithNewPrefixesExpression ( bool * );
    };
    int PreParser::ParseMemberWithNewPrefixesExpression ( bool * )
    {
      return 0;
    }
  }
}
