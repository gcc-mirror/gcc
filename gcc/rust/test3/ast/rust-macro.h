#ifndef RUST_AST_MACRO_H
#define RUST_AST_MACRO_H

#include "rust-ast.h"

namespace Rust {
    namespace AST {
        // Decls as definitions moved to rust-ast.h
        class MacroItem;
        class MacroInvocationSemi;

        enum MacroFragSpec {
            BLOCK,
            EXPR,
            IDENT,
            ITEM,
            LIFETIME,
            LITERAL,
            META,
            PAT,
            PATH,
            STMT,
            TT,
            TY,
            VIS,
            INVALID // not really a specifier, but used to mark invalid one passed in
        };

        inline MacroFragSpec get_frag_spec_from_str(::std::string str) {
            if (str == "block") 
              return BLOCK;
            else if (str == "expr")
              return EXPR;
            else if (str == "ident")
              return IDENT;
            else if (str == "item")
              return ITEM;
            else if (str == "lifetime")
              return LIFETIME;
            else if (str == "literal")
              return LITERAL;
            else if (str == "meta")
              return META;
            else if (str == "pat")
              return PAT;
            else if (str == "path")
              return PATH;
            else if (str == "stmt")
              return STMT;
            else if (str == "tt")
              return TT;
            else if (str == "ty")
              return TY;
            else if (str == "vis")
              return VIS;
            else {
              //error_at("invalid string '%s' used as fragment specifier", str->c_str());
              return INVALID;
            }
        }

        // A macro match that has an identifier and fragment spec
        class MacroMatchFragment : public MacroMatch {
            Identifier ident;
            MacroFragSpec frag_spec;

          public:
            MacroMatchFragment(Identifier ident, MacroFragSpec frag_spec) :
              ident(::std::move(ident)), frag_spec(frag_spec) {}

            // Returns whether macro match fragment is in an error state.
            inline bool is_error() const {
                return frag_spec == INVALID;
            }

            // Creates an error state macro match fragment.
            static MacroMatchFragment create_error() {
                return MacroMatchFragment(::std::string(""), INVALID);
            }

            ::std::string as_string() const;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual MacroMatchFragment* clone_macro_match_impl() const OVERRIDE {
                return new MacroMatchFragment(*this);
            }
        };

        // A repetition macro match
        class MacroMatchRepetition : public MacroMatch {
          public:
            enum MacroRepOp { NONE, ASTERISK, PLUS, QUESTION_MARK };

          private:
            //::std::vector<MacroMatch> matches;
            ::std::vector< ::std::unique_ptr<MacroMatch> > matches;
            MacroRepOp op;

            // bool has_sep;
            typedef Token MacroRepSep;
            // any token except delimiters and repetition operators
            ::std::unique_ptr<MacroRepSep> sep;

          public:
            // Returns whether macro match repetition has separator token.
            inline bool has_sep() const {
                return sep != NULL;
            }

            MacroMatchRepetition(::std::vector< ::std::unique_ptr<MacroMatch> > matches, MacroRepOp op,
              MacroRepSep* sep) :
              matches(::std::move(matches)),
              op(op), sep(sep) {}

            // Copy constructor with clone
            MacroMatchRepetition(MacroMatchRepetition const& other) :
              /*matches(other.matches),*/ op(other.op), sep(other.sep->clone_token()) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                matches.reserve(other.matches.size());

                for (const auto& e : other.matches) {
                    matches.push_back(e->clone_macro_match());
                }
            }

            // Destructor - define here if required

            // Overloaded assignment operator to clone
            MacroMatchRepetition& operator=(MacroMatchRepetition const& other) {
                // matches = other.matches; // TODO: this needs to clone somehow?
                op = other.op;
                sep = other.sep->clone_token();

                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                matches.reserve(other.matches.size());

                for (const auto& e : other.matches) {
                    matches.push_back(e->clone_macro_match());
                }

                return *this;
            }

            // move constructors
            MacroMatchRepetition(MacroMatchRepetition&& other) = default;
            MacroMatchRepetition& operator=(MacroMatchRepetition&& other) = default;

            ::std::string as_string() const;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual MacroMatchRepetition* clone_macro_match_impl() const OVERRIDE {
                return new MacroMatchRepetition(*this);
            }
        };

        // can't inline due to polymorphism
        class MacroMatcher : public MacroMatch {
            DelimType delim_type;
            //::std::vector<MacroMatch> matches;
            ::std::vector< ::std::unique_ptr<MacroMatch> > matches;

            // TODO: think of way to mark invalid that doesn't take up more space
            bool is_invalid;

          public:
            MacroMatcher(
              DelimType delim_type, ::std::vector< ::std::unique_ptr<MacroMatch> > matches) :
              delim_type(delim_type),
              matches(::std::move(matches)), is_invalid(false) {}

            // copy constructor with vector clone
            MacroMatcher(MacroMatcher const& other) : delim_type(other.delim_type) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                matches.reserve(other.matches.size());

                for (const auto& e : other.matches) {
                    matches.push_back(e->clone_macro_match());
                }
            }

            // overloaded assignment operator with vector clone
            MacroMatcher& operator=(MacroMatcher const& other) {
                delim_type = other.delim_type;

                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                matches.reserve(other.matches.size());

                for (const auto& e : other.matches) {
                    matches.push_back(e->clone_macro_match());
                }

                return *this;
            }

            // move constructors
            MacroMatcher(MacroMatcher&& other) = default;
            MacroMatcher& operator=(MacroMatcher&& other) = default;

            // Creates an error state macro matcher.
            static MacroMatcher create_error() {
                return MacroMatcher(true);
            }

            // Returns whether MacroMatcher is in an error state.
            inline bool is_error() const {
                return is_invalid;
            }

            ::std::string as_string() const;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual MacroMatcher* clone_macro_match_impl() const OVERRIDE {
                return new MacroMatcher(*this);
            }

            // constructor only used to create error matcher
            MacroMatcher(bool is_invalid) : delim_type(PARENS), is_invalid(is_invalid) {}
        };

        // TODO: inline?
        struct MacroTranscriber {
          private:
            DelimTokenTree token_tree;

          public:
            MacroTranscriber(DelimTokenTree token_tree) : token_tree(::std::move(token_tree)) {}
        };

        // A macro rule? Matcher and transcriber pair?
        struct MacroRule {
          private:
            MacroMatcher matcher;
            MacroTranscriber transcriber;

          public:
            MacroRule(MacroMatcher matcher, MacroTranscriber transcriber) :
              matcher(::std::move(matcher)), transcriber(::std::move(transcriber)) {}

            // Returns whether macro rule is in error state.
            inline bool is_error() const {
                return matcher.is_error();
            }

            // Creates an error state macro rule.
            static MacroRule create_error() {
                return MacroRule(
                  MacroMatcher::create_error(), MacroTranscriber(DelimTokenTree::create_empty()));
            }
        };

        // A macro rules definition item AST node
        class MacroRulesDefinition : public MacroItem {
            Identifier rule_name;
            // MacroRulesDef rules_def; // TODO: inline
            // only curly without required semicolon at end
            DelimType delim_type;
            // MacroRules rules;
            ::std::vector<MacroRule> rules; // inlined form

          public:
            ::std::string as_string() const;

            MacroRulesDefinition(Identifier rule_name, DelimType delim_type,
              ::std::vector<MacroRule> rules, ::std::vector<Attribute> outer_attrs) :
              MacroItem(::std::move(outer_attrs)), rule_name(::std::move(rule_name)),
              delim_type(delim_type), rules(::std::move(rules)) {}

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual MacroRulesDefinition* clone_item_impl() const OVERRIDE {
                return new MacroRulesDefinition(*this);
            }
        };

        // AST node of a macro invocation, which is replaced by the macro result at compile time
        class MacroInvocation
          : public TypeNoBounds
          , public Pattern
          , public ExprWithoutBlock {
            SimplePath path;
            DelimTokenTree token_tree;

          public:
            ::std::string as_string() const;

            MacroInvocation(
              SimplePath path, DelimTokenTree token_tree, ::std::vector<Attribute> outer_attrs) :
              ExprWithoutBlock(::std::move(outer_attrs)), path(::std::move(path)),
              token_tree(::std::move(token_tree)) {}

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual MacroInvocation* clone_pattern_impl() const OVERRIDE {
                return new MacroInvocation(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual MacroInvocation* clone_expr_impl() const OVERRIDE {
                return new MacroInvocation(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual MacroInvocation* clone_expr_without_block_impl() const OVERRIDE {
                return new MacroInvocation(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual MacroInvocation* clone_type_impl() const OVERRIDE {
                return new MacroInvocation(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual MacroInvocation* clone_type_no_bounds_impl() const OVERRIDE {
                return new MacroInvocation(*this);
            }
        };
    }
}

#endif