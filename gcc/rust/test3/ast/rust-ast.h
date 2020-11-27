#ifndef RUST_AST_BASE_H
#define RUST_AST_BASE_H
// Base for AST used in gccrs, basically required by all specific ast things

// GCC imports
#include "config.h"
//#define INCLUDE_UNIQUE_PTR
// should allow including the gcc emulation of std::unique_ptr
#include "system.h"
#include "coretypes.h" // order: config, INCLUDE, system, coretypes

// STL imports
#include <string>
#include <vector>
// with C++11, now can use actual std::unique_ptr
#include <memory>

// gccrs imports
// required for AST::Token
#include "rust-token.h"

namespace Rust {
    // TODO: remove typedefs and make actual types for these
    // typedef int location_t;
    // typedef ::std::string SimplePath;
    typedef ::std::string Identifier;
    typedef int TupleIndex;

    namespace AST {
        // Delimiter types - used in macros and whatever.
        enum DelimType { PARENS, SQUARE, CURLY };

        // Base AST node object - TODO is this really required or useful? Where to draw line?
        class Node {
          public:
            // Gets node's location_t.
            location_t get_locus() const {
                return loc;
            }

            // Sets node's location_t.
            void set_locus(location_t loc_) {
                loc = loc_;
            }

            // Get node output as a string. Pure virtual.
            virtual ::std::string as_string() const = 0;

            virtual ~Node() {}

          private:
            // The node's location.
            location_t loc;
        };

        // Attribute body - abstract base class
        class AttrInput {
          public:
            virtual ~AttrInput() {}

            // Unique pointer custom clone function
            ::std::unique_ptr<AttrInput> clone_attr_input() const {
                return ::std::unique_ptr<AttrInput>(clone_attr_input_impl());
            }

            virtual ::std::string as_string() const = 0;

          protected:
            // pure virtual clone implementation
            virtual AttrInput* clone_attr_input_impl() const = 0;
        };

        // A tree of tokens (or a single token) - abstract base class
        class TokenTree {
          public:
            virtual ~TokenTree() {}

            // Unique pointer custom clone function
            ::std::unique_ptr<TokenTree> clone_token_tree() const {
                return ::std::unique_ptr<TokenTree>(clone_token_tree_impl());
            }

            virtual ::std::string as_string() const = 0;

          protected:
            // pure virtual clone implementation
            virtual TokenTree* clone_token_tree_impl() const = 0;
        };

        // Abstract base class for a macro match
        class MacroMatch {
          public:
            virtual ~MacroMatch() {}

            virtual ::std::string as_string() const = 0;

            // Unique pointer custom clone function
            ::std::unique_ptr<MacroMatch> clone_macro_match() const {
                return ::std::unique_ptr<MacroMatch>(clone_macro_match_impl());
            }

          protected:
            // pure virtual clone implementation
            virtual MacroMatch* clone_macro_match_impl() const = 0;
        };

        // A token is a kind of token tree (except delimiter tokens)
        class Token
          : public TokenTree
          , public MacroMatch {
            // A token is a kind of token tree (except delimiter tokens)
            // A token is a kind of MacroMatch (except $ and delimiter tokens)
            // TODO: improve member variables - current ones are the same as lexer token
            // Token kind.
            TokenId token_id;
            // Token location.
            location_t locus;
            // Associated text (if any) of token.
            std::string str;
            // Token type hint (if any).
            PrimitiveCoreType type_hint;

          public:
            // Unique pointer custom clone function
            ::std::unique_ptr<Token> clone_token() const {
                return ::std::unique_ptr<Token>(clone_token_impl());
            }

            // Constructor from lexer const_TokenPtr
            /* TODO: find workaround for std::string being NULL - probably have to introduce new
             * method in lexer Token, or maybe make conversion method there*/
            Token(const_TokenPtr lexer_token_ptr) :
              token_id(lexer_token_ptr->get_id()), locus(lexer_token_ptr->get_locus()), str(""),
              type_hint(lexer_token_ptr->get_type_hint()) {
                // FIXME: change to "should have str" later?
                if (lexer_token_ptr->has_str()) {
                    str = lexer_token_ptr->get_str();

                    // DEBUG
                    fprintf(stderr, "ast token created with str '%s'\n", str.c_str());
                } else {
                    // FIXME: is this returning correct thing?
                    str = lexer_token_ptr->get_token_description();

                    // DEBUG
                    fprintf(stderr, "ast token created with string '%s'\n", str.c_str());
                }

                // DEBUG
                if (lexer_token_ptr->should_have_str() && !lexer_token_ptr->has_str()) {
                    fprintf(stderr, "BAD: for token '%s', should have string but does not!\n",
                      lexer_token_ptr->get_token_description());
                }
            }

            ::std::string as_string() const;

          protected:
            // No virtual for now as not polymorphic but can be in future
            /*virtual*/ Token* clone_token_impl() const {
                return new Token(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual Token* clone_token_tree_impl() const OVERRIDE {
                return new Token(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual Token* clone_macro_match_impl() const OVERRIDE {
                return new Token(*this);
            }
        };

        // A literal - value with a type. Used in LiteralExpr and LiteralPattern.
        struct Literal {
          public:
            enum LitType {
                CHAR,
                STRING,
                RAW_STRING,
                BYTE,
                BYTE_STRING,
                RAW_BYTE_STRING,
                INT,
                FLOAT,
                BOOL
            };

          private:
            // TODO: maybe make subclasses of each type of literal with their typed values (or
            // generics)
            ::std::string value_as_string;
            LitType type;

          public:
            ::std::string as_string() const {
                return value_as_string;
            }

            inline LitType get_lit_type() const {
                return type;
            }

            Literal(::std::string value_as_string, LitType type) :
              value_as_string(::std::move(value_as_string)), type(type) {}
        };

        // A token tree with delimiters
        class DelimTokenTree
          : public TokenTree
          , public AttrInput {
            DelimType delim_type;
            ::std::vector< ::std::unique_ptr<TokenTree> > token_trees;

          protected:
            // Use covariance to implement clone function as returning a DelimTokenTree object
            virtual DelimTokenTree* clone_attr_input_impl() const OVERRIDE {
                return new DelimTokenTree(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual DelimTokenTree* clone_token_tree_impl() const OVERRIDE {
                return new DelimTokenTree(*this);
            }

          public:
            DelimTokenTree(
              DelimType delim_type, ::std::vector< ::std::unique_ptr<TokenTree> > token_trees
                                    = ::std::vector< ::std::unique_ptr<TokenTree> >()) :
              delim_type(delim_type),
              token_trees(::std::move(token_trees)) {}

            // Copy constructor with vector clone
            DelimTokenTree(DelimTokenTree const& other) : delim_type(other.delim_type) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                token_trees.reserve(other.token_trees.size());

                for (const auto& e : other.token_trees) {
                    token_trees.push_back(e->clone_token_tree());
                }
            }

            // overloaded assignment operator with vector clone
            DelimTokenTree& operator=(DelimTokenTree const& other) {
                delim_type = other.delim_type;

                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                token_trees.reserve(other.token_trees.size());

                for (const auto& e : other.token_trees) {
                    token_trees.push_back(e->clone_token_tree());
                }

                return *this;
            }

            // move constructors
            DelimTokenTree(DelimTokenTree&& other) = default;
            DelimTokenTree& operator=(DelimTokenTree&& other) = default;

            static DelimTokenTree create_empty() {
                return DelimTokenTree(PARENS);
            }

            ::std::string as_string() const;
        };

        // Forward decl - definition moved to rust-expr.h as it requires LiteralExpr to be defined
        class AttrInputLiteral;

        // TODO: move applicable stuff into here
        // A segment of a path (maybe)
        class PathSegment {
          public:
            virtual ~PathSegment() {}

            virtual ::std::string as_string() const = 0;
        };

        // A segment of a simple path without generic or type arguments
        class SimplePathSegment : public PathSegment {
            ::std::string segment_name;

            // only allow identifiers, "super", "self", "crate", or "$crate"
          public:
            // TODO: put checks in constructor to enforce this rule?
            SimplePathSegment(::std::string segment_name) : segment_name(::std::move(segment_name)) {}

            // Returns whether simple path segment is in an invalid state (currently, if empty).
            inline bool is_error() const {
                return segment_name.empty();
            }

            // Creates an error SimplePathSegment
            static SimplePathSegment create_error() {
                return SimplePathSegment(::std::string(""));
            }

            ::std::string as_string() const;
        };

        // A simple path without generic or type arguments
        class SimplePath {
            bool has_opening_scope_resolution;
            ::std::vector<SimplePathSegment> segments;

          public:
            // Constructor
            SimplePath(::std::vector<SimplePathSegment> path_segments,
              bool has_opening_scope_resolution = false) :
              has_opening_scope_resolution(has_opening_scope_resolution),
              segments(::std::move(path_segments)) {}

            // Creates an empty SimplePath.
            static SimplePath create_empty() {
                return SimplePath(::std::vector<SimplePathSegment>());
            }

            // Returns whether the SimplePath is empty, i.e. has path segments.
            inline bool is_empty() const {
                return segments.empty();
            }

            ::std::string as_string() const;
        };

        // aka Attr
        // Attribute AST representation
        struct Attribute {
          private:
            SimplePath path;

            // bool has_attr_input;
            // AttrInput* attr_input;
            ::std::unique_ptr<AttrInput> attr_input;

          public:
            // Returns whether Attribute has AttrInput
            inline bool has_attr_input() const {
                return attr_input != NULL;
            }

            // Constructor has pointer AttrInput for polymorphism reasons
            Attribute(SimplePath path, AttrInput* input) :
              path(::std::move(path)), attr_input(input) {}

            // Copy constructor must deep copy attr_input as unique pointer
            Attribute(Attribute const& other) :
              path(other.path), attr_input(other.attr_input->clone_attr_input()) {}

            // default destructor
            ~Attribute() = default;

            // overload assignment operator to use custom clone method
            Attribute& operator=(Attribute const& other) {
                path = other.path;
                attr_input = other.attr_input->clone_attr_input();

                return *this;
            }

            // default move semantics
            Attribute(Attribute&& other) = default;
            Attribute& operator=(Attribute&& other) = default;

            // Unique pointer custom clone function
            ::std::unique_ptr<Attribute> clone_attribute() const {
                return ::std::unique_ptr<Attribute>(clone_attribute_impl());
            }

            /*~Attribute() {
                delete attr_input;
            }*/

            // Creates an empty attribute (which is invalid)
            static Attribute create_empty() {
                return Attribute(SimplePath::create_empty(), NULL);
            }

            // Returns whether the attribute is considered an "empty" attribute.
            inline bool is_empty() const {
                return attr_input == NULL && path.is_empty();
            }

            /* e.g.:
                #![crate_type = "lib"]
                #[test]
                #[cfg(target_os = "linux")]
                #[allow(non_camel_case_types)]
                #![allow(unused_variables)]
            */

            // Full built-in attribute list:
            /*   cfg
             *   cfg_attr
             *   test
             *   ignore
             *   should_panic
             *   derive
             *   macro_export
             *   macro_use
             *   proc_macro
             *   proc_macro_derive
             *   proc_macro_attribute
             *   allow
             *   warn
             *   deny
             *   forbid
             *   deprecated
             *   must_use
             *   link
             *   link_name
             *   no_link
             *   repr
             *   crate_type
             *   no_main
             *   export_name
             *   link_section
             *   no_mangle
             *   used
             *   crate_name
             *   inline
             *   cold
             *   no_builtins
             *   target_feature
             *   doc
             *   no_std
             *   no_implicit_prelude
             *   path
             *   recursion_limit
             *   type_length_limit
             *   panic_handler
             *   global_allocator
             *   windows_subsystem
             *   feature     */

            ::std::string as_string() const;

          protected:
            // not virtual as currently no subclasses of Attribute, but could be in future
            /*virtual*/ Attribute* clone_attribute_impl() const {
                return new Attribute(*this);
            }
        };

        // Syntax used for Attribute by most built-in attributes and the meta fragment spec
        class MetaItem {
            SimplePath path;

          protected:
            MetaItem(SimplePath path) : path(::std::move(path)) {}

            // pure virtual as MetaItem is abstract?
            virtual MetaItem* clone_meta_item_impl() const = 0;

          public:
            // Unique pointer custom clone function
            ::std::unique_ptr<MetaItem> clone_meta_item() const {
                return ::std::unique_ptr<MetaItem>(clone_meta_item_impl());
            }

            virtual ~MetaItem() {}

            virtual ::std::string as_string() const = 0;
        };

        // Forward decl - defined in rust-expr.h
        class MetaItemLit;

        // Forward decl - defined in rust-expr.h
        struct MetaItemInner;

        // Forward decl - defined in rust-expr.h
        class MetaItemSeq;

        // Forward decl - defined in rust-expr.h
        struct MetaWord;

        // Forward decl - defined in rust-expr.h
        struct MetaNameValueStr;

        // Forward decl - defined in rust-expr.h
        struct MetaListPaths;

        // Forward decl - defined in rust-expr.h
        struct MetaListIdents;

        // Forward decl - defined in rust-expr.h
        struct MetaListNameValueStr;

        /* Base statement abstract class. Note that most "statements" are not allowed in top-level
         * module scope - only a subclass of statements called "items" are. */
        class Stmt : public Node {
          public:
            // Unique pointer custom clone function
            ::std::unique_ptr<Stmt> clone_stmt() const {
                return ::std::unique_ptr<Stmt>(clone_stmt_impl());
            }

          protected:
            // Clone function implementation as pure virtual method
            virtual Stmt* clone_stmt_impl() const = 0;
        };

        // Rust "item" AST node (declaration of top-level/module-level allowed stuff)
        class Item : public Stmt {
            ::std::vector<Attribute> outer_attrs;

          public:
            // Unique pointer custom clone function
            ::std::unique_ptr<Item> clone_item() const {
                return ::std::unique_ptr<Item>(clone_item_impl());
            }

            ::std::string as_string() const;

          protected:
            // Constructor
            Item(::std::vector<Attribute> outer_attribs = ::std::vector<Attribute>()) :
              outer_attrs(::std::move(outer_attribs)) {}

            // Clone function implementation as pure virtual method
            virtual Item* clone_item_impl() const = 0;

            /* Save having to specify two clone methods in derived classes by making statement clone
             * return item clone. Hopefully won't affect performance too much. */
            virtual Item* clone_stmt_impl() const OVERRIDE {
                return clone_item_impl();
            }
        };

        // Base expression AST node - abstract
        class Expr : public Node {
            ::std::vector<Attribute> outer_attrs;

          public:
            inline const ::std::vector<Attribute>& get_outer_attrs() const {
                return outer_attrs;
            }

            // Unique pointer custom clone function
            ::std::unique_ptr<Expr> clone_expr() const {
                return ::std::unique_ptr<Expr>(clone_expr_impl());
            }

            /* TODO: public methods that could be useful:
             *  - get_type() - returns type of expression. set_type() may also be useful for some?
             *  - evaluate() - evaluates expression if constant? can_evaluate()? */

            ::std::string as_string() const;

          protected:
            // Constructor
            Expr(::std::vector<Attribute> outer_attribs = ::std::vector<Attribute>()) :
              outer_attrs(::std::move(outer_attribs)) {}

            // Clone function implementation as pure virtual method
            virtual Expr* clone_expr_impl() const = 0;
        };

        // HACK: IdentifierExpr, delete when figure out identifier vs expr problem in Pratt parser
        class IdentifierExpr : public Expr {
            Identifier ident;

          public:
            IdentifierExpr(
              Identifier ident, ::std::vector<Attribute> outer_attrs = ::std::vector<Attribute>()) :
              Expr(::std::move(outer_attrs)),
              ident(::std::move(ident)) {}

            ::std::string as_string() const {
                return "not implemented as a HACK";
            }

          protected:
            // Clone method implementation
            virtual IdentifierExpr* clone_expr_impl() const OVERRIDE {
                return new IdentifierExpr(*this);
            }
        };

        // AST node for an expression without an accompanying block - abstract
        class ExprWithoutBlock : public Expr {
          protected:
            // Constructor
            ExprWithoutBlock(::std::vector<Attribute> outer_attribs = ::std::vector<Attribute>()) :
              Expr(::std::move(outer_attribs)) {}

            // pure virtual clone implementation
            virtual ExprWithoutBlock* clone_expr_without_block_impl() const = 0;

            /* Save having to specify two clone methods in derived classes by making expr clone
             * return exprwithoutblock clone. Hopefully won't affect performance too much. */
            virtual ExprWithoutBlock* clone_expr_impl() const OVERRIDE {
                return clone_expr_without_block_impl();
            }

          public:
            // Unique pointer custom clone function
            ::std::unique_ptr<ExprWithoutBlock> clone_expr_without_block() const {
                return ::std::unique_ptr<ExprWithoutBlock>(clone_expr_without_block_impl());
            }
        };

        // Pattern base AST node
        class Pattern : public Node {
          public:
            // Unique pointer custom clone function
            ::std::unique_ptr<Pattern> clone_pattern() const {
                return ::std::unique_ptr<Pattern>(clone_pattern_impl());
            }

            // possible virtual methods: is_refutable()

          protected:
            // Clone pattern implementation as pure virtual method
            virtual Pattern* clone_pattern_impl() const = 0;
        };

        // forward decl for Type
        class TraitBound;

        // Base class for types as represented in AST - abstract
        class Type {
          public:
            // Unique pointer custom clone function
            ::std::unique_ptr<Type> clone_type() const {
                return ::std::unique_ptr<Type>(clone_type_impl());
            }

            // virtual destructor
            virtual ~Type() {}

            virtual ::std::string as_string() const = 0;

            // HACK: convert to trait bound. Virtual method overriden by classes that enable this.
            virtual TraitBound* to_trait_bound(bool in_parens) const {
                return NULL;
            }
            // as pointer, shouldn't require definition beforehand, only forward declaration.

          protected:
            // Clone function implementation as pure virtual method
            virtual Type* clone_type_impl() const = 0;
        };

        // A type without parentheses? - abstract
        class TypeNoBounds : public Type {
          public:
            // Unique pointer custom clone function
            ::std::unique_ptr<TypeNoBounds> clone_type_no_bounds() const {
                return ::std::unique_ptr<TypeNoBounds>(clone_type_no_bounds_impl());
            }

          protected:
            // Clone function implementation as pure virtual method
            virtual TypeNoBounds* clone_type_no_bounds_impl() const = 0;

            /* Save having to specify two clone methods in derived classes by making type clone
             * return typenobounds clone. Hopefully won't affect performance too much. */
            virtual TypeNoBounds* clone_type_impl() const OVERRIDE {
                return clone_type_no_bounds_impl();
            }
        };

        // Abstract base class representing a type param bound - Lifetime and TraitBound extends it
        class TypeParamBound {
          public:
            virtual ~TypeParamBound() {}

            // Unique pointer custom clone function
            ::std::unique_ptr<TypeParamBound> clone_type_param_bound() const {
                return ::std::unique_ptr<TypeParamBound>(clone_type_param_bound_impl());
            }

            virtual ::std::string as_string() const = 0;

          protected:
            // Clone function implementation as pure virtual method
            virtual TypeParamBound* clone_type_param_bound_impl() const = 0;
        };

        // Represents a lifetime (and is also a kind of type param bound)
        class Lifetime : public TypeParamBound {
          public:
            enum LifetimeType {
                NAMED,   // corresponds to LIFETIME_OR_LABEL
                STATIC,  // corresponds to 'static
                WILDCARD // corresponds to '_
            };

          private:
            LifetimeType lifetime_type;

            // TODO: LIFETIME_OR_LABEL (aka lifetime token) is only field
            // find way of enclosing token or something
            ::std::string lifetime_name;
            // only applies for NAMED lifetime_type

          public:
            // Constructor
            Lifetime(LifetimeType type, ::std::string name = ::std::string()) :
              lifetime_type(type), lifetime_name(::std::move(name)) {}

            // Creates an "error" lifetime.
            static Lifetime error() {
                return Lifetime(NAMED, ::std::string(""));
            }

            // Returns true if the lifetime is in an error state.
            inline bool is_error() const {
                return lifetime_type == NAMED && lifetime_name.empty();
            }

            ::std::string as_string() const;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual Lifetime* clone_type_param_bound_impl() const OVERRIDE {
                return new Lifetime(*this);
            }
        };

        // Base generic parameter in AST. Abstract - can be represented by a Lifetime or Type param
        class GenericParam {
          public:
            virtual ~GenericParam() {}

            // Unique pointer custom clone function
            ::std::unique_ptr<GenericParam> clone_generic_param() const {
                return ::std::unique_ptr<GenericParam>(clone_generic_param_impl());
            }

            virtual ::std::string as_string() const = 0;

          protected:
            // Clone function implementation as pure virtual method
            virtual GenericParam* clone_generic_param_impl() const = 0;
        };

        // A lifetime generic parameter (as opposed to a type generic parameter)
        class LifetimeParam : public GenericParam {
            Lifetime lifetime;

            // bool has_lifetime_bounds;
            // LifetimeBounds lifetime_bounds;
            ::std::vector<Lifetime> lifetime_bounds; // inlined LifetimeBounds

            // bool has_outer_attribute;
            //::std::unique_ptr<Attribute> outer_attr;
            Attribute outer_attr;

          public:
            // Returns whether the lifetime param has any lifetime bounds.
            inline bool has_lifetime_bounds() const {
                return !lifetime_bounds.empty();
            }

            // Returns whether the lifetime param has an outer attribute.
            inline bool has_outer_attribute() const {
                return !outer_attr.is_empty();
            }

            // Creates an error state lifetime param.
            static LifetimeParam create_error() {
                return LifetimeParam(Lifetime::error());
            }

            // Returns whether the lifetime param is in an error state.
            inline bool is_error() const {
                return lifetime.is_error();
            }

            // Constructor
            LifetimeParam(Lifetime lifetime,
              ::std::vector<Lifetime> lifetime_bounds = ::std::vector<Lifetime>(),
              Attribute outer_attr = Attribute::create_empty()) :
              lifetime(::std::move(lifetime)),
              lifetime_bounds(::std::move(lifetime_bounds)), outer_attr(::std::move(outer_attr)) {}

            // Copy constructor with clone
            LifetimeParam(LifetimeParam const& other) :
              lifetime(other.lifetime), lifetime_bounds(other.lifetime_bounds),
              outer_attr(other.outer_attr) {}

            // Destructor - define here if required

            // Overloaded assignment operator to clone attribute
            LifetimeParam& operator=(LifetimeParam const& other) {
                lifetime = other.lifetime;
                lifetime_bounds = other.lifetime_bounds;
                outer_attr = other.outer_attr;

                return *this;
            }

            // move constructors
            LifetimeParam(LifetimeParam&& other) = default;
            LifetimeParam& operator=(LifetimeParam&& other) = default;

            ::std::string as_string() const;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual LifetimeParam* clone_generic_param_impl() const OVERRIDE {
                return new LifetimeParam(*this);
            }
        };

        // A macro item AST node - potentially abstract base class
        class MacroItem : public Item {
            /*public:
            ::std::string as_string() const;*/
          protected:
            MacroItem(::std::vector<Attribute> outer_attribs) : Item(::std::move(outer_attribs)) {}
        };

        // Item used in trait declarations - abstract base class
        class TraitItem {
            // bool has_outer_attrs;
            // TODO: remove and rely on virtual functions and VisItem-derived attributes?
            ::std::vector<Attribute> outer_attrs;

          protected:
            // Constructor
            TraitItem(::std::vector<Attribute> outer_attrs = ::std::vector<Attribute>()) :
              outer_attrs(::std::move(outer_attrs)) {}

            // Clone function implementation as pure virtual method
            virtual TraitItem* clone_trait_item_impl() const = 0;

          public:
            virtual ~TraitItem() {}

            // Returns whether TraitItem has outer attributes.
            inline bool has_outer_attrs() const {
                return !outer_attrs.empty();
            }

            // Unique pointer custom clone function
            ::std::unique_ptr<TraitItem> clone_trait_item() const {
                return ::std::unique_ptr<TraitItem>(clone_trait_item_impl());
            }
        };

        // A macro invocation item (or statement) AST node (i.e. semi-coloned macro invocation)
        class MacroInvocationSemi
          : public MacroItem
          , public TraitItem
        /*, public Statement*/ {
            // already inherits from statement indirectly via item as item is a subclass of statement
            SimplePath path;
            // all delim types except curly must have invocation end with a semicolon
            DelimType delim_type;
            //::std::vector<TokenTree> token_trees;
            ::std::vector< ::std::unique_ptr<TokenTree> > token_trees;

          public:
            ::std::string as_string() const;

            MacroInvocationSemi(SimplePath macro_path, DelimType delim_type,
              ::std::vector< ::std::unique_ptr<TokenTree> > token_trees,
              ::std::vector<Attribute> outer_attribs) :
              MacroItem(outer_attribs),
              TraitItem(outer_attribs), path(::std::move(macro_path)), delim_type(delim_type),
              token_trees(::std::move(token_trees)) {}
            /* TODO: possible issue with Item and TraitItem hierarchies both having outer attributes
             * - storage inefficiency at least.
             * Best current idea is to make Item preferred and have TraitItem get virtual functions
             * for attributes or something.
             * Or just redo the "composition" approach, but then this prevents polymorphism and would
             * entail redoing quite a bit of the parser. */

            // Copy constructor with vector clone
            MacroInvocationSemi(MacroInvocationSemi const& other) :
              MacroItem(other), TraitItem(other), path(other.path), delim_type(other.delim_type) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                token_trees.reserve(other.token_trees.size());

                for (const auto& e : other.token_trees) {
                    token_trees.push_back(e->clone_token_tree());
                }
            }

            // Overloaded assignment operator to vector clone
            MacroInvocationSemi& operator=(MacroInvocationSemi const& other) {
                MacroItem::operator=(other);
                TraitItem::operator=(other);
                path = other.path;
                delim_type = other.delim_type;

                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                token_trees.reserve(other.token_trees.size());

                for (const auto& e : other.token_trees) {
                    token_trees.push_back(e->clone_token_tree());
                }

                return *this;
            }

            // Move constructors
            MacroInvocationSemi(MacroInvocationSemi&& other) = default;
            MacroInvocationSemi& operator=(MacroInvocationSemi&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual MacroInvocationSemi* clone_item_impl() const OVERRIDE {
                return new MacroInvocationSemi(*this);
            }

            // FIXME: remove if item impl virtual override works properly
            // Use covariance to implement clone function as returning this object rather than base
            /*virtual MacroInvocationSemi* clone_statement_impl() const OVERRIDE {
                return new MacroInvocationSemi(*this);
            }*/

            // Use covariance to implement clone function as returning this object rather than base
            virtual MacroInvocationSemi* clone_trait_item_impl() const OVERRIDE {
                return new MacroInvocationSemi(*this);
            }
        };

        // A crate AST object - holds all the data for a single compilation unit
        struct Crate {
            bool has_utf8bom;
            bool has_shebang;

            ::std::vector<Attribute> inner_attrs;
            //::std::vector<Item> items;
            // dodgy spacing required here
            // TODO: is it better to have a vector of items here or a module (implicit top-level one)?
            ::std::vector< ::std::unique_ptr<Item> > items;

          public:
            // Constructor
            Crate(::std::vector< ::std::unique_ptr<Item> > items,
              ::std::vector<Attribute> inner_attrs, bool has_utf8bom = false,
              bool has_shebang = false) :
              has_utf8bom(has_utf8bom),
              has_shebang(has_shebang), inner_attrs(::std::move(inner_attrs)),
              items(::std::move(items)) {}

            // Copy constructor with vector clone
            Crate(Crate const& other) :
              has_utf8bom(other.has_utf8bom), has_shebang(other.has_shebang),
              inner_attrs(other.inner_attrs) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                items.reserve(other.items.size());

                for (const auto& e : other.items) {
                    items.push_back(e->clone_item());
                }
            }

            ~Crate() = default;

            // Overloaded assignment operator with vector clone
            Crate& operator=(Crate const& other) {
                inner_attrs = other.inner_attrs;
                has_shebang = other.has_shebang;
                has_utf8bom = other.has_utf8bom;

                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                items.reserve(other.items.size());

                for (const auto& e : other.items) {
                    items.push_back(e->clone_item());
                }

                return *this;
            }

            // Move constructors
            Crate(Crate&& other) = default;
            Crate& operator=(Crate&& other) = default;

            // Get crate representation as string (e.g. for debugging).
            ::std::string as_string() const;
        };
    }
}

#endif