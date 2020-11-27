#ifndef RUST_AST_PATTERN_H
#define RUST_AST_PATTERN_H

#include "rust-ast.h"

namespace Rust {
    namespace AST {
        // Literal pattern AST node (comparing to a literal)
        class LiteralPattern : public Pattern {
            Literal lit; // make literal have a type given by enum, etc. rustc uses an extended form
            // of its literal token implementation
            // FIXME: literal representation - use LiteralExpr? or another thing?

            // Minus prefixed to literal (if integer or floating-point)
            bool has_minus;
            // Actually, this might be a good place to use a template.

          public:
            ::std::string as_string() const;

            // Constructor for a literal pattern
            LiteralPattern(Literal lit, bool has_minus = false) :
              lit(::std::move(lit)), has_minus(has_minus) {}

            LiteralPattern(::std::string val, Literal::LitType type, bool has_minus = false) :
              lit(Literal(::std::move(val), type)), has_minus(has_minus) {}

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual LiteralPattern* clone_pattern_impl() const OVERRIDE {
                return new LiteralPattern(*this);
            }
        };

        // Identifier pattern AST node (bind value matched to a variable)
        class IdentifierPattern : public Pattern {
            Identifier variable_ident;
            bool is_ref;
            bool is_mut;

            // bool has_pattern;
            // Pattern* to_bind;
            ::std::unique_ptr<Pattern> to_bind;

          public:
            /*~IdentifierPattern() {
                delete to_bind;
            }*/

            ::std::string as_string() const;

            // Returns whether the IdentifierPattern has a pattern to bind.
            inline bool has_pattern_to_bind() const {
                return to_bind != NULL;
            }

            // Constructor
            IdentifierPattern(
              Identifier ident, bool is_ref = false, bool is_mut = false, Pattern* to_bind = NULL) :
              variable_ident(::std::move(ident)),
              is_ref(is_ref), is_mut(is_mut), to_bind(to_bind) {}

            // Copy constructor with clone
            IdentifierPattern(IdentifierPattern const& other) :
              variable_ident(other.variable_ident), is_ref(other.is_ref), is_mut(other.is_mut),
              to_bind(other.to_bind->clone_pattern()) {}

            // Destructor - define here if required

            // Overload assignment operator to use clone
            IdentifierPattern& operator=(IdentifierPattern const& other) {
                variable_ident = other.variable_ident;
                is_ref = other.is_ref;
                is_mut = other.is_mut;
                to_bind = other.to_bind->clone_pattern();

                return *this;
            }

            // default move semantics
            IdentifierPattern(IdentifierPattern&& other) = default;
            IdentifierPattern& operator=(IdentifierPattern&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual IdentifierPattern* clone_pattern_impl() const OVERRIDE {
                return new IdentifierPattern(*this);
            }
        };

        // AST node for using the '_' wildcard "match any value" pattern
        class WildcardPattern : public Pattern {
          public:
            ::std::string as_string() const {
                return ::std::string(1, '_');
            }

            WildcardPattern() {}

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual WildcardPattern* clone_pattern_impl() const OVERRIDE {
                return new WildcardPattern(*this);
            }
        };

        // Base range pattern bound (lower or upper limit) - abstract
        class RangePatternBound {
            /*union {
                CharLiteral char_lit;
                ByteLiteral byte_lit;
                IntLiteral int_lit;
                FloatLiteral float_lit;
                PathInExpression path;
                QualifiedPathInExpression qual_path;
            } pattern;*/
          public:
            virtual ~RangePatternBound() {}

            // Unique pointer custom clone function
            ::std::unique_ptr<RangePatternBound> clone_range_pattern_bound() const {
                return ::std::unique_ptr<RangePatternBound>(clone_range_pattern_bound_impl());
            }

          protected:
            // pure virtual as RangePatternBound is abstract
            virtual RangePatternBound* clone_range_pattern_bound_impl() const = 0;
        };

        // Literal-based pattern bound
        class RangePatternBoundLiteral : public RangePatternBound {
            Literal literal;
            // Can only be a char, byte, int, or float literal - same impl here as previously

            // Minus prefixed to literal (if integer or floating-point)
            bool has_minus;

          public:
            // Constructor
            RangePatternBoundLiteral(Literal literal, bool has_minus = false) :
              literal(literal), has_minus(has_minus) {}

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual RangePatternBoundLiteral* clone_range_pattern_bound_impl() const OVERRIDE {
                return new RangePatternBoundLiteral(*this);
            }
        };

        // Path-based pattern bound
        class RangePatternBoundPath : public RangePatternBound {
            PathInExpression path;

          public:
            RangePatternBoundPath(PathInExpression path) : path(::std::move(path)) {}

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual RangePatternBoundPath* clone_range_pattern_bound_impl() const OVERRIDE {
                return new RangePatternBoundPath(*this);
            }
        };

        // Qualified path-based pattern bound
        class RangePatternBoundQualPath : public RangePatternBound {
            QualifiedPathInExpression path;

          public:
            RangePatternBoundQualPath(QualifiedPathInExpression path) : path(::std::move(path)) {}

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual RangePatternBoundQualPath* clone_range_pattern_bound_impl() const OVERRIDE {
                return new RangePatternBoundQualPath(*this);
            }
        };

        // AST node for matching within a certain range (range pattern)
        class RangePattern : public Pattern {
            /*RangePatternBound lower;
            RangePatternBound upper;*/
            ::std::unique_ptr<RangePatternBound> lower;
            ::std::unique_ptr<RangePatternBound> upper;

            bool has_ellipsis_syntax;

          public:
            ::std::string as_string() const;

            // Constructor
            RangePattern(
              RangePatternBound* lower, RangePatternBound* upper, bool has_ellipsis_syntax = false) :
              lower(lower),
              upper(upper), has_ellipsis_syntax(has_ellipsis_syntax) {}

            // Copy constructor with clone
            RangePattern(RangePattern const& other) :
              lower(other.lower->clone_range_pattern_bound()),
              upper(other.upper->clone_range_pattern_bound()),
              has_ellipsis_syntax(other.has_ellipsis_syntax) {}

            // Destructor - define here if required

            // Overloaded assignment operator to clone
            RangePattern& operator=(RangePattern const& other) {
                lower = other.lower->clone_range_pattern_bound();
                upper = other.upper->clone_range_pattern_bound();
                has_ellipsis_syntax = other.has_ellipsis_syntax;

                return *this;
            }

            // default move semantics
            RangePattern(RangePattern&& other) = default;
            RangePattern& operator=(RangePattern&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual RangePattern* clone_pattern_impl() const OVERRIDE {
                return new RangePattern(*this);
            }
        };

        // AST node for pattern based on dereferencing the pointers given
        class ReferencePattern : public Pattern {
            bool has_two_amps;
            bool is_mut;
            // Pattern* pattern;
            ::std::unique_ptr<Pattern> pattern;

          public:
            /*~ReferencePattern() {
                delete pattern;
            }*/

            ::std::string as_string() const;

            ReferencePattern(Pattern* pattern, bool is_mut_reference, bool ref_has_two_amps) :
              has_two_amps(ref_has_two_amps), is_mut(is_mut_reference), pattern(pattern) {}

            // Copy constructor requires clone
            ReferencePattern(ReferencePattern const& other) :
              has_two_amps(other.has_two_amps), is_mut(other.is_mut),
              pattern(other.pattern->clone_pattern()) {}

            // Destructor - define here if required

            // Overload assignment operator to clone
            ReferencePattern& operator=(ReferencePattern const& other) {
                pattern = other.pattern->clone_pattern();
                is_mut = other.is_mut;
                has_two_amps = other.has_two_amps;

                return *this;
            }

            // default move semantics
            ReferencePattern(ReferencePattern&& other) = default;
            ReferencePattern& operator=(ReferencePattern&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual ReferencePattern* clone_pattern_impl() const OVERRIDE {
                return new ReferencePattern(*this);
            }
        };

        // aka StructPatternEtCetera; potential element in struct pattern
        struct StructPatternEtc {
          private:
            ::std::vector<Attribute> outer_attrs;

          public:
            StructPatternEtc(::std::vector<Attribute> outer_attribs) :
              outer_attrs(::std::move(outer_attribs)) {}

            // Creates an empty StructPatternEtc
            static StructPatternEtc create_empty() {
                return StructPatternEtc(::std::vector<Attribute>());
            }
        };

        // Base class for a single field in a struct pattern - abstract
        class StructPatternField {
            ::std::vector<Attribute> outer_attrs;
            /*union {
                struct {
                    //TupleIndex index;
                    Pattern tuple_pattern;
                } tuple_pattern;
                struct {
                    //Identifier ident;
                    Pattern ident_pattern;
                } ident_pattern;
                struct {
                    bool has_ref;
                    bool has_mut;
                    //Identifier ident;
                } ident;
            } pattern;*/

          public:
            virtual ~StructPatternField() {}

            // Unique pointer custom clone function
            ::std::unique_ptr<StructPatternField> clone_struct_pattern_field() const {
                return ::std::unique_ptr<StructPatternField>(clone_struct_pattern_field_impl());
            }

          protected:
            StructPatternField(::std::vector<Attribute> outer_attribs) :
              outer_attrs(::std::move(outer_attribs)) {}

            // Clone function implementation as pure virtual method
            virtual StructPatternField* clone_struct_pattern_field_impl() const = 0;
        };

        // Tuple pattern single field in a struct pattern
        class StructPatternFieldTuplePat : public StructPatternField {
            TupleIndex index;
            // Pattern* tuple_pattern;
            ::std::unique_ptr<Pattern> tuple_pattern;

          public:
            /*~StructPatternFieldTuplePat() {
                delete tuple_pattern;
            }*/

            StructPatternFieldTuplePat(
              TupleIndex index, Pattern* tuple_pattern, ::std::vector<Attribute> outer_attribs) :
              StructPatternField(::std::move(outer_attribs)),
              index(index), tuple_pattern(tuple_pattern) {}

            // Copy constructor requires clone
            StructPatternFieldTuplePat(StructPatternFieldTuplePat const& other) :
              StructPatternField(other), index(other.index),
              tuple_pattern(other.tuple_pattern->clone_pattern()) {}

            // Destructor - define here if required

            // Overload assignment operator to perform clone
            StructPatternFieldTuplePat& operator=(StructPatternFieldTuplePat const& other) {
                StructPatternField::operator=(other);
                tuple_pattern = other.tuple_pattern->clone_pattern();
                index = other.index;
                // outer_attrs = other.outer_attrs;

                return *this;
            }

            // default move semantics
            StructPatternFieldTuplePat(StructPatternFieldTuplePat&& other) = default;
            StructPatternFieldTuplePat& operator=(StructPatternFieldTuplePat&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual StructPatternFieldTuplePat* clone_struct_pattern_field_impl() const OVERRIDE {
                return new StructPatternFieldTuplePat(*this);
            }
        };

        // Identifier pattern single field in a struct pattern
        class StructPatternFieldIdentPat : public StructPatternField {
            Identifier ident;
            // Pattern* ident_pattern;
            ::std::unique_ptr<Pattern> ident_pattern;

          public:
            /*~StructPatternFieldIdentPat() {
                delete ident_pattern;
            }*/

            StructPatternFieldIdentPat(
              Identifier ident, Pattern* ident_pattern, ::std::vector<Attribute> outer_attrs) :
              StructPatternField(::std::move(outer_attrs)),
              ident(::std::move(ident)), ident_pattern(ident_pattern) {}

            // Copy constructor requires clone
            StructPatternFieldIdentPat(StructPatternFieldIdentPat const& other) :
              StructPatternField(other), ident(other.ident),
              ident_pattern(other.ident_pattern->clone_pattern()) {}

            // Destructor - define here if required

            // Overload assignment operator to clone
            StructPatternFieldIdentPat& operator=(StructPatternFieldIdentPat const& other) {
                StructPatternField::operator=(other);
                ident = other.ident;
                ident_pattern = other.ident_pattern->clone_pattern();
                // outer_attrs = other.outer_attrs;

                return *this;
            }

            // default move semantics
            StructPatternFieldIdentPat(StructPatternFieldIdentPat&& other) = default;
            StructPatternFieldIdentPat& operator=(StructPatternFieldIdentPat&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual StructPatternFieldIdentPat* clone_struct_pattern_field_impl() const OVERRIDE {
                return new StructPatternFieldIdentPat(*this);
            }
        };

        // Identifier only (with no pattern) single field in a struct pattern
        class StructPatternFieldIdent : public StructPatternField {
            bool has_ref;
            bool has_mut;

            Identifier ident;

          public:
            StructPatternFieldIdent(
              Identifier ident, bool is_ref, bool is_mut, ::std::vector<Attribute> outer_attrs) :
              StructPatternField(::std::move(outer_attrs)),
              has_ref(is_ref), has_mut(is_mut), ident(::std::move(ident)) {}

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual StructPatternFieldIdent* clone_struct_pattern_field_impl() const OVERRIDE {
                return new StructPatternFieldIdent(*this);
            }
        };

        // Elements of a struct pattern
        struct StructPatternElements {
          private:
            // bool has_struct_pattern_fields;
            //::std::vector<StructPatternField> fields;
            ::std::vector< ::std::unique_ptr<StructPatternField> > fields;

            bool has_struct_pattern_etc;
            StructPatternEtc etc;

            // must have at least one of the two and maybe both

          public:
            // Returns whether there are any struct pattern fields
            inline bool has_struct_pattern_fields() const {
                return !fields.empty();
            }

            // Constructor for StructPatternElements with both (potentially)
            StructPatternElements(
              ::std::vector< ::std::unique_ptr<StructPatternField> > fields, StructPatternEtc etc) :
              fields(::std::move(fields)),
              has_struct_pattern_etc(true), etc(::std::move(etc)) {}

            // Constructor for StructPatternElements with no StructPatternEtc
            StructPatternElements(::std::vector< ::std::unique_ptr<StructPatternField> > fields) :
              fields(::std::move(fields)), has_struct_pattern_etc(false),
              etc(StructPatternEtc::create_empty()) {}

            // Copy constructor with vector clone
            StructPatternElements(StructPatternElements const& other) :
              has_struct_pattern_etc(other.has_struct_pattern_etc), etc(other.etc) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                fields.reserve(other.fields.size());

                for (const auto& e : other.fields) {
                    fields.push_back(e->clone_struct_pattern_field());
                }
            }

            // Overloaded assignment operator with vector clone
            StructPatternElements& operator=(StructPatternElements const& other) {
                etc = other.etc;
                has_struct_pattern_etc = other.has_struct_pattern_etc;

                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                fields.reserve(other.fields.size());

                for (const auto& e : other.fields) {
                    fields.push_back(e->clone_struct_pattern_field());
                }

                return *this;
            }

            // move constructors
            StructPatternElements(StructPatternElements&& other) = default;
            StructPatternElements& operator=(StructPatternElements&& other) = default;

            // Creates an empty StructPatternElements
            static StructPatternElements create_empty() {
                return StructPatternElements(
                  ::std::vector< ::std::unique_ptr<StructPatternField> >());
            }
        };

        // Struct pattern AST node representation
        class StructPattern : public Pattern {
            PathInExpression path;

            bool has_struct_pattern_elements;
            StructPatternElements elems;

          public:
            ::std::string as_string() const;

            // Constructs a struct pattern from specified StructPatternElements
            StructPattern(PathInExpression struct_path,
              StructPatternElements elems = StructPatternElements::create_empty()) :
              path(::std::move(struct_path)),
              has_struct_pattern_elements(true), elems(::std::move(elems)) {}

            // TODO: constructor to construct via elements included in StructPatternElements

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual StructPattern* clone_pattern_impl() const OVERRIDE {
                return new StructPattern(*this);
            }
        };

        // Base abstract class for patterns used in TupleStructPattern
        class TupleStructItems {
          public:
            virtual ~TupleStructItems() {}

            // Unique pointer custom clone function
            ::std::unique_ptr<TupleStructItems> clone_tuple_struct_items() const {
                return ::std::unique_ptr<TupleStructItems>(clone_tuple_struct_items_impl());
            }

          protected:
            // pure virtual clone implementation
            virtual TupleStructItems* clone_tuple_struct_items_impl() const = 0;
        };

        // Class for non-ranged tuple struct pattern patterns
        class TupleStructItemsNoRange : public TupleStructItems {
            //::std::vector<Pattern> patterns;
            ::std::vector< ::std::unique_ptr<Pattern> > patterns;

          public:
            TupleStructItemsNoRange(::std::vector< ::std::unique_ptr<Pattern> > patterns) :
              patterns(::std::move(patterns)) {}

            // Copy constructor with vector clone
            TupleStructItemsNoRange(TupleStructItemsNoRange const& other) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                patterns.reserve(other.patterns.size());

                for (const auto& e : other.patterns) {
                    patterns.push_back(e->clone_pattern());
                }
            }

            // Overloaded assignment operator with vector clone
            TupleStructItemsNoRange& operator=(TupleStructItemsNoRange const& other) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                patterns.reserve(other.patterns.size());

                for (const auto& e : other.patterns) {
                    patterns.push_back(e->clone_pattern());
                }

                return *this;
            }

            // move constructors
            TupleStructItemsNoRange(TupleStructItemsNoRange&& other) = default;
            TupleStructItemsNoRange& operator=(TupleStructItemsNoRange&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual TupleStructItemsNoRange* clone_tuple_struct_items_impl() const OVERRIDE {
                return new TupleStructItemsNoRange(*this);
            }
        };

        // Class for ranged tuple struct pattern patterns
        class TupleStructItemsRange : public TupleStructItems {
            /*::std::vector<Pattern> lower_patterns;
            ::std::vector<Pattern> upper_patterns;*/
            ::std::vector< ::std::unique_ptr<Pattern> > lower_patterns;
            ::std::vector< ::std::unique_ptr<Pattern> > upper_patterns;

          public:
            TupleStructItemsRange(::std::vector< ::std::unique_ptr<Pattern> > lower_patterns,
              ::std::vector< ::std::unique_ptr<Pattern> > upper_patterns) :
              lower_patterns(::std::move(lower_patterns)),
              upper_patterns(::std::move(upper_patterns)) {}

            // Copy constructor with vector clone
            TupleStructItemsRange(TupleStructItemsRange const& other) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                lower_patterns.reserve(other.lower_patterns.size());

                for (const auto& e : other.lower_patterns) {
                    lower_patterns.push_back(e->clone_pattern());
                }

                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                upper_patterns.reserve(other.upper_patterns.size());

                for (const auto& e : other.upper_patterns) {
                    upper_patterns.push_back(e->clone_pattern());
                }
            }

            // Overloaded assignment operator to clone
            TupleStructItemsRange& operator=(TupleStructItemsRange const& other) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                lower_patterns.reserve(other.lower_patterns.size());

                for (const auto& e : other.lower_patterns) {
                    lower_patterns.push_back(e->clone_pattern());
                }

                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                upper_patterns.reserve(other.upper_patterns.size());

                for (const auto& e : other.upper_patterns) {
                    upper_patterns.push_back(e->clone_pattern());
                }

                return *this;
            }

            // move constructors
            TupleStructItemsRange(TupleStructItemsRange&& other) = default;
            TupleStructItemsRange& operator=(TupleStructItemsRange&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual TupleStructItemsRange* clone_tuple_struct_items_impl() const OVERRIDE {
                return new TupleStructItemsRange(*this);
            }
        };

        // AST node representing a tuple struct pattern
        class TupleStructPattern : public Pattern {
            PathInExpression path;
            // TupleStructItems items;
            ::std::unique_ptr<TupleStructItems> items;

          public:
            ::std::string as_string() const;

            TupleStructPattern(PathInExpression tuple_struct_path, TupleStructItems* items) :
              path(::std::move(tuple_struct_path)), items(items) {}

            // Copy constructor required to clone
            TupleStructPattern(TupleStructPattern const& other) :
              path(other.path), items(other.items->clone_tuple_struct_items()) {}

            // Destructor - define here if required

            // Operator overload assignment operator to clone
            TupleStructPattern& operator=(TupleStructPattern const& other) {
                path = other.path;
                items = other.items->clone_tuple_struct_items();

                return *this;
            }

            // move constructors
            TupleStructPattern(TupleStructPattern&& other) = default;
            TupleStructPattern& operator=(TupleStructPattern&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual TupleStructPattern* clone_pattern_impl() const OVERRIDE {
                return new TupleStructPattern(*this);
            }
        };

        // Base abstract class representing TuplePattern patterns
        class TuplePatternItems {
          public:
            virtual ~TuplePatternItems() {}

            // Unique pointer custom clone function
            ::std::unique_ptr<TuplePatternItems> clone_tuple_pattern_items() const {
                return ::std::unique_ptr<TuplePatternItems>(clone_tuple_pattern_items_impl());
            }

          protected:
            // pure virtual clone implementation
            virtual TuplePatternItems* clone_tuple_pattern_items_impl() const = 0;
        };

        // Class representing TuplePattern patterns where there is only a single pattern
        /*class TuplePatternItemsSingle : public TuplePatternItems {
            // Pattern pattern;
            ::std::unique_ptr<Pattern> pattern;

          public:
            TuplePatternItemsSingle(Pattern* pattern) : pattern(pattern) {}

            // Copy constructor uses clone
            TuplePatternItemsSingle(TuplePatternItemsSingle const& other) :
              pattern(other.pattern->clone_pattern()) {}

            // Destructor - define here if required

            // Overload assignment operator to clone
            TuplePatternItemsSingle& operator=(TuplePatternItemsSingle const& other) {
                pattern = other.pattern->clone_pattern();

                return *this;
            }

            // move constructors
            TuplePatternItemsSingle(TuplePatternItemsSingle&& other) = default;
            TuplePatternItemsSingle& operator=(TuplePatternItemsSingle&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual TuplePatternItemsSingle* clone_tuple_pattern_items_impl() const OVERRIDE {
                return new TuplePatternItemsSingle(*this);
            }
        };*/
        // removed in favour of single-element TuplePatternItemsMultiple

        // Class representing TuplePattern patterns where there are multiple patterns
        class TuplePatternItemsMultiple : public TuplePatternItems {
            //::std::vector<Pattern> patterns;
            ::std::vector< ::std::unique_ptr<Pattern> > patterns;

          public:
            TuplePatternItemsMultiple(::std::vector< ::std::unique_ptr<Pattern> > patterns) :
              patterns(::std::move(patterns)) {}

            // Copy constructor with vector clone
            TuplePatternItemsMultiple(TuplePatternItemsMultiple const& other) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                patterns.reserve(other.patterns.size());

                for (const auto& e : other.patterns) {
                    patterns.push_back(e->clone_pattern());
                }
            }

            // Overloaded assignment operator to vector clone
            TuplePatternItemsMultiple& operator=(TuplePatternItemsMultiple const& other) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                patterns.reserve(other.patterns.size());

                for (const auto& e : other.patterns) {
                    patterns.push_back(e->clone_pattern());
                }

                return *this;
            }

            // move constructors
            TuplePatternItemsMultiple(TuplePatternItemsMultiple&& other) = default;
            TuplePatternItemsMultiple& operator=(TuplePatternItemsMultiple&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual TuplePatternItemsMultiple* clone_tuple_pattern_items_impl() const OVERRIDE {
                return new TuplePatternItemsMultiple(*this);
            }
        };

        // Class representing TuplePattern patterns where there are a range of patterns
        class TuplePatternItemsRanged : public TuplePatternItems {
            /*::std::vector<Pattern> lower_patterns;
            ::std::vector<Pattern> upper_patterns;*/
            ::std::vector< ::std::unique_ptr<Pattern> > lower_patterns;
            ::std::vector< ::std::unique_ptr<Pattern> > upper_patterns;

          public:
            TuplePatternItemsRanged(::std::vector< ::std::unique_ptr<Pattern> > lower_patterns,
              ::std::vector< ::std::unique_ptr<Pattern> > upper_patterns) :
              lower_patterns(::std::move(lower_patterns)),
              upper_patterns(::std::move(upper_patterns)) {}

            // Copy constructor with vector clone
            TuplePatternItemsRanged(TuplePatternItemsRanged const& other) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                lower_patterns.reserve(other.lower_patterns.size());

                for (const auto& e : other.lower_patterns) {
                    lower_patterns.push_back(e->clone_pattern());
                }

                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                upper_patterns.reserve(other.upper_patterns.size());

                for (const auto& e : other.upper_patterns) {
                    upper_patterns.push_back(e->clone_pattern());
                }
            }

            // Overloaded assignment operator to clone
            TuplePatternItemsRanged& operator=(TuplePatternItemsRanged const& other) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                lower_patterns.reserve(other.lower_patterns.size());

                for (const auto& e : other.lower_patterns) {
                    lower_patterns.push_back(e->clone_pattern());
                }

                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                upper_patterns.reserve(other.upper_patterns.size());

                for (const auto& e : other.upper_patterns) {
                    upper_patterns.push_back(e->clone_pattern());
                }

                return *this;
            }

            // move constructors
            TuplePatternItemsRanged(TuplePatternItemsRanged&& other) = default;
            TuplePatternItemsRanged& operator=(TuplePatternItemsRanged&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual TuplePatternItemsRanged* clone_tuple_pattern_items_impl() const OVERRIDE {
                return new TuplePatternItemsRanged(*this);
            }
        };

        // AST node representing a tuple pattern
        class TuplePattern : public Pattern {
            // bool has_tuple_pattern_items;
            // TuplePatternItems items;
            ::std::unique_ptr<TuplePatternItems> items;

          public:
            ::std::string as_string() const;

            // Returns true if the tuple pattern has items
            inline bool has_tuple_pattern_items() const {
                return items != NULL;
            }

            TuplePattern(TuplePatternItems* items) : items(items) {}

            // Copy constructor requires clone
            TuplePattern(TuplePattern const& other) :
              items(other.items->clone_tuple_pattern_items()) {}

            // Destructor - define here if required

            // Overload assignment operator to clone
            TuplePattern& operator=(TuplePattern const& other) {
                items = other.items->clone_tuple_pattern_items();

                return *this;
            }

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual TuplePattern* clone_pattern_impl() const OVERRIDE {
                return new TuplePattern(*this);
            }
        };

        // AST node representing a pattern in parentheses, used to control precedence
        class GroupedPattern : public Pattern {
            // Pattern pattern_in_parens;
            ::std::unique_ptr<Pattern> pattern_in_parens;

          public:
            ::std::string as_string() const {
                return "(" + pattern_in_parens->as_string() + ")";
            }

            GroupedPattern(Pattern* pattern_in_parens) : pattern_in_parens(pattern_in_parens) {}

            // Copy constructor uses clone
            GroupedPattern(GroupedPattern const& other) :
              pattern_in_parens(other.pattern_in_parens->clone_pattern()) {}

            // Destructor - define here if required

            // Overload assignment operator to clone
            GroupedPattern& operator=(GroupedPattern const& other) {
                pattern_in_parens = other.pattern_in_parens->clone_pattern();

                return *this;
            }

            // default move semantics
            GroupedPattern(GroupedPattern&& other) = default;
            GroupedPattern& operator=(GroupedPattern&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual GroupedPattern* clone_pattern_impl() const OVERRIDE {
                return new GroupedPattern(*this);
            }
        };

        // AST node representing patterns that can match slices and arrays
        class SlicePattern : public Pattern {
            //::std::vector<Pattern> items;
            ::std::vector< ::std::unique_ptr<Pattern> > items;

          public:
            ::std::string as_string() const;

            SlicePattern(::std::vector< ::std::unique_ptr<Pattern> > items) :
              items(::std::move(items)) {}

            // Copy constructor with vector clone
            SlicePattern(SlicePattern const& other) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                items.reserve(other.items.size());

                for (const auto& e : other.items) {
                    items.push_back(e->clone_pattern());
                }
            }

            // Overloaded assignment operator to vector clone
            SlicePattern& operator=(SlicePattern const& other) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                items.reserve(other.items.size());

                for (const auto& e : other.items) {
                    items.push_back(e->clone_pattern());
                }

                return *this;
            }

            // move constructors
            SlicePattern(SlicePattern&& other) = default;
            SlicePattern& operator=(SlicePattern&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual SlicePattern* clone_pattern_impl() const OVERRIDE {
                return new SlicePattern(*this);
            }
        };

        // forward decl PathExprSegment
        // class PathExprSegment;

        // Moved definition to rust-path.h
        class PathPattern;

        // Forward decls for paths (defined in rust-path.h)
        class PathInExpression;
        class QualifiedPathInExpression;

        // Replaced with forward decl - defined in rust-macro.h
        class MacroInvocation;
        /*class MacroInvocation : public Pattern {
          public:
            ::std::string as_string() const;
        };*/
    }
}

#endif