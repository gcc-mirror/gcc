#ifndef RUST_AST_TYPE_H
#define RUST_AST_TYPE_H

#include "rust-ast.h"
#include "rust-path.h"

namespace Rust {
    namespace AST {
        // definitions moved to rust-ast.h
        class TypeParamBound;
        class Lifetime;

        // A trait bound
        class TraitBound : public TypeParamBound {
            bool in_parens;
            bool opening_question_mark;

            // bool has_for_lifetimes;
            // LifetimeParams for_lifetimes;
            ::std::vector<LifetimeParam> for_lifetimes; // inlined LifetimeParams

            TypePath type_path;

            location_t locus;

          public:
            // Returns whether trait bound has "for" lifetimes
            inline bool has_for_lifetimes() const {
                return !for_lifetimes.empty();
            }

            TraitBound(TypePath type_path, location_t locus, bool in_parens = false,
              bool opening_question_mark = false,
              ::std::vector<LifetimeParam> for_lifetimes = ::std::vector<LifetimeParam>()) :
              in_parens(in_parens),
              opening_question_mark(opening_question_mark), for_lifetimes(::std::move(for_lifetimes)),
              type_path(::std::move(type_path)), locus(locus) {}

            ::std::string as_string() const;

            location_t get_locus() const {
                return locus;
            }

            virtual void accept_vis(ASTVisitor& vis) OVERRIDE;

          protected:
            // Clone function implementation as (not pure) virtual method
            virtual TraitBound* clone_type_param_bound_impl() const {
                return new TraitBound(*this);
            }
        };

        // definition moved to rust-ast.h
        class TypeNoBounds;

        // An impl trait? Poor reference material here.
        class ImplTraitType : public Type {
            // TypeParamBounds type_param_bounds;
            ::std::vector< ::std::unique_ptr<TypeParamBound> > type_param_bounds; // inlined form

            location_t locus;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual ImplTraitType* clone_type_impl() const OVERRIDE {
                return new ImplTraitType(*this);
            }

          public:
            ImplTraitType(::std::vector< ::std::unique_ptr<TypeParamBound> > type_param_bounds,
              location_t locus) :
              type_param_bounds(::std::move(type_param_bounds)),
              locus(locus) {}

            // copy constructor with vector clone
            ImplTraitType(ImplTraitType const& other) : locus(other.locus) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                type_param_bounds.reserve(other.type_param_bounds.size());

                for (const auto& e : other.type_param_bounds) {
                    type_param_bounds.push_back(e->clone_type_param_bound());
                }
            }

            // overloaded assignment operator to clone
            ImplTraitType& operator=(ImplTraitType const& other) {
                locus = other.locus;
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                type_param_bounds.reserve(other.type_param_bounds.size());

                for (const auto& e : other.type_param_bounds) {
                    type_param_bounds.push_back(e->clone_type_param_bound());
                }

                return *this;
            }

            // move constructors
            ImplTraitType(ImplTraitType&& other) = default;
            ImplTraitType& operator=(ImplTraitType&& other) = default;

            ::std::string as_string() const;

            location_t get_locus() const {
                return locus;
            }

            virtual void accept_vis(ASTVisitor& vis) OVERRIDE;
        };

        // An opaque value of another type that implements a set of traits
        class TraitObjectType : public Type {
            bool has_dyn;
            // TypeParamBounds type_param_bounds;
            ::std::vector< ::std::unique_ptr<TypeParamBound> > type_param_bounds; // inlined form

            location_t locus;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual TraitObjectType* clone_type_impl() const OVERRIDE {
                return new TraitObjectType(*this);
            }

          public:
            TraitObjectType(::std::vector< ::std::unique_ptr<TypeParamBound> > type_param_bounds,
              location_t locus, bool is_dyn_dispatch = false) :
              has_dyn(is_dyn_dispatch),
              type_param_bounds(::std::move(type_param_bounds)), locus(locus) {}

            // copy constructor with vector clone
            TraitObjectType(TraitObjectType const& other) :
              has_dyn(other.has_dyn), locus(other.locus) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                type_param_bounds.reserve(other.type_param_bounds.size());

                for (const auto& e : other.type_param_bounds) {
                    type_param_bounds.push_back(e->clone_type_param_bound());
                }
            }

            // overloaded assignment operator to clone
            TraitObjectType& operator=(TraitObjectType const& other) {
                has_dyn = other.has_dyn;
                locus = other.locus;
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                type_param_bounds.reserve(other.type_param_bounds.size());

                for (const auto& e : other.type_param_bounds) {
                    type_param_bounds.push_back(e->clone_type_param_bound());
                }

                return *this;
            }

            // move constructors
            TraitObjectType(TraitObjectType&& other) = default;
            TraitObjectType& operator=(TraitObjectType&& other) = default;

            ::std::string as_string() const;

            location_t get_locus() const {
                return locus;
            }

            virtual void accept_vis(ASTVisitor& vis) OVERRIDE;
        };

        // A type with parentheses around it, used to avoid ambiguity.
        class ParenthesisedType : public TypeNoBounds {
            // Type type_in_parens;
            ::std::unique_ptr<Type> type_in_parens;

            location_t locus;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual ParenthesisedType* clone_type_impl() const OVERRIDE {
                return new ParenthesisedType(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual ParenthesisedType* clone_type_no_bounds_impl() const OVERRIDE {
                return new ParenthesisedType(*this);
            }

          public:
            // Constructor uses Type pointer for polymorphism
            ParenthesisedType(::std::unique_ptr<Type> type_inside_parens, location_t locus) :
              type_in_parens(::std::move(type_inside_parens)), locus(locus) {}

            // Copy constructor uses custom deep copy method for type to preserve polymorphism
            ParenthesisedType(ParenthesisedType const& other) :
              type_in_parens(other.type_in_parens->clone_type()), locus(other.locus) {}

            // define destructor here if required

            // overload assignment operator to use custom clone method
            ParenthesisedType& operator=(ParenthesisedType const& other) {
                type_in_parens = other.type_in_parens->clone_type();
                locus = other.locus;
                return *this;
            }

            // default move semantics
            ParenthesisedType(ParenthesisedType&& other) = default;
            ParenthesisedType& operator=(ParenthesisedType&& other) = default;

            ::std::string as_string() const {
                return "(" + type_in_parens->as_string() + ")";
            }

            // Creates a trait bound (clone of this one's trait bound) - HACK
            virtual TraitBound* to_trait_bound(bool in_parens ATTRIBUTE_UNUSED) const OVERRIDE {
                /* NOTE: obviously it is unknown whether the internal type is a trait bound due to
                 * polymorphism, so just let the internal type handle it. As parenthesised type, it
                 * must be in parentheses. */
                return type_in_parens->to_trait_bound(true);
            }

            location_t get_locus() const {
                return locus;
            }

            virtual void accept_vis(ASTVisitor& vis) OVERRIDE;
        };

        // Impl trait with a single bound? Poor reference material here.
        class ImplTraitTypeOneBound : public TypeNoBounds {
            TraitBound trait_bound;

            location_t locus;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual ImplTraitTypeOneBound* clone_type_impl() const OVERRIDE {
                return new ImplTraitTypeOneBound(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual ImplTraitTypeOneBound* clone_type_no_bounds_impl() const OVERRIDE {
                return new ImplTraitTypeOneBound(*this);
            }

          public:
            ImplTraitTypeOneBound(TraitBound trait_bound, location_t locus) :
              trait_bound(::std::move(trait_bound)), locus(locus) {}

            ::std::string as_string() const;

            location_t get_locus() const {
                return locus;
            }

            virtual void accept_vis(ASTVisitor& vis) OVERRIDE;
        };

        /* A trait object with a single trait bound. The "trait bound" is really just the trait.
         * Basically like using an interface as a type in an OOP language. */
        class TraitObjectTypeOneBound : public TypeNoBounds {
            bool has_dyn;
            TraitBound trait_bound;

            location_t locus;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual TraitObjectTypeOneBound* clone_type_impl() const OVERRIDE {
                return new TraitObjectTypeOneBound(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual TraitObjectTypeOneBound* clone_type_no_bounds_impl() const OVERRIDE {
                return new TraitObjectTypeOneBound(*this);
            }

          public:
            TraitObjectTypeOneBound(
              TraitBound trait_bound, location_t locus, bool is_dyn_dispatch = false) :
              has_dyn(is_dyn_dispatch),
              trait_bound(::std::move(trait_bound)), locus(locus) {}

            ::std::string as_string() const;

            // Creates a trait bound (clone of this one's trait bound) - HACK
            virtual TraitBound* to_trait_bound(bool in_parens ATTRIBUTE_UNUSED) const OVERRIDE {
                /* NOTE: this assumes there is no dynamic dispatch specified- if there was, this
                 * cloning would not be required as parsing is unambiguous. */
                return new AST::TraitBound(trait_bound);
            }

            location_t get_locus() const {
                return locus;
            }

            virtual void accept_vis(ASTVisitor& vis) OVERRIDE;
        };

        class TypePath; // definition moved to "rust-path.h"

        // A type consisting of the "product" of others (the tuple's elements) in a specific order
        class TupleType : public TypeNoBounds {
            //::std::vector<Type> elems;
            ::std::vector< ::std::unique_ptr<Type> > elems;

            location_t locus;

          public:
            // Returns whether the tuple type is the unit type, i.e. has no elements.
            inline bool is_unit_type() const {
                return elems.empty();
            }

            TupleType(::std::vector< ::std::unique_ptr<Type> > elems, location_t locus) :
              elems(::std::move(elems)), locus(locus) {}

            // copy constructor with vector clone
            TupleType(TupleType const& other) : locus(other.locus) {
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                elems.reserve(other.elems.size());

                for (const auto& e : other.elems) {
                    elems.push_back(e->clone_type());
                }
            }

            // overloaded assignment operator to clone
            TupleType& operator=(TupleType const& other) {
                locus = other.locus;
                // crappy vector unique pointer clone - TODO is there a better way of doing this?
                elems.reserve(other.elems.size());

                for (const auto& e : other.elems) {
                    elems.push_back(e->clone_type());
                }

                return *this;
            }

            // move constructors
            TupleType(TupleType&& other) = default;
            TupleType& operator=(TupleType&& other) = default;

            ::std::string as_string() const;

            location_t get_locus() const {
                return locus;
            }

            virtual void accept_vis(ASTVisitor& vis) OVERRIDE;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual TupleType* clone_type_impl() const OVERRIDE {
                return new TupleType(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual TupleType* clone_type_no_bounds_impl() const OVERRIDE {
                return new TupleType(*this);
            }
        };

        /* A type with no values, representing the result of computations that never complete.
         * Expressions of NeverType can be coerced into any other types. Represented as "!". */
        class NeverType : public TypeNoBounds {
            location_t locus;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual NeverType* clone_type_impl() const OVERRIDE {
                return new NeverType(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual NeverType* clone_type_no_bounds_impl() const OVERRIDE {
                return new NeverType(*this);
            }

          public:
            NeverType(location_t locus) : locus(locus) {}

            ::std::string as_string() const {
                return "! (never type)";
            }

            location_t get_locus() const {
                return locus;
            }

            virtual void accept_vis(ASTVisitor& vis) OVERRIDE;
        };

        // A type consisting of a pointer without safety or liveness guarantees
        class RawPointerType : public TypeNoBounds {
          public:
            enum PointerType { MUT, CONST };

          private:
            PointerType pointer_type;

            // TypeNoBounds type;
            ::std::unique_ptr<TypeNoBounds> type;

            location_t locus;

          public:
            // Returns whether the pointer is mutable or constant.
            inline PointerType get_pointer_type() const {
                return pointer_type;
            }

            // Constructor requires pointer for polymorphism reasons
            RawPointerType(PointerType pointer_type, ::std::unique_ptr<TypeNoBounds> type_no_bounds,
              location_t locus) :
              pointer_type(pointer_type),
              type(::std::move(type_no_bounds)), locus(locus) {}

            // Copy constructor calls custom polymorphic clone function
            RawPointerType(RawPointerType const& other) :
              pointer_type(other.pointer_type), type(other.type->clone_type_no_bounds()),
              locus(other.locus) {}

            // no destructor required?

            // overload assignment operator to use custom clone method
            RawPointerType& operator=(RawPointerType const& other) {
                pointer_type = other.pointer_type;
                type = other.type->clone_type_no_bounds();
                locus = other.locus;
                return *this;
            }

            // default move semantics
            RawPointerType(RawPointerType&& other) = default;
            RawPointerType& operator=(RawPointerType&& other) = default;

            ::std::string as_string() const;

            location_t get_locus() const {
                return locus;
            }

            virtual void accept_vis(ASTVisitor& vis) OVERRIDE;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual RawPointerType* clone_type_impl() const OVERRIDE {
                return new RawPointerType(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual RawPointerType* clone_type_no_bounds_impl() const OVERRIDE {
                return new RawPointerType(*this);
            }
        };

        // A type pointing to memory owned by another value
        class ReferenceType : public TypeNoBounds {
            // bool has_lifetime; // TODO: handle in lifetime or something?
            Lifetime lifetime;

            bool has_mut;

            // TypeNoBounds type;
            ::std::unique_ptr<TypeNoBounds> type;

            location_t locus;

          public:
            // Returns whether the reference is mutable or immutable.
            inline bool is_mut() const {
                return has_mut;
            }

            // Returns whether the reference has a lifetime.
            inline bool has_lifetime() const {
                return !lifetime.is_error();
            }

            // Constructor
            ReferenceType(bool is_mut, ::std::unique_ptr<TypeNoBounds> type_no_bounds,
              location_t locus, Lifetime lifetime = Lifetime::error()) :
              lifetime(::std::move(lifetime)),
              has_mut(is_mut), type(::std::move(type_no_bounds)), locus(locus) {}

            // Copy constructor with custom clone method
            ReferenceType(ReferenceType const& other) :
              lifetime(other.lifetime), has_mut(other.has_mut),
              type(other.type->clone_type_no_bounds()), locus(other.locus) {}

            // Destructor not required?

            // Operator overload assignment operator to custom clone the unique pointer
            ReferenceType& operator=(ReferenceType const& other) {
                lifetime = other.lifetime;
                has_mut = other.has_mut;
                type = other.type->clone_type_no_bounds();
                locus = other.locus;

                return *this;
            }

            // move constructors
            ReferenceType(ReferenceType&& other) = default;
            ReferenceType& operator=(ReferenceType&& other) = default;

            ::std::string as_string() const;

            location_t get_locus() const {
                return locus;
            }

            virtual void accept_vis(ASTVisitor& vis) OVERRIDE;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual ReferenceType* clone_type_impl() const OVERRIDE {
                return new ReferenceType(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual ReferenceType* clone_type_no_bounds_impl() const OVERRIDE {
                return new ReferenceType(*this);
            }
        };

        // A fixed-size sequence of elements of a specified type
        class ArrayType : public TypeNoBounds {
            // Type elem_type;
            ::std::unique_ptr<Type> elem_type;
            // Expr* size;
            ::std::unique_ptr<Expr> size;

            location_t locus;

          public:
            // Constructor requires pointers for polymorphism
            ArrayType(
              ::std::unique_ptr<Type> type, ::std::unique_ptr<Expr> array_size, location_t locus) :
              elem_type(::std::move(type)),
              size(::std::move(array_size)), locus(locus) {}

            // Copy constructor requires deep copies of both unique pointers
            ArrayType(ArrayType const& other) :
              elem_type(other.elem_type->clone_type()), size(other.size->clone_expr()),
              locus(other.locus) {}

            // destructor not required?

            // Overload assignment operator to deep copy pointers
            ArrayType& operator=(ArrayType const& other) {
                elem_type = other.elem_type->clone_type();
                size = other.size->clone_expr();
                locus = other.locus;
                return *this;
            }

            // move constructors
            ArrayType(ArrayType&& other) = default;
            ArrayType& operator=(ArrayType&& other) = default;

            ::std::string as_string() const;

            location_t get_locus() const {
                return locus;
            }

            virtual void accept_vis(ASTVisitor& vis) OVERRIDE;

            /*~ArrayType() {
                delete size;
            }*/
          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual ArrayType* clone_type_impl() const OVERRIDE {
                return new ArrayType(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual ArrayType* clone_type_no_bounds_impl() const OVERRIDE {
                return new ArrayType(*this);
            }
        };

        // A dynamically-sized type representing a "view" into a sequence of elements of a type
        class SliceType : public TypeNoBounds {
            // Type elem_type;
            ::std::unique_ptr<Type> elem_type;

            location_t locus;

          public:
            // Constructor requires pointer for polymorphism
            SliceType(::std::unique_ptr<Type> type, location_t locus) :
              elem_type(::std::move(type)), locus(locus) {}

            // Copy constructor requires deep copy of Type smart pointer
            SliceType(SliceType const& other) :
              elem_type(other.elem_type->clone_type()), locus(other.locus) {}

            // destructor not required?

            // Overload assignment operator to deep copy
            SliceType& operator=(SliceType const& other) {
                elem_type = other.elem_type->clone_type();
                locus = other.locus;

                return *this;
            }

            // move constructors
            SliceType(SliceType&& other) = default;
            SliceType& operator=(SliceType&& other) = default;

            ::std::string as_string() const;

            location_t get_locus() const {
                return locus;
            }

            virtual void accept_vis(ASTVisitor& vis) OVERRIDE;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual SliceType* clone_type_impl() const OVERRIDE {
                return new SliceType(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual SliceType* clone_type_no_bounds_impl() const OVERRIDE {
                return new SliceType(*this);
            }
        };

        // Type used in generic arguments to explicitly request type inference (wildcard pattern)
        class InferredType : public TypeNoBounds {
            location_t locus;

            // e.g. Vec<_> = whatever
          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual InferredType* clone_type_impl() const OVERRIDE {
                return new InferredType(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual InferredType* clone_type_no_bounds_impl() const OVERRIDE {
                return new InferredType(*this);
            }

          public:
            InferredType(location_t locus) : locus(locus) {}

            ::std::string as_string() const;

            location_t get_locus() const {
                return locus;
            }

            virtual void accept_vis(ASTVisitor& vis) OVERRIDE;
        };

        class QualifiedPathInType; // definition moved to "rust-path.h"

        // A possibly named param used in a BaseFunctionType
        struct MaybeNamedParam {
          public:
            enum ParamKind { UNNAMED, IDENTIFIER, WILDCARD };

          private:
            // Type param_type;
            ::std::unique_ptr<Type> param_type;

            ParamKind param_kind;
            Identifier name; // technically, can be an identifier or '_'

            location_t locus;

          public:
            MaybeNamedParam(Identifier name, ParamKind param_kind, ::std::unique_ptr<Type> param_type,
              location_t locus) :
              param_type(::std::move(param_type)),
              param_kind(param_kind), name(::std::move(name)), locus(locus) {}

            // Copy constructor with clone
            MaybeNamedParam(MaybeNamedParam const& other) :
              param_type(other.param_type->clone_type()), param_kind(other.param_kind),
              name(other.name), locus(other.locus) {}

            ~MaybeNamedParam() = default;

            // Overloaded assignment operator with clone
            MaybeNamedParam& operator=(MaybeNamedParam const& other) {
                name = other.name;
                param_kind = other.param_kind;
                param_type = other.param_type->clone_type();
                locus = other.locus;

                return *this;
            }

            // move constructors
            MaybeNamedParam(MaybeNamedParam&& other) = default;
            MaybeNamedParam& operator=(MaybeNamedParam&& other) = default;

            ::std::string as_string() const;

            // Returns whether the param is in an error state.
            inline bool is_error() const {
                return param_type == NULL;
            }

            // Creates an error state param.
            static MaybeNamedParam create_error() {
                return MaybeNamedParam("", UNNAMED, NULL, UNKNOWN_LOCATION);
            }

            location_t get_locus() const {
                return locus;
            }
        };

        /* A function pointer type - can be created via coercion from function items and non-
         * capturing closures. */
        class BareFunctionType : public TypeNoBounds {
            // bool has_for_lifetimes;
            // ForLifetimes for_lifetimes;
            ::std::vector<LifetimeParam> for_lifetimes; // inlined version

            FunctionQualifiers function_qualifiers;
            ::std::vector<MaybeNamedParam> params;
            bool is_variadic;

            // bool has_return_type;
            // BareFunctionReturnType return_type;
            ::std::unique_ptr<TypeNoBounds> return_type; // inlined version

            location_t locus;

          public:
            // Whether a return type is defined with the function.
            inline bool has_return_type() const {
                return return_type != NULL;
            }

            // Whether the function has ForLifetimes.
            inline bool has_for_lifetimes() const {
                return !for_lifetimes.empty();
            }

            BareFunctionType(::std::vector<LifetimeParam> lifetime_params,
              FunctionQualifiers qualifiers, ::std::vector<MaybeNamedParam> named_params,
              bool is_variadic, ::std::unique_ptr<TypeNoBounds> type, location_t locus) :
              for_lifetimes(::std::move(lifetime_params)),
              function_qualifiers(::std::move(qualifiers)), params(::std::move(named_params)),
              is_variadic(is_variadic), return_type(::std::move(type)), locus(locus) {}

            // Copy constructor with clone
            BareFunctionType(BareFunctionType const& other) :
              for_lifetimes(other.for_lifetimes), function_qualifiers(other.function_qualifiers),
              params(other.params), is_variadic(other.is_variadic),
              return_type(other.return_type->clone_type_no_bounds()), locus(other.locus) {}

            // destructor - define here if required

            // Overload assignment operator to deep copy
            BareFunctionType& operator=(BareFunctionType const& other) {
                for_lifetimes = other.for_lifetimes;
                function_qualifiers = other.function_qualifiers;
                params = other.params;
                is_variadic = other.is_variadic;
                return_type = other.return_type->clone_type_no_bounds();
                locus = other.locus;

                return *this;
            }

            // move constructors
            BareFunctionType(BareFunctionType&& other) = default;
            BareFunctionType& operator=(BareFunctionType&& other) = default;

            ::std::string as_string() const;

            location_t get_locus() const {
                return locus;
            }

            virtual void accept_vis(ASTVisitor& vis) OVERRIDE;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual BareFunctionType* clone_type_impl() const OVERRIDE {
                return new BareFunctionType(*this);
            }

            // Use covariance to implement clone function as returning this object rather than base
            virtual BareFunctionType* clone_type_no_bounds_impl() const OVERRIDE {
                return new BareFunctionType(*this);
            }
        };

        // Forward decl - defined in rust-macro.h
        class MacroInvocation;

        /*// AST node of a macro invocation, which is replaced by the macro result at compile time
        class MacroInvocation : public TypeNoBounds, public Pattern, public ExprWithoutBlock {
            SimplePath path;
            DelimTokenTree token_tree;
        };*/

        /* TODO: possible types
         * struct type?
         * "enum" (tagged union) type?
         * C-like union type?
         * function item type?
         * closure expression types?
         * primitive types (bool, int, float, char, str (the slice))
         * Although supposedly TypePaths are used to reference these types (including primitives) */

        /* FIXME: Incomplete spec references:
         *  anonymous type parameters, aka "impl Trait in argument position" - impl then trait bounds
         *  abstract return types, aka "impl Trait in return position" - impl then trait bounds */
    }
}

#endif