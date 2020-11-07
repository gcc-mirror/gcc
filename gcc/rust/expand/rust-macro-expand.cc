#include "rust-macro-expand.h"
#include "rust-ast-full.h"
// is full really required?
#include "rust-ast-visitor.h"

namespace Rust {
    // Visitor used to expand attributes.
    class AttrVisitor : public AST::ASTVisitor {
      private:
        MacroExpander& expander;

      public:
        AttrVisitor(MacroExpander& expander) : expander(expander) {}

        void expand_struct_fields(std::vector<AST::StructField>& fields) {
            for (int i = 0; i < fields.size(); ) {
                auto& field_attrs = fields[i].get_outer_attrs ();
                expander.expand_cfg_attrs(field_attrs);
                if (expander.fails_cfg (field_attrs))
                    fields.erase (fields.begin() + i);
                else
                    i++;
            }
        }

        void expand_function_params(std::vector<AST::FunctionParam>& params) {
            for (int i = 0; i < params.size(); ) {
                auto& param_attrs = params[i].get_outer_attrs ();
                expander.expand_cfg_attrs(param_attrs);
                if (expander.fails_cfg (param_attrs))
                    params.erase (params.begin() + i);
                else
                    i++;
            }
        }

        void visit(AST::Token& tok) override {}
        void visit(AST::DelimTokenTree& delim_tok_tree) override {}
        void visit(AST::AttrInputMetaItemContainer& input) override {}
        void visit(AST::IdentifierExpr& ident_expr) override {}
        void visit(AST::Lifetime& lifetime) override {}
        void visit(AST::LifetimeParam& lifetime_param) override {}
        void visit(AST::MacroInvocationSemi& macro) override {}

        void visit(AST::PathInExpression& path) override {}
        void visit(AST::TypePathSegment& segment) override {}
        void visit(AST::TypePathSegmentGeneric& segment) override {}
        void visit(AST::TypePathSegmentFunction& segment) override {}
        void visit(AST::TypePath& path) override {}
        void visit(AST::QualifiedPathInExpression& path) override {}
        void visit(AST::QualifiedPathInType& path) override {}

        void visit(AST::LiteralExpr& expr) override {}
        void visit(AST::AttrInputLiteral& attr_input) override {}
        void visit(AST::MetaItemLitExpr& meta_item) override {}
        void visit(AST::MetaItemPathLit& meta_item) override {}
        void visit(AST::BorrowExpr& expr) override {}
        void visit(AST::DereferenceExpr& expr) override {}
        void visit(AST::ErrorPropagationExpr& expr) override {}
        void visit(AST::NegationExpr& expr) override {}
        void visit(AST::ArithmeticOrLogicalExpr& expr) override {}
        void visit(AST::ComparisonExpr& expr) override {}
        void visit(AST::LazyBooleanExpr& expr) override {}
        void visit(AST::TypeCastExpr& expr) override {}
        void visit(AST::AssignmentExpr& expr) override {}
        void visit(AST::CompoundAssignmentExpr& expr) override {}
        void visit(AST::GroupedExpr& expr) override {}
        void visit(AST::ArrayElemsValues& elems) override {}
        void visit(AST::ArrayElemsCopied& elems) override {}
        void visit(AST::ArrayExpr& expr) override {}
        void visit(AST::ArrayIndexExpr& expr) override {}
        void visit(AST::TupleExpr& expr) override {}
        void visit(AST::TupleIndexExpr& expr) override {}
        void visit(AST::StructExprStruct& expr) override {}
        void visit(AST::StructExprFieldIdentifier& field) override {}
        void visit(AST::StructExprFieldIdentifierValue& field) override {}
        void visit(AST::StructExprFieldIndexValue& field) override {}
        void visit(AST::StructExprStructFields& expr) override {}
        void visit(AST::StructExprStructBase& expr) override {}
        void visit(AST::StructExprTuple& expr) override {}
        void visit(AST::StructExprUnit& expr) override {}
        void visit(AST::EnumExprFieldIdentifier& field) override {}
        void visit(AST::EnumExprFieldIdentifierValue& field) override {}
        void visit(AST::EnumExprFieldIndexValue& field) override {}
        void visit(AST::EnumExprStruct& expr) override {}
        void visit(AST::EnumExprTuple& expr) override {}
        void visit(AST::EnumExprFieldless& expr) override {}
        void visit(AST::CallExpr& expr) override {}
        void visit(AST::MethodCallExpr& expr) override {}
        void visit(AST::FieldAccessExpr& expr) override {}
        void visit(AST::ClosureExprInner& expr) override {}
        void visit(AST::BlockExpr& expr) override {}
        void visit(AST::ClosureExprInnerTyped& expr) override {}
        void visit(AST::ContinueExpr& expr) override {}
        void visit(AST::BreakExpr& expr) override {}
        void visit(AST::RangeFromToExpr& expr) override {}
        void visit(AST::RangeFromExpr& expr) override {}
        void visit(AST::RangeToExpr& expr) override {}
        void visit(AST::RangeFullExpr& expr) override {}
        void visit(AST::RangeFromToInclExpr& expr) override {}
        void visit(AST::RangeToInclExpr& expr) override {}
        void visit(AST::ReturnExpr& expr) override {}
        void visit(AST::UnsafeBlockExpr& expr) override {}
        void visit(AST::LoopExpr& expr) override {}
        void visit(AST::WhileLoopExpr& expr) override {}
        void visit(AST::WhileLetLoopExpr& expr) override {}
        void visit(AST::ForLoopExpr& expr) override {}
        void visit(AST::IfExpr& expr) override {}
        void visit(AST::IfExprConseqElse& expr) override {}
        void visit(AST::IfExprConseqIf& expr) override {}
        void visit(AST::IfExprConseqIfLet& expr) override {}
        void visit(AST::IfLetExpr& expr) override {}
        void visit(AST::IfLetExprConseqElse& expr) override {}
        void visit(AST::IfLetExprConseqIf& expr) override {}
        void visit(AST::IfLetExprConseqIfLet& expr) override {}
        void visit(AST::MatchExpr& expr) override {}
        void visit(AST::AwaitExpr& expr) override {}
        void visit(AST::AsyncBlockExpr& expr) override {}

        void visit(AST::TypeParam& param) override {}
        void visit(AST::LifetimeWhereClauseItem& item) override {}
        void visit(AST::TypeBoundWhereClauseItem& item) override {}
        void visit(AST::Method& method) override {}
        void visit(AST::ModuleBodied& module) override {}
        void visit(AST::ModuleNoBody& module) override {}
        void visit(AST::ExternCrate& crate) override {}
        void visit(AST::UseTreeGlob& use_tree) override {}
        void visit(AST::UseTreeList& use_tree) override {}
        void visit(AST::UseTreeRebind& use_tree) override {}
        void visit(AST::UseDeclaration& use_decl) override {
            // strip test based on outer attrs
            expander.expand_cfg_attrs(use_decl.get_outer_attrs());
            if (expander.fails_cfg(use_decl.get_outer_attrs())) {
                use_decl.mark_for_strip();
                return;
            }
        }
        void visit(AST::Function& function) override {}
        void visit(AST::TypeAlias& type_alias) override {}
        void visit(AST::StructStruct& struct_item) override {}
        void visit(AST::TupleStruct& tuple_struct) override {}
        void visit(AST::EnumItem& item) override {}
        void visit(AST::EnumItemTuple& item) override {}
        void visit(AST::EnumItemStruct& item) override {}
        void visit(AST::EnumItemDiscriminant& item) override {}
        void visit(AST::Enum& enum_item) override {}
        void visit(AST::Union& union_item) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(union_item.get_outer_attrs());
            if (expander.fails_cfg(union_item.get_outer_attrs())) {
                union_item.mark_for_strip();
                return;
            }
            
            /* strip union fields if required - this is presumably
             * allowed by spec */
            expand_struct_fields(union_item.get_variants());
        }
        void visit(AST::ConstantItem& const_item) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(const_item.get_outer_attrs());
            if (expander.fails_cfg(const_item.get_outer_attrs())) {
                const_item.mark_for_strip();
                return;
            }
            /* TODO: is there any way to invalidate the expr? Are attributes 
             * even allowed on it? */
        }
        void visit(AST::StaticItem& static_item) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(static_item.get_outer_attrs());
            if (expander.fails_cfg(static_item.get_outer_attrs())) {
                static_item.mark_for_strip();
                return;
            }
            /* TODO: is there any way to invalidate the expr? Are attributes 
             * even allowed on it? */
        }
        void visit(AST::TraitItemFunc& item) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(item.get_outer_attrs());
            if (expander.fails_cfg(item.get_outer_attrs())) {
                item.mark_for_strip();
                return;
            }

            /* strip function parameters if required - this is specifically 
             * allowed by spec */
            expand_function_params(item.get_function_params());

            if (item.has_definition()) {
                item.get_definition()->accept_vis(*this);
                // TODO: can block as a whole be invalidated here? Assuming no
            }
        }
        void visit(AST::TraitItemMethod& item) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(item.get_outer_attrs());
            if (expander.fails_cfg(item.get_outer_attrs())) {
                item.mark_for_strip();
                return;
            }

            /* assuming you can't strip self param - wouldn't be a method 
             * anymore. spec allows outer attrs on self param, but doesn't
             * specify whether cfg is used. */

            /* strip function parameters if required - this is specifically 
             * allowed by spec */
            expand_function_params(item.get_function_params());

            if (item.has_definition()) {
                item.get_definition()->accept_vis(*this);
                // TODO: can block as a whole be invalidated here? Assuming no
            }
        }
        void visit(AST::TraitItemConst& item) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(item.get_outer_attrs());
            if (expander.fails_cfg(item.get_outer_attrs())) {
                item.mark_for_strip();
                return;
            }
            /* TODO: is there any way to invalidate the expr? Are attributes 
             * even allowed on it? */
        }
        void visit(AST::TraitItemType& item) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(item.get_outer_attrs());
            if (expander.fails_cfg(item.get_outer_attrs())) {
                item.mark_for_strip();
                return;
            }
        }
        void visit(AST::Trait& trait) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(trait.get_outer_attrs());
            if (expander.fails_cfg(trait.get_outer_attrs())) {
                trait.mark_for_strip();
                return;
            }

            // strip test based on inner attrs
            expander.expand_cfg_attrs(trait.get_inner_attrs());
            if (expander.fails_cfg(trait.get_inner_attrs())) {
                trait.mark_for_strip();
                return;
            }

            // strip trait items if required
            auto& trait_items = trait.get_trait_items();
            for (int i = 0; i < trait_items.size(); ) {
                auto& item = trait_items[i];

                // mark for stripping if required
                item->accept_vis(*this);

                if (item->is_marked_for_strip ())
                    trait_items.erase (trait_items.begin() + i);
                else
                    i++;
            }
        }
        void visit(AST::InherentImpl& impl) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(impl.get_outer_attrs());
            if (expander.fails_cfg(impl.get_outer_attrs())) {
                impl.mark_for_strip();
                return;
            }

            // strip test based on inner attrs
            expander.expand_cfg_attrs(impl.get_inner_attrs());
            if (expander.fails_cfg(impl.get_inner_attrs())) {
                impl.mark_for_strip();
                return;
            }

            // strip external items if required
            auto& impl_items = impl.get_impl_items();
            for (int i = 0; i < impl_items.size(); ) {
                auto& item = impl_items[i];

                // mark for stripping if required
                item->accept_vis(*this);

                if (item->is_marked_for_strip ())
                    impl_items.erase (impl_items.begin() + i);
                else
                    i++;
            }
        }
        void visit(AST::TraitImpl& impl) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(impl.get_outer_attrs());
            if (expander.fails_cfg(impl.get_outer_attrs())) {
                impl.mark_for_strip();
                return;
            }

            // strip test based on inner attrs
            expander.expand_cfg_attrs(impl.get_inner_attrs());
            if (expander.fails_cfg(impl.get_inner_attrs())) {
                impl.mark_for_strip();
                return;
            }

            // strip external items if required
            auto& impl_items = impl.get_impl_items();
            for (int i = 0; i < impl_items.size(); ) {
                auto& item = impl_items[i];

                // mark for stripping if required
                item->accept_vis(*this);

                if (item->is_marked_for_strip ())
                    impl_items.erase (impl_items.begin() + i);
                else
                    i++;
            }
        }
        void visit(AST::ExternalStaticItem& item) override {
            // strip test based on outer attrs
            expander.expand_cfg_attrs(item.get_outer_attrs());
            if (expander.fails_cfg(item.get_outer_attrs())) {
                item.mark_for_strip();
                return;
            }
        }
        void visit(AST::ExternalFunctionItem& item) override {
            // strip test based on outer attrs
            expander.expand_cfg_attrs(item.get_outer_attrs());
            if (expander.fails_cfg(item.get_outer_attrs())) {
                item.mark_for_strip();
                return;
            }

            /* strip function parameters if required - this is specifically 
             * allowed by spec */
            auto& params = item.get_function_params();
            for (int i = 0; i < params.size(); ) {
                auto& param_attrs = params[i].get_outer_attrs ();
                expander.expand_cfg_attrs(param_attrs);
                if (expander.fails_cfg (param_attrs))
                    params.erase (params.begin() + i);
                else
                    i++;
            }
            /* NOTE: these are extern function params, which may have different 
             * rules and restrictions to "normal" function params. So expansion 
             * handled separately. */

            /* TODO: assuming that variadic nature cannot be stripped. If this 
             * is not true, then have code here to do so. */
        }
        void visit(AST::ExternBlock& block) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(block.get_outer_attrs());
            if (expander.fails_cfg(block.get_outer_attrs())) {
                block.mark_for_strip();
                return;
            }

            // strip test based on inner attrs
            expander.expand_cfg_attrs(block.get_inner_attrs());
            if (expander.fails_cfg(block.get_inner_attrs())) {
                block.mark_for_strip();
                return;
            }

            // strip external items if required
            auto& extern_items = block.get_extern_items();
            for (int i = 0; i < extern_items.size(); ) {
                auto& item = extern_items[i];

                // mark for stripping if required
                item->accept_vis(*this);

                if (item->is_marked_for_strip ())
                    extern_items.erase (extern_items.begin() + i);
                else
                    i++;
            }
        }

        void visit(AST::MacroMatchFragment& match) override {}
        void visit(AST::MacroMatchRepetition& match) override {}
        void visit(AST::MacroMatcher& matcher) override {}
        void visit(AST::MacroRulesDefinition& rules_def) override {}
        void visit(AST::MacroInvocation& macro_invoc) override {}
        void visit(AST::MetaItemPath& meta_item) override {}
        void visit(AST::MetaItemSeq& meta_item) override {}
        void visit(AST::MetaWord& meta_item) override {}
        void visit(AST::MetaNameValueStr& meta_item) override {}
        void visit(AST::MetaListPaths& meta_item) override {}
        void visit(AST::MetaListNameValueStr& meta_item) override {}

        void visit(AST::LiteralPattern& pattern) override {}
        void visit(AST::IdentifierPattern& pattern) override {}
        void visit(AST::WildcardPattern& pattern) override {}
        void visit(AST::RangePatternBoundLiteral& bound) override {}
        void visit(AST::RangePatternBoundPath& bound) override {}
        void visit(AST::RangePatternBoundQualPath& bound) override {}
        void visit(AST::RangePattern& pattern) override {}
        void visit(AST::ReferencePattern& pattern) override {}
        void visit(AST::StructPatternFieldTuplePat& field) override {}
        void visit(AST::StructPatternFieldIdentPat& field) override {}
        void visit(AST::StructPatternFieldIdent& field) override {}
        void visit(AST::StructPattern& pattern) override {}
        void visit(AST::TupleStructItemsNoRange& tuple_items) override {}
        void visit(AST::TupleStructItemsRange& tuple_items) override {}
        void visit(AST::TupleStructPattern& pattern) override {}
        void visit(AST::TuplePatternItemsMultiple& tuple_items) override {}
        void visit(AST::TuplePatternItemsRanged& tuple_items) override {}
        void visit(AST::TuplePattern& pattern) override {}
        void visit(AST::GroupedPattern& pattern) override {}
        void visit(AST::SlicePattern& pattern) override {}

        void visit(AST::EmptyStmt& stmt) override {}
        void visit(AST::LetStmt& stmt) override {}
        void visit(AST::ExprStmtWithoutBlock& stmt) override {}
        void visit(AST::ExprStmtWithBlock& stmt) override {}

        void visit(AST::TraitBound& bound) override {}
        void visit(AST::ImplTraitType& type) override {}
        void visit(AST::TraitObjectType& type) override {}
        void visit(AST::ParenthesisedType& type) override {}
        void visit(AST::ImplTraitTypeOneBound& type) override {}
        void visit(AST::TraitObjectTypeOneBound& type) override {}
        void visit(AST::TupleType& type) override {}
        void visit(AST::NeverType& type) override {}
        void visit(AST::RawPointerType& type) override {}
        void visit(AST::ReferenceType& type) override {}
        void visit(AST::ArrayType& type) override {}
        void visit(AST::SliceType& type) override {}
        void visit(AST::InferredType& type) override {}
        void visit(AST::BareFunctionType& type) override {}
    };

    void MacroExpander::expand_invoc(std::unique_ptr<AST::MacroInvocation>& invoc) {
        /* if current expansion depth > recursion limit, create an error (maybe fatal
         * error) and return */

        /* switch on type of macro:
            - '!' syntax macro (inner switch)
                - procedural macro - "A token-based function-like macro"
                - 'macro_rules' (by example/pattern-match) macro? or not? "an
           AST-based function-like macro"
                - else is unreachable
            - attribute syntax macro (inner switch)
                - procedural macro attribute syntax - "A token-based attribute macro"
                - legacy macro attribute syntax? - "an AST-based attribute macro"
                - non-macro attribute: mark known
                - else is unreachable
            - derive macro (inner switch)
                - derive or legacy derive - "token-based" vs "AST-based"
                - else is unreachable
            - derive container macro - unreachable*/
    }

    /* Determines whether any cfg predicate is false and hence item with attributes should
     * be stripped.  */
    bool MacroExpander::fails_cfg(std::vector<AST::Attribute>& attrs) {
        for (auto& attr : attrs) {
            if (attr.get_path() == "cfg" && !attr.check_cfg_predicate(session))
                return true;
        }
        return false;
    }

    // Expands cfg_attr attributes.
    void MacroExpander::expand_cfg_attrs(std::vector<AST::Attribute>& attrs) {
        for (int i = 0; i < attrs.size();) {
            auto& attr = attrs[i];
            if (attr.get_path() == "cfg_attr") {
                if (attr.check_cfg_predicate(session)) {
                    // split off cfg_attr
                    std::vector<AST::Attribute> new_attrs = attr.separate_cfg_attrs();

                    // remove attr from vector
                    attrs.erase(attrs.begin() + i);

                    // add new attrs to vector
                    attrs.insert(attrs.begin() + i, std::make_move_iterator(new_attrs.begin()),
                      std::make_move_iterator(new_attrs.end()));
                }

                /* do something - if feature (first token in tree) is in fact enabled,
                 * make tokens listed afterwards into attributes. i.e.: for
                 * [cfg_attr(feature = "wow", wow1, wow2)], if "wow" is true, then add
                 * attributes [wow1] and [wow2] to attribute list. This can also be
                 * recursive, so check for expanded attributes being recursive and
                 * possibly recursively call the expand_attrs? */
            } else {
                i++;
            }
        }
        attrs.shrink_to_fit();
    }

    void MacroExpander::expand_crate() {
        /* fill macro/decorator map from init list? not sure where init list comes
         * from? */

        // TODO: does cfg apply for inner attributes? research.
        // the apparent answer (from playground test) is yes

        // expand crate cfg_attr attributes
        expand_cfg_attrs(crate.inner_attrs);

        if (fails_cfg(crate.inner_attrs)) {
            // basically, delete whole crate
            crate.strip_crate();
            // TODO: maybe create warning here? probably not desired behaviour
        }
        // expand module attributes?

        // expand attributes recursively
        AttrVisitor attr_visitor(*this);
        for (auto& i : crate.items) {
            i->accept_vis(attr_visitor);
        }
        // TODO: should recursive attribute and macro expansion be done in the same transversal? Or in
        // separate ones like currently?

        // expand module tree recursively

        // post-process

        // extract exported macros?
    }
} // namespace Rust
