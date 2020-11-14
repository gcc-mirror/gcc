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
            for (int i = 0; i < fields.size();) {
                auto& field_attrs = fields[i].get_outer_attrs();
                expander.expand_cfg_attrs(field_attrs);
                if (expander.fails_cfg(field_attrs))
                    fields.erase(fields.begin() + i);
                else
                    i++;
            }
        }

        void expand_tuple_fields(std::vector<AST::TupleField>& fields) {
            for (int i = 0; i < fields.size();) {
                auto& field_attrs = fields[i].get_outer_attrs();
                expander.expand_cfg_attrs(field_attrs);
                if (expander.fails_cfg(field_attrs))
                    fields.erase(fields.begin() + i);
                else
                    i++;
            }
        }

        void expand_function_params(std::vector<AST::FunctionParam>& params) {
            for (int i = 0; i < params.size();) {
                auto& param_attrs = params[i].get_outer_attrs();
                expander.expand_cfg_attrs(param_attrs);
                if (expander.fails_cfg(param_attrs))
                    params.erase(params.begin() + i);
                else
                    i++;
            }
        }

        void visit(AST::Token& tok) override {
            // shouldn't require?
        }
        void visit(AST::DelimTokenTree& delim_tok_tree) override {
            // shouldn't require?
        }
        void visit(AST::AttrInputMetaItemContainer& input) override {
            // shouldn't require?
        }
        void visit(AST::IdentifierExpr& ident_expr) override {
            // strip test based on outer attrs
            expander.expand_cfg_attrs(ident_expr.get_outer_attrs());
            if (expander.fails_cfg(ident_expr.get_outer_attrs())) {
                ident_expr.mark_for_strip();
                return;
            }
        }
        void visit(AST::Lifetime& lifetime) override {
            // shouldn't require?
        }
        void visit(AST::LifetimeParam& lifetime_param) override {
            // supposedly does not require - cfg does nothing
        }
        void visit(AST::MacroInvocationSemi& macro_invoc) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(macro_invoc.get_outer_attrs());
            if (expander.fails_cfg(macro_invoc.get_outer_attrs())) {
                macro_invoc.mark_for_strip();
                return;
            }

            // I don't think any macro token trees can be stripped in any way
        }

        void visit(AST::PathInExpression& path) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(path.get_outer_attrs());
            if (expander.fails_cfg(path.get_outer_attrs())) {
                path.mark_for_strip();
                return;
            }
        }
        void visit(AST::TypePathSegment& segment) override {
            // shouldn't require?
        }
        void visit(AST::TypePathSegmentGeneric& segment) override {
            // shouldn't require?
        }
        void visit(AST::TypePathSegmentFunction& segment) override {
            // shouldn't require?
        }
        void visit(AST::TypePath& path) override {
            // shouldn't require?
        }
        void visit(AST::QualifiedPathInExpression& path) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(path.get_outer_attrs());
            if (expander.fails_cfg(path.get_outer_attrs())) {
                path.mark_for_strip();
                return;
            }
        }
        void visit(AST::QualifiedPathInType& path) override {
            // shouldn't require?
        }

        void visit(AST::LiteralExpr& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }
        }
        void visit(AST::AttrInputLiteral& attr_input) override {
            // shouldn't require?
        }
        void visit(AST::MetaItemLitExpr& meta_item) override {
            // shouldn't require?
        }
        void visit(AST::MetaItemPathLit& meta_item) override {
            // shouldn't require?
        }
        void visit(AST::BorrowExpr& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* strip any internal sub-expressions - expression itself isn't
             * allowed to have external attributes in this position so can't be
             * stripped. */
            expr.get_borrowed_expr()->accept_vis(*this);
        }
        void visit(AST::DereferenceExpr& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* strip any internal sub-expressions - expression itself isn't
             * allowed to have external attributes in this position so can't be
             * stripped. */
            expr.get_dereferenced_expr()->accept_vis(*this);
        }
        void visit(AST::ErrorPropagationExpr& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* strip any internal sub-expressions - expression itself isn't
             * allowed to have external attributes in this position so can't be
             * stripped. */
            expr.get_propagating_expr()->accept_vis(*this);
        }
        void visit(AST::NegationExpr& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* strip any internal sub-expressions - expression itself isn't
             * allowed to have external attributes in this position so can't be
             * stripped. */
            expr.get_negated_expr()->accept_vis(*this);
        }
        void visit(AST::ArithmeticOrLogicalExpr& expr) override {
            /* outer attributes never allowed before these. while cannot strip 
             * two direct descendant expressions, can strip ones below that */

            /* should have no possibility for outer attrs as would be parsed 
             * with outer expr */
            expr.get_left_expr()->accept_vis(*this);
            /* should syntactically not have outer attributes, though this may 
             * not have worked in practice */
            expr.get_right_expr()->accept_vis(*this);

            // ensure that they are not marked for strip
            rust_assert(!expr.get_left_expr()->is_marked_for_strip());
            rust_assert(!expr.get_right_expr()->is_marked_for_strip());
        }
        void visit(AST::ComparisonExpr& expr) override {
            /* outer attributes never allowed before these. while cannot strip 
             * two direct descendant expressions, can strip ones below that */

            /* should have no possibility for outer attrs as would be parsed 
             * with outer expr */
            expr.get_left_expr()->accept_vis(*this);
            /* should syntactically not have outer attributes, though this may 
             * not have worked in practice */
            expr.get_right_expr()->accept_vis(*this);

            // ensure that they are not marked for strip
            rust_assert(!expr.get_left_expr()->is_marked_for_strip());
            rust_assert(!expr.get_right_expr()->is_marked_for_strip());
        }
        void visit(AST::LazyBooleanExpr& expr) override {
            /* outer attributes never allowed before these. while cannot strip 
             * two direct descendant expressions, can strip ones below that */

            /* should have no possibility for outer attrs as would be parsed 
             * with outer expr */
            expr.get_left_expr()->accept_vis(*this);
            /* should syntactically not have outer attributes, though this may 
             * not have worked in practice */
            expr.get_right_expr()->accept_vis(*this);

            // ensure that they are not marked for strip
            rust_assert(!expr.get_left_expr()->is_marked_for_strip());
            rust_assert(!expr.get_right_expr()->is_marked_for_strip());
        }
        void visit(AST::TypeCastExpr& expr) override {
            /* outer attributes never allowed before these. while cannot strip 
             * two direct descendant expressions, can strip ones below that */

            /* should have no possibility for outer attrs as would be parsed 
             * with outer expr */
            expr.get_casted_expr()->accept_vis(*this);

            // ensure that they are not marked for strip
            rust_assert(!expr.get_casted_expr()->is_marked_for_strip());
        }
        void visit(AST::AssignmentExpr& expr) override {
            /* outer attributes never allowed before these. while cannot strip 
             * two direct descendant expressions, can strip ones below that */

            /* should have no possibility for outer attrs as would be parsed 
             * with outer expr */
            expr.get_left_expr()->accept_vis(*this);
            /* should syntactically not have outer attributes, though this may 
             * not have worked in practice */
            expr.get_right_expr()->accept_vis(*this);

            // ensure that they are not marked for strip
            rust_assert(!expr.get_left_expr()->is_marked_for_strip());
            rust_assert(!expr.get_right_expr()->is_marked_for_strip());
        }
        void visit(AST::CompoundAssignmentExpr& expr) override {
            /* outer attributes never allowed before these. while cannot strip 
             * two direct descendant expressions, can strip ones below that */

            /* should have no possibility for outer attrs as would be parsed 
             * with outer expr */
            expr.get_left_expr()->accept_vis(*this);
            /* should syntactically not have outer attributes, though this may 
             * not have worked in practice */
            expr.get_right_expr()->accept_vis(*this);

            // ensure that they are not marked for strip
            rust_assert(!expr.get_left_expr()->is_marked_for_strip());
            rust_assert(!expr.get_right_expr()->is_marked_for_strip());
        }
        void visit(AST::GroupedExpr& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* strip test based on inner attrs - spec says these are inner 
             * attributes, not outer attributes of inner expr */
            expander.expand_cfg_attrs(expr.get_inner_attrs());
            if (expander.fails_cfg(expr.get_inner_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* strip any internal sub-expressions - expression itself isn't
             * allowed to have external attributes in this position so can't be
             * stripped. */
            expr.get_expr_in_parens()->accept_vis(*this);
        }
        void visit(AST::ArrayElemsValues& elems) override {
            /* apparently outer attributes are allowed in "elements of array 
             * expressions" according to spec */
            auto& values = elems.get_values();
            for (int i = 0; i < values.size();) {
                auto& value = values[i];

                // mark for stripping if required
                value->accept_vis(*this);

                if (value->is_marked_for_strip())
                    values.erase(values.begin() + i);
                else
                    i++;
            }
        }
        void visit(AST::ArrayElemsCopied& elems) override {
            /* apparently outer attributes are allowed in "elements of array 
             * expressions" according to spec. on the other hand, it would not
             * make conceptual sense to be able to remove either expression. As
             * such, not implementing. TODO clear up the ambiguity here */

            // only intend stripping for internal sub-expressions
            elems.get_elem_to_copy()->accept_vis(*this);
            elems.get_num_copies()->accept_vis(*this);
        }
        void visit(AST::ArrayExpr& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* strip test based on inner attrs - spec says there are separate 
             * inner attributes, not just outer attributes of inner exprs */
            expander.expand_cfg_attrs(expr.get_inner_attrs());
            if (expander.fails_cfg(expr.get_inner_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* assuming you can't strip away the ArrayElems type, but can strip 
             * internal expressions and whatever */
            if (expr.has_array_elems())
                expr.get_array_elems()->accept_vis(*this);
        }
        void visit(AST::ArrayIndexExpr& expr) override {
            /* it is unclear whether outer attributes are supposed to be 
             * allowed, but conceptually it wouldn't make much sense, so 
             * assuming no. TODO */

            /* strip any internal sub-expressions - expression itself isn't
             * allowed to have external attributes in this position so can't be
             * stripped. */
            expr.get_array_expr()->accept_vis(*this);
            expr.get_index_expr()->accept_vis(*this);
        }
        void visit(AST::TupleExpr& expr) override {
            /* according to spec, outer attributes are allowed on "elements of 
             * tuple expressions" */

            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* strip test based on inner attrs - spec says these are inner 
             * attributes, not outer attributes of inner expr */
            expander.expand_cfg_attrs(expr.get_inner_attrs());
            if (expander.fails_cfg(expr.get_inner_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* apparently outer attributes are allowed in "elements of tuple 
             * expressions" according to spec */
            auto& values = expr.get_tuple_elems();
            for (int i = 0; i < values.size();) {
                auto& value = values[i];

                // mark for stripping if required
                value->accept_vis(*this);

                if (value->is_marked_for_strip())
                    values.erase(values.begin() + i);
                else
                    i++;
            }
        }
        void visit(AST::TupleIndexExpr& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* wouldn't strip this directly (as outer attrs should be 
             * associated with this level), but any sub-expressions would be 
             * stripped. Thus, no need to erase when strip check called. */
            expr.get_tuple_expr()->accept_vis(*this);
        }
        void visit(AST::StructExprStruct& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* strip test based on inner attrs - spec says these are inner 
             * attributes, not outer attributes of inner expr */
            expander.expand_cfg_attrs(expr.get_inner_attrs());
            if (expander.fails_cfg(expr.get_inner_attrs())) {
                expr.mark_for_strip();
                return;
            }  
        }
        void visit(AST::StructExprFieldIdentifier& field) override {
            // as no attrs (at moment, at least), no stripping possible
        }
        void visit(AST::StructExprFieldIdentifierValue& field) override {
            /* as no attrs possible (at moment, at least), only sub-expression
             * stripping is possible */
            field.get_value()->accept_vis(*this);
        }
        void visit(AST::StructExprFieldIndexValue& field) override {
            /* as no attrs possible (at moment, at least), only sub-expression
             * stripping is possible */
            field.get_value()->accept_vis(*this);
        }
        void visit(AST::StructExprStructFields& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* strip test based on inner attrs - spec says these are inner 
             * attributes, not outer attributes of inner expr */
            expander.expand_cfg_attrs(expr.get_inner_attrs());
            if (expander.fails_cfg(expr.get_inner_attrs())) {
                expr.mark_for_strip();
                return;
            }  

            /* spec does not specify whether expressions are allowed to be 
             * stripped at top level of struct fields, but I wouldn't think 
             * that they would be, so operating under the assumption that only 
             * sub-expressions can be stripped. */
            for (auto& field : expr.get_fields()) {
                field->accept_vis(*this);
                // shouldn't strip in this
            }

            /* struct base presumably can't be stripped, as the '..' is before
             * the expression. as such, can only strip sub-expressions. */
            if (expr.has_struct_base())
                expr.get_struct_base().get_base_struct()->accept_vis(*this);
        }
        void visit(AST::StructExprStructBase& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* strip test based on inner attrs - spec says these are inner 
             * attributes, not outer attributes of inner expr */
            expander.expand_cfg_attrs(expr.get_inner_attrs());
            if (expander.fails_cfg(expr.get_inner_attrs())) {
                expr.mark_for_strip();
                return;
            }  

            /* struct base presumably can't be stripped, as the '..' is before
             * the expression. as such, can only strip sub-expressions. */
            rust_assert(!expr.get_struct_base().is_invalid());
            expr.get_struct_base().get_base_struct()->accept_vis(*this);
        }
        void visit(AST::StructExprTuple& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* strip test based on inner attrs - spec says these are inner 
             * attributes, not outer attributes of inner expr */
            expander.expand_cfg_attrs(expr.get_inner_attrs());
            if (expander.fails_cfg(expr.get_inner_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* spec says outer attributes are specifically allowed for elements 
             * of tuple-style struct expressions, so full stripping possible */
            auto& values = expr.get_elems();
            for (int i = 0; i < values.size();) {
                auto& value = values[i];

                // mark for stripping if required
                value->accept_vis(*this);

                if (value->is_marked_for_strip())
                    values.erase(values.begin() + i);
                else
                    i++;
            }
        }
        void visit(AST::StructExprUnit& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }
        }
        void visit(AST::EnumExprFieldIdentifier& field) override {
            // as no attrs (at moment, at least), no stripping possible
        }
        void visit(AST::EnumExprFieldIdentifierValue& field) override {
            /* as no attrs possible (at moment, at least), only sub-expression
             * stripping is possible */
            field.get_value()->accept_vis(*this);
        }
        void visit(AST::EnumExprFieldIndexValue& field) override {
            /* as no attrs possible (at moment, at least), only sub-expression
             * stripping is possible */
            field.get_value()->accept_vis(*this);
        }
        void visit(AST::EnumExprStruct& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            // supposedly spec doesn't allow inner attributes in enum exprs

            /* spec does not specify whether expressions are allowed to be 
             * stripped at top level of expression fields, but I wouldn't think
             * that they would be, so operating under the assumption that only 
             * sub-expressions can be stripped. */
            for (auto& field : expr.get_fields()) {
                field->accept_vis(*this);
                // shouldn't strip in this
            }
        }
        void visit(AST::EnumExprTuple& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            // supposedly spec doesn't allow inner attributes in enum exprs

            /* spec says outer attributes are specifically allowed for elements 
             * of tuple-style enum expressions, so full stripping possible */
            auto& values = expr.get_elems();
            for (int i = 0; i < values.size();) {
                auto& value = values[i];

                // mark for stripping if required
                value->accept_vis(*this);

                if (value->is_marked_for_strip())
                    values.erase(values.begin() + i);
                else
                    i++;
            }
        }
        void visit(AST::EnumExprFieldless& expr) override {
            // can't be stripped as no attrs
        }
        void visit(AST::CallExpr& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* should not be outer attrs on "function" expression - outer attrs 
             * should be associated with call expr as a whole. only sub-expr 
             * expansion is possible. */
            expr.get_function_expr()->accept_vis(*this);

            /* spec says outer attributes are specifically allowed for elements 
             * of call expressions, so full stripping possible */
            auto& params = expr.get_params();
            for (int i = 0; i < params.size();) {
                auto& param = params[i];

                // mark for stripping if required
                param->accept_vis(*this);

                if (param->is_marked_for_strip())
                    params.erase(params.begin() + i);
                else
                    i++;
            }
        }
        void visit(AST::MethodCallExpr& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* should not be outer attrs on "receiver" expression - outer attrs 
             * should be associated with call expr as a whole. only sub-expr 
             * expansion is possible. */
            expr.get_receiver_expr()->accept_vis(*this);

            // no outer attrs on paths possible

            /* spec says outer attributes are specifically allowed for elements 
             * of method call expressions, so full stripping possible */
            auto& params = expr.get_params();
            for (int i = 0; i < params.size();) {
                auto& param = params[i];

                // mark for stripping if required
                param->accept_vis(*this);

                if (param->is_marked_for_strip())
                    params.erase(params.begin() + i);
                else
                    i++;
            }
        }
        void visit(AST::FieldAccessExpr& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* should not be outer attrs on "receiver" expression - outer attrs 
             * should be associated with call expr as a whole. only sub-expr 
             * expansion is possible. */
            expr.get_receiver_expr()->accept_vis(*this);
        }
        void visit(AST::ClosureExprInner& expr) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(expr.get_outer_attrs());
            if (expander.fails_cfg(expr.get_outer_attrs())) {
                expr.mark_for_strip();
                return;
            }

            /* strip closure parameters if required - this is specifically
             * allowed by spec */
            auto& params = expr.get_params();
            for (int i = 0; i < params.size();) {
                auto& param_attrs = params[i].get_outer_attrs();
                expander.expand_cfg_attrs(param_attrs);
                if (expander.fails_cfg(param_attrs))
                    params.erase(params.begin() + i);
                else
                    i++;
            }
        }
        void visit(AST::BlockExpr& expr) override {
            
        }
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

        void visit(AST::TypeParam& param) override {
            // shouldn't require?
        }
        void visit(AST::LifetimeWhereClauseItem& item) override {
            // shouldn't require?
        }
        void visit(AST::TypeBoundWhereClauseItem& item) override {
            // shouldn't require?
        }
        void visit(AST::Method& method) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(method.get_outer_attrs());
            if (expander.fails_cfg(method.get_outer_attrs())) {
                method.mark_for_strip();
                return;
            }

            /* assuming you can't strip self param - wouldn't be a method
             * anymore. spec allows outer attrs on self param, but doesn't
             * specify whether cfg is used. */

            /* strip method parameters if required - this is specifically
             * allowed by spec */
            expand_function_params(method.get_function_params());

            /* body should always exist - if error state, should have returned
             * before now */
            method.get_definition()->accept_vis(*this);
            // TODO: can block as a whole be invalidated here? Assuming no
        }
        void visit(AST::ModuleBodied& module) override {
            // strip test based on outer attrs
            expander.expand_cfg_attrs(module.get_outer_attrs());
            if (expander.fails_cfg(module.get_outer_attrs())) {
                module.mark_for_strip();
                return;
            }

            // strip test based on inner attrs
            expander.expand_cfg_attrs(module.get_inner_attrs());
            if (expander.fails_cfg(module.get_inner_attrs())) {
                module.mark_for_strip();
                return;
            }

            // strip items if required
            auto& items = module.get_items();
            for (int i = 0; i < items.size();) {
                auto& item = items[i];

                // mark for stripping if required
                item->accept_vis(*this);

                if (item->is_marked_for_strip())
                    items.erase(items.begin() + i);
                else
                    i++;
            }
        }
        void visit(AST::ModuleNoBody& module) override {
            // strip test based on outer attrs
            expander.expand_cfg_attrs(module.get_outer_attrs());
            if (expander.fails_cfg(module.get_outer_attrs())) {
                module.mark_for_strip();
                return;
            }
        }
        void visit(AST::ExternCrate& crate) override {
            // strip test based on outer attrs
            expander.expand_cfg_attrs(crate.get_outer_attrs());
            if (expander.fails_cfg(crate.get_outer_attrs())) {
                crate.mark_for_strip();
                return;
            }
        }
        void visit(AST::UseTreeGlob& use_tree) override {
            // shouldn't require?
        }
        void visit(AST::UseTreeList& use_tree) override {
            // shouldn't require?
        }
        void visit(AST::UseTreeRebind& use_tree) override {
            // shouldn't require?
        }
        void visit(AST::UseDeclaration& use_decl) override {
            // strip test based on outer attrs
            expander.expand_cfg_attrs(use_decl.get_outer_attrs());
            if (expander.fails_cfg(use_decl.get_outer_attrs())) {
                use_decl.mark_for_strip();
                return;
            }
        }
        void visit(AST::Function& function) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(function.get_outer_attrs());
            if (expander.fails_cfg(function.get_outer_attrs())) {
                function.mark_for_strip();
                return;
            }

            /* strip function parameters if required - this is specifically
             * allowed by spec */
            expand_function_params(function.get_function_params());

            /* body should always exist - if error state, should have returned
             * before now */
            function.get_definition()->accept_vis(*this);
            // TODO: can block as a whole be invalidated here? Assuming no
        }
        void visit(AST::TypeAlias& type_alias) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(type_alias.get_outer_attrs());
            if (expander.fails_cfg(type_alias.get_outer_attrs())) {
                type_alias.mark_for_strip();
                return;
            }
        }
        void visit(AST::StructStruct& struct_item) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(struct_item.get_outer_attrs());
            if (expander.fails_cfg(struct_item.get_outer_attrs())) {
                struct_item.mark_for_strip();
                return;
            }

            /* strip struct fields if required - this is presumably
             * allowed by spec */
            expand_struct_fields(struct_item.get_fields());
        }
        void visit(AST::TupleStruct& tuple_struct) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(tuple_struct.get_outer_attrs());
            if (expander.fails_cfg(tuple_struct.get_outer_attrs())) {
                tuple_struct.mark_for_strip();
                return;
            }

            /* strip struct fields if required - this is presumably
             * allowed by spec */
            expand_tuple_fields(tuple_struct.get_fields());
        }
        void visit(AST::EnumItem& item) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(item.get_outer_attrs());
            if (expander.fails_cfg(item.get_outer_attrs())) {
                item.mark_for_strip();
                return;
            }
        }
        void visit(AST::EnumItemTuple& item) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(item.get_outer_attrs());
            if (expander.fails_cfg(item.get_outer_attrs())) {
                item.mark_for_strip();
                return;
            }

            /* strip item fields if required - this is presumably
             * allowed by spec */
            expand_tuple_fields(item.get_tuple_fields());
        }
        void visit(AST::EnumItemStruct& item) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(item.get_outer_attrs());
            if (expander.fails_cfg(item.get_outer_attrs())) {
                item.mark_for_strip();
                return;
            }

            /* strip item fields if required - this is presumably
             * allowed by spec */
            expand_struct_fields(item.get_struct_fields());
        }
        void visit(AST::EnumItemDiscriminant& item) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(item.get_outer_attrs());
            if (expander.fails_cfg(item.get_outer_attrs())) {
                item.mark_for_strip();
                return;
            }

            /* strip any internal sub-expressions - expression itself isn't
             * allowed to have external attributes in this position so can't be
             * stripped. */
            item.get_expr()->accept_vis(*this);
        }
        void visit(AST::Enum& enum_item) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(enum_item.get_outer_attrs());
            if (expander.fails_cfg(enum_item.get_outer_attrs())) {
                enum_item.mark_for_strip();
                return;
            }

            /* strip enum fields if required - this is presumably
             * allowed by spec */
            auto& variants = enum_item.get_variants();
            for (int i = 0; i < variants.size();) {
                auto& variant = variants[i];

                // mark for stripping if required
                variant->accept_vis(*this);

                if (variant->is_marked_for_strip())
                    variants.erase(variants.begin() + i);
                else
                    i++;
            }
        }
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

            /* strip any internal sub-expressions - expression itself isn't
             * allowed to have external attributes in this position so can't be
             * stripped. */
            const_item.get_expr()->accept_vis(*this);
        }
        void visit(AST::StaticItem& static_item) override {
            // initial test based on outer attrs
            expander.expand_cfg_attrs(static_item.get_outer_attrs());
            if (expander.fails_cfg(static_item.get_outer_attrs())) {
                static_item.mark_for_strip();
                return;
            }

            /* strip any internal sub-expressions - expression itself isn't
             * allowed to have external attributes in this position so can't be
             * stripped. */
            static_item.get_expr()->accept_vis(*this);
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

            /* strip any internal sub-expressions - expression itself isn't
             * allowed to have external attributes in this position so can't be
             * stripped */
            if (item.has_expression())
                item.get_expr()->accept_vis(*this);
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
            for (int i = 0; i < trait_items.size();) {
                auto& item = trait_items[i];

                // mark for stripping if required
                item->accept_vis(*this);

                if (item->is_marked_for_strip())
                    trait_items.erase(trait_items.begin() + i);
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

            // strip inherent impl items if required
            auto& impl_items = impl.get_impl_items();
            for (int i = 0; i < impl_items.size();) {
                auto& item = impl_items[i];

                // mark for stripping if required
                item->accept_vis(*this);

                if (item->is_marked_for_strip())
                    impl_items.erase(impl_items.begin() + i);
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

            // strip trait impl items if required
            auto& impl_items = impl.get_impl_items();
            for (int i = 0; i < impl_items.size();) {
                auto& item = impl_items[i];

                // mark for stripping if required
                item->accept_vis(*this);

                if (item->is_marked_for_strip())
                    impl_items.erase(impl_items.begin() + i);
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
            for (int i = 0; i < params.size();) {
                auto& param_attrs = params[i].get_outer_attrs();
                expander.expand_cfg_attrs(param_attrs);
                if (expander.fails_cfg(param_attrs))
                    params.erase(params.begin() + i);
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
            for (int i = 0; i < extern_items.size();) {
                auto& item = extern_items[i];

                // mark for stripping if required
                item->accept_vis(*this);

                if (item->is_marked_for_strip())
                    extern_items.erase(extern_items.begin() + i);
                else
                    i++;
            }
        }

        void visit(AST::MacroMatchFragment& match) override {}
        void visit(AST::MacroMatchRepetition& match) override {}
        void visit(AST::MacroMatcher& matcher) override {}
        void visit(AST::MacroRulesDefinition& rules_def) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(rules_def.get_outer_attrs());
            if (expander.fails_cfg(rules_def.get_outer_attrs())) {
                rules_def.mark_for_strip();
                return;
            }

            // I don't think any macro rules can be stripped in any way
        }
        void visit(AST::MacroInvocation& macro_invoc) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(macro_invoc.get_outer_attrs());
            if (expander.fails_cfg(macro_invoc.get_outer_attrs())) {
                macro_invoc.mark_for_strip();
                return;
            }

            // I don't think any macro token trees can be stripped in any way
        }
        void visit(AST::MetaItemPath& meta_item) override {}
        void visit(AST::MetaItemSeq& meta_item) override {}
        void visit(AST::MetaWord& meta_item) override {}
        void visit(AST::MetaNameValueStr& meta_item) override {}
        void visit(AST::MetaListPaths& meta_item) override {}
        void visit(AST::MetaListNameValueStr& meta_item) override {}

        // stripping shouldn't be required or possible for patterns
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

        void visit(AST::EmptyStmt& stmt) override {
            // assuming no outer attributes, so nothing can happen
        }
        void visit(AST::LetStmt& stmt) override {
            // initial strip test based on outer attrs
            expander.expand_cfg_attrs(stmt.get_outer_attrs());
            if (expander.fails_cfg(stmt.get_outer_attrs())) {
                stmt.mark_for_strip();
                return;
            }

            /* strip any internal sub-expressions - expression itself isn't
             * allowed to have external attributes in this position so can't be
             * stripped */
            if (stmt.has_init_expr())
                stmt.get_init_expr()->accept_vis(*this);
        }
        void visit(AST::ExprStmtWithoutBlock& stmt) override {
            // outer attributes associated with expr, so rely on expr

            // guard - should prevent null pointer expr
            if (stmt.is_marked_for_strip())
                return;

            // strip if expr is to be stripped
            auto& expr = stmt.get_expr();
            expr->accept_vis(*this);
            if (expr->is_marked_for_strip()) {
                stmt.mark_for_strip();
                return;
            }
        }
        void visit(AST::ExprStmtWithBlock& stmt) override {
            // outer attributes associated with expr, so rely on expr

            // guard - should prevent null pointer expr
            if (stmt.is_marked_for_strip())
                return;

            // strip if expr is to be stripped
            auto& expr = stmt.get_expr();
            expr->accept_vis(*this);
            if (expr->is_marked_for_strip()) {
                stmt.mark_for_strip();
                return;
            }
        }

        // stripping shouldn't be required or possible for types
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

        // expand attributes recursively and strip items if required
        AttrVisitor attr_visitor(*this);
        auto& items = crate.items;
        for (int i = 0; i < items.size();) {
            auto& item = items[i];

            // mark for stripping if required
            item->accept_vis(attr_visitor);

            if (item->is_marked_for_strip())
                items.erase(items.begin() + i);
            else
                i++;
        }
        // TODO: should recursive attribute and macro expansion be done in the same transversal? Or in
        // separate ones like currently?

        // expand module tree recursively

        // post-process

        // extract exported macros?
    }
} // namespace Rust
