#include "rust-ast-full.h"
// Dummy implementations of as_string() for now - will complete later for debugging purposes

namespace Rust {
    namespace AST {
        ::std::string Crate::as_string() const {
            ::std::string str("Crate: ");
            // add utf8bom and shebang
            if (has_utf8bom) {
                str += "\n has utf8bom";
            }
            if (has_shebang) {
                str += "\n has shebang";
            }

            // inner attributes
            str += "\n inner attributes: ";
            if (inner_attrs.empty()) {
                str += "none";
            } else {
                // note that this does not print them with "inner attribute" syntax - just the body
                for (const auto& attr : inner_attrs) {
                    str += "\n  " + attr.as_string();
                }
            }

            // items
            str += "\n items: ";
            if (items.empty()) {
                str += "none";
            } else {
                for (const auto& item : items) {
                    str += "\n  " + item->as_string();
                }
            }

            return str + "\n";
        }

        ::std::string Attribute::as_string() const {
            ::std::string path_str = path.as_string();
            if (attr_input == NULL) {
                return path_str;
            } else {
                return path_str + attr_input->as_string();
            }
        }

        ::std::string DelimTokenTree::as_string() const {
            ::std::string start_delim;
            ::std::string end_delim;
            switch (delim_type) {
                case PARENS:
                    start_delim = "(";
                    end_delim = ")";
                    break;
                case SQUARE:
                    start_delim = "[";
                    end_delim = "]";
                    break;
                case CURLY:
                    start_delim = "{";
                    end_delim = "}";
                    break;
                default:
                    // error
                    return "";
            }
            ::std::string str = start_delim;
            if (token_trees.empty()) {
                str += "none";
            } else {
                for (const auto& tree : token_trees) {
                    str += tree->as_string() + ", ";
                }
            }
            str += end_delim;

            return str;
        }

        ::std::string Token::as_string() const {
            /* FIXME: only works when not identifier or literal or whatever, i.e. when doesn't store
             * string value */
            //return get_token_description(token_id);

            // maybe fixed - stores everything as string though, so storage-inefficient
            return str;
        }

        ::std::string SimplePathSegment::as_string() const {
            return segment_name;
        }

        ::std::string SimplePath::as_string() const {
            ::std::string path;
            if (has_opening_scope_resolution) {
                path = "::";
            }

            // crappy hack because doing proper for loop would be more code
            bool first_time = true;
            for (const auto& segment : segments) {
                if (first_time) {
                    path += segment.as_string();
                    first_time = false;
                } else {
                    path += "::" + segment.as_string();
                }

                // DEBUG: remove later. Checks for path error.
                if (segment.is_error()) {
                    fprintf(stderr, "segment in path is error - this should've been filtered out. first segment was '%s' \n", segments.at(0).as_string().c_str());
                }
            }

            return path;
        }

        ::std::string Visibility::as_string() const {
            switch (public_vis_type) {
                case NONE:
                    return ::std::string("pub");
                case CRATE:
                    return ::std::string("ub(crate)");
                case SELF:
                    return ::std::string("pub(self)");
                case SUPER:
                    return ::std::string("pub(super)");
                case IN_PATH:
                    return ::std::string("pub(in ") + in_path.as_string() + ::std::string(")");
                default:
                    gcc_unreachable();
            }
        }

        // Creates a string that reflects the visibility stored.
        ::std::string VisItem::as_string() const {
            // FIXME: can't do formatting on string to make identation occur. 
            ::std::string str = Item::as_string();

            if (has_visibility()) {
                str = visibility.as_string() + " ";
            }

            return str;
        }

        // Creates a string that reflects the outer attributes stored.
        ::std::string Item::as_string() const {
            ::std::string str;
            
            if (!outer_attrs.empty()) {
                for (const auto& attr : outer_attrs) {
                    str += attr.as_string() + "\n";
                }
            }

            return str;
        }

        ::std::string ModuleBodied::as_string() const {
            ::std::string vis_item = VisItem::as_string();

            return ::std::string("not implemented");
        }

        ::std::string ModuleNoBody::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string StaticItem::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string ExternCrate::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string TupleStruct::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string ConstantItem::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string InherentImpl::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string StructStruct::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string UseDeclaration::as_string() const {
            ::std::string str = VisItem::as_string();

            str += "use " + use_tree->as_string();

            return str;
        }

        ::std::string UseTreeGlob::as_string() const {
            switch (glob_type) {
                case NO_PATH:
                    return "*";
                case GLOBAL:
                    return "::*";
                case PATH_PREFIXED: {
                    ::std::string path_str = path.as_string();
                    return path_str + "::*";
                }
                default:
                    // some kind of error
                    return "ERROR-PATH";
            }
            gcc_unreachable();
        }

        ::std::string UseTreeList::as_string() const {
            ::std::string path_str;
            switch (path_type) {
                case NO_PATH:
                    path_str = "{";
                    break;
                case GLOBAL:
                    path_str = "::{";
                    break;
                case PATH_PREFIXED: {
                    path_str = path.as_string() + "::{";
                    break;
                }
                default:
                    // some kind of error
                    return "ERROR-PATH-LIST";
            }

            if (has_trees()) {
                for (const auto& tree : trees) {
                    path_str += tree->as_string() + ", ";
                }
            } else {
                path_str += "none";
            }

            return path_str + "}";
        }

        ::std::string UseTreeRebind::as_string() const {
            ::std::string path_str = path.as_string();

            switch (bind_type) {
                case NONE:
                    // nothing to add, just path
                    break;
                case IDENTIFIER:
                    path_str += " as " + identifier;
                    break;
                case WILDCARD:
                    path_str += " as _";
                    break;
                default:
                    // error
                    return "ERROR-PATH-REBIND";
            }

            return path_str;
        }

        ::std::string Enum::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string Trait::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string Union::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string Function::as_string() const {
            ::std::string str = VisItem::as_string() + "Function: ";
            ::std::string qualifiers_str = qualifiers.as_string();

            ::std::string generic_params_str("Generic params: ");
            if (has_generics()) {
                for (const auto& generic_param : generic_params) {
                    generic_params_str += generic_param->as_string() + ", ";
                }
            } else {
                generic_params_str += "none";
            }

            ::std::string function_params_str("Function params: ");
            if (has_function_params()) {
                for (const auto& param : function_params) {
                    function_params_str += param.as_string() + ", ";
                }
            } else {
                function_params_str += "none";
            }

            ::std::string return_type_str("Return type: ");
            if (has_function_return_type()) {
                return_type_str += return_type->as_string();
            } else {
                return_type_str += "none (void)";
            }

            ::std::string where_clause_str("Where clause: ");
            if (has_where_clause()) {
                where_clause_str += where_clause.as_string();
            } else {
                where_clause_str += "none";
            }

            ::std::string body_str = "Body: " + function_body->as_string();

            str += "\n   " + qualifiers_str + "\n   " + generic_params_str + "\n   " + function_params_str + "\n   " + return_type_str + "\n   " + where_clause_str + "\n   " + body_str;

            return str;
        }

        ::std::string WhereClause::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string BlockExpr::as_string() const {
            ::std::string str = "BlockExpr: ";

            // get outer attributes
            str += "\n " + Expr::as_string();
            
            // inner attributes
            str += "\n inner attributes: ";
            if (inner_attrs.empty()) {
                str += "none";
            } else {
                // note that this does not print them with "inner attribute" syntax - just the body
                for (const auto& attr : inner_attrs) {
                    str += "\n  " + attr.as_string();
                }
            }

            // statements
            str += "\n statements: ";
            if (statements.empty()) {
                str += "none";
            } else {
                for (const auto& stmt : statements) {
                    str += "\n  " + stmt->as_string();
                }
            }

            // final expression
            str += "\n final expression: ";
            if (expr == NULL) {
                str += "none";
            } else {
                str += "\n  " + expr->as_string();
            }

            return str;
        }

        ::std::string TraitImpl::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string TypeAlias::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string MacroInvocationSemi::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string ExternBlock::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string MacroRulesDefinition::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string MacroInvocation::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string PathInExpression::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string ExprStmtWithBlock::as_string() const {
            ::std::string str("ExprStmtWithBlock: \n ");

            if (expr == NULL) {
                str += "none (this should not happen and is an error)";
            } else {
                str += expr->as_string();
            }

            return str;
        }

        ::std::string ClosureExprInnerTyped::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string QualifiedPathInExpression::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string BorrowExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string ReturnExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string GroupedExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string RangeToExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string ContinueExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string NegationExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string RangeFromExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string RangeFullExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string WhileLoopExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string ArrayIndexExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string AssignmentExpr::as_string() const {
            ::std::string str("AssignmentExpr: ");

            if (main_or_left_expr == NULL || right_expr == NULL) {
                str += "error (either or both expressions are null)";
            } else {
                // left expr
                str += "\n left: " + main_or_left_expr->as_string();

                // right expr
                str += "\n right: " + right_expr->as_string();
            }

            return str;
        }

        ::std::string AsyncBlockExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string ComparisonExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string IfExprConseqIf::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string MethodCallExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string TupleIndexExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string DereferenceExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string FieldAccessExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string LazyBooleanExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string RangeFromToExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string RangeToInclExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string UnsafeBlockExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string ClosureExprInner::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string IfExprConseqElse::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string WhileLetLoopExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string IfExprConseqIfLet::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string IfLetExprConseqIf::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string IfLetExprConseqElse::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string RangeFromToInclExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string ErrorPropogationExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string IfLetExprConseqIfLet::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string CompoundAssignmentExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string ArithmeticOrLogicalExpr::as_string() const {
            ::std::string operator_str;
            operator_str.reserve(1);

            // get operator string
            switch (expr_type) {
                case ADD:
                    operator_str = "+";
                    break;
                case SUBTRACT: 
                    operator_str = "-";
                    break;
                case MULTIPLY:
                    operator_str = "*";
                    break;
                case DIVIDE:
                    operator_str = "/";
                    break;
                case MODULUS:
                    operator_str = "%";
                    break;
                case BITWISE_AND:
                    operator_str = "&";
                    break;
                case BITWISE_OR:
                    operator_str = "|";
                    break;
                case BITWISE_XOR:
                    operator_str = "^";
                    break;
                case LEFT_SHIFT:
                    operator_str = "<<";
                    break;
                case RIGHT_SHIFT:
                    operator_str = ">>";
                    break;
                default:
                    operator_str = "invalid operator. wtf";
                    break;
            }

            ::std::string str("ArithmeticOrLogicalExpr: ");
            if (main_or_left_expr == NULL || right_expr == NULL) {
                str += "error. this is probably a parsing failure.";
            } else {
                str += "\n left: " + main_or_left_expr->as_string();
                str += "\n right: " + right_expr->as_string();
                str += "\n operator: " + operator_str;
            }

            return str;
        }

        ::std::string CallExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string LoopExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string ArrayExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string AwaitExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string BreakExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string IfExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string IfLetExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string LoopLabel::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string MatchExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string TupleExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string ExprStmtWithoutBlock::as_string() const {
            ::std::string str("ExprStmtWithoutBlock: \n ");

            if (expr == NULL) {
                str += "none (this shouldn't happen and is probably an error)";
            } else {
                str += expr->as_string();
            }

            return str;
        }

        ::std::string FunctionParam::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string FunctionQualifiers::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string TraitBound::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string MacroMatcher::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string LifetimeParam::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string MacroMatchFragment::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string QualifiedPathInType::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string MacroMatchRepetition::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string Lifetime::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string TypePath::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string TypeParam::as_string() const {
            return ::std::string("not implemented");
        }

        SimplePath PathPattern::convert_to_simple_path(bool with_opening_scope_resolution) const {
            if (!has_segments()) {
                return SimplePath::create_empty();
            }

            // create vector of reserved size (to minimise reallocations)
            ::std::vector<SimplePathSegment> simple_segments;
            simple_segments.reserve(segments.size());

            for (const auto& segment : segments) {
                // return empty path if doesn't meet simple path segment requirements
                if (segment.is_error() || segment.has_generic_args() || segment.as_string() == "Self") {
                    return SimplePath::create_empty();
                }

                // create segment and add to vector
                ::std::string segment_str = segment.as_string();
                simple_segments.push_back(SimplePathSegment(::std::move(segment_str)));
            }

            return SimplePath(::std::move(simple_segments), with_opening_scope_resolution);
        }

        SimplePath TypePath::as_simple_path() const {
            if (segments.empty()) {
                return SimplePath::create_empty();
            }

            // create vector of reserved size (to minimise reallocations)
            ::std::vector<SimplePathSegment> simple_segments;
            simple_segments.reserve(segments.size());

            for (const auto& segment : segments) {
                // return empty path if doesn't meet simple path segment requirements
                if (segment == NULL || segment->is_error() || !segment->is_ident_only() || segment->as_string() == "Self") {
                    return SimplePath::create_empty();
                }

                // create segment and add to vector
                ::std::string segment_str = segment->as_string();
                simple_segments.push_back(SimplePathSegment(::std::move(segment_str)));
            }

            return SimplePath(::std::move(simple_segments), has_opening_scope_resolution);
        }

        ::std::string PathExprSegment::as_string() const {
            ::std::string ident_str = segment_name.as_string();
            if (has_generic_args()) {
                ident_str += "::<" + generic_args.as_string() + ">";
            } 

            return ident_str;
        }

        ::std::string GenericArgs::as_string() const {
            // TODO: write GenericArgs as string
            return "not implemented";
        }

        ::std::string ForLoopExpr::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string RangePattern::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string SlicePattern::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string TuplePattern::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string StructPattern::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string LiteralPattern::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string ReferencePattern::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string IdentifierPattern::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string TupleStructPattern::as_string() const {
            return ::std::string("not implemented");
        }

        ::std::string LetStmt::as_string() const {
            return ::std::string("not implemented");
        }

        // Used to get outer attributes for expressions.
        ::std::string Expr::as_string() const {
            // outer attributes
            ::std::string str = "outer attributes: ";
            if (outer_attrs.empty()) {
                str += "none";
            } else {
                // note that this does not print them with "outer attribute" syntax - just the body
                for (const auto& attr : outer_attrs) {
                    str += "\n  " + attr.as_string();
                }
            }

            return str;
        }

        // hopefully definition here will prevent circular dependency issue
        TraitBound* TypePath::to_trait_bound(bool in_parens) const {
            // create clone FIXME is this required? or is copy constructor automatically called?
            TypePath copy(*this);
            return new TraitBound(::std::move(copy), in_parens);
        }
    }
}