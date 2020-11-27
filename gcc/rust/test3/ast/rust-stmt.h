#ifndef RUST_AST_STATEMENT_H
#define RUST_AST_STATEMENT_H

#include "rust-ast.h"
#include "rust-path.h"
#include "rust-expr.h"

namespace Rust {
    namespace AST {
        // Just a semi-colon, which apparently is a statement.
        class EmptyStmt : public Stmt {
          public:
            ::std::string as_string() const {
                return ::std::string(1, ';');
            }

            EmptyStmt() {}

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual EmptyStmt* clone_stmt_impl() const OVERRIDE {
                return new EmptyStmt(*this);
            }
        };

        /* This is syntactically identical to declaring an item inside a module BUT it has block
         * scope. Type of "declaration statement" as it introduces new name into scope */
        /*class ItemStatement : public Statement {
            // TODO: put in same params as regular item
            // maybe even merge data structure with module item?

          public:
            ::std::string as_string() const;
        };*/
        // removed - just made item inherit from statement

        /* Variable assignment let statement - type of "declaration statement" as it introduces new
         * name into scope */
        class LetStmt : public Stmt {
            // bool has_outer_attrs;
            ::std::vector<Attribute> outer_attrs;

            // Pattern variables_pattern;
            ::std::unique_ptr<Pattern> variables_pattern;

            // bool has_type;
            // Type type;
            ::std::unique_ptr<Type> type;

            // bool has_init_expr;
            // Expr* init_expr;
            ::std::unique_ptr<Expr> init_expr;

          public:
            // Returns whether let statement has outer attributes.
            inline bool has_outer_attrs() const {
                return !outer_attrs.empty();
            }

            // Returns whether let statement has a given return type.
            inline bool has_type() const {
                return type != NULL;
            }

            // Returns whether let statement has an initialisation expression.
            inline bool has_init_expr() const {
                return init_expr != NULL;
            }

            /*~LetStatement() {
                if (has_init_expr) {
                    delete init_expr;
                }
            }*/

            ::std::string as_string() const;

            LetStmt(Pattern* variables_pattern, Expr* init_expr, Type* type,
              ::std::vector<Attribute> outer_attrs) :
              outer_attrs(::std::move(outer_attrs)),
              variables_pattern(variables_pattern), type(type), init_expr(init_expr) {}

            // Copy constructor with clone
            LetStmt(LetStmt const& other) :
              outer_attrs(other.outer_attrs),
              variables_pattern(other.variables_pattern->clone_pattern()),
              type(other.type->clone_type()), init_expr(other.init_expr->clone_expr()) {}

            // Destructor - define here if required

            // Overloaded assignment operator to clone
            LetStmt& operator=(LetStmt const& other) {
                variables_pattern = other.variables_pattern->clone_pattern();
                init_expr = other.init_expr->clone_expr();
                type = other.type->clone_type();
                outer_attrs = other.outer_attrs;

                return *this;
            }

            // move constructors
            LetStmt(LetStmt&& other) = default;
            LetStmt& operator=(LetStmt&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual LetStmt* clone_stmt_impl() const OVERRIDE {
                return new LetStmt(*this);
            }
        };

        // Abstract base class for expression statements (statements containing an expression)
        class ExprStmt : public Stmt {
            // TODO: add any useful virtual functions
        };

        /* Statement containing an expression without a block (or, due to technical difficulties, can
         * only be guaranteed to hold an expression). */
        class ExprStmtWithoutBlock : public ExprStmt {
            // ExprWithoutBlock* expr;
            /* HACK: cannot ensure type safety of ExprWithoutBlock due to Pratt parsing, so have to
             * store more general type of Expr. FIXME: fix this issue somehow or redesign AST. */
            //::std::unique_ptr<ExprWithoutBlock> expr;
            ::std::unique_ptr<Expr> expr;

          public:
            /*~ExpressionStatementWithoutBlock() {
                delete expr;
            }*/

            ::std::string as_string() const;

            // ExprStmtWithoutBlock(ExprWithoutBlock* expr) : expr(expr) {}
            ExprStmtWithoutBlock(Expr* expr) : expr(expr) {}

            // Copy constructor with clone
            ExprStmtWithoutBlock(ExprStmtWithoutBlock const& other) :
              expr(other.expr->clone_expr /*_without_block*/ ()) {}

            // Destructor - define here if required

            // Overloaded assignment operator to clone
            ExprStmtWithoutBlock& operator=(ExprStmtWithoutBlock const& other) {
                expr = other.expr->clone_expr /*_without_block*/ ();

                return *this;
            }

            // move constructors
            ExprStmtWithoutBlock(ExprStmtWithoutBlock&& other) = default;
            ExprStmtWithoutBlock& operator=(ExprStmtWithoutBlock&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual ExprStmtWithoutBlock* clone_stmt_impl() const OVERRIDE {
                return new ExprStmtWithoutBlock(*this);
            }
        };

        // Statement containing an expression with a block
        class ExprStmtWithBlock : public ExprStmt {
            // ExprWithBlock* expr;
            ::std::unique_ptr<ExprWithBlock> expr;

          public:
            /*~ExpressionStatementWithBlock() {
                delete expr;
            }*/

            ::std::string as_string() const;

            ExprStmtWithBlock(ExprWithBlock* expr) : expr(expr) {}

            // Copy constructor with clone
            ExprStmtWithBlock(ExprStmtWithBlock const& other) :
              expr(other.expr->clone_expr_with_block()) {}

            // Destructor - define here if required

            // Overloaded assignment operator to clone
            ExprStmtWithBlock& operator=(ExprStmtWithBlock const& other) {
                expr = other.expr->clone_expr_with_block();

                return *this;
            }

            // move constructors
            ExprStmtWithBlock(ExprStmtWithBlock&& other) = default;
            ExprStmtWithBlock& operator=(ExprStmtWithBlock&& other) = default;

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual ExprStmtWithBlock* clone_stmt_impl() const OVERRIDE {
                return new ExprStmtWithBlock(*this);
            }
        };

        // Replaced definition of MacroInvocationSemi with forward decl - defined in rust-macro.h
        class MacroInvocationSemi;
        /*class MacroInvocationSemi : public Statement {
          public:
            ::std::string as_string() const;
        };*/
    }
}

#endif