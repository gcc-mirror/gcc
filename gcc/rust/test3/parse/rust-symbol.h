#ifndef RUST_SYMBOL_H
#define RUST_SYMBOL_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
// order: config, system, coretypes, tree

#include <string>
//#include <tr1/memory> // as shared_ptr is not available in std memory in c++03
// replaced with proper memory in c++11
#include <memory>

namespace Rust {
    // Kinds of symbols.
    enum SymbolKind { INVALID, VARIABLE, TYPENAME /*change to STRUCT*/, FUNCTION };
    // TODO: possibly add typedef, struct, union, "enum"

    /* TODO: apparently Rust's type symbol table is separate to its identifier symbol table, so have a
     * different symbol table for each? */

    /* The symbol table(s) will be generated as a pass over the AST (the first, probably) and each 
     * scope, including module namespacing, will be a separate symbol table (with earlier ones still
     * accessible but searched afterward). It will be preserved in later passes over the AST to type
     * check and maybe bind identifiers together or something. 
     * It may have to be preserved, at least partially, for GENERIC lowering. */

    // A symbol used for identifiers, etc. - TODO: extend to support namespacing (Rust paths?)
    struct Symbol {
      public:
        // Constructs a new symbol of name with no declaration tree set.
        Symbol(SymbolKind kind, const std::string& name_) :
          kind(kind), name(name_), decl(error_mark_node) {
            gcc_assert(name.size() > 0);
        }

        // Gets symbol's kind.
        SymbolKind get_kind() const {
            return kind;
        }

        // Gets symbol's name.
        std::string get_name() const {
            return name;
        }

        // Sets symbol's declaration tree.
        void set_tree_decl(Tree decl_) {
            // Ensure declaration tree is a variable or type declaration.
            gcc_assert((decl_.get_tree_code() == VAR_DECL) || (decl_.get_tree_code() == TYPE_DECL));
            decl = decl_;
        }

        // Gets tree with the location of variable declaration.
        Tree get_tree_decl() const {
            return decl;
        }

      private:
        // Symbol's kind.
        SymbolKind kind;
        // Symbol's name.
        std::string name;
        // Symbol's declaration tree.
        Tree decl;

        // Note: in other languages, other info about symbols would also be kept, e.g. "kind"
        // Also would be able to store more than just variable declaration trees.
    };

    // Symbol shared pointer.
    typedef std::shared_ptr<Symbol> SymbolPtr;
    // Const symbol shared pointer (i.e. to const Symbol).
    typedef std::shared_ptr<const Symbol> const_SymbolPtr;
}

#endif