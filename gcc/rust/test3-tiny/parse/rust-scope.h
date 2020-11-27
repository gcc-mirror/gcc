#ifndef RUST_SCOPE_H
#define RUST_SCOPE_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
// order: config, system, coretypes, tree
// may not all be required

#include <map>
#include <string>
#include <tr1/memory> // as shared_ptr is not available in std memory in c++03
#include <vector>

#include "rust-tree.h"

// maybe split out scope into Symbol, SymbolMapping, and Scope headers

namespace Rust {
    // Kinds of symbols.
    enum SymbolKind { INVALID, VARIABLE, TYPENAME };

    // A symbol used for identifiers, etc.
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
    typedef std::tr1::shared_ptr<Symbol> SymbolPtr;
    // Const symbol shared pointer (i.e. to const Symbol).
    typedef std::tr1::shared_ptr<const Symbol> const_SymbolPtr;

    // Map of strings (identifiers) to SymbolPtrs
    struct SymbolMapping {
      public:
        // Inserts a new Symbol into the map using its name as the key. Also checks name is unique.
        void insert(SymbolPtr s);
        // Returns the mapped Symbol for the given string. May return a null Symbol.
        SymbolPtr get(const std::string& str) const;

      private:
        typedef std::map<std::string, SymbolPtr> Map;
        // SymbolMapping's map.
        Map map;
    };

    // Scope class that holds mapping in it.
    class Scope {
      public:
        // Gets current mapping (created in last push that hasn't been popped yet).
        SymbolMapping& get_current_mapping() {
            gcc_assert(!map_stack.empty());
            return map_stack.back();
        }

        // Create new mapping.
        void push_scope();
        // Get rid of mapping?
        void pop_scope();

        Scope();

        // Get the last mapping for a given string (or null if no such mapping exists).
        SymbolPtr lookup(const std::string& str);

      private:
        typedef std::vector<SymbolMapping> MapStack;
        // Scope's MapStack.
        MapStack map_stack;
    };
}

#endif // RUST_SCOPE_H