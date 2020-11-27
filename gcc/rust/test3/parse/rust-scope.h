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
#include <vector>

#include "rust-tree.h"
#include "rust-symbol.h"

// maybe split out scope into Symbol, SymbolMapping, and Scope headers

namespace Rust {
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
    }; // TODO: have multiple scopes (for modules, function, etc) at once?
}

#endif // RUST_SCOPE_H