#include "rust-scope.h"

#include <utility> // for std::make_pair

namespace Rust {
    Scope::Scope() {}

    void SymbolMapping::insert(SymbolPtr s) {
        gcc_assert(s != NULL);
        std::pair<Map::iterator, bool> p = map.insert(std::make_pair(s->get_name(), s));

        gcc_assert(p.second);
    }

    SymbolPtr SymbolMapping::get(const std::string& str) const {
        Map::const_iterator it = map.find(str);
        if (it != map.end()) {
            return it->second;
        }

        return SymbolPtr();
    }

    SymbolPtr Scope::lookup(const std::string& str) {
        // Traverse stack from top (end of MapStack) to bottom, so use reverse_iterator.
        for (MapStack::reverse_iterator map = map_stack.rbegin(); map != map_stack.rend(); map++) {
            if (SymbolPtr sym = map->get(str)) {
                return sym;
            }
        }

        return SymbolPtr();
    }

    void Scope::push_scope() {
        map_stack.push_back(SymbolMapping());
    }

    void Scope::pop_scope() {
        gcc_assert(!map_stack.empty());
        map_stack.pop_back();
    }
}