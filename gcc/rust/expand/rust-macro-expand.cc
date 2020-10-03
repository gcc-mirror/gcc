#include "rust-macro-expand.h"
#include "rust-ast-full.h"
// is full really required?

namespace Rust {
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

    /* Determines whether cfg predicate is true and item with attribute should not
     * be stripped. */
    bool check_cfg_predicate() {}

    /* Determines whether cfg predicate is true and item with attribute should not
     * be stripped. */
    bool check_cfg(AST::Attribute& attr) {}

    // Expands cfg_attr attributes.
    void expand_attrs_cfgattr(std::vector<AST::Attribute>& attrs) {
        for (auto it = attrs.begin(); it != attrs.end();) {
            auto& attr = *it;
            if (attr.get_path() == "cfg_attr") {
                if (check_cfg(attr)) {
                }

                /* do something - if feature (first token in tree) is in fact enabled,
                 * make tokens listed afterwards into attributes. i.e.: for
                 * [cfg_attr(feature = "wow", wow1, wow2)], if "wow" is true, then add
                 * attributes [wow1] and [wow2] to attribute list. This can also be
                 * recursive, so check for expanded attributes being recursive and
                 * possibly recursively call the expand_attrs? */
            } else {
                ++it;
            }
        }
    }

    void MacroExpander::expand_crate() {
        /* fill macro/decorator map from init list? not sure where init list comes
         * from? */

        // expand crate attributes
        expand_attrs_cfgattr(crate.inner_attrs);

        // expand module attributes?

        // expand module tree recursively

        // post-process

        // extract exported macros?
    }
} // namespace Rust
