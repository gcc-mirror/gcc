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
     * be stripped. TODO can this be const reference or does it have to mutate? */
    bool MacroExpander::check_cfg(AST::Attribute& attr) {}

    // Expands cfg_attr attributes.
    void MacroExpander::expand_cfg_attrs(std::vector<AST::Attribute>& attrs) {
        for (int i = 0; i < attrs.size (); ) {
            auto& attr = attrs[i];
            if (attr.get_path() == "cfg_attr") {
                if (attr.check_cfg_predicate (session)) {
                    // split off cfg_attr
                    std::vector<AST::Attribute> new_attrs = attr.separate_cfg_attrs ();

                    // remove attr from vector
                    attrs.erase (attrs.begin () + i);

                    // add new attrs to vector
                    attrs.insert (attrs.begin() + i, std::make_move_iterator (new_attrs.begin ()), std::make_move_iterator (new_attrs.end ()));
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
        attrs.shrink_to_fit ();
    }

    void MacroExpander::expand_crate() {
        /* fill macro/decorator map from init list? not sure where init list comes
         * from? */

        // expand crate cfg_attr attributes
        expand_cfg_attrs(crate.inner_attrs);

        // expand module attributes?

        // expand module tree recursively

        // post-process

        // extract exported macros?
    }
} // namespace Rust
