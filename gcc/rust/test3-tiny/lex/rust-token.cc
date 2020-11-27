#include "rust-token.h"

namespace Rust {
    // Hackily defined way to get token description for enum value using x-macros
    const char* get_token_description(TokenId id) {
        switch (id) {
#define RS_TOKEN(name, descr) \
    case name:                \
        return descr;
#define RS_TOKEN_KEYWORD(x, y) RS_TOKEN(x, y)
            RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD
#undef RS_TOKEN
            default:
                gcc_unreachable();
        }
    }

    // Hackily defined way to get token description as a string for enum value using x-macros
    const char* token_id_to_str(TokenId id) {
        switch (id) {
#define RS_TOKEN(name, _) \
    case name:            \
        return #name;
#define RS_TOKEN_KEYWORD(x, y) RS_TOKEN(x, y)
            RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD
#undef RS_TOKEN
            default:
                gcc_unreachable();
        }
    }
}