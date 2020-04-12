#ifndef RUST_CODEPOINT_H
#define RUST_CODEPOINT_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
// config, system, coretypes - TODO: ensure all are needed

#include <string>

namespace Rust {
    struct Codepoint {
        uint32_t value;

        // Creates a zero codepoint.
        Codepoint() : value(0) {}

        // Creates a codepoint from UTF-8 value.
        Codepoint(uint32_t value_) : value(value_) {}

        // Returns a C++ string containing value of codepoint.
        ::std::string as_string();
    };
}

#endif