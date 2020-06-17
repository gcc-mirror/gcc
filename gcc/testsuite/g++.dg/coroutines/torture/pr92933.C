//  { dg-do compile }

// Test that we compile the simple case described in PR 92933

#include "../coro.h"

#define RETURN_VOID
#include "../coro1-ret-int-yield-int.h"

struct some_error {};

coro1
foo() {
    try {
        co_return;
    } catch (some_error) {
    }
}
