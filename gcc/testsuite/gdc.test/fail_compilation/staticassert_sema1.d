/*
TEST_OUTPUT:
---
fail_compilation/staticassert_sema1.d(17): Error: static assert:  "unsupported OS"
---
*/

// https://issues.dlang.org/show_bug.cgi?id=24645
// Test that a static assert(0) is not drowned out by subsequent import errors.

version(_NONEXISTENT_OS)
{

}
else
{
    static assert(0, msg);
}

import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;
import object: _NONEXISTENT;

enum msg = "unsupported OS";
