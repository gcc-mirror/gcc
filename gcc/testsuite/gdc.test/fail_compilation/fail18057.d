/**
TEST_OUTPUT:
---
fail_compilation/fail18057.d(16): Error: template instance RBNode!int `RBNode` is not a template declaration, it is a struct
fail_compilation/fail18057.d(13): Error: variable fail18057.RBNode.copy recursive initialization of field
---
*/

// https://issues.dlang.org/show_bug.cgi?id=18057
// Recursive field initializer causes segfault.
struct RBNode
{
    RBNode *copy = new RBNode;
}

alias bug18057 = RBNode!int;
